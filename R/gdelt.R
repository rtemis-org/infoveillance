# gdelt.R
# ::rtemis::
# 2026- EDG rtemis.org

utils::globalVariables(c("seendate", "series", "value", "norm", "bin", "count"))

gdelt_doc_url <- "https://api.gdeltproject.org/api/v2/doc/doc"

gdelt_modes <- c(
  "artlist",
  "timelinevol",
  "timelinevolraw",
  "timelinetone",
  "timelinelang",
  "timelinesourcecountry",
  "tonechart"
)

gdelt_sorts <- c("datedesc", "dateasc", "tonedesc", "toneasc", "hybridrel")

# GDELT stamps every date as "YYYYMMDDTHHMMSSZ" in UTC.
gdelt_datetime_format <- "%Y%m%dT%H%M%SZ"

# GDELT rejects more than one request every 5 seconds per client with HTTP 429.
gdelt_min_interval <- 5

# Time the last GDELT response was received, shared across calls in this session.
gdelt_state <- new.env(parent = emptyenv())


# %% query_gdelt ----
#' Query GDELT for news articles matching keywords
#'
#' Search the GDELT Global Knowledge Graph's DOC 2.0 API for online news
#' coverage matching one or more keywords, and return either the matching
#' articles or an aggregate timeline of coverage volume, tone, language, or
#' source country. Coverage begins on 2017-01-01 and is updated every 15
#' minutes; a query with no explicit window searches the last three months.
#'
#' Each keyword is one search term; a keyword containing whitespace is
#' searched as an exact phrase. With `match = "all"` every keyword must appear
#' in an article; with `match = "any"` at least one must. GDELT requires each
#' term to be at least three characters and rejects terms that are too common
#' to search on. Filters are combined with the keywords: an article must be in
#' one of `source_lang`, from one of `source_country`, published on one of
#' `domain`, tagged with one of `theme`, and have a tone within `tone`.
#'
#' `timespan` and `start_datetime`/`end_datetime` are two ways to set the
#' search window; supply one or the other. `timespan` is a number followed by
#' a unit: `min` (minutes), `h` (hours), `d` (days), `w` (weeks), or `m`
#' (months), e.g. `"24h"` or `"2w"`.
#'
#' GDELT accepts at most one request every five seconds per client.
#' `query_gdelt()` makes one request per call and waits as needed so that
#' repeated calls in the same session respect that limit; if GDELT still
#' rejects a request as too frequent, it is retried once after a pause.
#'
#' @param keywords Character: One or more keywords or phrases to search for.
#' @param match Character: `"all"` requires every keyword; `"any"` requires at
#'   least one.
#' @param mode Character: What to return. `"artlist"` returns matching
#'   articles; `"timelinevol"` the share of all monitored coverage matching the
#'   query over time; `"timelinevolraw"` the raw article counts together with
#'   the total monitored (`norm`); `"timelinetone"` average tone over time;
#'   `"timelinelang"` and `"timelinesourcecountry"` volume over time split by
#'   language or source country; `"tonechart"` a histogram of article tone.
#' @param timespan Character or NULL: Search window ending now, e.g. `"1w"`.
#'   Not used together with `start_datetime`/`end_datetime`.
#' @param start_datetime Date, POSIXct, or character or NULL: Start of the
#'   search window, in UTC. A character value must be `"YYYYMMDDHHMMSS"`.
#' @param end_datetime Date, POSIXct, or character or NULL: End of the search
#'   window, in UTC. A `Date` end covers the whole day.
#' @param source_lang Character or NULL: Language(s) of the source article, as
#'   a name (`"english"`) or ISO 639 code (`"eng"`).
#' @param source_country Character or NULL: Country/ies the source outlet is
#'   based in, as a name (`"unitedstates"`) or FIPS code (`"US"`).
#' @param domain Character or NULL: Source domain(s), e.g. `"nytimes.com"`.
#' @param theme Character or NULL: GDELT Global Knowledge Graph theme(s), e.g.
#'   `"TAX_DISEASE_CORONAVIRUS"`.
#' @param tone Character or NULL: Tone filter, a comparison such as `">5"`
#'   (positive) or `"<-5"` (negative).
#' @param max_records Integer: Maximum number of articles to return in
#'   `"artlist"` mode, at most `250L`. Ignored by other modes.
#' @param sort Character: Order of articles in `"artlist"` mode.
#' @param timeline_smooth Integer or NULL: Moving-average window (in time
#'   steps, up to `30L`) applied to timeline modes.
#' @param timeout Numeric: Seconds to wait for a response.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `data.table`. In `"artlist"` mode, one row per article with columns
#'   `url`, `url_mobile`, `title`, `seendate` (POSIXct, UTC), `socialimage`,
#'   `domain`, `language`, and `sourcecountry`. In timeline modes, one row per
#'   time step with columns `series`, `date` (POSIXct, UTC), `value`, and, for
#'   `"timelinevolraw"`, `norm`. In `"tonechart"` mode, one row per tone bin
#'   with columns `bin`, `count`, and `toparts` (a list of `data.frame`s of
#'   sample articles). A query with no matches returns an empty `data.table`
#'   with the same columns. The query string sent to GDELT and the full request
#'   URL are attached as attributes `"query"` and `"url"`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' # Articles from the last week mentioning both terms
#' query_gdelt(c("measles", "outbreak"), timespan = "1w")
#'
#' # Daily coverage volume mentioning either phrase, English sources only
#' query_gdelt(
#'   c("bird flu", "avian influenza"),
#'   match = "any",
#'   mode = "timelinevol",
#'   start_datetime = as.Date("2025-01-01"),
#'   end_datetime = as.Date("2025-03-31"),
#'   source_lang = "english"
#' )
#' }
query_gdelt <- function(
  keywords,
  match = c("all", "any"),
  mode = c(
    "artlist",
    "timelinevol",
    "timelinevolraw",
    "timelinetone",
    "timelinelang",
    "timelinesourcecountry",
    "tonechart"
  ),
  timespan = NULL,
  start_datetime = NULL,
  end_datetime = NULL,
  source_lang = NULL,
  source_country = NULL,
  domain = NULL,
  theme = NULL,
  tone = NULL,
  max_records = 250L,
  sort = c("datedesc", "dateasc", "tonedesc", "toneasc", "hybridrel"),
  timeline_smooth = NULL,
  timeout = 60,
  verbosity = 1L
) {
  match <- match.arg(match)
  mode <- match.arg(mode)
  sort <- match.arg(sort)
  check_bounded_integer_scalar(max_records, 1L, 250L)
  check_optional_bounded_integer_scalar(timeline_smooth, 1L, 30L)
  check_pos_double_scalar(timeout)
  check_integer_scalar(verbosity)

  query <- gdelt_build_query(
    keywords = keywords,
    match = match,
    source_lang = source_lang,
    source_country = source_country,
    domain = domain,
    theme = theme,
    tone = tone
  )
  params <- c(
    query = query,
    mode = mode,
    format = "json",
    gdelt_window_params(timespan, start_datetime, end_datetime),
    if (mode == "artlist") {
      c(maxrecords = as.character(max_records), sort = sort)
    },
    if (!is.null(timeline_smooth)) {
      c(timelinesmooth = as.character(timeline_smooth))
    }
  )
  url <- paste0(
    gdelt_doc_url,
    "?",
    paste0(names(params), "=", curl::curl_escape(params), collapse = "&")
  )
  info("Querying GDELT: ", query, verbosity = verbosity)
  res <- gdelt_fetch(url, timeout = timeout)
  out <- switch(
    mode,
    artlist = gdelt_parse_articles(res),
    tonechart = gdelt_parse_tonechart(res),
    gdelt_parse_timeline(res, mode = mode)
  )
  info(
    "Received ",
    nrow(out),
    if (mode == "artlist") " articles" else " rows",
    verbosity = verbosity
  )
  attr(out, "query") <- query
  attr(out, "url") <- url
  out
} # /infoveillance::query_gdelt


# %% gdelt_build_query ----
#' Build a GDELT DOC API query string
#'
#' @inheritParams query_gdelt
#'
#' @return Character: Query string, not URL-encoded.
#'
#' @keywords internal
#' @noRd
gdelt_build_query <- function(
  keywords,
  match = "all",
  source_lang = NULL,
  source_country = NULL,
  domain = NULL,
  theme = NULL,
  tone = NULL
) {
  check_character(keywords, allow_null = FALSE)
  keywords <- trimws(keywords)
  if (length(keywords) == 0L || any(is.na(keywords) | !nzchar(keywords))) {
    abort(
      "`keywords` must contain at least one keyword and no NA or empty strings.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (any(nchar(keywords) < 3L)) {
    abort(
      "GDELT requires each keyword to be at least 3 characters long. Received: ",
      paste(shQuote(keywords[nchar(keywords) < 3L]), collapse = ", "),
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_optional_character_scalar(tone)
  if (!is.null(tone) && !grepl("^[<>]=?-?[0-9]+(\\.[0-9]+)?$", tone)) {
    abort(
      "`tone` must be a comparison such as \">5\" or \"<-5\". Received: ",
      shQuote(tone),
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Quote phrases; leave single words and already-quoted terms as they are.
  needs_quote <- grepl("\\s", keywords) & !grepl("^\".*\"$", keywords)
  keywords[needs_quote] <- paste0("\"", keywords[needs_quote], "\"")
  terms <- if (match == "any" && length(keywords) > 1L) {
    paste0("(", paste(keywords, collapse = " OR "), ")")
  } else {
    paste(keywords, collapse = " ")
  }
  paste(
    c(
      terms,
      gdelt_filter("sourcelang", source_lang),
      gdelt_filter("sourcecountry", source_country),
      gdelt_filter("domain", domain),
      gdelt_filter("theme", theme),
      if (!is.null(tone)) paste0("tone", tone)
    ),
    collapse = " "
  )
} # /infoveillance::gdelt_build_query


# %% gdelt_filter ----
# One GDELT filter operator, OR-ed across values: `sourcelang:eng` or
# `(sourcelang:eng OR sourcelang:fra)`. NULL yields NULL so it drops out of a
# `c()` of filters.
gdelt_filter <- function(name, values) {
  if (is.null(values)) {
    return(NULL)
  }
  check_character(values, arg_name = name)
  values <- gsub("\\s", "", values)
  if (any(is.na(values) | !nzchar(values))) {
    abort(
      "`",
      name,
      "` must not contain NA or empty strings.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  terms <- paste0(name, ":", values)
  if (length(terms) == 1L) {
    terms
  } else {
    paste0("(", paste(terms, collapse = " OR "), ")")
  }
} # /infoveillance::gdelt_filter


# %% gdelt_window_params ----
# URL parameters for the search window, or NULL when GDELT's default applies.
gdelt_window_params <- function(timespan, start_datetime, end_datetime) {
  has_range <- !is.null(start_datetime) || !is.null(end_datetime)
  if (!is.null(timespan) && has_range) {
    abort(
      "Supply either `timespan` or `start_datetime`/`end_datetime`, not both.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(timespan)) {
    check_character_scalar(timespan)
    if (!grepl("^[0-9]+(min|h|d|w|m)$", timespan)) {
      abort(
        "`timespan` must be a number followed by one of \"min\", \"h\", \"d\", \"w\", \"m\", ",
        "e.g. \"24h\" or \"2w\". Received: ",
        shQuote(timespan),
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(c(timespan = timespan))
  }
  if (!has_range) {
    return(NULL)
  }
  start <- gdelt_format_datetime(start_datetime, end = FALSE)
  end <- gdelt_format_datetime(end_datetime, end = TRUE)
  if (!is.null(start) && !is.null(end) && start > end) {
    abort(
      "`start_datetime` must not be later than `end_datetime`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  c(startdatetime = start, enddatetime = end)
} # /infoveillance::gdelt_window_params


# %% gdelt_format_datetime ----
# "YYYYMMDDHHMMSS" in UTC from a Date, POSIXct, or already-formatted string.
# A Date is expanded to the start of the day, or its end when `end` is TRUE.
gdelt_format_datetime <- function(
  x,
  end = FALSE,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) != 1L || is.na(x)) {
    abort(
      "`",
      arg_name,
      "` must be a single non-NA value.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (inherits(x, "Date")) {
    return(paste0(format(x, "%Y%m%d"), if (end) "235959" else "000000"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.POSIXct(x), "%Y%m%d%H%M%S", tz = "UTC"))
  }
  if (is.character(x) && grepl("^[0-9]{14}$", x)) {
    return(x)
  }
  abort(
    "`",
    arg_name,
    "` must be a Date, POSIXct, or a \"YYYYMMDDHHMMSS\" string.",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /infoveillance::gdelt_format_datetime


# %% gdelt_fetch ----
# GET `url` and return the parsed JSON. GDELT reports query errors as HTTP 200
# with a plain-text body, so a body that is not JSON is surfaced as the error.
gdelt_fetch <- function(url, timeout = 60) {
  handle <- curl::new_handle(
    useragent = paste0(
      "infoveillance/",
      utils::packageVersion("infoveillance"),
      " (R)"
    ),
    timeout = as.integer(ceiling(timeout)),
    connecttimeout = as.integer(ceiling(timeout))
  )
  res <- gdelt_request(url, handle)
  # GDELT's rate limiter is stricter than the documented interval when the
  # server is slow; back off and retry before giving up.
  for (backoff in c(1, 2) * gdelt_min_interval) {
    if (res[["status_code"]] != 429L) {
      break
    }
    Sys.sleep(backoff)
    res <- gdelt_request(url, handle)
  }
  body <- rawToChar(res[["content"]])
  Encoding(body) <- "UTF-8"
  body <- trimws(body)
  if (res[["status_code"]] != 200L) {
    abort(
      "GDELT returned HTTP ",
      res[["status_code"]],
      if (nzchar(body)) paste0(": ", body),
      class = "infoveillance_gdelt_error"
    )
  }
  if (!nzchar(body)) {
    return(list())
  }
  tryCatch(
    jsonlite::fromJSON(body, simplifyVector = TRUE),
    error = function(e) {
      abort(
        "GDELT rejected the query: ",
        body,
        class = "infoveillance_gdelt_error"
      )
    }
  )
} # /infoveillance::gdelt_fetch


# %% gdelt_request ----
# One GET, sent no sooner than `gdelt_min_interval` seconds after the previous
# response in this session. GDELT counts the interval from when it answered,
# not from when the request arrived, so the timestamp is taken after the fetch.
gdelt_request <- function(url, handle) {
  last <- gdelt_state[["last_response"]]
  if (!is.null(last)) {
    wait <- gdelt_min_interval - as.numeric(Sys.time() - last, units = "secs")
    if (wait > 0) {
      Sys.sleep(wait)
    }
  }
  on.exit(gdelt_state[["last_response"]] <- Sys.time())
  tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) {
      abort(
        "GDELT request failed: ",
        conditionMessage(e),
        class = "infoveillance_gdelt_error",
        parent = e
      )
    }
  )
} # /infoveillance::gdelt_request


# %% gdelt_parse_articles ----
gdelt_parse_articles <- function(res) {
  cols <- c(
    "url",
    "url_mobile",
    "title",
    "seendate",
    "socialimage",
    "domain",
    "language",
    "sourcecountry"
  )
  articles <- res[["articles"]]
  if (length(articles) == 0L) {
    out <- as.data.table(
      stats::setNames(rep(list(character()), length(cols)), cols)
    )
    out[, seendate := as.POSIXct(character(), tz = "UTC")]
    return(out)
  }
  out <- as.data.table(articles)
  for (col in setdiff(cols, names(out))) {
    out[, (col) := NA_character_]
  }
  out <- out[, cols, with = FALSE]
  out[,
    seendate := as.POSIXct(seendate, format = gdelt_datetime_format, tz = "UTC")
  ]
  out[]
} # /infoveillance::gdelt_parse_articles


# %% gdelt_parse_timeline ----
# Timeline modes return a list of series, each a data.frame of (date, value)
# with an extra `norm` column in "timelinevolraw" mode. Single-series modes
# name their one series after the mode.
gdelt_parse_timeline <- function(res, mode) {
  cols <- c("series", "date", "value", if (mode == "timelinevolraw") "norm")
  timeline <- res[["timeline"]]
  if (length(timeline) == 0L) {
    out <- data.table(
      series = character(),
      date = as.POSIXct(character(), tz = "UTC"),
      value = numeric()
    )
    if (mode == "timelinevolraw") {
      out[, norm := numeric()]
    }
    return(out)
  }
  out <- rbindlist(
    lapply(seq_len(nrow(timeline)), function(i) {
      dt <- as.data.table(timeline[["data"]][[i]])
      dt[, series := timeline[["series"]][[i]]]
      dt
    }),
    fill = TRUE
  )
  out[, date := as.POSIXct(date, format = gdelt_datetime_format, tz = "UTC")]
  out[, value := as.numeric(value)]
  if (mode == "timelinevolraw") {
    out[, norm := as.numeric(norm)]
  }
  out[, cols, with = FALSE][]
} # /infoveillance::gdelt_parse_timeline


# %% gdelt_parse_tonechart ----
gdelt_parse_tonechart <- function(res) {
  tonechart <- res[["tonechart"]]
  if (length(tonechart) == 0L) {
    return(data.table(
      bin = integer(),
      count = integer(),
      toparts = list()
    ))
  }
  out <- as.data.table(tonechart)
  out[, bin := as.integer(bin)]
  out[, count := as.integer(count)]
  out[, intersect(c("bin", "count", "toparts"), names(out)), with = FALSE][]
} # /infoveillance::gdelt_parse_tonechart
