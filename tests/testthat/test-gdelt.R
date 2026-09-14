# test-gdelt.R

test_that("gdelt_build_query joins keywords and quotes phrases", {
  expect_equal(gdelt_build_query("measles"), "measles")
  expect_equal(gdelt_build_query(c("measles", "outbreak")), "measles outbreak")
  expect_equal(gdelt_build_query("bird flu"), "\"bird flu\"")
  expect_equal(gdelt_build_query("\"bird flu\""), "\"bird flu\"")
  expect_equal(
    gdelt_build_query(c("bird flu", "avian influenza"), match = "any"),
    "(\"bird flu\" OR \"avian influenza\")"
  )
  expect_equal(gdelt_build_query("measles", match = "any"), "measles")
})


test_that("gdelt_build_query appends filters", {
  expect_equal(
    gdelt_build_query("measles", source_lang = "english"),
    "measles sourcelang:english"
  )
  expect_equal(
    gdelt_build_query(
      "measles",
      source_lang = c("eng", "fra"),
      source_country = "US",
      domain = "nytimes.com",
      theme = "TAX_DISEASE_MEASLES",
      tone = "<-5"
    ),
    paste(
      "measles (sourcelang:eng OR sourcelang:fra) sourcecountry:US",
      "domain:nytimes.com theme:TAX_DISEASE_MEASLES tone<-5"
    )
  )
})


test_that("gdelt_build_query rejects bad input", {
  expect_error(gdelt_build_query(NULL), class = "rtemis_input_error")
  expect_error(gdelt_build_query(character()), class = "rtemis_input_error")
  expect_error(
    gdelt_build_query(c("measles", "")),
    class = "rtemis_input_error"
  )
  expect_error(gdelt_build_query(NA_character_), class = "rtemis_input_error")
  expect_error(gdelt_build_query("ab"), class = "rtemis_input_error")
  expect_error(gdelt_build_query(1L), class = "rtemis_input_error")
  expect_error(
    gdelt_build_query("measles", tone = "5"),
    class = "rtemis_input_error"
  )
  expect_error(
    gdelt_build_query("measles", domain = ""),
    class = "rtemis_input_error"
  )
})


test_that("gdelt_window_params handles timespan and date ranges", {
  expect_null(gdelt_window_params(NULL, NULL, NULL))
  expect_equal(gdelt_window_params("1w", NULL, NULL), c(timespan = "1w"))
  expect_equal(gdelt_window_params("15min", NULL, NULL), c(timespan = "15min"))
  expect_equal(
    gdelt_window_params(NULL, as.Date("2025-01-01"), as.Date("2025-01-31")),
    c(startdatetime = "20250101000000", enddatetime = "20250131235959")
  )
  expect_equal(
    gdelt_window_params(NULL, "20250101120000", NULL),
    c(startdatetime = "20250101120000")
  )
  expect_equal(
    gdelt_window_params(
      NULL,
      NULL,
      as.POSIXct("2025-01-01 12:30:00", tz = "UTC")
    ),
    c(enddatetime = "20250101123000")
  )
  expect_error(
    gdelt_window_params("1w", as.Date("2025-01-01"), NULL),
    class = "rtemis_input_error"
  )
  expect_error(
    gdelt_window_params("1 week", NULL, NULL),
    class = "rtemis_input_error"
  )
  expect_error(
    gdelt_window_params("1y", NULL, NULL),
    class = "rtemis_input_error"
  )
  expect_error(
    gdelt_window_params(NULL, as.Date("2025-02-01"), as.Date("2025-01-01")),
    class = "rtemis_input_error"
  )
  expect_error(
    gdelt_window_params(NULL, "2025-01-01", NULL),
    class = "rtemis_input_error"
  )
})


test_that("gdelt_parse_articles returns a typed data.table", {
  res <- list(
    articles = data.frame(
      url = "https://example.com/a",
      url_mobile = "",
      title = "Measles outbreak",
      seendate = "20250102T153000Z",
      socialimage = "",
      domain = "example.com",
      language = "English",
      sourcecountry = "United States"
    )
  )
  out <- gdelt_parse_articles(res)
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 1L)
  expect_s3_class(out[["seendate"]], "POSIXct")
  expect_equal(
    format(out[["seendate"]], "%Y-%m-%d %H:%M", tz = "UTC"),
    "2025-01-02 15:30"
  )

  empty <- gdelt_parse_articles(list())
  expect_s3_class(empty, "data.table")
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), names(out))
  expect_s3_class(empty[["seendate"]], "POSIXct")
})


test_that("gdelt_parse_timeline stacks series", {
  res <- list(
    timeline = data.frame(
      series = c("English", "French"),
      data = I(list(
        data.frame(
          date = c("20250101T000000Z", "20250102T000000Z"),
          value = c(1, 2)
        ),
        data.frame(date = "20250101T000000Z", value = 3)
      ))
    )
  )
  out <- gdelt_parse_timeline(res, mode = "timelinelang")
  expect_s3_class(out, "data.table")
  expect_equal(names(out), c("series", "date", "value"))
  expect_equal(nrow(out), 3L)
  expect_equal(out[["series"]], c("English", "English", "French"))
  expect_s3_class(out[["date"]], "POSIXct")
  expect_type(out[["value"]], "double")

  raw <- list(
    timeline = data.frame(
      series = "Article Count",
      data = I(list(data.frame(
        date = "20250101T000000Z",
        value = 5L,
        norm = 100L
      )))
    )
  )
  out_raw <- gdelt_parse_timeline(raw, mode = "timelinevolraw")
  expect_equal(names(out_raw), c("series", "date", "value", "norm"))

  empty <- gdelt_parse_timeline(list(), mode = "timelinevolraw")
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), c("series", "date", "value", "norm"))
})


test_that("gdelt_parse_tonechart returns bins", {
  res <- list(
    tonechart = data.frame(
      bin = c(-2L, -1L),
      count = c(3L, 5L),
      toparts = I(list(
        data.frame(url = "a", title = "b"),
        data.frame(url = "c", title = "d")
      ))
    )
  )
  out <- gdelt_parse_tonechart(res)
  expect_equal(names(out), c("bin", "count", "toparts"))
  expect_type(out[["toparts"]], "list")
  expect_equal(nrow(gdelt_parse_tonechart(list())), 0L)
})


test_that("query_gdelt validates arguments before making a request", {
  expect_error(
    query_gdelt("measles", max_records = 0L),
    class = "rtemis_input_error"
  )
  expect_error(
    query_gdelt("measles", max_records = 251L),
    class = "rtemis_input_error"
  )
  expect_error(
    query_gdelt("measles", timeline_smooth = 31L),
    class = "rtemis_input_error"
  )
  expect_error(query_gdelt("measles", mode = "nope"))
  expect_error(
    query_gdelt(
      "measles",
      timespan = "1w",
      start_datetime = as.Date("2025-01-01")
    ),
    class = "rtemis_input_error"
  )
})


test_that("gdelt_request paces requests", {
  gdelt_state[["last_response"]] <- Sys.time()
  # A request that fails to resolve still waits out the interval first.
  t0 <- Sys.time()
  expect_error(
    gdelt_request("http://localhost:9/", curl::new_handle(timeout = 1L)),
    class = "infoveillance_gdelt_error"
  )
  expect_true(
    as.numeric(Sys.time() - t0, units = "secs") >= gdelt_min_interval - 0.5
  )
  rm("last_response", envir = gdelt_state)
})


test_that("query_gdelt retrieves articles and timelines", {
  skip_on_cran()
  # GDELT rate-limits per IP, and CI runners share theirs.
  skip_on_ci()
  skip_if_offline("api.gdeltproject.org")

  out <- query_gdelt(
    "measles",
    timespan = "1w",
    max_records = 5L,
    verbosity = 0L
  )
  expect_s3_class(out, "data.table")
  expect_equal(attr(out, "query"), "measles")
  expect_true(startsWith(attr(out, "url"), gdelt_doc_url))
  expect_true(nrow(out) <= 5L)
  expect_true(all(
    c("url", "title", "seendate", "domain", "language", "sourcecountry") %in%
      names(out)
  ))

  tl <- query_gdelt(
    c("bird flu", "avian influenza"),
    match = "any",
    mode = "timelinevol",
    timespan = "1w",
    verbosity = 0L
  )
  expect_equal(names(tl), c("series", "date", "value"))
  expect_true(nrow(tl) > 0L)
})
