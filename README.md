[![r-ci](https://github.com/rtemis-org/infoveillance/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/infoveillance/actions/workflows/r-ci.yml) [![infoveillance status badge](https://rtemis-org.r-universe.dev/infoveillance/badges/version)](https://rtemis-org.r-universe.dev/infoveillance)

# infoveillance: Web-based data for public health surveillance

Collect web-based data to track public health trends, behaviors, and disease
outbreaks in real time. Part of the [rtemis](https://www.rtemis.org) ecosystem.

## Data sources

- **GDELT** ([DOC 2.0 API](https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/)):
  `query_gdelt()` searches global online news by keyword and returns matching
  articles, coverage timelines (volume, tone, language, source country), or a
  tone histogram as a `data.table`.

```r
library(infoveillance)

# Articles from the last week mentioning both terms
query_gdelt(c("measles", "outbreak"), timespan = "1w")

# Daily coverage volume mentioning either phrase, English-language sources only
query_gdelt(
  c("bird flu", "avian influenza"),
  match = "any",
  mode = "timelinevol",
  start_datetime = as.Date("2025-01-01"),
  end_datetime = as.Date("2025-03-31"),
  source_lang = "english"
)
```

## Installation

### Latest version from `r-universe`

```r
pak::repo_add(rtemis = "https://rtemis-org.r-universe.dev")
pak::pak("infoveillance")
```

or using `install.packages`:

```r
install.packages(
  "infoveillance",
  repos = c("https://rtemis-org.r-universe.dev", "https://cloud.r-project.org")
)
```

### Development version from GitHub

```r
pak::pak("rtemis-org/infoveillance")
```
