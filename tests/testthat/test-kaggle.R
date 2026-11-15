# test-kaggle.R
# ::croissant::
# 2026- EDG rtemis.org

# spec: croissant/read-write#kaggle

test_that("a Kaggle dataset is named by its slug or its URL", {
  expect_identical(kaggle_dataset("heptapod/titanic"), "heptapod/titanic")
  expect_identical(
    kaggle_dataset("https://www.kaggle.com/datasets/heptapod/titanic"),
    "heptapod/titanic"
  )
  expect_identical(
    kaggle_dataset("https://kaggle.com/datasets/yasserh/titanic-dataset/data"),
    "yasserh/titanic-dataset"
  )
  expect_error(kaggle_dataset("titanic"), class = "rtemis_value_error")
  expect_error(
    kaggle_dataset("heptapod/titanic/data"),
    class = "rtemis_value_error"
  )
  expect_error(
    kaggle_dataset("https://www.kaggle.com/competitions/titanic"),
    class = "rtemis_value_error"
  )
})

test_that("Kaggle metadata reads into a Croissant object", {
  generated <- jsonlite::parse_json(write_croissant(Croissant(
    name = "Titanic",
    record_set = list(RecordSet(id = "r"))
  )))
  generated[["version"]] <- 1L
  requested <- NULL
  local_mocked_bindings(
    kaggle_get = function(url, dest) {
      requested <<- url
      jsonlite::write_json(generated, dest, auto_unbox = TRUE)
      NULL
    }
  )
  expect_message(
    x <- read_croissant_kaggle("https://www.kaggle.com/datasets/a/b"),
    "Read Titanic"
  )
  expect_identical(
    requested,
    "https://www.kaggle.com/datasets/a/b/croissant/download"
  )
  expect_identical(x@dataset_version, "1.0.0")
  expect_null(x@base)
})

test_that("a Kaggle response without Croissant metadata is an error", {
  local_mocked_bindings(
    kaggle_get = function(url, dest) {
      writeLines("<!DOCTYPE html><html></html>", dest)
      NULL
    }
  )
  expect_error(
    read_croissant_kaggle("uciml/iris", verbosity = 0L),
    "https://www.kaggle.com/datasets/uciml/iris",
    class = "croissant_download_error"
  )
  local_mocked_bindings(
    kaggle_get = function(url, dest) simpleError("HTTP status 404")
  )
  expect_error(
    read_croissant_kaggle("uciml/iris", verbosity = 0L),
    "no Croissant metadata for `uciml/iris`",
    class = "croissant_download_error"
  )
})

test_that("the metadata of a public Kaggle dataset reads from Kaggle", {
  skip_on_cran()
  skip_if_offline("www.kaggle.com")
  x <- read_croissant_kaggle("mlg-ulb/creditcardfraud", verbosity = 0L)
  expect_identical(first_text(x@name), "Credit Card Fraud Detection")
  expect_match(x@dataset_version, "^[0-9]+\\.0\\.0$")
  expect_identical(names(record_sets(x)), "creditcard.csv")
  archive <- x@distribution[[1L]]
  expect_match(archive@content_url, "^https://www.kaggle.com/api/v1/")
  expect_identical(nchar(checksum_hex(archive@md5, 16L)), 32L)
})

test_that("a file path in a Kaggle dataset resolves to its API download URL", {
  root <- "https://www.kaggle.com/api/v1/datasets/download/a/b"
  expect_identical(
    kaggle_file_url(root, "data/x y.parquet"),
    paste0(root, "/data%2Fx%20y.parquet")
  )
  expect_identical(
    kaggle_download_parts(paste0(root, "/data%2Fx.parquet")),
    list(root = root, path = "data/x.parquet")
  )
  expect_identical(
    kaggle_download_parts(paste0(root, "?datasetVersionNumber=1")),
    list(root = root, path = "")
  )
  expect_null(kaggle_download_parts("https://www.kaggle.com/datasets/a/b"))
  expect_identical(document_base(paste0(root, "/croissant.json")), root)
  expect_identical(
    document_base(paste0(root, "/meta%2Fcroissant.json")),
    paste0(root, "/meta")
  )
  expect_identical(
    kaggle_resolve(root, "data/x.parquet"),
    paste0(root, "/data%2Fx.parquet")
  )
  expect_identical(
    kaggle_resolve(paste0(root, "/meta"), "data/x.parquet"),
    paste0(root, "/meta%2Fdata%2Fx.parquet")
  )
})

test_that("a Croissant file stored in a Kaggle dataset reads with its records", {
  skip_on_cran()
  skip_if_offline("www.kaggle.com")
  skip_if_not_installed("arrow")
  x <- read_croissant_kaggle(
    "rtemisorg/rtemis-demo",
    file = "croissant.json",
    verbosity = 0L
  )
  expect_identical(
    x@base,
    "https://www.kaggle.com/api/v1/datasets/download/rtemisorg/rtemis-demo"
  )
  expect_setequal(names(record_sets(x)), c("classification", "regression"))
  cache_dir <- local_tempdir()
  dat <- read_record_set(
    x,
    "classification",
    cache_dir = cache_dir,
    verbosity = 0L
  )
  expect_gt(nrow(dat), 0L)
  expect_true(is.factor(dat[["class"]]))
})

test_that("the Kaggle token resolves as the Kaggle tools resolve it", {
  home <- local_tempdir()
  dir.create(file.path(home, ".kaggle"))
  local_envvar(HOME = home, KAGGLE_API_TOKEN = NA)
  expect_identical(resolve_kaggle_token(), "")
  writeLines("from-file\n", file.path(home, ".kaggle", "access_token.txt"))
  expect_identical(resolve_kaggle_token(), "from-file")
  writeLines("from-access-token", file.path(home, ".kaggle", "access_token"))
  expect_identical(resolve_kaggle_token(), "from-access-token")
  local_envvar(KAGGLE_API_TOKEN = "from-env")
  expect_identical(resolve_kaggle_token(), "from-env")
  token_file <- file.path(home, "token")
  writeLines("from-env-file", token_file)
  local_envvar(KAGGLE_API_TOKEN = token_file)
  expect_identical(resolve_kaggle_token(), "from-env-file")
  expect_identical(resolve_kaggle_token("given"), "given")
})

test_that("the Kaggle token is sent to the Kaggle API only", {
  api <- "https://www.kaggle.com/api/v1/datasets/download/a/b/x.csv"
  expect_identical(
    kaggle_headers(api, "t"),
    c(Authorization = "Bearer t")
  )
  expect_null(kaggle_headers(api, ""))
  expect_null(kaggle_headers("https://www.kaggle.com/datasets/a/b", "t"))
  expect_null(kaggle_headers("https://huggingface.co/x.csv", "t"))
  sent <- list()
  local_mocked_bindings(
    download_url = function(url, dest, headers = NULL) {
      sent[[url]] <<- headers
      writeLines("a\n1", dest)
      NULL
    }
  )
  ctx <- new_load_context(
    Croissant(name = "x"),
    NULL,
    local_tempdir(),
    0L,
    kaggle_token = "t"
  )
  download_cached(ctx, api, NULL, NULL)
  download_cached(ctx, "https://example.org/x.csv", NULL, NULL)
  expect_identical(sent[[api]], c(Authorization = "Bearer t"))
  expect_null(sent[["https://example.org/x.csv"]])
})

test_that("a stored Croissant file reads with the token and its base", {
  doc <- write_croissant(Croissant(name = "Stored"))
  requested <- NULL
  local_mocked_bindings(
    kaggle_get = function(url, dest, token = "") {
      requested <<- list(url = url, token = token)
      writeLines(doc, dest)
      NULL
    }
  )
  x <- read_croissant_kaggle(
    "a/b",
    file = "meta/croissant.json",
    token = "t",
    verbosity = 0L
  )
  expect_identical(
    requested,
    list(
      url = "https://www.kaggle.com/api/v1/datasets/download/a/b/meta%2Fcroissant.json",
      token = "t"
    )
  )
  expect_identical(
    x@base,
    "https://www.kaggle.com/api/v1/datasets/download/a/b/meta"
  )
  local_mocked_bindings(
    kaggle_get = function(url, dest, token = "") {
      writeLines("<!DOCTYPE html><html></html>", dest)
      NULL
    }
  )
  local_envvar(HOME = local_tempdir(), KAGGLE_API_TOKEN = NA)
  expect_error(
    read_croissant_kaggle("a/b", file = "croissant.json", verbosity = 0L),
    "A private dataset requires a Kaggle API token",
    class = "croissant_download_error"
  )
  expect_error(
    read_croissant_kaggle(
      "a/b",
      file = "croissant.json",
      token = "t",
      verbosity = 0L
    ),
    "the token may not grant access",
    class = "croissant_download_error"
  )
})

test_that("a private Kaggle dataset reads with the token", {
  skip_on_cran()
  skip_if_offline("www.kaggle.com")
  skip_if_not(
    nzchar(resolve_kaggle_token()),
    "no Kaggle API token with access to rtemisorg/croissant-private-test"
  )
  x <- read_croissant_kaggle(
    "rtemisorg/croissant-private-test",
    file = "croissant.json",
    verbosity = 0L
  )
  dat <- read_record_set(
    x,
    "default",
    cache_dir = local_tempdir(),
    verbosity = 0L
  )
  expect_identical(dat[["species"]], c("Adelie", "Gentoo", "Chinstrap"))
})
