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
  cache_dir <- tempfile()
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  dat <- read_record_set(
    x,
    "classification",
    cache_dir = cache_dir,
    verbosity = 0L
  )
  expect_gt(nrow(dat), 0L)
  expect_true(is.factor(dat[["class"]]))
})
