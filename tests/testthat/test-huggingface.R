# test-huggingface.R
# ::croissant::
# 2026- EDG rtemis.org

test_that("hf:// paths parse as rtemis parses them", {
  expect_identical(
    parse_hf("hf://datasets/rtemis/demo"),
    list(repo = "rtemis/demo", revision = "main", file = NULL)
  )
  expect_identical(
    parse_hf("hf://datasets/rtemis/demo@v1.0.0/data/classification.parquet"),
    list(
      repo = "rtemis/demo",
      revision = "v1.0.0",
      file = "data/classification.parquet"
    )
  )
  expect_identical(
    parse_hf(
      "hf://datasets/rtemis/demo@refs%2Fconvert%2Fparquet/classification/train/0000.parquet"
    )[["revision"]],
    "refs/convert/parquet"
  )
  expect_error(parse_hf("hf://models/x/y"), class = "rtemis_value_error")
  expect_error(parse_hf("hf://datasets/x/y/../z"), class = "rtemis_value_error")
  expect_true(is_hf("HF://datasets/x/y"))
  expect_false(is_hf("https://huggingface.co/datasets/x/y"))
})

test_that("Hub repository URLs in Croissant metadata resolve to repo and revision", {
  local_envvar(HF_ENDPOINT = NA)
  expect_identical(
    hf_repo_url(
      "https://huggingface.co/datasets/rtemis/demo/tree/refs%2Fconvert%2Fparquet"
    ),
    list(repo = "rtemis/demo", revision = "refs/convert/parquet")
  )
  expect_identical(
    hf_repo_url("https://huggingface.co/datasets/rtemis/demo/"),
    list(repo = "rtemis/demo", revision = "main")
  )
  expect_null(hf_repo_url("https://example.org/datasets/a/b"))
  local_envvar(HF_ENDPOINT = "ftp://hub")
  expect_error(hf_endpoint(), class = "rtemis_value_error")
})

test_that("a downloaded file is checked against size, LFS SHA-256, and git blob SHA-1", {
  path <- local_tempfile()
  writeBin(charToRaw("hello\n"), path)
  # `git hash-object` of "hello\n".
  blob <- "ce013625030ba8dba906f756967f9e9ca394464a"
  listed <- function(size = 6, lfs = NA_character_, blob_id = NA_character_) {
    data.frame(size = size, lfs_sha256 = lfs, blob_id = blob_id)
  }
  expect_null(hf_verify(path, listed(blob_id = blob)))
  expect_null(hf_verify(path, listed(lfs = sha256_file(path))))
  expect_match(hf_verify(path, listed(size = 7))[["actual"]], "^size")
  expect_match(
    hf_verify(path, listed(lfs = strrep("0", 64L)))[["actual"]],
    "^SHA-256"
  )
  expect_match(
    hf_verify(path, listed(blob_id = strrep("0", 40L)))[["actual"]],
    "^git blob SHA-1"
  )
})

test_that("a token is read from HF_TOKEN", {
  local_envvar(HF_TOKEN = "secret")
  expect_identical(hf_token(), "secret")
  expect_identical(hf_token("given"), "given")
  local_envvar(HF_TOKEN = NA)
  expect_identical(hf_token(), "")
})

test_that("the metadata file of rtemis/demo reads factors from the Hub", {
  skip_on_cran()
  skip_if_offline("huggingface.co")
  local_options(croissant.cache_dir = local_tempdir())
  local_envvar(HF_ENDPOINT = NA)
  path <- "hf://datasets/rtemis/demo@v1.0.0/croissant.json"
  x <- read_croissant(path, verbosity = 0L)
  expect_identical(nrow(issues(validate_croissant(x))), 0L)
  expect_identical(names(record_sets(x)), c("regression", "classification"))
  expect_length(enumerations(x), 11L)
  tab <- read_record_set(path, "classification", verbosity = 0L)
  expect_identical(dim(tab), c(1000L, 101L))
  expect_identical(levels(tab[["class"]]), c("no", "yes"))
  expect_identical(levels(tab[["x39"]]), c("A", "B", "C", "D", "E"))
  pinned <- "hf://datasets/rtemis/demo@53a1ec8/data/classification.parquet"
  cache_dir <- local_tempdir()
  expect_message(
    file <- hf_download(
      "hf://datasets/rtemis/demo@v1.0.0/data/classification.parquet",
      cache_dir = cache_dir
    ),
    paste0("Downloading ", pinned, " (690.7 kB)"),
    fixed = TRUE
  )
  expect_true(file.exists(file))
  expect_message(
    hf_download(pinned, cache_dir = cache_dir),
    paste("Reading cached", pinned),
    fixed = TRUE
  )
  expect_identical(
    attr(file, "hf"),
    paste0(
      "hf://datasets/rtemis/demo@53a1ec8877d38aa522f243ff03e1becc027ed6c0/",
      "data/classification.parquet"
    )
  )
})

test_that("Hub-generated metadata of rtemis/demo reads its records", {
  skip_on_cran()
  skip_if_offline("huggingface.co")
  local_options(croissant.cache_dir = local_tempdir())
  local_envvar(HF_ENDPOINT = NA)
  x <- read_croissant_hf("rtemis/demo", verbosity = 0L)
  expect_true(is_valid(validate_croissant(x)))
  expect_setequal(names(record_sets(x)), c("regression", "classification"))
  expect_setequal(
    names(enumerations(x)),
    c("regression_splits", "classification_splits")
  )
  tab <- read_record_set(x, "classification", n_max = 5L, verbosity = 0L)
  expect_identical(nrow(tab), 5L)
  expect_identical(names(tab)[c(1L, 2L, 102L)], c("split", "x1", "class"))
  expect_identical(tab[["class"]], c("yes", "no", "yes", "yes", "no"))
  expect_identical(unique(tab[["split"]]), "train")
  path <- hf_download(
    "hf://datasets/rtemis/demo@refs%2Fconvert%2Fparquet/classification/train/0000.parquet",
    verbosity = 0L
  )
  expect_match(
    attr(path, "hf"),
    "^hf://datasets/rtemis/demo@[0-9a-f]{40}/classification/train/0000.parquet$"
  )
})

test_that("a Hub response without Croissant metadata names the repo's own file", {
  stub <- list(
    `@context` = "https://schema.org",
    `@type` = "Dataset",
    name = "demo"
  )
  local_mocked_bindings(
    hf_json = function(url, token, what) stub,
    hf_revision = function(repo, revision, token) {
      list(files = data.frame(rfilename = c("README.md", "croissant.json")))
    }
  )
  expect_error(
    read_croissant_hf("rtemis/demo", verbosity = 0L),
    'read_croissant\\("hf://datasets/rtemis/demo/croissant.json"\\)',
    class = "croissant_hf_metadata_error"
  )
  expect_error(
    read_croissant("hf://datasets/rtemis/demo", verbosity = 0L),
    "has no Croissant metadata for `rtemis/demo` yet",
    class = "croissant_hf_metadata_error"
  )
  local_mocked_bindings(
    hf_revision = function(repo, revision, token) stop("offline")
  )
  expect_error(
    read_croissant_hf("rtemis/demo", verbosity = 0L),
    "Retry when the dataset viewer shows the data",
    class = "croissant_hf_metadata_error"
  )
})
