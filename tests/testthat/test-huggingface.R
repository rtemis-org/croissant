# test-huggingface.R
# ::croissant::
# 2026- EDG rtemis.org

test_that("hf:// paths parse as rtemis parses them", {
  expect_identical(
    parse_hf("hf://datasets/ylecun/mnist"),
    list(repo = "ylecun/mnist", revision = "main", file = NULL)
  )
  expect_identical(
    parse_hf(
      "hf://datasets/ylecun/mnist@refs%2Fconvert%2Fparquet/mnist/test/0000.parquet"
    ),
    list(
      repo = "ylecun/mnist",
      revision = "refs/convert/parquet",
      file = "mnist/test/0000.parquet"
    )
  )
  expect_error(parse_hf("hf://models/x/y"), class = "rtemis_value_error")
  expect_error(parse_hf("hf://datasets/x/y/../z"), class = "rtemis_value_error")
  expect_true(is_hf("HF://datasets/x/y"))
  expect_false(is_hf("https://huggingface.co/datasets/x/y"))
})

test_that("Hub repository URLs in Croissant metadata resolve to repo and revision", {
  withr::local_envvar(HF_ENDPOINT = NA)
  expect_identical(
    hf_repo_url(
      "https://huggingface.co/datasets/ylecun/mnist/tree/refs%2Fconvert%2Fparquet"
    ),
    list(repo = "ylecun/mnist", revision = "refs/convert/parquet")
  )
  expect_identical(
    hf_repo_url("https://huggingface.co/datasets/ylecun/mnist/"),
    list(repo = "ylecun/mnist", revision = "main")
  )
  expect_null(hf_repo_url("https://example.org/datasets/a/b"))
  withr::local_envvar(HF_ENDPOINT = "ftp://hub")
  expect_error(hf_endpoint(), class = "rtemis_value_error")
})

test_that("a downloaded file is checked against size, LFS SHA-256, and git blob SHA-1", {
  path <- withr::local_tempfile()
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
  withr::local_envvar(HF_TOKEN = "secret")
  expect_identical(hf_token(), "secret")
  expect_identical(hf_token("given"), "given")
  withr::local_envvar(HF_TOKEN = NA)
  expect_identical(hf_token(), "")
})

test_that("Hub metadata and records read from the Hugging Face Hub", {
  skip_on_cran()
  skip_if_offline("huggingface.co")
  withr::local_options(croissant.cache_dir = withr::local_tempdir())
  withr::local_envvar(HF_ENDPOINT = NA)
  x <- read_croissant_hf("ylecun/mnist")
  expect_true(is_valid(validate_croissant(x)))
  tab <- read_record_set(x, "mnist", n_max = 5L, verbosity = 0L)
  expect_identical(nrow(tab), 5L)
  expect_identical(tab[["mnist/label"]], c(7L, 2L, 1L, 0L, 4L))
  expect_identical(unique(tab[["mnist/split"]]), "test")
  expect_type(tab[["mnist/image"]][[1L]], "raw")
  path <- hf_download(
    "hf://datasets/ylecun/mnist@refs%2Fconvert%2Fparquet/mnist/test/0000.parquet",
    verbosity = 0L
  )
  expect_true(file.exists(path))
  expect_match(
    attr(path, "hf"),
    "^hf://datasets/ylecun/mnist@[0-9a-f]{40}/mnist/test/0000.parquet$"
  )
})
