# test-read_record_set.R
# ::croissant::
# 2026- EDG rtemis.org

# A value in the form mlcroissant's output files write it.
record_value <- function(v) {
  if (is.null(v) || (length(v) == 1L && (is.na(v) || identical(v, "None")))) {
    return(NA_character_)
  }
  if (is.list(v)) {
    return(paste(vapply(v, record_value, character(1L)), collapse = "|"))
  }
  if (inherits(v, "POSIXct")) {
    return(format(v, "%Y-%m-%d %H:%M:%S"))
  }
  if (inherits(v, "Date")) {
    return(format(v))
  }
  if (is.numeric(v)) {
    return(format(v, digits = 15L))
  }
  if (grepl("^\\[.*\\]$", v)) {
    nums <- as.numeric(strsplit(gsub("[][ ]", "", v), ",")[[1L]])
    return(paste(vapply(nums, record_value, character(1L)), collapse = "|"))
  }
  num <- suppressWarnings(as.numeric(v))
  if (!is.na(num) && grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", v)) {
    return(format(num, digits = 15L))
  }
  as.character(v)
}

# Records of mlcroissant's output file, nested subfields flattened.
expected_records <- function(path) {
  lapply(readLines(path, warn = FALSE), function(line) {
    r <- jsonlite::parse_json(line)
    out <- list()
    for (k in names(r)) {
      v <- r[[k]]
      if (is.list(v) && !is.null(names(v))) out <- c(out, v) else out[[k]] <- v
    }
    out
  })
}

# Image, audio, and DICOM content appear in mlcroissant's output as Python
# reprs; read_record_set() returns file paths for them.
is_media_repr <- function(v) {
  is.character(v) && length(v) == 1L && grepl("^(<PIL|<BINARY|\\(array)", v)
}

expect_records <- function(tab, expected, label) {
  expect_gte(nrow(tab), length(expected), label = label)
  for (i in seq_along(expected)) {
    for (k in names(expected[[i]])) {
      e <- expected[[i]][[k]]
      if (is_media_repr(e)) {
        next
      }
      expect_true(k %in% names(tab), label = paste(label, k))
      expect_identical(
        record_value(tab[[k]][[i]]),
        record_value(e),
        label = paste0(label, " row ", i, " ", k)
      )
    }
  }
}

dataset_path <- function(...) test_path("fixtures", "datasets", ...)

test_that("records match mlcroissant on its hermetic datasets and titanic", {
  local_options(croissant.cache_dir = local_tempdir())
  cases <- list(
    c("1.0", "audio_test", "metadata.json", "records"),
    c("1.0", "pass-mini", "metadata.json", "images"),
    c("1.0", "recipes", "file_object_in_zip.json", "csv1"),
    c("1.0", "recipes", "file_object_in_zip.json", "csv2"),
    c(
      "1.0",
      "recipes",
      "read_binary_file_by_line.json",
      "translations_from_directory"
    ),
    c(
      "1.0",
      "recipes",
      "read_binary_file_by_line.json",
      "translations_from_zip"
    ),
    c(
      "1.0",
      "recipes",
      "read_from_directory.json",
      "read_from_directory_example"
    ),
    c("1.0", "recipes", "read_from_tar.json", "images_with_annotations"),
    c("1.0", "simple-join", "metadata.json", "publications_by_user"),
    c("1.0", "simple-parquet", "metadata.json", "persons"),
    c("1.0", "coco2014-mini", "metadata.json", "bounding_boxes"),
    c("1.0", "coco2014-mini", "metadata.json", "captions"),
    c("1.0", "coco2014-mini", "metadata.json", "images"),
    c("1.0", "coco2014-mini", "metadata.json", "split_enums"),
    c("1.0", "simple-split", "metadata.json", "data"),
    c("1.0", "json-join", "metadata.json", "items_with_prices"),
    c("1.0", "titanic", "metadata.json", "passengers"),
    c("1.0", "titanic", "metadata.json", "genders"),
    c("1.0", "titanic", "metadata.json", "embarkation_ports"),
    c("1.1", "recipes", "minimal_multilingual.json", "examples")
  )
  for (cs in cases) {
    label <- paste(cs, collapse = " ")
    if (cs[[2L]] == "simple-parquet") {
      skip_if_not_installed("arrow")
    }
    tab <- read_record_set(
      dataset_path(cs[[1L]], cs[[2L]], cs[[3L]]),
      cs[[4L]],
      verbosity = 0L
    )
    expected <- expected_records(
      dataset_path(cs[[1L]], cs[[2L]], "output", paste0(cs[[4L]], ".jsonl"))
    )
    # mlcroissant's test filters this record set to the train split.
    if (cs[[2L]] == "simple-split") {
      tab <- tab[tab[["data/split"]] == "train"]
    }
    # The two tar archives of pass-mini are extracted to directories whose
    # names decide mlcroissant's order; records compare by hash.
    if (cs[[2L]] == "pass-mini") {
      tab <- tab[order(tab[["images/hash"]])]
      expected <- expected[order(vapply(expected, \(r) r[["images/hash"]], ""))]
    }
    expect_records(tab, expected, label)
  }
})

test_that("values are cast by data type", {
  local_options(croissant.cache_dir = local_tempdir())
  tab <- read_record_set(
    dataset_path("1.0", "titanic", "metadata.json"),
    "passengers"
  )
  expect_type(tab[["passengers/survived"]], "integer")
  expect_type(tab[["passengers/name"]], "character")
  images <- read_record_set(
    dataset_path("1.0", "pass-mini", "metadata.json"),
    "images"
  )
  expect_s3_class(images[["images/date_taken"]], "POSIXct")
  expect_type(images[["images/latitude"]], "double")
  expect_true(all(file.exists(images[["images/image_content"]])))
  boxes <- read_record_set(
    dataset_path("1.0", "coco2014-mini", "metadata.json"),
    "bounding_boxes"
  )
  expect_type(boxes[["bounding_boxes/bbox"]], "list")
  expect_length(boxes[["bounding_boxes/bbox"]][[1L]], 4L)
})

test_that("output selects the table class", {
  local_options(croissant.cache_dir = local_tempdir())
  path <- dataset_path("1.0", "titanic", "metadata.json")
  expect_s3_class(read_record_set(path, "genders"), "data.table")
  df <- read_record_set(path, "genders", output = "data.frame")
  expect_identical(class(df), "data.frame")
  skip_if_not_installed("tibble")
  expect_s3_class(read_record_set(path, "genders", output = "tibble"), "tbl_df")
  skip_if_not_installed("arrow")
  expect_s3_class(read_record_set(path, "genders", output = "arrow"), "Table")
})

test_that("a Croissant object reads with base, and its record set by name", {
  local_options(croissant.cache_dir = local_tempdir())
  dir <- dataset_path("1.0", "titanic")
  x <- read_croissant(file.path(dir, "metadata.json"))
  tab <- read_record_set(x, "genders", base = dir)
  expect_identical(tab[["genders/label"]], c("male", "female"))
  expect_error(
    read_record_set(x, "genders", base = tempdir()),
    class = "croissant_file_error"
  )
  expect_error(
    read_record_set(x, "nope", base = dir),
    class = "rtemis_value_error"
  )
})

test_that("a downloaded file is verified against its sha256 and cached", {
  cache <- local_tempdir()
  src <- local_tempfile(fileext = ".csv")
  writeLines(c("a", "1"), src)
  url <- paste0("file://", normalizePath(src))
  sha <- sha256_file(src)
  ctx <- new_load_context(Croissant(), NULL, cache, 0L)
  path <- download_cached(ctx, url, sha, NULL)
  expect_true(file.exists(path))
  expect_identical(download_cached(ctx, url, sha, NULL), path)
  expect_error(
    download_cached(ctx, url, strrep("0", 64L), NULL),
    class = "croissant_checksum_error"
  )
})

test_that("globs match as Python's fnmatch does", {
  expect_true(grepl(glob_to_regex("*.jpg"), "train2014/a.jpg", perl = TRUE))
  expect_true(grepl(
    glob_to_regex("*/img?.jpg"),
    "training/img1.jpg",
    perl = TRUE
  ))
  expect_false(grepl(glob_to_regex("*.jpg"), "a.jpeg", perl = TRUE))
  expect_true(grepl(glob_to_regex("data/[ab].txt"), "data/a.txt", perl = TRUE))
})

test_that("JSONPath evaluates keys, indices, wildcards, and descent", {
  doc <- list(items = list(list(id = "a", n = 1L), list(id = "b", n = 2L)))
  expect_identical(unlist(jsonpath(doc, "$.items[*].id")), c("a", "b"))
  expect_identical(unlist(jsonpath(doc, "$.items[-1].n")), 2L)
  expect_identical(unlist(jsonpath(doc, "$..id")), c("a", "b"))
  expect_identical(unlist(jsonpath(doc, "$['items'][0]['id']")), "a")
  expect_error(
    jsonpath(doc, "$.items[?(@.n > 1)]"),
    class = "croissant_jsonpath_error"
  )
})
