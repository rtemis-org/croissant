# test-croissant_from_data.R
# ::croissant::
# 2026- EDG rtemis.org

typed_table <- function() {
  data.frame(
    i = 1:3,
    d = c(1.5, NA, 3.25),
    b = c(TRUE, FALSE, NA),
    s = c("a", NA, "c"),
    f = factor(c("u", "v", "u"), levels = c("v", "u", "w")),
    dt = as.Date("2024-01-01") + 0:2,
    ts = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * 0:2,
    `my col` = 7:9,
    check.names = FALSE
  )
}

round_trip <- function(x, format, version = "1.1") {
  dir <- local_tempdir(envir = parent.frame())
  m <- croissant_from_data(
    x,
    file.path(dir, paste0("data.", format)),
    name = "t",
    format = format,
    version = version,
    license = "CC0-1.0",
    cite_as = "Cite.",
    dataset_version = "1.0.0",
    date_published = "2026-10-27"
  )
  meta <- file.path(dir, "metadata.json")
  write_croissant(m, meta)
  list(metadata = m, path = meta)
}

test_that("CSV and Parquet metadata validate and read back the table", {
  x <- typed_table()
  expected <- x
  names(expected) <- c("i", "d", "b", "s", "f", "dt", "ts", "my_col")
  formats <- "csv"
  if (requireNamespace("arrow", quietly = TRUE)) {
    formats <- c(formats, "parquet")
  }
  for (format in formats) {
    for (version in c("1.0", "1.1")) {
      rt <- round_trip(x, format, version)
      issues <- validate_croissant(rt[["path"]])
      expect_identical(nrow(issues@issues), 0L, label = paste(format, version))
      y <- read_record_set(
        rt[["path"]],
        "default",
        output = "data.frame",
        verbosity = 0L
      )
      expect_equal(
        y,
        expected,
        ignore_attr = TRUE,
        label = paste(format, version)
      )
      expect_s3_class(y[["ts"]], "POSIXct")
      expect_s3_class(y[["dt"]], "Date")
    }
  }
})

test_that("data types follow column classes", {
  rt <- round_trip(typed_table(), "csv")
  fields <- rt[["metadata"]]@record_set[[1L]]@field
  types <- vapply(fields, \(f) f@data_type, character(1L))
  expect_identical(
    unname(types),
    c(
      "sc:Integer",
      "sc:Float",
      "sc:Boolean",
      "sc:Text",
      "sc:Text",
      "sc:Date",
      "sc:DateTime",
      "sc:Integer"
    )
  )
  expect_identical(fields[[8L]]@id, "default/my_col")
  expect_identical(fields[[8L]]@name, "my col")
  expect_identical(fields[[8L]]@source@extract@column, "my col")
})

test_that("a factor's levels form an enumeration record set", {
  rt <- round_trip(typed_table(), "csv")
  m <- rt[["metadata"]]
  expect_length(m@record_set, 2L)
  enum <- m@record_set[[2L]]
  expect_identical(enum@id, "default_f_levels")
  expect_identical(enum@data_type, "sc:Enumeration")
  expect_identical(enum@key, "default_f_levels/label")
  expect_identical(
    vapply(enum@data, \(r) r[["default_f_levels/label"]], character(1L)),
    c("v", "u", "w")
  )
  expect_identical(m@record_set[[1L]]@field[[5L]]@references@field, enum@key)
  y <- read_record_set(rt[["path"]], "default", verbosity = 0L)
  expect_identical(levels(y[["f"]]), c("v", "u", "w"))
})

test_that("Parquet keeps a factor with its levels in order; CSV its labels", {
  skip_if_not_installed("arrow")
  x <- data.frame(f = factor(c("u", "v", "u"), levels = c("v", "u", "w")))
  dir <- local_tempdir()
  croissant_from_data(
    x,
    file.path(dir, "d.parquet"),
    name = "t",
    format = "parquet"
  )
  y <- arrow::read_parquet(file.path(dir, "d.parquet"))
  expect_identical(levels(y[["f"]]), c("v", "u", "w"))
  croissant_from_data(x, file.path(dir, "d.csv"), name = "t")
  expect_identical(readLines(file.path(dir, "d.csv")), c("f", "u", "v", "u"))
})

test_that("a value outside the enumeration keeps the column character", {
  rt <- round_trip(data.frame(f = factor(c("a", "b"))), "csv")
  m <- rt[["metadata"]]
  m@record_set[[2L]]@data <- list(list(`default_f_levels/label` = "a"))
  write_croissant(m, rt[["path"]])
  expect_warning(
    y <- read_record_set(rt[["path"]], "default", verbosity = 0L),
    "stays character"
  )
  expect_identical(y[["f"]], c("a", "b"))
})

test_that("the object reads its records from the directory of its data file", {
  dir <- local_tempdir()
  x <- data.frame(a = 1:3, f = factor(c("u", "v", "u")))
  m <- croissant_from_data(
    x,
    file.path(dir, "data", "t.csv"),
    name = "t",
    content_url = "data/t.csv"
  )
  expect_identical(m@base, normalizePath(dir, winslash = "/"))
  y <- read_record_set(m, "default", output = "data.frame", verbosity = 0L)
  expect_equal(y, x, ignore_attr = TRUE)
  expect_null(
    croissant_from_data(
      x,
      file.path(dir, "u.csv"),
      name = "u",
      content_url = "elsewhere/u.csv"
    )@base
  )
  expect_null(
    croissant_from_data(
      x,
      file.path(dir, "v.csv"),
      name = "v",
      content_url = "https://example.org/v.csv"
    )@base
  )
})

test_that("the FileObject records the file's size and sha256", {
  rt <- round_trip(typed_table(), "csv")
  file <- rt[["metadata"]]@distribution[[1L]]
  path <- file.path(dirname(rt[["path"]]), file@content_url)
  expect_identical(file@sha256, sha256_file(path))
  expect_identical(file@content_size, paste0(file.info(path)[["size"]], " B"))
  expect_identical(file@encoding_format, "text/csv")
})

test_that("unsupported columns and ids are rejected", {
  dir <- local_tempdir()
  x <- data.frame(a = 1:2)
  x[["l"]] <- list(1, 2)
  expect_error(
    croissant_from_data(x, file.path(dir, "x.csv"), name = "x"),
    class = "rtemis_type_error"
  )
  expect_error(
    croissant_from_data(
      data.frame(a = 1),
      file.path(dir, "x.csv"),
      name = "x",
      record_set = "my set"
    ),
    class = "rtemis_value_error"
  )
})

test_that("column names that sanitize to the same @id stay unique", {
  expect_identical(field_ids(c("a b", "a_b"), "rs"), c("rs/a_b", "rs/a_b_1"))
})

test_that("CSV keeps empty strings apart from missing values", {
  rt <- round_trip(data.frame(s = c("a", "", NA)), "csv")
  y <- read_record_set(
    rt[["path"]],
    "default",
    output = "data.frame",
    verbosity = 0L
  )
  expect_identical(y[["s"]], c("a", "", NA))
})
