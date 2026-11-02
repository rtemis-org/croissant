# test-read_performance.R
# ::croissant::
# 2026- EDG rtemis.org

# A wide record set reads, validates, and loads in time linear in its fields.
# spec: croissant/read-performance

# Path of a Croissant document whose record set `wide` reads `n_fields`
# columns of a CSV file, both written to `dir`.
wide_document <- function(dir, n_fields, n_rows = 10L) {
  cols <- paste0("x", seq_len(n_fields))
  csv <- file.path(dir, "wide.csv")
  values <- matrix(
    round(seq_len(n_rows * n_fields) / 7, 3),
    nrow = n_rows,
    dimnames = list(NULL, cols)
  )
  data.table::fwrite(data.table::as.data.table(values), csv)
  doc <- list(
    `@context` = croissant_context("1.1"),
    `@type` = "sc:Dataset",
    name = "wide",
    conformsTo = "http://mlcommons.org/croissant/1.1",
    distribution = list(list(
      `@type` = "cr:FileObject",
      `@id` = "wide.csv",
      contentUrl = "wide.csv",
      encodingFormat = "text/csv",
      sha256 = sha256_file(csv)
    )),
    recordSet = list(list(
      `@type` = "cr:RecordSet",
      `@id` = "wide",
      field = lapply(cols, function(col) {
        list(
          `@type` = "cr:Field",
          `@id` = paste0("wide/", col),
          name = col,
          dataType = "sc:Float",
          source = list(
            fileObject = list(`@id` = "wide.csv"),
            extract = list(column = col)
          )
        )
      })
    ))
  )
  path <- file.path(dir, "croissant.json")
  jsonlite::write_json(doc, path, auto_unbox = TRUE)
  path
}

# The bound, 15 s, is about 1.9 ms per field, about 3.5 times the time
# measured on an arm64 Mac (spec: croissant/read-performance#verification).
test_that("a record set of 8000 fields reads, validates, and loads within 15 s", {
  skip_on_cran()
  local_options(croissant.cache_dir = local_tempdir())
  path <- wide_document(local_tempdir(), 8000L)
  elapsed <- system.time({
    x <- read_croissant(path, verbosity = 0L)
    issues <- validate_croissant(x)
    tab <- read_record_set(x, "wide", verbosity = 0L)
  })[["elapsed"]]
  expect_true(is_valid(issues))
  expect_identical(dim(tab), c(10L, 8000L))
  expect_type(tab[["x8000"]], "double")
  expect_lt(elapsed, 15)
})
