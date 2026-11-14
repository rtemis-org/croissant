# test-accessors.R
# ::croissant::
# 2026- EDG rtemis.org

titanic <- function() {
  read_croissant(test_path("fixtures", "1.0", "titanic.json"), verbosity = 0L)
}

test_that("record sets and enumerations partition every record set", {
  for (path in fixture_paths()) {
    x <- suppressWarnings(read_croissant(path, verbosity = 0L))
    data <- record_sets(x)
    enums <- enumerations(x)
    expect_identical(
      sort(c(names(data), names(enums))),
      sort(names(x@record_set %||% list())),
      label = basename(path)
    )
    expect_false(any(vapply(data, is_enumeration, logical(1L))))
    expect_true(all(vapply(enums, is_enumeration, logical(1L))))
  }
})

test_that("titanic holds one data record set and two enumerations", {
  x <- titanic()
  expect_identical(names(record_sets(x)), "passengers")
  expect_identical(names(enumerations(x)), c("genders", "embarkation_ports"))
  expect_identical(record_sets(x)[["passengers"]], x@record_set$passengers)
})

test_that("Hugging Face splits are enumerations", {
  x <- suppressWarnings(read_croissant(
    test_path("fixtures", "1.1", "huggingface-squad_v2.json"),
    verbosity = 0L
  ))
  expect_identical(names(record_sets(x)), "squad_v2")
  expect_identical(names(enumerations(x)), "squad_v2_splits")
})

test_that("a document without record sets has none", {
  x <- Croissant(name = "empty")
  expect_identical(record_sets(x), list())
  expect_identical(enumerations(x), list())
  expect_identical(nrow(fields(RecordSet(id = "r"))), 0L)
  expect_identical(nrow(files(x)), 0L)
})

test_that("fields describe each field's type, source, and reference", {
  f <- fields(titanic(), "passengers")
  expect_s3_class(f, "data.table")
  expect_identical(
    names(f),
    c(
      "column",
      "id",
      "data_type",
      "description",
      "source",
      "extract",
      "references",
      "repeated"
    )
  )
  expect_identical(nrow(f), 14L)
  gender <- f[f[["column"]] == "gender", ]
  expect_identical(gender[["id"]], "passengers/gender")
  expect_identical(gender[["data_type"]], "sc:Text")
  expect_identical(gender[["source"]], "passengers.csv")
  expect_identical(gender[["extract"]], "column sex")
  expect_identical(gender[["references"]], "genders/label")
  expect_false(gender[["repeated"]])
  expect_identical(
    fields(titanic()@record_set$passengers, output = "data.frame"),
    as.data.frame(f)
  )
})

test_that("fields name the columns read_record_set() returns", {
  local_options(croissant.cache_dir = local_tempdir())
  cases <- list(
    c("titanic", "passengers"),
    c("coco2014-mini", "bounding_boxes"),
    c("coco2014-mini", "images"),
    c("json-join", "items_with_prices")
  )
  for (case in cases) {
    path <- test_path(
      "fixtures",
      "datasets",
      "1.0",
      case[[1L]],
      "metadata.json"
    )
    x <- suppressWarnings(read_croissant(path, verbosity = 0L))
    records <- suppressWarnings(read_record_set(
      path,
      case[[2L]],
      n_max = 2L,
      verbosity = 0L
    ))
    expect_identical(
      fields(x, case[[2L]])[["column"]],
      names(records),
      label = paste(case, collapse = "/")
    )
  }
})

test_that("fields take a record set key only with a Croissant", {
  x <- titanic()
  expect_error(fields(x), "record_set")
  expect_error(fields(x, "nope"), "has no record set \"nope\"")
  expect_error(fields(x@record_set$passengers, "passengers"), "without it")
})

test_that("files describe file objects and file sets", {
  f <- files(titanic())
  expect_identical(
    f[["id"]],
    c("passengers.csv", "genders.csv", "embarkation_ports.csv")
  )
  expect_identical(unique(f[["type"]]), "FileObject")
  expect_identical(f[["content_size"]][[1L]], "117743 B")
  expect_identical(f[["encoding_format"]][[1L]], "text/csv")
  expect_true(all(is.na(f[["includes"]])))
  coco <- files(suppressWarnings(read_croissant(
    test_path("fixtures", "1.0", "coco2014-mini.json"),
    verbosity = 0L
  )))
  set <- coco[coco[["id"]] == "image-files", ]
  expect_identical(set[["type"]], "FileSet")
  expect_identical(set[["includes"]], "*.jpg")
  expect_identical(set[["contained_in"]], "train2014.zip")
  expect_true(is.na(set[["content_url"]]))
  expect_s3_class(files(titanic(), output = "data.frame"), "data.frame")
})

test_that("about returns the dataset-level values the document sets", {
  a <- about(titanic())
  expect_identical(a[["name"]], "Titanic")
  expect_identical(a[["license"]], "afl-3.0")
  expect_identical(a[["dataset_version"]], "1.0.0")
  expect_identical(a[["croissant_version"]], "1.0")
  expect_identical(a[["url"]], "https://www.openml.org/d/40945")
  expect_match(a[["cite_as"]], "Encyclopedia Titanica", fixed = TRUE)
  expect_false("keywords" %in% names(a))
  b <- about(Croissant(
    name = c(en = "birds", fr = "oiseaux"),
    license = list(CreativeWork(name = "CC BY 4.0")),
    creator = list(Person(name = "Ada"), Organization(name = "MLCommons"))
  ))
  expect_identical(b[["name"]], "birds")
  expect_identical(b[["license"]], "CC BY 4.0")
  expect_identical(b[["creator"]], c("Ada", "MLCommons"))
})

test_that("record sets and files are named by `@id`", {
  x <- Croissant(
    name = "people",
    distribution = list(FileObject(
      id = "people.csv",
      content_url = "people.csv"
    )),
    record_set = list(RecordSet(
      id = "people",
      field = list(Field(id = "people/name", data_type = "sc:Text"))
    ))
  )
  expect_identical(names(x@record_set), "people")
  expect_identical(names(x@distribution), "people.csv")
  expect_identical(x@record_set$people@id, "people")
  x@record_set <- c(x@record_set, list(RecordSet(name = "extra")))
  expect_identical(names(x@record_set), c("people", "extra"))
  # A node without `@id` or name is keyed by its position, and repeated keys
  # are made distinct.
  x@record_set[[3L]] <- RecordSet()
  expect_identical(names(x@record_set), c("people", "extra", "3"))
  x@record_set <- c(x@record_set, list(RecordSet(id = "people")))
  expect_identical(names(x@record_set), c("people", "extra", "3", "people.1"))
  json <- jsonlite::parse_json(write_croissant(x))
  expect_null(names(json[["recordSet"]]))
  expect_null(names(json[["distribution"]]))
})

test_that("issues return the validation issues, by severity", {
  v <- validate_croissant(Croissant(
    name = "dangling",
    record_set = list(RecordSet(
      id = "r",
      field = list(Field(
        id = "r/a",
        data_type = "sc:Text",
        source = Source(
          file_object = "missing.csv",
          extract = Extract(column = "a")
        )
      ))
    ))
  ))
  all <- issues(v)
  expect_s3_class(all, "data.table")
  expect_identical(as.data.frame(all), v@issues)
  expect_true(all(c("error", "warning") %in% all[["severity"]]))
  errors <- issues(v, severity = "error")
  expect_identical(unique(errors[["severity"]]), "error")
  expect_identical(errors[["check"]], "unresolved_reference")
  expect_identical(
    nrow(issues(v, severity = "warning")),
    sum(v@issues[["severity"]] == "warning")
  )
  expect_s3_class(issues(v, output = "data.frame"), "data.frame")
  expect_error(issues(v, severity = "note"), "should be one of")
  expect_identical(nrow(issues(validate_croissant(titanic()), "error")), 0L)
})

test_that("issues of a read error are the issues of the document", {
  json <- write_croissant(Croissant(name = "x"))
  wrong <- sub('"name": "x"', '"name": 42', json, fixed = TRUE)
  e <- tryCatch(
    read_croissant(wrong, verbosity = 0L),
    croissant_read_error = function(e) e
  )
  expect_s3_class(e, "croissant_read_error")
  expect_identical(issues(e), issues(e[["issues"]]))
  expect_true("error" %in% issues(e)[["severity"]])
})
