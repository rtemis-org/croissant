# test-read_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

test_that("every upstream 1.0 and 1.1 example reads into a Croissant object", {
  paths <- fixture_paths()
  expect_length(paths, 46L)
  for (nm in names(paths)) {
    # huggingface-data_provenance_initiative uses equivalentProperty without
    # defining it, which read_croissant() reports as a warning.
    x <- suppressWarnings(read_croissant(paths[[nm]]))
    expect_true(S7::S7_inherits(x, Croissant), label = nm)
    expect_identical(x@version, sub("/.*$", "", nm), label = nm)
    expect_gt(length(x@record_set), 0L)
  }
})

test_that("typed properties are read from the titanic example", {
  x <- read_croissant(test_path("fixtures", "1.0", "titanic.json"))
  expect_identical(x@name, "Titanic")
  expect_identical(x@license, "afl-3.0")
  expect_identical(x@dataset_version, "1.0.0")
  file <- x@distribution[[1L]]
  expect_true(S7::S7_inherits(file, FileObject))
  expect_identical(file@id, "passengers.csv")
  expect_identical(file@encoding_format, "text/csv")
  rs_ids <- vapply(x@record_set, \(rs) rs@id, character(1L))
  passengers <- x@record_set[[match("passengers", rs_ids)]]
  name_field <- passengers@field[[match(
    "passengers/name",
    vapply(passengers@field, \(f) f@id, character(1L))
  )]]
  expect_identical(name_field@data_type, "sc:Text")
  expect_identical(name_field@source@file_object, "passengers.csv")
  expect_identical(name_field@source@extract@column, "name")
})

test_that("a JSON string reads as a file does", {
  path <- test_path("fixtures", "1.0", "titanic.json")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_equal(read_croissant(txt), read_croissant(path))
})

test_that("1.1 text values read as character vectors named by language", {
  doc <- parse_file(test_path("fixtures", "1.1-multilingual.json"))
  # The upstream file holds deliberate type errors in the distribution's
  # description and in the third field's name and description.
  doc[["distribution"]][[1L]][["description"]] <- "A CSV table."
  doc[["recordSet"]][[1L]][["field"]][[3L]][["name"]] <- "third"
  doc[["recordSet"]][[1L]][["field"]][[3L]][["description"]] <- "Third."
  x <- read_croissant(as.character(jsonlite::toJSON(
    doc,
    auto_unbox = TRUE,
    null = "null"
  )))
  rs <- x@record_set[[1L]]
  expect_identical(rs@name, c(en = "english", fr = "fran\u00e7ais"))
  expect_identical(
    rs@description,
    c(en = "alternate-jsonld-form-ok", fr = "forme-alternate-jsonld-marche")
  )
  expect_identical(
    rs@field[[1L]]@description,
    c(fr = "sulement un langue marche")
  )
})

test_that("a value of the wrong JSON type raises croissant_read_error", {
  expect_error(
    read_croissant(test_path("fixtures", "1.1-multilingual.json")),
    class = "croissant_read_error"
  )
})

test_that("a 0.8 document raises croissant_version_error", {
  expect_error(
    read_croissant(test_path("fixtures", "0.8", "titanic.json")),
    class = "croissant_version_error"
  )
})

test_that("an unsupported conformsTo version raises croissant_version_error", {
  doc <- parse_file(test_path("fixtures", "1.0", "titanic.json"))
  doc[["conformsTo"]] <- "http://mlcommons.org/croissant/2.0"
  json <- as.character(jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null"))
  expect_error(read_croissant(json), class = "croissant_version_error")
})

test_that("a non-standard context is read through jsonld with the same result", {
  skip_if_not_installed("jsonld")
  path <- test_path("fixtures", "1.0", "titanic.json")
  doc <- parse_file(path)
  # Alias recordSet to `rs`, bind schema.org to `schema`, and use both.
  doc[["@context"]][["rs"]] <- "cr:recordSet"
  doc[["@context"]][["recordSet"]] <- NULL
  doc[["@context"]][["schema"]] <- "https://schema.org/"
  names(doc)[names(doc) == "recordSet"] <- "rs"
  names(doc)[names(doc) == "license"] <- "schema:license"
  json <- as.character(jsonlite::toJSON(
    doc,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
  expect_equal(read_croissant(json), read_croissant(path))
})

test_that("a missing file raises an input error", {
  expect_error(
    read_croissant(file.path(tempdir(), "no-such-file.json")),
    class = "rtemis_input_error"
  )
})

test_that("dataset properties typed by mlcroissant are read into typed slots", {
  x <- read_croissant(test_path("fixtures", "1.0", "dices-350.json"))
  expect_true(S7::S7_inherits(x@creator[[1L]], Organization))
  expect_type(x@data_collection, "character")
  expect_type(x@data_biases, "character")
  expect_null(x@other_properties[["rai:dataCollection"]])
  y <- read_croissant(
    test_path("fixtures", "1.1", "commoncrawl-CC-MAIN-2025-43-draft.json")
  )
  expect_identical(y@date_published, "2025-10-19T01:06:58Z")
})

test_that("a creator given as a plain name stays in other_properties", {
  doc <- parse_file(test_path("fixtures", "1.0", "titanic.json"))
  doc[["creator"]] <- "Thomas Cason"
  json <- as.character(jsonlite::toJSON(
    doc,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
  x <- read_croissant(json)
  expect_null(x@creator)
  expect_identical(x@other_properties[["creator"]], "Thomas Cason")
  expect_match(
    write_croissant(x),
    "\"creator\": \"Thomas Cason\"",
    fixed = TRUE
  )
})

test_that("licenses read as strings or CreativeWork objects", {
  doc <- parse_file(test_path("fixtures", "1.0", "titanic.json"))
  doc[["license"]] <- list(
    "afl-3.0",
    list(`@type` = "sc:CreativeWork", name = "CC BY 4.0")
  )
  json <- as.character(jsonlite::toJSON(
    doc,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
  x <- read_croissant(json)
  expect_identical(x@license[[1L]], "afl-3.0")
  expect_true(S7::S7_inherits(x@license[[2L]], CreativeWork))
  expect_identical(x@license[[2L]]@name, "CC BY 4.0")
  expect_equal(read_croissant(write_croissant(x)), x)
})
