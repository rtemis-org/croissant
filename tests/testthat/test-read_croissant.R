# test-read_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

test_that("every upstream 1.0 and 1.1 example reads into a Croissant object", {
  paths <- fixture_paths()
  expect_length(paths, 43L)
  expect_true(all(file.exists(paths)))
  for (nm in names(paths)) {
    # huggingface-data_provenance_initiative uses equivalentProperty without
    # defining it, which read_croissant() reports as a warning.
    x <- suppressWarnings(read_croissant(paths[[nm]], verbosity = 0L))
    expect_true(S7::S7_inherits(x, Croissant), label = nm)
    expect_identical(x@version, sub("/.*$", "", nm), label = nm)
    expect_gt(length(x@record_set), 0L)
  }
})

test_that("typed properties are read from the titanic example", {
  x <- read_croissant(
    test_path("fixtures", "1.0", "titanic.json"),
    verbosity = 0L
  )
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
  expect_equal(
    read_croissant(txt, verbosity = 0L),
    unlocated(read_croissant(path, verbosity = 0L))
  )
})

test_that("1.1 text values read as character vectors named by language", {
  doc <- parse_file(test_path("fixtures", "1.1-multilingual.json"))
  # The upstream file holds deliberate type errors in the distribution's
  # description and in the third field's name and description.
  doc[["distribution"]][[1L]][["description"]] <- "A CSV table."
  doc[["recordSet"]][[1L]][["field"]][[3L]][["name"]] <- "third"
  doc[["recordSet"]][[1L]][["field"]][[3L]][["description"]] <- "Third."
  x <- read_croissant(
    as.character(jsonlite::toJSON(
      doc,
      auto_unbox = TRUE,
      null = "null"
    )),
    verbosity = 0L
  )
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
    read_croissant(
      test_path("fixtures", "1.1-multilingual.json"),
      verbosity = 0L
    ),
    class = "croissant_read_error"
  )
})

test_that("the read error states the number of problems", {
  json <- write_croissant(Croissant(name = "x"))
  one <- sub('"name": "x"', '"name": 42', json, fixed = TRUE)
  expect_error(
    read_croissant(one, verbosity = 0L),
    "has 1 problem that prevents reading it",
    class = "croissant_read_error"
  )
  two <- sub('"@type": "sc:Dataset"', '"@type": "sc:Thing"', one, fixed = TRUE)
  expect_error(
    read_croissant(two, verbosity = 0L),
    "has 2 problems that prevent reading it",
    class = "croissant_read_error"
  )
})

test_that("a 0.8 document raises croissant_version_error", {
  expect_error(
    read_croissant(
      test_path("fixtures", "0.8", "titanic.json"),
      verbosity = 0L
    ),
    class = "croissant_version_error"
  )
})

test_that("an unsupported conformsTo version raises croissant_version_error", {
  doc <- parse_file(test_path("fixtures", "1.0", "titanic.json"))
  doc[["conformsTo"]] <- "http://mlcommons.org/croissant/2.0"
  json <- as.character(jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null"))
  expect_error(
    read_croissant(json, verbosity = 0L),
    class = "croissant_version_error"
  )
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
  expect_equal(
    read_croissant(json, verbosity = 0L),
    unlocated(read_croissant(path, verbosity = 0L))
  )
})

test_that("a missing file raises an input error", {
  expect_error(
    read_croissant(file.path(tempdir(), "no-such-file.json"), verbosity = 0L),
    class = "rtemis_input_error"
  )
})

test_that("an unreachable URL raises a download error", {
  skip_on_cran()
  expect_error(
    suppressWarnings(read_croissant(
      "https://croissant.invalid/metadata.json",
      verbosity = 0L
    )),
    class = "croissant_download_error"
  )
})

test_that("dataset properties typed by mlcroissant are read into typed slots", {
  x <- read_croissant(
    test_path("fixtures", "1.0", "dices-350.json"),
    verbosity = 0L
  )
  expect_true(S7::S7_inherits(x@creator[[1L]], Organization))
  expect_type(x@data_collection, "character")
  expect_type(x@data_biases, "character")
  expect_null(x@other_properties[["rai:dataCollection"]])
  y <- read_croissant(
    test_path("fixtures", "1.1", "commoncrawl-CC-MAIN-2025-43-draft.json"),
    verbosity = 0L
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
  x <- read_croissant(json, verbosity = 0L)
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
  x <- read_croissant(json, verbosity = 0L)
  expect_identical(x@license[[1L]], "afl-3.0")
  expect_true(S7::S7_inherits(x@license[[2L]], CreativeWork))
  expect_identical(x@license[[2L]]@name, "CC BY 4.0")
  expect_equal(read_croissant(write_croissant(x), verbosity = 0L), x)
})

test_that("verbosity 1 reports the input, then the name and counts", {
  path <- test_path("fixtures", "1.0", "titanic.json")
  out <- paste(capture_messages(read_croissant(path)), collapse = "")
  expect_match(out, "Reading titanic.json...", fixed = TRUE)
  expect_match(
    out,
    "Read Titanic: Croissant 1.0, 1 record set, 2 enumerations, 3 files",
    fixed = TRUE
  )
  expect_no_message(read_croissant(path, verbosity = 0L))
})

test_that("a document read from a path or URL records its location as `base`", {
  path <- test_path("fixtures", "1.0", "titanic.json")
  expect_identical(
    read_croissant(path, verbosity = 0L)@base,
    dirname(normalizePath(path))
  )
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_null(read_croissant(txt, verbosity = 0L)@base)
  expect_identical(
    document_base("https://example.org/data/croissant.json"),
    "https://example.org/data"
  )
  expect_identical(
    document_base("hf://datasets/rtemis/demo@v1.0.0/croissant.json"),
    "hf://datasets/rtemis/demo@v1.0.0"
  )
  expect_null(document_base("hf://datasets/rtemis/demo"))
  json <- jsonlite::parse_json(write_croissant(read_croissant(
    path,
    verbosity = 0L
  )))
  expect_false(any(c("base", "@base") %in% names(json)))
})

test_that("a node is built as its constructor builds it", {
  args <- list(
    id = "rs/x",
    name = "x",
    data_type = "sc:Float",
    source = Source(file_object = "f.csv", extract = Extract(column = "x"))
  )
  # The attributes are in the constructor's order.
  same <- function(x, y) identical(x, y, attrib.as.set = FALSE)
  expect_true(same(new_node("Field", args), do.call(Field, args)))
  expect_true(same(new_node("Field", list()), Field()))
  nodes <- list(record_set = list(RecordSet(id = "a"), RecordSet(id = "b")))
  expect_true(same(new_node("Croissant", nodes), do.call(Croissant, nodes)))
  # Read order: typed properties, a setter property before a later property
  # without a setter, and extension properties last.
  nodes <- list(
    name = "d",
    distribution = list(FileObject(id = "f", content_url = "f.csv")),
    data_biases = "none known",
    other_properties = list(`https://example.org/extra` = "retained")
  )
  expect_true(same(new_node("Croissant", nodes), do.call(Croissant, nodes)))
  # Every node of a read document, with RAI and extension properties.
  # read_croissant() sets `base` after construction, so the rebuilt root is
  # constructed without `base` and then assigned it.
  rebuild <- function(x) {
    if (S7::S7_inherits(x)) {
      do.call(S7::S7_class(x), lapply(S7::props(x), rebuild))
    } else if (is.list(x) && !is.object(x)) {
      lapply(x, rebuild)
    } else {
      x
    }
  }
  x <- read_croissant(
    test_path("fixtures", "1.0", "bigcode-the-stack.json"),
    verbosity = 0L
  )
  props <- S7::props(x)
  y <- do.call(Croissant, lapply(props[names(props) != "base"], rebuild))
  y@base <- x@base
  expect_true(same(x, y))
  error_of <- function(expr) tryCatch(expr, error = conditionMessage)
  expect_identical(
    error_of(new_node("Field", list(id = "x", name = character()))),
    error_of(Field(id = "x", name = character()))
  )
  # new_node() runs no class validator.
  for (cls in names(.node_terms)) {
    expect_null(node_class(cls)@validator, label = cls)
  }
})

test_that("text whose tags repeat or are missing reads as tagged strings", {
  doc <- list(
    `@context` = croissant_context("1.1"),
    `@type` = "sc:Dataset",
    name = list(
      "Titanic",
      list(`@value` = "Titanic", `@language` = "fr"),
      list(`@value` = "Le Titanic", `@language` = "fr")
    ),
    description = list(list(`@value` = "Passengers.", `@language` = "en"))
  )
  json <- as.character(jsonlite::toJSON(doc, auto_unbox = TRUE))
  x <- read_croissant(json, verbosity = 0L)
  expect_identical(
    x@name,
    list(
      list(value = "Titanic"),
      list(value = "Titanic", language = "fr"),
      list(value = "Le Titanic", language = "fr")
    )
  )
  expect_identical(x@description, c(en = "Passengers."))
  expect_identical(about(x)[["name"]], "Titanic")
  back <- jsonlite::parse_json(write_croissant(x), simplifyVector = FALSE)
  expect_identical(back[["name"]], doc[["name"]])
})

test_that("a text property rejects a character vector with empty or repeated tags", {
  expect_error(Croissant(name = c(en = "a", "b")))
  expect_error(Croissant(name = c(fr = "a", fr = "b")))
  expect_no_error(Croissant(
    name = list(list(value = "a"), list(value = "b", language = "fr"))
  ))
})
