# test-write_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

test_that("every upstream example round-trips to JSON-LD-equivalent JSON", {
  skip_if_not_installed("jsonld")
  paths <- fixture_paths()
  for (nm in names(paths)) {
    original <- paste(readLines(paths[[nm]], warn = FALSE), collapse = "\n")
    written <- write_croissant(suppressWarnings(read_croissant(paths[[nm]])))
    expect_equal(expanded_json(written), expanded_json(original), label = nm)
  }
})

test_that("write then read returns an equal object", {
  for (path in fixture_paths()) {
    x <- suppressWarnings(read_croissant(path))
    file <- tempfile(fileext = ".json")
    write_croissant(x, file)
    expect_equal(read_croissant(file), x)
    unlink(file)
  }
})

test_that("the written context is the standard context of the version", {
  x <- Croissant(version = "1.0", name = "minimal")
  doc <- jsonlite::parse_json(write_croissant(x), simplifyVector = FALSE)
  expect_identical(doc[["@context"]], croissant_context("1.0"))
  expect_identical(doc[["conformsTo"]], "http://mlcommons.org/croissant/1.0")
  expect_identical(doc[["@type"]], "sc:Dataset")
})

test_that("language-tagged text is a language map in 1.1 and value objects in 1.0", {
  name <- c(en = "english", fr = "fran\u00e7ais")
  doc11 <- jsonlite::parse_json(
    write_croissant(Croissant(version = "1.1", name = name)),
    simplifyVector = FALSE
  )
  expect_identical(doc11[["name"]], list(en = "english", fr = "fran\u00e7ais"))
  doc10 <- jsonlite::parse_json(
    write_croissant(Croissant(version = "1.0", name = name)),
    simplifyVector = FALSE
  )
  expect_identical(
    doc10[["name"]][[2L]],
    list(`@value` = "fran\u00e7ais", `@language` = "fr")
  )
  expect_identical(
    read_croissant(write_croissant(Croissant(
      version = "1.0",
      name = name
    )))@name,
    name
  )
})

test_that("objects built in R write and read back", {
  x <- Croissant(
    name = "Titanic",
    distribution = list(
      FileObject(
        id = "passengers.csv",
        content_url = "titanic.csv",
        encoding_format = "text/csv"
      )
    ),
    record_set = list(
      RecordSet(
        id = "passengers",
        field = list(
          Field(
            id = "passengers/name",
            data_type = "sc:Text",
            source = Source(
              file_object = "passengers.csv",
              extract = Extract(column = "name")
            )
          )
        )
      )
    )
  )
  y <- read_croissant(write_croissant(x))
  expect_identical(y@record_set[[1L]]@field[[1L]]@source@extract@column, "name")
  expect_identical(y@conforms_to, "http://mlcommons.org/croissant/1.1")
})

test_that("print shows name, version, distribution, and record sets", {
  x <- read_croissant(test_path("fixtures", "1.0", "titanic.json"))
  out <- paste(capture.output(print(x)), collapse = "\n")
  expect_match(out, "Titanic")
  expect_match(out, "Croissant 1.0")
  expect_match(out, "passengers: ")
})
