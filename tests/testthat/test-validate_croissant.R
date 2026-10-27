# test-validate_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

issue_checks <- function(x, severity) {
  df <- x@issues
  sort(df[["check"]][df[["severity"]] == severity])
}

test_that("upstream examples have no errors and the recommended warnings of mlcroissant", {
  expected <- jsonlite::read_json(test_path(
    "fixtures",
    "mlcroissant-warnings.json"
  ))
  paths <- fixture_paths()
  for (nm in names(paths)) {
    issues <- validate_croissant(paths[[nm]])
    df <- issues@issues
    expect_true(is_valid(issues), label = nm)
    recommended <- sub(
      "^The recommended property (\\S+) is absent\\.$",
      "\\1",
      df[["message"]][df[["check"]] == "recommended_property"]
    )
    expect_identical(
      sort(recommended),
      sort(as.character(unlist(expected[[nm]][["recommended"]]))),
      label = nm
    )
  }
})

test_that("each invalid upstream document reports the errors mlcroissant reports", {
  # One check per mlcroissant error. mlcroissant reports two further errors
  # that follow from the whitespace @id in distribution_bad_id, and a second
  # message for the unresolved source in mlfield_bad_source.
  expected <- list(
    "1.0/distribution_bad_contained_in" = "unresolved_reference",
    "1.0/distribution_bad_id" = "invalid_id",
    "1.0/distribution_bad_type" = "node_type",
    "1.0/distribution_missing_encoding_format" = "missing_property",
    "1.0/distribution_missing_property_content_url" = "missing_property",
    "1.0/metadata_bad_type" = "node_type",
    "1.0/mlfield_bad_source" = "unresolved_reference",
    "1.0/mlfield_bad_type" = "node_type",
    "1.0/mlfield_missing_source" = "missing_source",
    "1.0/recordset_bad_type" = "node_type",
    "1.0/recordset_missing_context_for_datatype" = "missing_data_type",
    "1.0/recordset_wrong_join" = "undeclared_join",
    "1.1/mlfield_bad_array_definition" = "array_shape",
    "1.1/mlfield_bad_array_shape" = "array_shape",
    "1.1/multilingual_fields" = c("type", "type", "type")
  )
  for (nm in names(expected)) {
    issues <- validate_croissant(test_path(
      "fixtures",
      "invalid",
      paste0(nm, ".json")
    ))
    expect_identical(issue_checks(issues, "error"), expected[[nm]], label = nm)
    expect_false(is_valid(issues), label = nm)
  }
})

test_that("a Croissant term missing from the context is a warning and is not read", {
  issues <- validate_croissant(
    test_path(
      "fixtures",
      "invalid",
      "1.0",
      "recordset_missing_context_for_datatype.json"
    )
  )
  df <- issues@issues
  expect_true(any(
    df[["check"]] == "undefined_term" & grepl("dataType", df[["message"]])
  ))
})

test_that("read_croissant() reports every read problem in one error", {
  e <- tryCatch(
    read_croissant(test_path(
      "fixtures",
      "invalid",
      "1.1",
      "multilingual_fields.json"
    )),
    croissant_read_error = function(e) e
  )
  expect_s3_class(e, "croissant_read_error")
  expect_identical(
    issue_checks(e[["issues"]], "error"),
    c("type", "type", "type")
  )
  expect_match(conditionMessage(e), "3 problems")
})

test_that("checks on objects built in R report each problem", {
  x <- Croissant(
    name = "built",
    cite_as = "Cite.",
    license = "CC-BY-4.0",
    dataset_version = "1.0.0",
    date_published = "2026-10-27",
    distribution = list(
      FileObject(
        id = "a.csv",
        content_url = "a.csv",
        encoding_format = "text/csv"
      ),
      FileObject(
        id = "a.csv",
        content_url = "b.csv",
        encoding_format = "text/csv",
        sha256 = "0"
      ),
      FileSet(
        id = "images",
        encoding_format = "image/png",
        contained_in = "archive.zip"
      )
    ),
    record_set = list(
      RecordSet(
        id = "rs",
        key = "rs/x",
        data = list(list(`rs/y` = 1L)),
        field = list(
          Field(
            id = "rs/y",
            data_type = "sc:Integer",
            source = Source(
              file_object = "a.csv",
              file_set = "images",
              transform = list(Transform(regex = "a", separator = ","))
            )
          ),
          Field(id = "rs/z", is_array = FALSE, array_shape = "a,b")
        )
      )
    )
  )
  issues <- validate_croissant(x)
  expect_identical(
    issue_checks(issues, "error"),
    sort(c(
      "array_shape",
      "array_shape",
      "duplicate_id",
      "inline_data",
      "missing_checksum",
      "missing_data_type",
      "missing_property",
      "source_target",
      "transform",
      "unresolved_reference"
    ))
  )
  expect_identical(issue_checks(issues, "warning"), "unresolved_key")
})

test_that("a valid object built in R has no issues", {
  x <- Croissant(
    name = "minimal",
    cite_as = "Cite.",
    license = "CC-BY-4.0",
    dataset_version = "1.0.0",
    date_published = "2026-10-27",
    distribution = list(
      FileObject(
        id = "a.csv",
        content_url = "a.csv",
        encoding_format = "text/csv",
        sha256 = "0"
      )
    ),
    record_set = list(RecordSet(
      id = "rs",
      field = list(Field(
        id = "rs/x",
        data_type = "sc:Integer",
        source = Source(file_object = "a.csv", extract = Extract(column = "x"))
      ))
    ))
  )
  issues <- validate_croissant(x)
  expect_identical(nrow(issues@issues), 0L)
  expect_true(is_valid(issues))
})

test_that("a 0.8 document and invalid JSON are reported as issues", {
  issues <- validate_croissant(test_path("fixtures", "0.8", "titanic.json"))
  expect_identical(issue_checks(issues, "error"), "version")
  issues <- validate_croissant("{not json")
  expect_identical(issue_checks(issues, "error"), "input")
})

test_that("print lists errors and warnings with their nodes", {
  issues <- validate_croissant(
    test_path("fixtures", "invalid", "1.0", "recordset_wrong_join.json")
  )
  out <- paste(capture.output(print(issues)), collapse = "\n")
  expect_match(out, "1 error")
  expect_match(out, "RecordSet(my-record-set)", fixed = TRUE)
})
