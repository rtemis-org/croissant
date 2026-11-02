# helper-fixtures.R
# ::croissant::
# 2026- EDG rtemis.org

# Paths of the upstream example documents, named by version and dataset. The
# examples are the keys of fixtures/mlcroissant-warnings.json, so a file left
# in fixtures/1.0 or fixtures/1.1 by an earlier check of the package on the
# same machine is not read as an example.
fixture_paths <- function(versions = c("1.0", "1.1")) {
  examples <- names(jsonlite::read_json(test_path(
    "fixtures",
    "mlcroissant-warnings.json"
  )))
  examples <- examples[sub("/.*$", "", examples) %in% versions]
  paths <- test_path("fixtures", paste0(examples, ".json"))
  names(paths) <- examples
  paths
}

# A parsed JSON value in a form where JSON-LD-equivalent documents compare
# equal: object keys sorted, one-element arrays replaced by their element, and
# `@context` removed.
canonical_json <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (is.null(names(x))) {
    if (length(x) == 1L) {
      return(canonical_json(x[[1L]]))
    }
    return(lapply(x, canonical_json))
  }
  x[["@context"]] <- NULL
  x <- x[order(names(x))]
  lapply(x, canonical_json)
}

# A JSON-LD document in expanded form, every key an absolute IRI, so that
# documents with different contexts compare by meaning.
expanded_json <- function(txt) {
  out <- jsonld::jsonld_expand(
    txt,
    options = list(base = "http://croissant.invalid/")
  )
  canonical_json(jsonlite::parse_json(out, simplifyVector = FALSE))
}

parse_file <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

# A Croissant object without its `base`, the location it was read from, so
# that documents read from different places compare by content.
unlocated <- function(x) {
  x@base <- NULL
  x
}
