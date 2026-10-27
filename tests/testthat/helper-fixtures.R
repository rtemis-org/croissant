# helper-fixtures.R
# ::croissant::
# 2026- EDG rtemis.org

# Paths of the upstream example documents, named by version and dataset.
fixture_paths <- function(versions = c("1.0", "1.1")) {
  paths <- unlist(lapply(versions, function(v) {
    list.files(
      test_path("fixtures", v),
      pattern = "\\.json$",
      full.names = TRUE
    )
  }))
  names(paths) <- paste0(
    basename(dirname(paths)),
    "/",
    sub("\\.json$", "", basename(paths))
  )
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
