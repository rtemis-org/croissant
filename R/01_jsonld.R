# 01_jsonld.R
# ::croissant::
# 2026- EDG rtemis.org

# From JSON text to a Croissant document with canonical keys.
# spec: croissant/read-write#json-ld-processing
#
# A document whose `@context` passes `is_croissant_context()` is canonicalized
# by key lookup. Any other document is first compacted by `jsonld` against
# `.compaction_context()`, which produces the same keys. Both then pass through
# `canonicalize_node()`, so every downstream reader sees one vocabulary.

# %% read_json_input ----
#' Read and parse JSON from a path, URL, or string
#'
#' @param x Character: Path to a file, an `http(s)` URL, a JSON string, or an
#' `hf://datasets/` path: of a file, or of a dataset, whose Hub-generated
#' Croissant metadata is read.
#'
#' @return List: The parsed JSON, objects as named lists and arrays as unnamed
#' lists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_json_input <- function(x) {
  check_character_scalar(x)
  if (is_hf(x)) {
    ref <- parse_hf(x)
    if (is.null(ref[["file"]])) {
      return(hf_json(
        paste0(hf_endpoint(), "/api/datasets/", ref[["repo"]], "/croissant"),
        hf_token(),
        paste0("Croissant metadata for `", ref[["repo"]], "`")
      ))
    }
    x <- hf_download(x, verbosity = 0L)
  }
  if (grepl("^\\s*\\{", x)) {
    txt <- x
  } else if (grepl("^https?://", x)) {
    con <- url(x, encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    txt <- paste(readLines(con, warn = FALSE), collapse = "\n")
  } else {
    if (!file.exists(x)) {
      abort(
        "File not found: ",
        x,
        ". Pass a path to a Croissant JSON-LD file, an http(s) URL, or a JSON string.",
        class = c("rtemis_file_error", "rtemis_input_error")
      )
    }
    txt <- paste(
      readLines(x, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
  }
  doc <- tryCatch(
    jsonlite::parse_json(txt, simplifyVector = FALSE),
    error = function(e) {
      abort(
        "The input is not valid JSON: ",
        conditionMessage(e),
        class = c("croissant_json_error", "rtemis_input_error")
      )
    }
  )
  if (!is.list(doc) || is.null(names(doc))) {
    abort(
      "A Croissant document is a JSON object; the input is a JSON ",
      if (is.list(doc)) "array" else "scalar",
      ".",
      class = c("croissant_json_error", "rtemis_input_error")
    )
  }
  doc
} # /croissant::read_json_input


# %% normalize_document ----
#' Canonicalize a parsed Croissant document and identify its version
#'
#' @param doc List: Parsed JSON-LD document.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return List with elements `doc` (the document with canonical keys and
#' without `@context`) and `version` (character scalar).
#'
#' @author EDG
#' @keywords internal
#' @noRd
normalize_document <- function(doc, log) {
  check_not_v0_8(doc)
  if (is_croissant_context(doc[["@context"]]) && !has_nested_context(doc)) {
    terms <- context_terms(doc[["@context"]])
  } else {
    doc <- compact_with_jsonld(doc)
    terms <- context_terms(.compaction_context())
  }
  doc[["@context"]] <- NULL
  doc <- canonicalize_node(doc, terms, log, node = "Croissant")
  list(doc = doc, version = document_version(doc, log))
} # /croissant::normalize_document


# %% compact_with_jsonld ----
#' Compact a JSON-LD document against the Croissant context
#'
#' @param doc List: Parsed JSON-LD document.
#'
#' @return List: The compacted document.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compact_with_jsonld <- function(doc) {
  if (!requireNamespace("jsonld", quietly = TRUE)) {
    abort(
      "This document's @context differs from the standard Croissant context, ",
      "and reading it requires JSON-LD processing by the 'jsonld' package. ",
      "Install it with install.packages(\"jsonld\").",
      class = c("rtemis_dependency_error", "rtemis_input_error")
    )
  }
  # Relative `@id`s resolve against `base` during expansion and are made
  # relative to it again during compaction. Without a base, jsonld resolves
  # "passengers.csv" to "/passengers.csv".
  out <- jsonld::jsonld_compact(
    to_json_text(doc, pretty = FALSE),
    to_json_text(list(`@context` = .compaction_context()), pretty = FALSE),
    options = list(base = .jsonld_base)
  )
  out <- jsonlite::parse_json(out, simplifyVector = FALSE)
  if (!is.null(out[["@graph"]])) {
    abort(
      "The document holds ",
      length(out[["@graph"]]),
      " top-level nodes after JSON-LD processing. ",
      "A Croissant document describes one dataset in a single top-level node.",
      class = c("croissant_structure_error", "rtemis_input_error")
    )
  }
  out
} # /croissant::compact_with_jsonld


# Base IRI for JSON-LD processing. The reserved `.invalid` domain never
# resolves.
.jsonld_base <- "http://croissant.invalid/"


# %% .compaction_context ----
#' Context used to compact a document with a non-standard context
#'
#' The 1.1 context without the language containers on `name` and
#' `description`. With the containers and `@language: "en"`, compaction
#' would write every English name as a language map; without them, a value
#' in the default language compacts to a plain string and any other language
#' to a `@value`/`@language` object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.compaction_context <- function() {
  ctx <- croissant_context("1.1")
  ctx[setdiff(names(ctx), c("name", "description"))]
} # /croissant::.compaction_context


# %% has_nested_context ----
#' Test whether any node below the root carries its own `@context`
#'
#' @param x Parsed JSON value.
#' @param root Logical scalar: Whether `x` is the document root.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
has_nested_context <- function(x, root = TRUE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (!root && !is.null(names(x)) && "@context" %in% names(x)) {
    return(TRUE)
  }
  keys <- names(x) %||% rep("", length(x))
  for (i in seq_along(x)) {
    if (keys[[i]] %in% c("@context", "data", "examples")) {
      next
    }
    if (has_nested_context(x[[i]], root = FALSE)) {
      return(TRUE)
    }
  }
  FALSE
} # /croissant::has_nested_context


# %% canonicalize_node ----
#' Rewrite keys and IRI values to the canonical Croissant vocabulary
#'
#' Keys become the term, schema.org local name, or compact IRI that
#' `compact_key()` returns. Values of `@type` and `dataType` become compact
#' IRIs. The values of `data` and `examples` are JSON literals and are kept
#' verbatim.
#'
#' A key that names a Croissant term the context omits expands through
#' `@vocab` to a schema.org IRI, as in any JSON-LD processor, and is logged
#' as a warning. Two keys of one node that name the same property are logged
#' as an error, and the first is kept.
#'
#' @param x Parsed JSON value.
#' @param terms Named list: Term definitions of the document's context.
#' @param log Environment: Issue log from `new_issue_log()`.
#' @param node Character: Path of the enclosing node, for issues.
#'
#' @return `x` with canonical keys and IRI values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
canonicalize_node <- function(x, terms, log, node) {
  if (!is.list(x)) {
    return(x)
  }
  if (is.null(names(x))) {
    return(lapply(x, canonicalize_node, terms = terms, log = log, node = node))
  }
  if (is_string(x[["@id"]])) {
    node <- x[["@id"]]
  }
  keys <- vapply(
    names(x),
    \(k) compact_key(expand_iri(k, terms = terms)),
    character(1L)
  )
  undefined <- names(x)[
    keys == paste0("sc:", names(x)) & !keys %in% .alt_keys()
  ]
  for (k in undefined) {
    log_issue(
      log,
      "warning",
      "undefined_term",
      node,
      "The key `",
      k,
      "` is a Croissant term, but the @context does not define it, so it ",
      "expands to https://schema.org/",
      k,
      " and is not read as the Croissant property. Add the standard ",
      "definition of `",
      k,
      "` to the @context."
    )
  }
  keep <- !duplicated(keys)
  for (dup in unique(keys[!keep])) {
    log_issue(
      log,
      "error",
      "duplicate_property",
      node,
      "The property ",
      dup,
      " is given under more than one key (",
      paste(names(x)[keys == dup], collapse = ", "),
      "); only the first is read. Give each property once."
    )
  }
  x <- x[keep]
  keys <- keys[keep]
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    key <- keys[[i]]
    value <- x[[i]]
    if (key %in% c("@type", "dataType")) {
      value <- map_strings(value, \(v) {
        compact_value(expand_iri(v, terms = terms))
      })
    } else if (!key %in% c("data", "examples", "@value", "@language")) {
      value <- canonicalize_node(value, terms, log, node)
    }
    if (!is.null(value)) out[[i]] <- value
  }
  names(out) <- keys
  out
} # /croissant::canonicalize_node


# %% map_strings ----
#' Apply a function to a string or to each string of an array
#'
#' @param x Character scalar or list of character scalars.
#' @param f Function: Applied to each string.
#'
#' @return `x` with `f` applied to each string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
map_strings <- function(x, f) {
  if (is.character(x) && length(x) == 1L) {
    return(f(x))
  }
  if (is.list(x) && is.null(names(x))) {
    return(lapply(x, \(v) if (is.character(v)) f(v) else v))
  }
  x
} # /croissant::map_strings


# %% check_not_v0_8 ----
#' Reject a Croissant 0.8 document
#'
#' A 0.8 document has no `conformsTo` and binds the prefix `ml` to
#' `http://mlcommons.org/schema/`.
#'
#' @param doc List: Parsed JSON-LD document.
#'
#' @return Called for side effects. Throws an error for a 0.8 document.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_not_v0_8 <- function(doc) {
  ctx <- doc[["@context"]]
  if (
    is.null(doc[["conformsTo"]]) &&
      is.list(ctx) &&
      identical(ctx[["ml"]], "http://mlcommons.org/schema/")
  ) {
    abort(
      "This is a Croissant 0.8 document. The croissant package reads ",
      "Croissant 1.0 and 1.1. Convert the file to 1.0 or later, for example ",
      "with the Croissant editor.",
      class = c(
        "croissant_version_error",
        "rtemis_value_error",
        "rtemis_input_error"
      ),
      data = list(version = "0.8")
    )
  }
  invisible()
} # /croissant::check_not_v0_8


# %% document_version ----
#' Croissant version a document conforms to
#'
#' Reads the version from the `conformsTo` IRI
#' `http://mlcommons.org/croissant/<version>`. A document without that IRI is
#' read as 1.0, and a warning is logged.
#'
#' @param doc List: Canonicalized document.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return Character scalar: `"1.0"` or `"1.1"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
document_version <- function(doc, log) {
  conforms <- unlist(doc[["conformsTo"]], use.names = FALSE)
  pattern <- "^https?://mlcommons\\.org/croissant/([0-9]+\\.[0-9]+)/?$"
  hits <- conforms[grepl(pattern, conforms)]
  if (length(hits) == 0L) {
    log_issue(
      log,
      "warning",
      "missing_property",
      "Croissant",
      "The document has no conformsTo IRI of the form ",
      "http://mlcommons.org/croissant/<version>, so it is read as Croissant ",
      "1.0. Add conformsTo with the version the document follows."
    )
    return("1.0")
  }
  version <- sub(pattern, "\\1", hits[[1L]])
  if (!version %in% .croissant_versions) {
    abort(
      "The document conforms to Croissant ",
      version,
      ". The croissant package reads Croissant ",
      paste(.croissant_versions, collapse = " and "),
      ".",
      class = c(
        "croissant_version_error",
        "rtemis_value_error",
        "rtemis_input_error"
      ),
      data = list(version = version)
    )
  }
  version
} # /croissant::document_version
