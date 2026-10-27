# 04_write_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

# %% write_croissant ----
#' Write Croissant metadata
#'
#' Writes a `Croissant` object as a JSON-LD document with the standard
#' context of its version (see [croissant_context()]). `conformsTo` always
#' includes the Croissant IRI of `x@version`.
#'
#' In version 1.1, a name or description named by language tag is written as
#' a language map. In version 1.0, it is written as an array of
#' `@value`/`@language` objects.
#'
#' @param x `Croissant` object.
#' @param path Optional Character: File to write. If `NULL`, the JSON text is
#' returned.
#' @param pretty Logical: Whether to indent the JSON.
#'
#' @return If `path` is `NULL`, the JSON text as a character scalar of class
#' `json`; otherwise `path`, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- Croissant(
#'   name = "Titanic",
#'   distribution = list(
#'     FileObject(id = "passengers.csv", content_url = "titanic.csv", encoding_format = "text/csv")
#'   )
#' )
#' cat(write_croissant(x))
#' path <- tempfile(fileext = ".json")
#' write_croissant(x, path)
#' read_croissant(path)
write_croissant <- function(x, path = NULL, pretty = TRUE) {
  if (!S7_inherits(x, Croissant)) {
    abort(
      "`x` must be a Croissant object, as returned by read_croissant() or Croissant().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  check_optional_character_scalar(path)
  check_logical_scalar(pretty)
  txt <- to_json_text(croissant_to_list(x), pretty = pretty)
  if (is.null(path)) {
    return(txt)
  }
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(txt, con)
  invisible(path)
} # /croissant::write_croissant


# %% croissant_to_list ----
#' Convert a `Croissant` object to a JSON-LD document
#'
#' @param x `Croissant` object.
#'
#' @return Named list: The document, ready for `to_json_text()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
croissant_to_list <- function(x) {
  iri <- paste0("http://mlcommons.org/croissant/", x@version)
  if (!iri %in% x@conforms_to) {
    x@conforms_to <- c(iri, x@conforms_to)
  }
  c(
    list(`@context` = croissant_context(x@version)),
    node_to_list(x, "Croissant", x@version)
  )
} # /croissant::croissant_to_list


# %% node_to_list ----
#' Convert a node object to a JSON object
#'
#' @param x Node object.
#' @param class_name Character: Node class name, a name in `.node_terms`.
#' @param version Character: Croissant version being written.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_to_list <- function(x, class_name, version) {
  spec <- .node_terms[[class_name]]
  other <- x@other_properties
  out <- list()
  if (!is.null(other[["@type"]])) {
    out[["@type"]] <- other[["@type"]]
    other[["@type"]] <- NULL
  } else if (!is.null(spec[["type"]])) {
    out[["@type"]] <- spec[["type"]]
  }
  for (nm in names(spec[["terms"]])) {
    value <- prop(x, nm)
    # A string may carry an S3 class, such as the `hash` class of
    # `openssl::sha256()`, which `jsonlite::toJSON()` has no method for.
    # `unclass()` keeps the language-tag names of text.
    if (is.atomic(value) && !is.null(oldClass(value))) {
      value <- unclass(value)
    }
    if (!is.null(value)) {
      term <- spec[["terms"]][[nm]]
      out[[term[["key"]]]] <- write_value(value, term, version)
    }
  }
  for (key in names(other)) {
    out[key] <- list(other[[key]])
  }
  out
} # /croissant::node_to_list


# %% write_value ----
#' Convert the R value of a typed property to a JSON value
#'
#' @param value R value of the property.
#' @param term List: Term from `.node_terms`.
#' @param version Character: Croissant version being written.
#'
#' @return Value ready for `to_json_text()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
write_value <- function(value, term, version) {
  switch(
    term[["kind"]],
    string = ,
    logical = ,
    json = value,
    strings = if (length(value) == 1L) value else as.list(value),
    text = write_text(value, version),
    ref = list(`@id` = value),
    refs = {
      refs <- lapply(value, \(v) list(`@id` = v))
      if (length(refs) == 1L) refs[[1L]] else refs
    },
    node = node_to_list(value, term[["node"]], version),
    nodes = lapply(
      value,
      node_to_list,
      class_name = term[["node"]],
      version = version
    ),
    nodes_or_one = {
      nodes <- lapply(
        value,
        node_to_list,
        class_name = term[["node"]],
        version = version
      )
      if (length(nodes) == 1L) nodes[[1L]] else nodes
    },
    distribution = ,
    agents = {
      nodes <- lapply(value, function(v) {
        node_to_list(v, S7_class(v)@name, version)
      })
      if (term[["kind"]] == "agents" && length(nodes) == 1L) {
        nodes[[1L]]
      } else {
        nodes
      }
    },
    number = value,
    license = {
      if (is.character(value)) {
        if (length(value) == 1L) value else as.list(value)
      } else {
        lapply(value, function(v) {
          if (is.character(v)) v else node_to_list(v, "CreativeWork", version)
        })
      }
    }
  )
} # /croissant::write_value


# %% write_text ----
#' Convert a text value to JSON
#'
#' @param value Character: Text, named by language tag where tagged.
#' @param version Character: Croissant version being written.
#'
#' @return A string, a language map, or an array of strings and value objects.
#'
#' @author EDG
#' @keywords internal
#' @noRd
write_text <- function(value, version) {
  langs <- names(value)
  if (is.null(langs) || !any(nzchar(langs))) {
    return(if (length(value) == 1L) value else as.list(unname(value)))
  }
  if (version == "1.1" && all(nzchar(langs)) && !anyDuplicated(langs)) {
    return(as.list(value))
  }
  lapply(seq_along(value), function(i) {
    if (nzchar(langs[[i]])) {
      list(`@value` = unname(value[[i]]), `@language` = langs[[i]])
    } else {
      unname(value[[i]])
    }
  })
} # /croissant::write_text


# %% to_json_text ----
#' Serialize parsed JSON values to JSON text
#'
#' The inverse of `jsonlite::parse_json(simplifyVector = FALSE)`: a named list
#' is an object, an unnamed list an array, a length-one atomic vector a
#' scalar, and `NULL` is `null`.
#'
#' @param x Value to serialize.
#' @param pretty Logical: Whether to indent the JSON.
#'
#' @return Character scalar of class `json`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_json_text <- function(x, pretty = TRUE) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = pretty
  )
} # /croissant::to_json_text
