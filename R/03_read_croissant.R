# 03_read_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

# %% read_croissant ----
#' Read Croissant metadata
#'
#' Reads a Croissant JSON-LD document into a `Croissant` object. A document
#' whose `@context` defines only standard Croissant terms is read with
#' 'jsonlite'. A document with any other context is first compacted against
#' the standard context by the 'jsonld' package, which must then be installed.
#'
#' Croissant 1.0 and 1.1 documents are supported. A 0.8 document, or one that
#' conforms to another version, raises an error of class
#' `croissant_version_error`.
#'
#' @param x Character: Path to a Croissant JSON-LD file, an `http(s)` URL, a
#' JSON string, or an `hf://datasets/` path. A path naming a file of a
#' Hugging Face dataset reads that file; one naming the dataset only, as
#' `hf://datasets/<owner>/<name>`, reads the Croissant metadata the Hub
#' generates for it, as [read_croissant_hf()] does.
#'
#' @return `Croissant` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' json <- '{
#'   "@context": {"@vocab": "https://schema.org/", "cr": "http://mlcommons.org/croissant/",
#'     "recordSet": "cr:recordSet", "field": "cr:field",
#'     "dataType": {"@id": "cr:dataType", "@type": "@vocab"}},
#'   "@type": "sc:Dataset",
#'   "name": "minimal",
#'   "conformsTo": "http://mlcommons.org/croissant/1.1",
#'   "recordSet": [{"@type": "cr:RecordSet", "@id": "rs", "field": [
#'     {"@type": "cr:Field", "@id": "rs/x", "dataType": "sc:Integer"}]}]
#' }'
#' x <- read_croissant(json)
#' x
read_croissant <- function(x) {
  log <- new_issue_log()
  out <- read_document(x, log)
  errors <- log_errors(log)
  if (length(errors) > 0L) {
    abort(
      "The document has ",
      length(errors),
      ngettext(length(errors), " problem", " problems"),
      " that prevent reading it:\n",
      paste0("  - ", errors, collapse = "\n"),
      "\nvalidate_croissant() reports these together with every other issue.",
      class = c("croissant_read_error", "rtemis_input_error"),
      data = list(issues = as_issues(log))
    )
  }
  warnings <- as_issues(log)@issues
  warnings <- warnings[warnings[["severity"]] == "warning", , drop = FALSE]
  for (i in seq_len(nrow(warnings))) {
    warn(
      "[",
      warnings[["node"]][[i]],
      "] ",
      warnings[["message"]][[i]],
      use_warning = TRUE
    )
  }
  out
} # /croissant::read_croissant


# %% read_document ----
#' Read a document into a `Croissant` object, logging every problem
#'
#' @param x Character: Path, URL, or JSON string.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `Croissant` object. Values that fail a check are logged and kept in
#' the `other_properties` list of their node.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_document <- function(x, log) {
  doc <- read_json_input(x)
  norm <- normalize_document(doc, log)
  out <- node_from_list(norm[["doc"]], "Croissant", log, node = "Croissant")
  out@version <- norm[["version"]]
  out
} # /croissant::read_document


# %% node_from_list ----
#' Build a node object from a canonicalized JSON object
#'
#' A value of the wrong JSON type for a typed property is logged as an error
#' and kept in `other_properties`. A node whose `@type` does not include its
#' class's type is logged as an error and read as that class. A value of a
#' term with `fallback` that does not fit the term is kept in
#' `other_properties` without an issue.
#'
#' @param x List: Canonicalized JSON object.
#' @param class_name Character: Node class name, a name in `.node_terms`.
#' @param log Environment: Issue log from `new_issue_log()`.
#' @param node Character: `@id` of the enclosing node, for issues about a node
#' without its own `@id`.
#'
#' @return Node object of class `class_name`, or `NULL` when `x` is not a JSON
#' object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_from_list <- function(x, class_name, log, node) {
  if (!is.list(x) || (length(x) > 0L && is.null(names(x)))) {
    log_issue(
      log,
      "error",
      "type",
      node,
      "A ",
      class_name,
      " must be a JSON object. Received: ",
      to_json_text(x, pretty = FALSE)
    )
    return(NULL)
  }
  spec <- .node_terms[[class_name]]
  if (is_string(x[["@id"]])) {
    node <- x[["@id"]]
  }
  other <- x
  type <- other[["@type"]]
  other[["@type"]] <- NULL
  if (!is.null(spec[["type"]])) {
    types <- unlist(type, use.names = FALSE)
    if (!is.null(types) && !spec[["type"]] %in% types) {
      log_issue(
        log,
        "error",
        "node_type",
        node,
        "The node has @type ",
        paste(types, collapse = ", "),
        " where a ",
        class_name,
        " is expected. Set @type to ",
        spec[["type"]],
        "."
      )
    }
    # A node may carry further types beside the class's own; they are kept
    # verbatim so they are written back.
    if (length(types) > 1L || (!is.null(types) && !spec[["type"]] %in% types)) {
      other[["@type"]] <- type
    }
  } else if (!is.null(type)) {
    other[["@type"]] <- type
  }
  args <- list()
  for (prop in names(spec[["terms"]])) {
    term <- spec[["terms"]][[prop]]
    if (is.null(other[[term[["key"]]]]) && !is.null(term[["alt"]])) {
      term[["key"]] <- term[["alt"]]
    }
    value <- other[[term[["key"]]]]
    if (is.null(value)) {
      next
    }
    if (term[["fallback"]] && !fits_term(value, term)) {
      next
    }
    read <- tryCatch(
      read_value(value, term, log, node),
      croissant_value_error = function(e) {
        log_issue(
          log,
          "error",
          "type",
          node,
          "Property ",
          term[["key"]],
          " ",
          conditionMessage(e)
        )
        NULL
      }
    )
    if (!is.null(read)) {
      args[[prop]] <- read
      other[[term[["key"]]]] <- NULL
    }
  }
  if (length(other) > 0L) {
    args[["other_properties"]] <- other
  }
  do.call(node_class(class_name), args)
} # /croissant::node_from_list


# %% read_value ----
#' Convert a parsed JSON value to the R value of a typed property
#'
#' @param value Parsed JSON value.
#' @param term List: Term from `.node_terms`.
#' @param log Environment: Issue log from `new_issue_log()`.
#' @param node Character: `@id` of the enclosing node.
#'
#' @return R value of the property. A value of the wrong JSON type signals a
#' condition of class `croissant_value_error`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_value <- function(value, term, log, node) {
  value <- unwrap_value(value)
  switch(
    term[["kind"]],
    string = {
      if (!is_string(value)) {
        type_error("a string", value)
      }
      value
    },
    strings = {
      values <- as_array(value)
      if (!all(vapply(values, is_string, logical(1L)))) {
        type_error("a string or an array of strings", value)
      }
      unlist(values, use.names = FALSE)
    },
    text = read_text(value),
    logical = {
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        type_error("true or false", value)
      }
      value
    },
    ref = read_ref(value),
    refs = vapply(as_array(value), read_ref, character(1L)),
    node = {
      if (!is.list(value) || is.null(names(value))) {
        type_error(paste0("a ", term[["node"]], " object"), value)
      }
      node_from_list(value, term[["node"]], log, node)
    },
    nodes = ,
    nodes_or_one = drop_null(lapply(
      as_array(value),
      node_from_list,
      class_name = term[["node"]],
      log = log,
      node = node
    )),
    distribution = drop_null(lapply(as_array(value), function(v) {
      types <- if (is.list(v)) unlist(v[["@type"]], use.names = FALSE)
      class_name <- if ("cr:FileSet" %in% types) "FileSet" else "FileObject"
      node_from_list(v, class_name, log, node)
    })),
    number = {
      if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
        type_error("a number", value)
      }
      value
    },
    license = {
      values <- as_array(value)
      if (all(vapply(values, is_string, logical(1L)))) {
        unlist(values, use.names = FALSE)
      } else {
        lapply(values, function(v) {
          if (is_string(v)) {
            return(v)
          }
          if (!is.list(v) || is.null(names(v))) {
            type_error("a string or a CreativeWork object", value)
          }
          node_from_list(v, "CreativeWork", log, node)
        })
      }
    },
    agents = lapply(as_array(value), function(v) {
      types <- unlist(v[["@type"]], use.names = FALSE)
      class_name <- if ("sc:Organization" %in% types) {
        "Organization"
      } else {
        "Person"
      }
      node_from_list(v, class_name, log, node)
    }),
    json = value
  )
} # /croissant::read_value


# %% fits_term ----
#' Test whether a value has the form a fallback term reads
#'
#' `containedIn` reads references; in 1.1 it may give a source with a
#' transform instead. `creator` and `publisher` read `Person` and
#' `Organization` objects; schema.org also allows a plain name or another
#' type.
#'
#' @param value Parsed JSON value.
#' @param term List: Term from `.node_terms`.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fits_term <- function(value, term) {
  values <- as_array(value)
  switch(
    term[["kind"]],
    refs = all(vapply(values, is_ref, logical(1L))),
    agents = all(vapply(
      values,
      function(v) {
        if (!is.list(v) || is.null(names(v))) {
          return(FALSE)
        }
        types <- unlist(v[["@type"]], use.names = FALSE)
        is.null(types) || any(c("sc:Person", "sc:Organization") %in% types)
      },
      logical(1L)
    )),
    TRUE
  )
} # /croissant::fits_term


# %% drop_null ----
#' Remove `NULL` elements from a list
#'
#' @param x List.
#'
#' @return `x` without its `NULL` elements.
#'
#' @author EDG
#' @keywords internal
#' @noRd
drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1L))]
} # /croissant::drop_null


# %% read_text ----
#' Read a text value
#'
#' Accepts a string, a language map (`{"en": "...", "fr": "..."}`), a value
#' object (`{"@value": "...", "@language": "fr"}`), or an array of strings and
#' value objects.
#'
#' @param value Parsed JSON value.
#'
#' @return Character vector, named by language tag when any value carries
#' one.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_text <- function(value) {
  if (is_string(value)) {
    return(value)
  }
  if (is.list(value) && !is.null(names(value)) && !"@value" %in% names(value)) {
    if (length(value) > 0L && all(vapply(value, is_string, logical(1L)))) {
      return(unlist(value))
    }
    type_error("text", value)
  }
  values <- as_array(value)
  langs <- character(length(values))
  texts <- character(length(values))
  for (i in seq_along(values)) {
    v <- values[[i]]
    if (is_string(v)) {
      texts[[i]] <- v
    } else if (is.list(v) && is_string(v[["@value"]])) {
      texts[[i]] <- v[["@value"]]
      if (is_string(v[["@language"]])) langs[[i]] <- v[["@language"]]
    } else {
      type_error("text", value)
    }
  }
  if (any(nzchar(langs))) stats::setNames(texts, langs) else texts
} # /croissant::read_text


# %% read_ref ----
#' Read a node reference
#'
#' @param value Parsed JSON value: `{"@id": "..."}` or a string.
#'
#' @return Character scalar: The referenced `@id`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_ref <- function(value) {
  if (is_ref(value)) {
    return(if (is_string(value)) value else value[["@id"]])
  }
  type_error("a reference {\"@id\": \"...\"}", value)
} # /croissant::read_ref


# %% is_ref ----
#' Test for a node reference
#'
#' @param value Parsed JSON value.
#'
#' @return Logical scalar: Whether `value` is `{"@id": "..."}` or a string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_ref <- function(value) {
  is_string(value) ||
    (is.list(value) &&
      identical(names(value), "@id") &&
      is_string(value[["@id"]]))
} # /croissant::is_ref


# %% unwrap_value ----
#' Unwrap a value object without a language tag
#'
#' `{"@value": x}` and `x` are the same JSON-LD value.
#'
#' @param value Parsed JSON value.
#'
#' @return `value`, or its `@value` when it is a value object with no other
#' key.
#'
#' @author EDG
#' @keywords internal
#' @noRd
unwrap_value <- function(value) {
  if (is.list(value) && identical(names(value), "@value")) {
    return(value[["@value"]])
  }
  value
} # /croissant::unwrap_value


# %% as_array ----
#' Wrap a single JSON value as a one-element array
#'
#' A JSON-LD property with one value is written either as the value or as a
#' one-element array.
#'
#' @param value Parsed JSON value.
#'
#' @return Unnamed list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_array <- function(value) {
  if (is.list(value) && is.null(names(value))) value else list(value)
} # /croissant::as_array


# %% is_string ----
#' Test for a parsed JSON string
#'
#' @param x Parsed JSON value.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
} # /croissant::is_string


# %% type_error ----
#' Signal a value of the wrong JSON type
#'
#' `node_from_list()` catches the condition and logs it with the node and
#' property.
#'
#' @param expected Character: Description of the expected value.
#' @param value Parsed JSON value received.
#'
#' @return Does not return.
#'
#' @author EDG
#' @keywords internal
#' @noRd
type_error <- function(expected, value) {
  abort(
    "must be ",
    expected,
    ". Received: ",
    to_json_text(value, pretty = FALSE),
    class = c(
      "croissant_value_error",
      "rtemis_type_error",
      "rtemis_input_error"
    ),
    verbosity = 0L
  )
} # /croissant::type_error
