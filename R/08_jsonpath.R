# 08_jsonpath.R
# ::croissant::
# 2026- EDG rtemis.org

# JSONPath on parsed JSON.
# spec: croissant/read-write#record-sets
#
# The subset Croissant documents use: the root `$`, child keys (`.name`,
# `['name']`), array indices (`[0]`, `[-1]`), wildcards (`[*]`, `.*`), and
# recursive descent (`..name`).

# %% jsonpath ----
#' Evaluate a JSONPath expression
#'
#' @param x Parsed JSON value, as returned by
#' `jsonlite::parse_json(simplifyVector = FALSE)`.
#' @param path Character: JSONPath expression.
#'
#' @return List: Every value `path` matches, in document order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
jsonpath <- function(x, path) {
  steps <- jsonpath_steps(path)
  nodes <- list(x)
  for (step in steps) {
    nodes <- unlist(
      lapply(nodes, jsonpath_step, step = step),
      recursive = FALSE
    )
    if (length(nodes) == 0L) {
      return(list())
    }
  }
  nodes
} # /croissant::jsonpath


# %% jsonpath_steps ----
#' Split a JSONPath expression into steps
#'
#' @param path Character: JSONPath expression.
#'
#' @return List of steps, each a list with `type` (`"key"`, `"index"`,
#' `"wildcard"`, or `"descend"`) and `value`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
jsonpath_steps <- function(path) {
  rest <- trimws(path)
  if (!startsWith(rest, "$")) {
    rest <- paste0("$.", rest)
  }
  rest <- substring(rest, 2L)
  steps <- list()
  patterns <- c(
    descend = "^\\.\\.([A-Za-z0-9_$@-]+)",
    wildcard = "^(\\.\\*|\\[\\*\\])",
    key = "^\\.([A-Za-z0-9_$@-]+)",
    quoted = "^\\[['\"]([^'\"]*)['\"]\\]",
    index = "^\\[(-?[0-9]+)\\]"
  )
  while (nzchar(rest)) {
    matched <- FALSE
    for (type in names(patterns)) {
      m <- regmatches(rest, regexec(patterns[[type]], rest, perl = TRUE))[[1L]]
      if (length(m) > 0L) {
        steps[[length(steps) + 1L]] <- list(
          type = if (type == "quoted") "key" else type,
          value = if (type == "index") as.integer(m[[2L]]) else m[[2L]]
        )
        rest <- substring(rest, nchar(m[[1L]]) + 1L)
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      abort(
        "The JSONPath \"",
        path,
        "\" uses syntax that croissant does not evaluate at \"",
        rest,
        "\". Supported: $, .key, ['key'], [n], [*], .*, and ..key.",
        class = c(
          "croissant_jsonpath_error",
          "rtemis_value_error",
          "rtemis_input_error"
        )
      )
    }
  }
  steps
} # /croissant::jsonpath_steps


# %% jsonpath_step ----
#' Apply one JSONPath step to one node
#'
#' @param node Parsed JSON value.
#' @param step List: Step from `jsonpath_steps()`.
#'
#' @return List of matched values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
jsonpath_step <- function(node, step) {
  is_object <- is.list(node) && !is.null(names(node))
  is_array <- is.list(node) && is.null(names(node))
  switch(
    step[["type"]],
    key = if (is_object && step[["value"]] %in% names(node)) {
      list(node[[step[["value"]]]])
    } else {
      list()
    },
    index = {
      if (!is_array) {
        return(list())
      }
      i <- step[["value"]]
      i <- if (i < 0L) length(node) + i + 1L else i + 1L
      if (i >= 1L && i <= length(node)) list(node[[i]]) else list()
    },
    wildcard = if (is.list(node)) unname(node) else list(),
    descend = {
      out <- list()
      if (is_object && step[["value"]] %in% names(node)) {
        out <- list(node[[step[["value"]]]])
      }
      if (is.list(node)) {
        for (child in node) {
          out <- c(out, jsonpath_step(child, step))
        }
      }
      out
    }
  )
} # /croissant::jsonpath_step
