# 06_issues.R
# ::croissant::
# 2026- EDG rtemis.org

# Issues found while reading and validating a document.
# spec: croissant/read-write#issues
#
# Every check logs to one issue log, an environment, and continues, so a
# single pass reports every problem. `as_issues()` turns the log into a
# `CroissantIssues` object.

# %% CroissantIssues ----
#' CroissantIssues: problems found in Croissant metadata
#'
#' Returned by [validate_croissant()]. An error makes the metadata invalid; a
#' warning names a recommended property that is absent or a construct that is
#' read in a way the author may not intend.
#'
#' @param issues data.frame: One row per issue, with character columns
#' `severity` (`"error"` or `"warning"`), `check` (the check that found it),
#' `node` (the `@id` of the node, or the node type when it has none), and
#' `message`.
#'
#' @return `CroissantIssues` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- validate_croissant(Croissant(name = "minimal"))
#' x
#' x@issues
CroissantIssues <- new_class(
  name = "CroissantIssues",
  package = "croissant",
  properties = list(
    issues = new_property(
      class_data.frame,
      default = quote(data.frame(
        severity = character(),
        check = character(),
        node = character(),
        message = character()
      )),
      validator = function(value) {
        cols <- c("severity", "check", "node", "message")
        if (!identical(names(value), cols)) {
          return(paste0("must have columns ", paste(cols, collapse = ", ")))
        }
        if (!all(value[["severity"]] %in% c("error", "warning"))) {
          return("severity must be \"error\" or \"warning\"")
        }
        NULL
      }
    )
  )
) # /croissant::CroissantIssues


# %% is_valid ----
#' Test whether validation found no errors
#'
#' @param x `CroissantIssues` object.
#'
#' @return Logical scalar: `TRUE` when `x` holds no error. Warnings do not
#' count.
#'
#' @author EDG
#' @export
#'
#' @examples
#' is_valid(validate_croissant(Croissant(name = "minimal")))
is_valid <- function(x) {
  if (!S7_inherits(x, CroissantIssues)) {
    abort(
      "`x` must be a CroissantIssues object, as returned by validate_croissant().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  !any(x@issues[["severity"]] == "error")
} # /croissant::is_valid


# %% repr.CroissantIssues ----
#' Represent validation issues
#'
#' A count line, then the errors, then the warnings, one per line with the
#' node in brackets.
#'
#' @param x `CroissantIssues` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, CroissantIssues) <- function(x, output_type = NULL, ...) {
  df <- x@issues
  n_err <- sum(df[["severity"]] == "error")
  n_warn <- sum(df[["severity"]] == "warning")
  lines <- function(severity) {
    rows <- df[df[["severity"]] == severity, , drop = FALSE]
    if (nrow(rows) == 0L) {
      return("")
    }
    paste0(
      "  - ",
      gray(paste0("[", rows[["node"]], "] "), output_type = output_type),
      rows[["message"]],
      "\n",
      collapse = ""
    )
  }
  paste0(
    repr_S7name("CroissantIssues", output_type = output_type),
    "  ",
    n_err,
    ngettext(n_err, " error", " errors"),
    ", ",
    n_warn,
    ngettext(n_warn, " warning", " warnings"),
    "\n",
    if (n_err > 0L) {
      paste0(bold("  Errors\n", output_type = output_type), lines("error"))
    },
    if (n_warn > 0L) {
      paste0(bold("  Warnings\n", output_type = output_type), lines("warning"))
    }
  )
} # /croissant::repr.CroissantIssues


# %% print.CroissantIssues ----
#' @author EDG
#' @keywords internal
#' @noRd
method(print, CroissantIssues) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /croissant::print.CroissantIssues


# %% new_issue_log ----
#' Create an empty issue log
#'
#' @return Environment with an empty `rows` list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_issue_log <- function() {
  log <- new.env(parent = emptyenv())
  log[["rows"]] <- list()
  log
} # /croissant::new_issue_log


# %% log_issue ----
#' Add an issue to a log
#'
#' @param log Environment: Issue log from `new_issue_log()`.
#' @param severity Character \{"error", "warning"\}: Severity.
#' @param check Character: Name of the check.
#' @param node Character: `@id` of the node, or its type.
#' @param ... Message components, concatenated with no separator.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
log_issue <- function(log, severity, check, node, ...) {
  log[["rows"]][[length(log[["rows"]]) + 1L]] <- c(
    severity = severity,
    check = check,
    node = node,
    message = paste0(...)
  )
  invisible(NULL)
} # /croissant::log_issue


# %% as_issues ----
#' Convert an issue log to a `CroissantIssues` object
#'
#' Repeated issues (same severity, check, node, and message) are kept once.
#'
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `CroissantIssues` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_issues <- function(log) {
  rows <- log[["rows"]]
  df <- if (length(rows) == 0L) {
    data.frame(
      severity = character(),
      check = character(),
      node = character(),
      message = character()
    )
  } else {
    as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  }
  df <- df[!duplicated(df), , drop = FALSE]
  rownames(df) <- NULL
  CroissantIssues(issues = df)
} # /croissant::as_issues


# %% log_errors ----
#' Errors of an issue log
#'
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return Character: The error messages, each prefixed by its node.
#'
#' @author EDG
#' @keywords internal
#' @noRd
log_errors <- function(log) {
  df <- as_issues(log)@issues
  df <- df[df[["severity"]] == "error", , drop = FALSE]
  if (nrow(df) == 0L) {
    return(character())
  }
  paste0("[", df[["node"]], "] ", df[["message"]])
} # /croissant::log_errors
