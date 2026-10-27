# 05_repr.R
# ::croissant::
# 2026- EDG rtemis.org

# %% repr.Croissant ----
#' Represent Croissant metadata
#'
#' One line each for the name, the Croissant version, and the distribution,
#' then one line per record set with its field count.
#'
#' @param x `Croissant` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Croissant) <- function(x, output_type = NULL, ...) {
  label <- function(text) {
    gray(paste0("  ", text, ": "), output_type = output_type)
  }
  dist_classes <- vapply(x@distribution, \(v) S7_class(v)@name, character(1L))
  dist <- if (length(dist_classes) == 0L) {
    "none"
  } else {
    counts <- table(factor(dist_classes, levels = c("FileObject", "FileSet")))
    counts <- counts[counts > 0L]
    paste(paste(counts, names(counts)), collapse = ", ")
  }
  record_sets <- vapply(
    x@record_set,
    function(rs) {
      n <- length(rs@field)
      paste0(
        "    ",
        rs@id %||% first_text(rs@name) %||% "(unnamed)",
        ": ",
        n,
        ngettext(n, " field", " fields"),
        if (!is.null(rs@data)) {
          paste0(", ", length(as_array(rs@data)), " inline records")
        } else {
          ""
        }
      )
    },
    character(1L)
  )
  paste0(
    repr_S7name("Croissant", output_type = output_type),
    label("name"),
    first_text(x@name) %||% "(unnamed)",
    "\n",
    label("version"),
    "Croissant ",
    x@version,
    "\n",
    label("distribution"),
    dist,
    "\n",
    label("record sets"),
    length(record_sets),
    "\n",
    paste0(record_sets, "\n", collapse = "")
  )
} # /croissant::repr.Croissant


# %% print.Croissant ----
#' @author EDG
#' @keywords internal
#' @noRd
method(print, Croissant) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /croissant::print.Croissant


# %% first_text ----
#' First value of a text property
#'
#' @param x Optional Character: Text, named by language tag where tagged.
#'
#' @return Character scalar, or `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
first_text <- function(x) {
  if (is.null(x)) NULL else unname(x[[1L]])
} # /croissant::first_text
