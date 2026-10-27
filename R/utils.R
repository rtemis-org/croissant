# utils.R
# ::croissant::
# 2026- EDG rtemis.org

# %% %||% ----
#' Default for NULL
#'
#' @param x Value.
#' @param y Value returned when `x` is `NULL`.
#'
#' @return `x`, or `y` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
} # /croissant::%||%
