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


# %% rtemis_welcome ----
#' Startup message
#'
#' One line, as the other rtemis packages print it: package name, version, a
#' croissant, and the platform.
#'
#' @param pkgname Character: Package name.
#' @param pkgversion Character or package_version: Package version.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rtemis_welcome <- function(pkgname, pkgversion) {
  paste0(
    ".:",
    pkgname,
    " ",
    pkgversion,
    " \U1F950",
    " ",
    utils::sessionInfo()[["platform"]]
  )
} # /croissant::rtemis_welcome
