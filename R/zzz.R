# zzz.R
# ::croissant::
# 2026- EDG rtemis.org

pkgversion <- utils::packageVersion("croissant")

# data.table's `[` method applies to data.tables subset in this namespace,
# which imports data.table functions by `::` only.
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  # S7 methods for generics of other packages (rtemis.core's `repr`, base
  # `print`) register when the namespace loads.
  S7::methods_register()
} # /onLoad

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      rtemis_welcome(pkgname = pkgname, pkgversion = pkgversion)
    )
  }
} # /onAttach
