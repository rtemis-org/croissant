# croissant-package.R
# ::croissant::
# 2026- EDG rtemis.org

#' \pkg{croissant}: Read and Write 'Croissant' Metadata
#'
#' @description
#' Read, write, and inspect Croissant JSON-LD metadata for machine learning
#' datasets. [read_croissant()] reads a document into a [Croissant] object and
#' [write_croissant()] writes it back.
#'
#' [record_sets()], [enumerations()], [fields()], [files()], and [about()]
#' return the contents of a document as R values, [issues()] the problems
#' [validate_croissant()] finds, and [read_record_set()]
#' returns the records of a record set. The S7 properties of [Croissant] and
#' the node classes follow the Croissant vocabulary, one property per term, and
#' are read and set with `@`: `x@name`, `x@description <- "..."`. An
#' assignment is validated by the class.
#'
#' @name croissant-package
#'
#' @import methods S7 rtemis.core
"_PACKAGE"

NULL
