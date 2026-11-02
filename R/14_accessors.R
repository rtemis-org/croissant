# 14_accessors.R
# ::croissant::
# 2026- EDG rtemis.org

# Generics that read a Croissant document: its data record sets, enumerations,
# fields, files, and dataset-level metadata, and the issues of its validation,
# as plain R values.
# spec: croissant/accessors#functions

# %% record_sets ----
#' Record sets that hold data
#'
#' Returns the record sets of a dataset that hold its data: every record set
#' whose data type is neither `sc:Enumeration` nor `cr:Split`. Enumeration
#' record sets list the values of categorical fields; [enumerations()]
#' returns them.
#'
#' @param x `Croissant` object.
#' @param ... Not used.
#'
#' @return Named list of `RecordSet` objects, named by `@id`, in document
#' order.
#'
#' @author EDG
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' x <- croissant_from_data(
#'   data.frame(age = c(22L, 38L), sex = factor(c("male", "female"))),
#'   file = file,
#'   name = "people"
#' )
#' names(record_sets(x))
#' record_sets(x)[["default"]]
#' unlink(file)
record_sets <- new_generic("record_sets", "x", function(x, ...) {
  S7_dispatch()
}) # /croissant::record_sets

method(record_sets, Croissant) <- function(x, ...) {
  rs <- x@record_set %||% list()
  rs[!vapply(rs, is_enumeration, logical(1L))]
} # /croissant::record_sets.Croissant


# %% enumerations ----
#' Enumeration record sets
#'
#' Returns the record sets that list the values of categorical fields: every
#' record set whose data type includes `sc:Enumeration`, or `cr:Split`, the
#' enumeration of a dataset's splits. A field that `references` a field of an
#' `sc:Enumeration` record set reads as a factor with [read_record_set()].
#'
#' @param x `Croissant` object.
#' @param ... Not used.
#'
#' @return Named list of `RecordSet` objects, named by `@id`, in document
#' order.
#'
#' @author EDG
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' x <- croissant_from_data(
#'   data.frame(age = c(22L, 38L), sex = factor(c("male", "female"))),
#'   file = file,
#'   name = "people"
#' )
#' names(enumerations(x))
#' unlink(file)
enumerations <- new_generic("enumerations", "x", function(x, ...) {
  S7_dispatch()
}) # /croissant::enumerations

method(enumerations, Croissant) <- function(x, ...) {
  rs <- x@record_set %||% list()
  rs[vapply(rs, is_enumeration, logical(1L))]
} # /croissant::enumerations.Croissant


# %% fields ----
#' Fields of a record set
#'
#' Returns one row per column of the table [read_record_set()] returns for the
#' record set, in the same order: one row per field without sub-fields, depth
#' first.
#'
#' @param x `Croissant` or `RecordSet` object.
#' @param record_set Character: `@id` or name of the record set, when `x` is a
#' `Croissant`. `NULL` when `x` is a `RecordSet`.
#' @param output Character \{"data.table", "tibble", "data.frame", "arrow"\}:
#' Class of the result. `"tibble"` requires the 'tibble' package and
#' `"arrow"` the 'arrow' package.
#' @param ... Not used.
#'
#' @return A data.table, tibble, data.frame, or arrow Table, by `output`, with
#' character columns `column` (the column name in [read_record_set()]), `id`,
#' `data_type`, `description`, `source` (`@id` of the file object, file set,
#' field, or record set the field reads from), `extract` (the column, JSONPath,
#' or file property extracted), and `references` (`@id` of the referenced
#' field or record set), and logical column `repeated` (a list column in
#' [read_record_set()]). A property the field does not set is `NA`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' x <- croissant_from_data(
#'   data.frame(age = c(22L, 38L), sex = factor(c("male", "female"))),
#'   file = file,
#'   name = "people"
#' )
#' fields(x, "default")
#' fields(record_sets(x)[["default"]], output = "data.frame")
#' unlink(file)
fields <- new_generic(
  "fields",
  "x",
  function(
    x,
    record_set = NULL,
    output = c("data.table", "tibble", "data.frame", "arrow"),
    ...
  ) {
    S7_dispatch()
  }
) # /croissant::fields

method(fields, Croissant) <- function(
  x,
  record_set = NULL,
  output = c("data.table", "tibble", "data.frame", "arrow"),
  ...
) {
  check_character_scalar(record_set)
  fields(find_record_set(x, record_set), output = output)
} # /croissant::fields.Croissant

method(fields, RecordSet) <- function(
  x,
  record_set = NULL,
  output = c("data.table", "tibble", "data.frame", "arrow"),
  ...
) {
  output <- match.arg(output)
  if (!is.null(record_set)) {
    abort(
      "`record_set` selects a record set of a Croissant object; ",
      "call fields() on the RecordSet without it.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  leaves <- leaf_fields(x@field %||% list())
  values <- function(fun) {
    vapply(
      leaves,
      \(l) fun(l[["field"]]) %||% NA_character_,
      character(1L),
      USE.NAMES = FALSE
    )
  }
  ids <- values(node_key)
  out <- data.table::data.table(
    column = column_names(x, ids),
    id = ids,
    data_type = values(\(f) items_label(f@data_type)),
    description = values(\(f) first_text(f@description)),
    source = values(\(f) source_ref(f@source)),
    extract = values(\(f) {
      if (!is.null(f@source) && !is.null(f@source@extract)) {
        describe_extract(f@source@extract)
      }
    }),
    references = values(\(f) source_ref(f@references)),
    repeated = vapply(leaves, \(l) l[["repeated"]], logical(1L))
  )
  table_output(out, output)
} # /croissant::fields.RecordSet


# %% files ----
#' Files of a Croissant document
#'
#' Returns one row per element of the distribution: each `FileObject`, a
#' single file, and each `FileSet`, the files that match its glob patterns.
#'
#' @param x `Croissant` object.
#' @param output Character \{"data.table", "tibble", "data.frame", "arrow"\}:
#' Class of the result. `"tibble"` requires the 'tibble' package and
#' `"arrow"` the 'arrow' package.
#' @param ... Not used.
#'
#' @return A data.table, tibble, data.frame, or arrow Table, by `output`, with
#' character columns `id`, `type` (`"FileObject"` or `"FileSet"`),
#' `content_url`, `includes` (the glob patterns of a file set),
#' `encoding_format`, `content_size` (as the document writes it, such as
#' `"117743 B"`), `sha256`, `md5`, and `contained_in` (`@id` of each
#' enclosing file object or file set). A property the node does not set is
#' `NA`; several values are separated by `", "`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' x <- croissant_from_data(
#'   data.frame(age = c(22L, 38L), sex = factor(c("male", "female"))),
#'   file = file,
#'   name = "people"
#' )
#' files(x)
#' unlink(file)
files <- new_generic(
  "files",
  "x",
  function(
    x,
    output = c("data.table", "tibble", "data.frame", "arrow"),
    ...
  ) {
    S7_dispatch()
  }
) # /croissant::files

method(files, Croissant) <- function(
  x,
  output = c("data.table", "tibble", "data.frame", "arrow"),
  ...
) {
  output <- match.arg(output)
  nodes <- x@distribution %||% list()
  is_set <- vapply(nodes, \(n) S7_inherits(n, FileSet), logical(1L))
  # `class` limits a column to the nodes of one class; the rest are NA.
  values <- function(fun, class = NULL) {
    vapply(
      seq_along(nodes),
      function(i) {
        if (!is.null(class) && !S7_inherits(nodes[[i]], class)) {
          return(NA_character_)
        }
        fun(nodes[[i]]) %||% NA_character_
      },
      character(1L)
    )
  }
  out <- data.table::data.table(
    id = values(\(n) n@id %||% first_text(n@name)),
    type = c("FileObject", "FileSet")[is_set + 1L],
    content_url = values(\(n) n@content_url, FileObject),
    includes = values(\(n) items_label(n@includes), FileSet),
    encoding_format = values(\(n) items_label(n@encoding_format)),
    content_size = values(\(n) n@content_size, FileObject),
    sha256 = values(\(n) n@sha256, FileObject),
    md5 = values(\(n) n@md5, FileObject),
    contained_in = values(\(n) items_label(n@contained_in))
  )
  table_output(out, output)
} # /croissant::files.Croissant


# %% about ----
#' Dataset-level metadata
#'
#' Returns what a Croissant document says about its dataset: name,
#' description, versions, license, citation, creators, publishers, dates, URL,
#' keywords, and languages. Text in several languages gives its first value.
#'
#' @param x `Croissant` object.
#' @param ... Not used.
#'
#' @return Named list of the values the document sets, in this order:
#' `name`, `description`, `dataset_version`, `croissant_version`, `license`
#' (strings, and the names or URLs of `CreativeWork` licenses), `cite_as`,
#' `creator` and `publisher` (names of persons and organizations),
#' `date_created`, `date_modified`, `date_published`, `url`, `keywords`, and
#' `in_language`. Each value is character.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- Croissant(
#'   name = "Titanic",
#'   description = "Passengers of the Titanic.",
#'   license = "https://creativecommons.org/licenses/by/4.0/",
#'   creator = list(Organization(name = "OpenML")),
#'   keywords = c("survival", "tabular")
#' )
#' about(x)
#' about(x)[["license"]]
about <- new_generic("about", "x", function(x, ...) {
  S7_dispatch()
}) # /croissant::about

method(about, Croissant) <- function(x, ...) {
  out <- list(
    name = first_text(x@name),
    description = first_text(x@description),
    dataset_version = x@dataset_version,
    croissant_version = x@version,
    license = license_names(x@license),
    cite_as = x@cite_as,
    creator = agent_names(x@creator),
    publisher = agent_names(x@publisher),
    date_created = x@date_created,
    date_modified = x@date_modified,
    date_published = x@date_published,
    url = x@url,
    keywords = x@keywords,
    in_language = x@in_language
  )
  out[!vapply(out, is.null, logical(1L))]
} # /croissant::about.Croissant


# %% issues ----
#' Issues found in Croissant metadata
#'
#' Returns the issues of a validation, one row per issue, errors and warnings
#' in the order they were found. A `CroissantIssues` object is returned by
#' [validate_croissant()]; a `croissant_read_error` condition, signaled by
#' [read_croissant()], carries the issues of the document it read.
#'
#' @param x `CroissantIssues` object, or `croissant_read_error` condition.
#' @param severity Character \{"error", "warning"\}: Severities to return.
#' @param output Character \{"data.table", "tibble", "data.frame", "arrow"\}:
#' Class of the result. `"tibble"` requires the 'tibble' package and
#' `"arrow"` the 'arrow' package.
#' @param ... Not used.
#'
#' @return A data.table, tibble, data.frame, or arrow Table, by `output`, with
#' character columns `severity` (`"error"` or `"warning"`), `check` (the check
#' that found the issue), `node` (the `@id` of the node, or the node type when
#' it has none), and `message`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- validate_croissant(Croissant(name = "minimal"))
#' issues(x)
#' issues(x, severity = "error")
#'
#' json <- write_croissant(Croissant(name = "minimal"))
#' wrong <- sub('"name": "minimal"', '"name": 42', json, fixed = TRUE)
#' e <- tryCatch(
#'   read_croissant(wrong, verbosity = 0L),
#'   croissant_read_error = function(e) e
#' )
#' issues(e)
issues <- new_generic(
  "issues",
  "x",
  function(
    x,
    severity = c("error", "warning"),
    output = c("data.table", "tibble", "data.frame", "arrow"),
    ...
  ) {
    S7_dispatch()
  }
) # /croissant::issues

method(issues, CroissantIssues) <- function(
  x,
  severity = c("error", "warning"),
  output = c("data.table", "tibble", "data.frame", "arrow"),
  ...
) {
  severity <- match.arg(severity, several.ok = TRUE)
  output <- match.arg(output)
  out <- data.table::as.data.table(x@issues)
  # Inside `[.data.table`, `severity` names the column, not the argument.
  keep <- out[["severity"]] %in% severity
  table_output(out[keep], output)
} # /croissant::issues.CroissantIssues

method(issues, new_S3_class("croissant_read_error")) <- function(
  x,
  severity = c("error", "warning"),
  output = c("data.table", "tibble", "data.frame", "arrow"),
  ...
) {
  issues(x[["issues"]], severity = severity, output = output)
} # /croissant::issues.croissant_read_error


# %% source_ref ----
#' `@id` a source or reference names
#'
#' @param s Optional `Source` object.
#'
#' @return Character scalar: The file object, file set, field, or record set
#' `s` names, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
source_ref <- function(s) {
  if (is.null(s)) {
    return(NULL)
  }
  s@file_object %||% s@file_set %||% s@field %||% s@record_set
} # /croissant::source_ref
