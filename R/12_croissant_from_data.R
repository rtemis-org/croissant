# 12_croissant_from_data.R
# ::croissant::
# 2026- EDG rtemis.org

# Croissant metadata for an R table.
# spec: croissant/read-write#metadata-from-data

# Croissant data type of each R column class, by the full class vector, so
# that a class built on another (difftime on double, IDate on Date) has no
# entry. `read_record_set()` returns each class as it was written.
.type_by_class <- c(
  integer = "sc:Integer",
  integer64 = "cr:Int64",
  numeric = "sc:Float",
  logical = "sc:Boolean",
  Date = "sc:Date",
  `POSIXct/POSIXt` = "sc:DateTime",
  factor = "sc:Text",
  `ordered/factor` = "sc:Text",
  character = "sc:Text"
)


# %% croissant_from_data ----
#' Create Croissant metadata for a table
#'
#' Writes `x` to `file` as CSV or Parquet, and returns a `Croissant` object
#' that describes it: one `FileObject` with the file's size and SHA-256
#' checksum, one `RecordSet`, and one `Field` per column, whose data type
#' follows the column class: integer to `sc:Integer`, integer64 (bit64) to
#' `cr:Int64` (Croissant 1.1), double to `sc:Float`, logical to `sc:Boolean`,
#' Date to `sc:Date`, POSIXct to `sc:DateTime`, and character, factor, and
#' ordered factor to `sc:Text`. [read_record_set()] returns each column with
#' the class it was written with, so a column of any other class, including a
#' class built on one of these, such as difftime, is an error. Write the
#' metadata with
#' [write_croissant()] into the directory where `content_url` resolves to
#' `file`. That directory is the `base` of the returned object, so
#' [read_record_set()] reads the object's records before it is written.
#'
#' A factor is written as its labels in CSV, and as a dictionary-encoded
#' column with its levels, in order, in Parquet, which Parquet readers return
#' as a factor or categorical column. Its levels, in order, also form an
#' enumeration record set, `<record_set>_<column>_levels`, with data type
#' `sc:Enumeration`, one field `label`, and the levels as inline records; the
#' factor's field `references` that field, so [read_record_set()] returns the
#' column as a factor with the same levels. An ordered factor reads back
#' ordered, and a POSIXct column with its time zone, from Parquet; CSV holds
#' neither, so an ordered factor, or a POSIXct column in a time zone other
#' than UTC, is an error with `format = "csv"`. In CSV, a missing value is
#' written as an empty field and an empty string as `""`, and they read back
#' as `NA` and `""`.
#'
#' @param x data.frame, data.table, or tibble: The data.
#' @param file Character: Path of the data file to write.
#' @param name Character: Name of the dataset.
#' @param format Character \{"csv", "parquet"\}: File format. `"parquet"`
#' requires the 'arrow' package.
#' @param record_set Character: `@id` of the record set.
#' @param content_url Character: `contentUrl` of the file, relative to the
#' metadata document.
#' @param version Character \{"1.0", "1.1"\}: Croissant version.
#' @param ... Further properties of the `Croissant` object, such as
#' `description`, `license`, `cite_as`, `creator`, or `date_published`.
#'
#' @return `Croissant` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' x <- croissant_from_data(
#'   iris,
#'   file.path(dir, "iris.csv"),
#'   name = "iris",
#'   license = "CC0-1.0"
#' )
#' x
#' write_croissant(x, file.path(dir, "metadata.json"))
#' head(read_record_set(file.path(dir, "metadata.json"), "default"))
#' unlink(dir, recursive = TRUE)
croissant_from_data <- function(
  x,
  file,
  name,
  format = c("csv", "parquet"),
  record_set = "default",
  content_url = basename(file),
  version = "1.1",
  ...
) {
  check_tabular(x)
  check_character_scalar(file)
  check_character_scalar(name)
  format <- match.arg(format)
  check_character_scalar(record_set)
  check_character_scalar(content_url)
  check_enum(version, .croissant_versions)
  if (grepl("\\s", record_set)) {
    abort(
      "`record_set` is an @id and must not contain whitespace. Received: \"",
      record_set,
      "\".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (ncol(x) == 0L) {
    abort(
      "`x` has no columns, so its record set would have no fields.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  types <- vapply(
    names(x),
    \(col) column_data_type(x[[col]], col, format, version),
    character(1L)
  )
  data <- as.data.frame(x)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  encoding_format <- if (format == "csv") {
    for (col in names(data)[vapply(data, is.factor, logical(1L))]) {
      data[[col]] <- as.character(data[[col]])
    }
    data.table::fwrite(data, file, dateTimeAs = "write.csv", na = "")
    "text/csv"
  } else {
    check_dependencies("arrow")
    arrow::write_parquet(data, file)
    "application/x-parquet"
  }
  file_id <- basename(file)
  ids <- field_ids(names(x), record_set)
  enums <- list()
  fields <- lapply(seq_along(ids), function(i) {
    v <- x[[i]]
    references <- if (is.factor(v)) {
      enum <- enumeration_record_set(levels(v), ids[[i]], record_set)
      enums[[length(enums) + 1L]] <<- enum
      Source(field = enum@key)
    }
    Field(
      id = ids[[i]],
      name = names(x)[[i]],
      data_type = types[[i]],
      source = Source(
        file_object = file_id,
        extract = Extract(column = names(x)[[i]])
      ),
      references = references
    )
  })
  Croissant(
    version = version,
    name = name,
    distribution = list(FileObject(
      id = file_id,
      name = file_id,
      content_url = content_url,
      content_size = paste0(file.info(file)[["size"]], " B"),
      encoding_format = encoding_format,
      sha256 = sha256_file(file)
    )),
    record_set = c(
      list(RecordSet(id = record_set, name = record_set, field = fields)),
      enums
    ),
    base = data_base(file, content_url),
    ...
  )
} # /croissant::croissant_from_data


# %% data_base ----
#' Directory that a relative `contentUrl` resolves against
#'
#' @param file Character: Path of the data file written.
#' @param content_url Character: `contentUrl` of the file.
#'
#' @return Character scalar: The directory `d` of which `file` is
#' `<d>/<content_url>`, or `NULL` when `content_url` is a URL, an absolute
#' path, or not the end of `file`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
data_base <- function(file, content_url) {
  if (
    grepl("^[A-Za-z][A-Za-z0-9+.-]*://", content_url) ||
      is_absolute_path(content_url)
  ) {
    return(NULL)
  }
  path <- normalizePath(file, winslash = "/")
  suffix <- paste0("/", sub("^(\\./)+", "", content_url))
  if (!endsWith(path, suffix)) {
    return(NULL)
  }
  substr(path, 1L, nchar(path) - nchar(suffix))
} # /croissant::data_base


# %% column_data_type ----
#' Croissant data type of a column
#'
#' Each class in `.type_by_class` reads back through `read_record_set()` as
#' the same class, so every other class is an error. A format or version that
#' cannot hold a class exactly is an error too: CSV holds neither the order
#' of an ordered factor nor a time zone other than UTC, and `cr:Int64` is a
#' Croissant 1.1 data type.
#'
#' @param v Column vector.
#' @param col Character: Column name, for messages.
#' @param format Character \{"csv", "parquet"\}: File format.
#' @param version Character \{"1.0", "1.1"\}: Croissant version.
#'
#' @return Character scalar: Compact IRI.
#'
#' @author EDG
#' @keywords internal
#' @noRd
column_data_type <- function(v, col, format, version) {
  cls <- paste(class(v), collapse = "/")
  if (!cls %in% names(.type_by_class)) {
    abort(
      "Column \"",
      col,
      "\" has class ",
      cls,
      ", which does not read back as the same class. Convert it to integer, ",
      "integer64, double, logical, character, factor, ordered factor, Date, ",
      "or POSIXct",
      if (inherits(v, "difftime")) {
        paste0(", as `as.numeric(x, units = \"", units(v), "\")`")
      } else if (inherits(v, "Date")) {
        ", as `as.Date(x)`"
      },
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (format == "csv" && is.ordered(v)) {
    abort(
      "Column \"",
      col,
      "\" is an ordered factor, and CSV records no level order. Write ",
      "`format = \"parquet\"`, or convert it with `factor(x, ordered = FALSE)`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (
    format == "csv" &&
      inherits(v, "POSIXct") &&
      !identical(attr(v, "tzone"), "UTC")
  ) {
    abort(
      "Column \"",
      col,
      "\" is a date-time in time zone \"",
      attr(v, "tzone") %||% "",
      "\", and CSV records date-times in UTC. Write ",
      "`format = \"parquet\"`, which keeps the time zone, or set ",
      "`attr(x, \"tzone\") <- \"UTC\"`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (cls == "integer64" && version != "1.1") {
    abort(
      "Column \"",
      col,
      "\" is integer64, whose data type cr:Int64 is defined in Croissant ",
      "1.1. Use `version = \"1.1\"`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  unname(.type_by_class[[cls]])
} # /croissant::column_data_type


# %% field_ids ----
#' Field `@id`s for column names
#'
#' `<record set>/<column>`, with whitespace in the column name replaced by
#' `_`, and a numeric suffix where two columns would share an `@id`.
#'
#' @param cols Character: Column names.
#' @param record_set Character: `@id` of the record set.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
field_ids <- function(cols, record_set) {
  ids <- paste0(record_set, "/", gsub("\\s+", "_", cols))
  make.unique(ids, sep = "_")
} # /croissant::field_ids


# %% enumeration_record_set ----
#' Enumeration record set of a factor's levels
#'
#' @param levels Character: Levels, in order.
#' @param field_id Character: `@id` of the factor's field,
#' `<record_set>/<column>`.
#' @param record_set Character: `@id` of the factor's record set.
#'
#' @return `RecordSet` object with `@id` `<record_set>_<column>_levels`, data
#' type `sc:Enumeration`, one key field `<@id>/label`, and one inline record
#' per level.
#'
#' @author EDG
#' @keywords internal
#' @noRd
enumeration_record_set <- function(levels, field_id, record_set) {
  column <- substring(field_id, nchar(record_set) + 2L)
  id <- paste0(record_set, "_", column, "_levels")
  label <- paste0(id, "/label")
  RecordSet(
    id = id,
    name = id,
    description = paste0("Levels of ", column, ", in order."),
    data_type = "sc:Enumeration",
    key = label,
    field = list(Field(id = label, name = "label", data_type = "sc:Text")),
    data = lapply(levels, \(l) stats::setNames(list(l), label))
  )
} # /croissant::enumeration_record_set
