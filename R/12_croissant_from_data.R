# 12_croissant_from_data.R
# ::croissant::
# 2026- EDG rtemis.org

# Croissant metadata for an R table.
# spec: croissant/read-write#metadata-from-data

# Croissant data type of each R column class, checked in order.
.type_by_class <- c(
  integer64 = "sc:Integer",
  integer = "sc:Integer",
  numeric = "sc:Float",
  logical = "sc:Boolean",
  Date = "sc:Date",
  POSIXct = "sc:DateTime",
  factor = "sc:Text",
  character = "sc:Text"
)


# %% croissant_from_data ----
#' Create Croissant metadata for a table
#'
#' Writes `x` to `file` as CSV or Parquet, and returns a `Croissant` object
#' that describes it: one `FileObject` with the file's size and SHA-256
#' checksum, one `RecordSet`, and one `Field` per column, whose data type
#' follows the column class: integer to `sc:Integer`, double to `sc:Float`,
#' logical to `sc:Boolean`, Date to `sc:Date`, POSIXct to `sc:DateTime`, and
#' character and factor to `sc:Text`. Write the metadata with
#' [write_croissant()] into the directory of `file`, where `content_url`
#' resolves.
#'
#' A factor is written as its labels. Its levels, in order, form an
#' enumeration record set, `<record_set>_<column>_levels`, with data type
#' `sc:Enumeration`, one field `label`, and the levels as inline records; the
#' factor's field `references` that field, so [read_record_set()] returns the
#' column as a factor with the same levels. In CSV, a missing value is written
#' as an empty field and an empty string as `""`, and they read back as `NA`
#' and `""`. Parquet keeps every other column type.
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
    \(col) column_data_type(x[[col]], col),
    character(1L)
  )
  data <- as.data.frame(x)
  for (col in names(data)[vapply(data, is.factor, logical(1L))]) {
    data[[col]] <- as.character(data[[col]])
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  encoding_format <- if (format == "csv") {
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
    ...
  )
} # /croissant::croissant_from_data


# %% column_data_type ----
#' Croissant data type of a column
#'
#' @param v Column vector.
#' @param col Character: Column name, for messages.
#'
#' @return Character scalar: Compact IRI.
#'
#' @author EDG
#' @keywords internal
#' @noRd
column_data_type <- function(v, col) {
  hit <- names(.type_by_class)[names(.type_by_class) %in% class(v)]
  if (is.double(v) && length(hit) == 0L) {
    hit <- "numeric"
  }
  if (length(hit) == 0L) {
    abort(
      "Column \"",
      col,
      "\" has class ",
      paste(class(v), collapse = "/"),
      ", which has no Croissant data type. Convert it to integer, double, ",
      "logical, character, factor, Date, or POSIXct.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  unname(.type_by_class[[hit[[1L]]]])
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
