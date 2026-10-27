# 11_cast.R
# ::croissant::
# 2026- EDG rtemis.org

# Transforms and data types of field values.
# spec: croissant/read-write#record-sets

# R type of each data type, by the local name of its IRI. The first data type
# of a field found here decides the cast; mlcroissant's EXPECTED_DATA_TYPES
# (core/data_types.py) maps the same names.
.cast_by_type <- c(
  Boolean = "logical",
  Date = "Date",
  DateTime = "POSIXct",
  Float = "double",
  Float16 = "double",
  Float32 = "double",
  Float64 = "double",
  Number = "double",
  Integer = "integer",
  Int8 = "integer",
  Int16 = "integer",
  Int32 = "integer",
  Int64 = "double",
  UInt8 = "integer",
  UInt16 = "integer",
  UInt32 = "double",
  UInt64 = "double",
  Text = "character",
  URL = "character",
  Time = "character",
  ImageObject = "path",
  AudioObject = "path",
  VideoObject = "path",
  BoundingBox = "box"
)


# %% field_cast ----
#' R type a field's values are cast to
#'
#' @param f `Field` object.
#'
#' @return Character scalar, a value of `.cast_by_type`, or `NA` when no data
#' type of the field has one.
#'
#' @author EDG
#' @keywords internal
#' @noRd
field_cast <- function(f) {
  locals <- sub("^.*[:/#]", "", f@data_type %||% character())
  hit <- locals[locals %in% names(.cast_by_type)]
  if (length(hit) == 0L) NA_character_ else unname(.cast_by_type[[hit[[1L]]]])
} # /croissant::field_cast


# %% is_text_type ----
#' Test whether a field's values are text
#'
#' @param f `Field` object.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_text_type <- function(f) {
  identical(field_cast(f), "character")
} # /croissant::is_text_type


# %% cast_values ----
#' Cast a field's values to its R type
#'
#' @param values Vector or list: One value per record.
#' @param f `Field` object.
#' @param repeated Logical: Whether each value is a list of values.
#'
#' @return Atomic vector, or a list when the field is repeated or a value is
#' itself a list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cast_values <- function(values, f, repeated = FALSE) {
  type <- field_cast(f)
  # Image, audio, and video bytes, as Parquet files hold them, stay raw
  # vectors, one per record.
  if (is.list(values) && any(vapply(values, is.raw, logical(1L)))) {
    return(values)
  }
  if (repeated || (is.list(values) && any(lengths(values) > 1L))) {
    return(lapply(values, function(v) {
      if (is.null(v)) NULL else cast_atomic(unlist(v, use.names = FALSE), type)
    }))
  }
  if (is.list(values)) {
    values <- vapply(
      values,
      function(v) {
        if (is.null(v) || length(v) == 0L) {
          NA_character_
        } else {
          as.character(v[[1L]])
        }
      },
      character(1L)
    ) |>
      restore_json_type(values)
  }
  cast_atomic(values, type)
} # /croissant::cast_values


# %% restore_json_type ----
#' Restore the JSON type of values flattened to strings
#'
#' @param x Character: Flattened values.
#' @param values List: The original values.
#'
#' @return `x` as logical, integer, or double when every non-missing original
#' value has that type; otherwise `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
restore_json_type <- function(x, values) {
  present <- values[
    !vapply(values, \(v) is.null(v) || length(v) == 0L, logical(1L))
  ]
  if (length(present) == 0L) {
    return(x)
  }
  if (all(vapply(present, is.logical, logical(1L)))) {
    return(as.logical(x))
  }
  if (all(vapply(present, is.integer, logical(1L)))) {
    return(as.integer(x))
  }
  if (all(vapply(present, is.numeric, logical(1L)))) {
    return(as.numeric(x))
  }
  x
} # /croissant::restore_json_type


# %% cast_atomic ----
#' Cast an atomic vector to an R type
#'
#' @param x Atomic vector.
#' @param type Character: Value of `.cast_by_type`, or `NA` to keep `x`.
#'
#' @return Vector of the type; a list of numeric vectors for `"box"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cast_atomic <- function(x, type) {
  if (is.na(type)) {
    return(x)
  }
  switch(
    type,
    character = ,
    path = as.character(x),
    logical = {
      if (is.logical(x)) {
        x
      } else {
        s <- tolower(trimws(as.character(x)))
        out <- rep(NA, length(s))
        out[s %in% c("true", "t", "yes", "y", "1")] <- TRUE
        out[s %in% c("false", "f", "no", "n", "0")] <- FALSE
        out
      }
    },
    integer = {
      num <- suppressWarnings(as.numeric(x))
      if (all(is.na(num) | abs(num) <= .Machine[["integer.max"]])) {
        as.integer(num)
      } else {
        num
      }
    },
    double = suppressWarnings(as.numeric(x)),
    # mlcroissant reads both sc:Date and sc:DateTime as pandas Timestamps,
    # which keep a time of day. A sc:Date value with a time is read as
    # POSIXct, so the time is kept.
    Date = {
      s <- as.character(x)
      if (all(is.na(s) | grepl("^\\d{4}-\\d{2}-\\d{2}$", s))) {
        as.Date(s)
      } else {
        cast_atomic(s, "POSIXct")
      }
    },
    POSIXct = as.POSIXct(
      as.character(x),
      tz = "UTC",
      tryFormats = c(
        "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%dT%H:%M:%OS",
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d"
      )
    ),
    box = lapply(as.character(x), function(v) {
      if (is.na(v)) {
        return(NULL)
      }
      as.numeric(
        strsplit(gsub("[][(),]", " ", v), "\\s+")[[1L]] |> Filter(f = nzchar)
      )
    })
  )
} # /croissant::cast_atomic


# %% apply_transforms ----
#' Apply a source's transforms to its values, in order
#'
#' Follows mlcroissant's `_apply_transform_fn()`: `regex` keeps the first
#' matched capture group of a match at the start of the value, and the value
#' itself when the regex does not match; `jsonPath` keeps the first match;
#' `format` formats a date-time; `separator` splits a string into a list.
#' Missing values pass through.
#'
#' @param values Vector or list: One value per record.
#' @param transforms Optional List: `Transform` objects.
#'
#' @return Values after the transforms.
#'
#' @author EDG
#' @keywords internal
#' @noRd
apply_transforms <- function(values, transforms) {
  for (t in transforms) {
    if (!is.null(t@regex)) {
      values <- transform_regex(values, t@regex)
    } else if (!is.null(t@json_path)) {
      values <- lapply(as.list(values), function(v) {
        if (is.null(v) || (length(v) == 1L && is.na(v))) {
          return(NULL)
        }
        if (is.character(v)) {
          v <- jsonlite::parse_json(v, simplifyVector = FALSE)
        }
        m <- jsonpath(v, t@json_path)
        if (length(m) == 0L) NULL else m[[1L]]
      })
    } else if (!is.null(t@format)) {
      values <- format(
        as.POSIXct(as.character(values), tz = "UTC", optional = TRUE),
        t@format
      )
    } else if (!is.null(t@separator)) {
      values <- lapply(as.character(values), function(v) {
        if (is.na(v)) NULL else strsplit(v, t@separator, fixed = TRUE)[[1L]]
      })
    }
  }
  values
} # /croissant::apply_transforms


# %% transform_regex ----
#' Keep the first capture group of a regex matched at the start of each value
#'
#' @param values Vector or list of values.
#' @param regex Character: Regular expression, as in Python's `re`.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
transform_regex <- function(values, regex) {
  values <- if (is.list(values)) {
    vapply(
      values,
      \(v) if (is.null(v)) NA_character_ else as.character(v[[1L]]),
      character(1L)
    )
  } else {
    as.character(values)
  }
  pattern <- paste0("^(?:", regex, ")")
  m <- regmatches(values, regexec(pattern, values, perl = TRUE))
  vapply(
    seq_along(values),
    function(i) {
      if (is.na(values[[i]]) || length(m[[i]]) == 0L) {
        return(values[[i]])
      }
      groups <- m[[i]][-1L]
      groups <- groups[nzchar(groups)]
      if (length(groups) == 0L) values[[i]] else groups[[1L]]
    },
    character(1L)
  )
} # /croissant::transform_regex
