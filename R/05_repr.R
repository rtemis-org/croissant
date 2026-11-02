# 05_repr.R
# ::croissant::
# 2026- EDG rtemis.org

# Printed representations of the node classes.
# spec: croissant/read-write#printing
#
# Each `repr()` method writes the class name, the node's `@id` or name, and its
# description, then only the properties that are set, most informative first.
# Lists show as many items as fit in `getOption("width")` and count the rest.

# %% repr.Croissant ----
#' Represent Croissant metadata
#'
#' The name and description, then the record sets that hold data with their
#' fields, the enumeration record sets with their value counts, the files, the
#' dataset metadata, and the calls that describe a record set's fields and read
#' its records.
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
  record_sets <- x@record_set %||% list()
  is_enum <- vapply(record_sets, is_enumeration, logical(1L))
  data_sets <- record_sets[!is_enum]
  enums <- record_sets[is_enum]
  enum_items <- vapply(
    enums,
    function(rs) {
      n <- n_records(rs)
      paste0(node_key(rs), if (n > 0L) paste0(" (", n, ")"))
    },
    character(1L)
  )
  files <- x@distribution %||% list()
  paste0(
    repr_header(
      "Croissant",
      first_text(x@name) %||% x@id %||% "(unnamed)",
      x@description,
      output_type = output_type
    ),
    repr_section(
      "Record sets",
      length(data_sets),
      repr_rows(
        vapply(data_sets, node_key, character(1L)),
        function(indent) {
          vapply(
            data_sets,
            \(rs) record_set_summary(rs, indent),
            character(1L)
          )
        },
        output_type = output_type
      ),
      output_type = output_type
    ),
    repr_section(
      "Enumerations",
      length(enums),
      if (length(enums) > 0L) {
        paste0("    ", fit_items(enum_items, 4L), "\n")
      },
      output_type = output_type
    ),
    repr_section(
      "Files",
      length(files),
      repr_rows(
        vapply(files, node_key, character(1L)),
        function(indent) vapply(files, file_summary, character(1L)),
        output_type = output_type
      ),
      output_type = output_type
    ),
    repr_section(
      "Metadata",
      -1L,
      repr_props(
        list(
          version = paste("Croissant", x@version),
          `dataset version` = x@dataset_version,
          license = license_label(x@license),
          creator = agents_label(x@creator),
          publisher = agents_label(x@publisher),
          published = x@date_published,
          url = x@url,
          keywords = items_label(x@keywords),
          `responsible AI` = items_label(
            names(.rai_terms)[
              !vapply(names(.rai_terms), \(p) is.null(prop(x, p)), logical(1L))
            ]
          ),
          `other properties` = items_label(names(x@other_properties))
        ),
        pad = 4L,
        output_type = output_type
      ),
      output_type = output_type
    ),
    repr_usage(
      c(
        data_sets[vapply(data_sets, \(rs) is.null(rs@data), logical(1L))],
        data_sets,
        record_sets
      ),
      output_type
    )
  )
} # /croissant::repr.Croissant


# %% repr.RecordSet ----
#' Represent a record set
#'
#' The `@id` and description, the data type, key, files, and inline values,
#' then one line per field with its data type, source, and reference.
#'
#' @param x `RecordSet` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, RecordSet) <- function(x, output_type = NULL, ...) {
  values <- enumeration_values(x)
  n <- n_records(x)
  origins <- unique(unlist(lapply(
    x@field %||% list(),
    \(f) source_file(f@source)
  )))
  paste0(
    repr_header("RecordSet", node_title(x), x@description, output_type),
    repr_props(
      list(
        name = other_name(x),
        type = type_label(x@data_type),
        key = items_label(short_ids(x@key, x@id)),
        files = items_label(origins),
        values = if (!is.null(values)) {
          \(indent) fit_items(values, indent)
        },
        `inline records` = if (is.null(values) && n > 0L) as.character(n),
        examples = if (!is.null(x@examples)) {
          as.character(length(as_array(x@examples)))
        },
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    ),
    repr_fields("Fields", x@field, x@id, output_type),
    repr_fields("Annotations", x@annotation, x@id, output_type)
  )
} # /croissant::repr.RecordSet


# %% repr.Field ----
#' Represent a field
#'
#' The `@id` and description, the data type, source, and reference, then one
#' line per sub-field.
#'
#' @param x `Field` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Field) <- function(x, output_type = NULL, ...) {
  parent <- x@parent_field
  paste0(
    repr_header("Field", node_title(x), x@description, output_type),
    repr_props(
      list(
        name = other_name(x),
        type = type_label(x@data_type),
        source = describe_source(x@source),
        references = describe_source(x@references),
        value = if (!is.null(x@value)) {
          as.character(to_json_text(x@value, pretty = FALSE))
        },
        repeated = json_logical(x@repeated),
        array = json_logical(x@is_array),
        `array shape` = x@array_shape,
        `parent field` = if (!is.null(parent)) {
          describe_source(parent@references %||% parent@source)
        },
        `equivalent to` = items_label(x@equivalent_property),
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    ),
    repr_fields("Sub-fields", x@sub_field, x@id, output_type),
    repr_fields("Annotations", x@annotation, x@id, output_type)
  )
} # /croissant::repr.Field


# %% repr.FileObject ----
#' Represent a file object
#'
#' @param x `FileObject` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, FileObject) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_header("FileObject", node_title(x), x@description, output_type),
    repr_props(
      list(
        name = other_name(x),
        url = x@content_url,
        format = items_label(x@encoding_format),
        size = if (!is.null(x@content_size)) {
          size <- size_label(x@content_size)
          if (identical(size, x@content_size)) {
            size
          } else {
            paste0(size, " (", x@content_size, ")")
          }
        },
        `contained in` = items_label(x@contained_in),
        source = describe_source(x@source),
        sha256 = x@sha256,
        md5 = x@md5,
        `same as` = items_label(x@same_as),
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    )
  )
} # /croissant::repr.FileObject


# %% repr.FileSet ----
#' Represent a file set
#'
#' @param x `FileSet` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, FileSet) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_header("FileSet", node_title(x), x@description, output_type),
    repr_props(
      list(
        name = other_name(x),
        includes = items_label(x@includes),
        excludes = items_label(x@excludes),
        format = items_label(x@encoding_format),
        `contained in` = items_label(x@contained_in),
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    )
  )
} # /croissant::repr.FileSet


# %% repr.Source ----
#' Represent a source
#'
#' @param x `Source` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Source) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_header("Source", describe_source(x), NULL, output_type),
    repr_props(
      list(
        `sampling rate` = if (!is.null(x@sampling_rate)) {
          format(x@sampling_rate)
        },
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    )
  )
} # /croissant::repr.Source


# %% repr.Extract ----
#' Represent an extraction
#'
#' @param x `Extract` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Extract) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_header("Extract", describe_extract(x), NULL, output_type),
    repr_props(
      list(`other properties` = items_label(names(x@other_properties))),
      output_type = output_type
    )
  )
} # /croissant::repr.Extract


# %% repr.Transform ----
#' Represent a transform
#'
#' @param x `Transform` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Transform) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_header("Transform", describe_transform(x), NULL, output_type),
    repr_props(
      list(`other properties` = items_label(names(x@other_properties))),
      output_type = output_type
    )
  )
} # /croissant::repr.Transform


# %% repr.ParentField ----
#' Represent a parent field
#'
#' @param x `ParentField` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, ParentField) <- function(x, output_type = NULL, ...) {
  paste0(
    repr_S7name("ParentField", output_type = output_type),
    repr_props(
      list(
        source = describe_source(x@source),
        references = describe_source(x@references),
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    )
  )
} # /croissant::repr.ParentField


# %% repr.Person, repr.Organization, repr.CreativeWork ----
#' Represent a person, an organization, or a creative work
#'
#' @param x `Person`, `Organization`, or `CreativeWork` object.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_agent <- function(x, output_type = NULL, ...) {
  title <- first_text(x@name)
  paste0(
    repr_header(
      S7_class(x)@name,
      title %||% x@id,
      x@description,
      output_type
    ),
    repr_props(
      list(
        id = if (!is.null(title)) x@id,
        email = if ("email" %in% prop_names(x)) x@email,
        url = x@url,
        `other properties` = items_label(names(x@other_properties))
      ),
      output_type = output_type
    )
  )
} # /croissant::repr_agent

method(repr, Person) <- repr_agent
method(repr, Organization) <- repr_agent
method(repr, CreativeWork) <- repr_agent


# %% print ----
# One `print()` method per node class, which writes `repr()`.
local({
  for (cls in list(
    Croissant,
    RecordSet,
    Field,
    FileObject,
    FileSet,
    Source,
    Extract,
    Transform,
    ParentField,
    Person,
    Organization,
    CreativeWork
  )) {
    method(print, cls) <- function(x, output_type = NULL, ...) {
      cat(repr(x, output_type = output_type))
      invisible(x)
    }
  }
})


# %% repr_header ----
#' Class name, title, and description lines of a node
#'
#' @param class_name Character: Class name.
#' @param title Optional Character: `@id` or name of the node.
#' @param description Optional Character: Description, named by language tag
#' where tagged.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_header <- function(class_name, title, description, output_type = NULL) {
  description <- first_text(description)
  paste0(
    repr_S7name(class_name, output_type = output_type),
    if (!is.null(title)) {
      paste0(
        "  ",
        highlight(fit_width(title, 2L), output_type = output_type),
        "\n"
      )
    },
    if (!is.null(description)) {
      paste0("  ", fit_width(one_line(description), 2L), "\n")
    }
  )
} # /croissant::repr_header


# %% repr_section ----
#' A bold section title with an item count, followed by its lines
#'
#' @param title Character: Section title.
#' @param n Integer: Number of items. A section with no item is omitted; a
#' negative `n` shows the title without a count.
#' @param body Character: Lines of the section.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_section <- function(title, n, body, output_type = NULL) {
  if (n == 0L || !nzchar(paste(body, collapse = ""))) {
    return("")
  }
  paste0(
    "  ",
    bold(title, output_type = output_type),
    if (n > 0L) gray(paste0(" (", n, ")"), output_type = output_type),
    "\n",
    body
  )
} # /croissant::repr_section


# %% repr_props ----
#' Aligned label and value lines
#'
#' @param values Named list: For each label, a character scalar, `NULL`, or a
#' function that takes the indentation of the value column and returns a
#' character scalar. `NULL` values are omitted.
#' @param pad Integer: Indentation.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_props <- function(values, pad = 2L, output_type = NULL) {
  values <- values[!vapply(values, is.null, logical(1L))]
  if (length(values) == 0L) {
    return("")
  }
  labels <- format(names(values))
  indent <- pad + nchar(labels[[1L]]) + 2L
  values <- vapply(
    values,
    \(v) if (is.function(v)) v(indent) else v,
    character(1L),
    USE.NAMES = FALSE
  )
  paste0(
    strrep(" ", pad),
    gray(labels, output_type = output_type),
    "  ",
    fit_width(values, indent),
    "\n",
    collapse = ""
  )
} # /croissant::repr_props


# %% repr_rows ----
#' Two-column rows: a bold key and its summary
#'
#' Shows the first `limit` rows and counts the rest.
#'
#' @param keys Character: Row keys.
#' @param summarize Function: Takes the indentation of the summary column and
#' returns one summary per key.
#' @param pad Integer: Indentation.
#' @param limit Integer: Maximum number of rows.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_rows <- function(
  keys,
  summarize,
  pad = 4L,
  limit = 12L,
  output_type = NULL
) {
  n <- length(keys)
  if (n == 0L) {
    return("")
  }
  shown <- seq_len(min(n, limit))
  keys <- format(unname(keys))
  indent <- pad + nchar(keys[[1L]]) + 2L
  summaries <- unname(summarize(indent))
  paste0(
    paste0(
      strrep(" ", pad),
      bold(keys[shown], output_type = output_type),
      "  ",
      fit_width(summaries[shown], indent),
      "\n",
      collapse = ""
    ),
    if (n > limit) {
      paste0(
        strrep(" ", pad),
        gray(paste0("... and ", n - limit, " more"), output_type = output_type),
        "\n"
      )
    }
  )
} # /croissant::repr_rows


# %% repr_fields ----
#' A section of fields, one row each with data type, source, and reference
#'
#' The data types form an aligned column. When every source names the same
#' file object or file set, the rows omit it.
#'
#' @param title Character: Section title.
#' @param fields Optional List: `Field` objects.
#' @param parent_id Optional Character: `@id` of the record set or field that
#' holds `fields`, removed from the start of each field's `@id`.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_fields <- function(title, fields, parent_id, output_type = NULL) {
  fields <- fields %||% list()
  repr_section(
    title,
    length(fields),
    repr_rows(
      vapply(fields, \(f) field_label(f, parent_id), character(1L)),
      function(indent) {
        origins <- unique(unlist(lapply(fields, \(f) source_file(f@source))))
        types <- format(vapply(
          fields,
          \(f) type_label(f@data_type) %||% "",
          character(1L)
        ))
        details <- vapply(
          fields,
          function(f) {
            n_sub <- length(f@sub_field)
            parts <- c(
              describe_source(f@source, origin = length(origins) > 1L),
              if (!is.null(f@references)) {
                paste("\u2192", describe_source(f@references))
              },
              if (n_sub > 0L) {
                paste0(
                  "(",
                  n_sub,
                  ngettext(n_sub, " sub-field)", " sub-fields)")
                )
              }
            )
            paste(parts, collapse = " ")
          },
          character(1L)
        )
        trimws(paste0(types, "  ", details), which = "right")
      },
      output_type = output_type
    ),
    output_type = output_type
  )
} # /croissant::repr_fields


# %% repr_usage ----
#' Calls that describe a record set's fields and read its records
#'
#' @param record_sets List: `RecordSet` objects; the first is used.
#' @param output_type Optional Character \{"ansi", "html", "plain"\}: Passed to
#' `fmt()`.
#'
#' @return Character scalar: Empty when there is no record set with a key.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_usage <- function(record_sets, output_type = NULL) {
  if (length(record_sets) == 0L) {
    return("")
  }
  key <- node_key(record_sets[[1L]])
  if (!nzchar(key)) {
    return("")
  }
  code <- format(paste0(c("fields", "read_record_set"), "(x, \"", key, "\")"))
  repr_section(
    "Usage",
    -1L,
    paste0(
      "    ",
      code,
      "  ",
      gray(c("fields", "records"), output_type = output_type),
      "\n",
      collapse = ""
    ),
    output_type = output_type
  )
} # /croissant::repr_usage


# %% record_set_summary ----
#' One-line summary of a record set: field count, inline records, and fields
#'
#' @param rs `RecordSet` object.
#' @param indent Integer: Column at which the summary starts.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_set_summary <- function(rs, indent) {
  fields <- rs@field %||% list()
  n <- length(fields)
  n_inline <- n_records(rs)
  lead <- paste0(
    n,
    ngettext(n, " field", " fields"),
    if (n_inline > 0L) {
      paste0(
        ", ",
        n_inline,
        ngettext(n_inline, " inline record", " inline records")
      )
    }
  )
  if (n == 0L) {
    return(lead)
  }
  lead <- paste0(lead, ": ")
  labels <- vapply(fields, \(f) field_label(f, rs@id), character(1L))
  paste0(lead, fit_items(labels, indent + nchar(lead)))
} # /croissant::record_set_summary


# %% file_summary ----
#' One-line summary of a file object or file set
#'
#' @param x `FileObject` or `FileSet` object.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
file_summary <- function(x) {
  is_set <- S7_inherits(x, FileSet)
  parts <- c(
    if (is_set) "file set",
    items_label(x@encoding_format),
    if (is_set) {
      if (!is.null(x@includes)) paste("includes", items_label(x@includes))
    } else {
      c(
        size_label(x@content_size),
        if (!identical(x@content_url, x@id)) x@content_url
      )
    },
    if (!is.null(x@contained_in)) paste("in", items_label(x@contained_in))
  )
  paste(parts, collapse = ", ")
} # /croissant::file_summary


# %% size_label ----
#' A content size in bytes as SI units
#'
#' @param x Optional Character: Content size, such as `"117743 B"`.
#'
#' @return Character scalar: `x` in kB, MB, GB, or TB, such as `"117.7 kB"`,
#' when `x` is a number of bytes; otherwise `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
size_label <- function(x) {
  if (is.null(x) || !grepl("^[0-9]+ ?B$", x)) {
    return(x)
  }
  bytes <- as.numeric(sub(" ?B$", "", x))
  format(
    structure(bytes, class = "object_size"),
    units = "auto",
    standard = "SI"
  )
} # /croissant::size_label


# %% describe_source ----
#' One-line description of a source: origin, extraction, and transforms
#'
#' @param s Optional `Source` object.
#' @param origin Logical: Whether to include the file object, file set, field,
#' or record set the source names.
#'
#' @return Character scalar, or `NULL` when `s` is `NULL` or has nothing to
#' describe.
#'
#' @author EDG
#' @keywords internal
#' @noRd
describe_source <- function(s, origin = TRUE) {
  if (is.null(s)) {
    return(NULL)
  }
  parts <- c(
    if (origin) source_ref(s),
    if (!is.null(s@extract)) describe_extract(s@extract),
    vapply(s@transform %||% list(), describe_transform, character(1L)),
    if (!is.null(s@format)) paste("format", s@format)
  )
  if (length(parts) == 0L) NULL else paste(parts, collapse = ", ")
} # /croissant::describe_source


# %% source_file ----
#' File object or file set a source reads from
#'
#' @param s Optional `Source` object.
#'
#' @return Character scalar, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
source_file <- function(s) {
  if (is.null(s)) NULL else s@file_object %||% s@file_set
} # /croissant::source_file


# %% describe_extract ----
#' One-line description of an extraction
#'
#' @param x `Extract` object.
#'
#' @return Character scalar, or `NULL` when no property is set.
#'
#' @author EDG
#' @keywords internal
#' @noRd
describe_extract <- function(x) {
  parts <- c(
    column = x@column,
    JSONPath = x@json_path,
    `file property` = x@file_property
  )
  if (length(parts) == 0L) NULL else paste(names(parts), parts, collapse = ", ")
} # /croissant::describe_extract


# %% describe_transform ----
#' One-line description of a transform
#'
#' @param x `Transform` object.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
describe_transform <- function(x) {
  parts <- c(
    regex = x@regex,
    replace = x@replace,
    format = x@format,
    JSONPath = x@json_path,
    separator = if (!is.null(x@separator)) dQuote(x@separator, FALSE),
    `sampling rate` = if (!is.null(x@sampling_rate)) format(x@sampling_rate)
  )
  out <- c(
    if (length(parts) > 0L) paste(names(parts), parts),
    if (isTRUE(x@read_lines)) "read lines",
    if (isTRUE(x@un_archive)) "extract archive"
  )
  if (length(out) == 0L) "transform" else paste(out, collapse = ", ")
} # /croissant::describe_transform


# %% is_enumeration ----
#' Test whether a record set is an enumeration
#'
#' @param rs `RecordSet` object.
#'
#' @return Logical scalar: `TRUE` when the data type includes `sc:Enumeration`
#' or `cr:Split`, the enumeration of a dataset's splits.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_enumeration <- function(rs) {
  any(c("sc:Enumeration", "cr:Split") %in% rs@data_type)
} # /croissant::is_enumeration


# %% n_records ----
#' Number of inline records of a record set
#'
#' @param rs `RecordSet` object.
#'
#' @return Integer scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
n_records <- function(rs) {
  if (is.null(rs@data)) 0L else length(as_array(rs@data))
} # /croissant::n_records


# %% enumeration_values ----
#' Inline values of a one-field record set
#'
#' @param rs `RecordSet` object.
#'
#' @return Character: One value per inline record, or `NULL` when the record
#' set has no inline records or a record holds more than one scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
enumeration_values <- function(rs) {
  if (is.null(rs@data)) {
    return(NULL)
  }
  records <- as_array(rs@data)
  scalar <- vapply(
    records,
    \(r) {
      is.list(r) &&
        length(r) == 1L &&
        is.atomic(r[[1L]]) &&
        length(r[[1L]]) == 1L
    },
    logical(1L)
  )
  if (!all(scalar)) {
    return(NULL)
  }
  vapply(records, \(r) as.character(r[[1L]]), character(1L))
} # /croissant::enumeration_values


# %% node_title ----
#' `@id` of a node, or its first name
#'
#' @param x Node with `id` and `name` properties.
#'
#' @return Character scalar, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_title <- function(x) {
  x@id %||% first_text(x@name)
} # /croissant::node_title


# %% other_name ----
#' First name of a node, when it differs from the node's title
#'
#' @param x Node with `id` and `name` properties.
#'
#' @return Character scalar, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
other_name <- function(x) {
  name <- first_text(x@name)
  if (is.null(name) || identical(name, node_title(x))) NULL else name
} # /croissant::other_name


# %% field_label ----
#' Name of a field, without the `@id` prefix of its record set or parent field
#'
#' @param f `Field` object.
#' @param parent_id Optional Character: `@id` of the record set or parent
#' field.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
field_label <- function(f, parent_id = NULL) {
  label <- first_text(f@name) %||% f@id %||% "(unnamed)"
  short_ids(label, parent_id)
} # /croissant::field_label


# %% short_ids ----
#' Remove a `<parent_id>/` prefix
#'
#' @param x Optional Character: `@id` values.
#' @param parent_id Optional Character: `@id` of the parent node.
#'
#' @return Character, or `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
short_ids <- function(x, parent_id) {
  if (is.null(x) || is.null(parent_id)) {
    return(x)
  }
  prefix <- paste0(parent_id, "/")
  has_prefix <- startsWith(x, prefix) & nchar(x) > nchar(prefix)
  x[has_prefix] <- substring(x[has_prefix], nchar(prefix) + 1L)
  x
} # /croissant::short_ids


# %% type_label ----
#' Data types without the `sc:` and `cr:` prefixes
#'
#' @param x Optional Character: Data types as compact IRIs.
#'
#' @return Character scalar, or `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
type_label <- function(x) {
  if (is.null(x)) NULL else paste(sub("^(sc|cr):", "", x), collapse = ", ")
} # /croissant::type_label


# %% license_label ----
#' Licenses as one line
#'
#' @param x Optional Character or List: Licenses, as strings and
#' `CreativeWork` objects.
#'
#' @return Character scalar, or `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
license_label <- function(x) {
  items_label(license_names(x))
} # /croissant::license_label


# %% license_names ----
#' Licenses as strings
#'
#' @param x Optional Character or List: Licenses, as strings and
#' `CreativeWork` objects.
#'
#' @return Character: Each string, and each `CreativeWork`'s name, URL, or
#' `@id`; `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
license_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  vapply(
    as.list(x),
    function(v) {
      if (is.character(v)) {
        v
      } else {
        first_text(v@name) %||% v@url %||% v@id %||% "license"
      }
    },
    character(1L),
    USE.NAMES = FALSE
  )
} # /croissant::license_names


# %% agents_label ----
#' Names of persons and organizations as one line
#'
#' @param x Optional List: `Person` and `Organization` objects.
#'
#' @return Character scalar, or `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
agents_label <- function(x) {
  items_label(agent_names(x))
} # /croissant::agents_label


# %% agent_names ----
#' Names of persons and organizations
#'
#' @param x Optional List: `Person` and `Organization` objects.
#'
#' @return Character: Each agent's first name, else its `@id`, else its class
#' name; `NULL` when `x` is `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
agent_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  vapply(
    x,
    \(v) first_text(v@name) %||% v@id %||% S7_class(v)@name,
    character(1L),
    USE.NAMES = FALSE
  )
} # /croissant::agent_names


# %% items_label ----
#' Values as one comma-separated line
#'
#' @param x Optional Character: Values.
#'
#' @return Character scalar, or `NULL` when `x` is empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
items_label <- function(x) {
  if (length(x) == 0L) NULL else paste(x, collapse = ", ")
} # /croissant::items_label


# %% json_logical ----
#' A logical scalar as JSON text
#'
#' @param x Optional Logical scalar.
#'
#' @return `"true"`, `"false"`, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
json_logical <- function(x) {
  if (is.null(x)) NULL else tolower(as.character(x))
} # /croissant::json_logical


# %% fit_items ----
#' As many values as fit in the console width, then a count of the rest
#'
#' @param x Character: Values.
#' @param indent Integer: Column at which the line starts.
#'
#' @return Character scalar, such as `"a, b, c, and 8 more"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_items <- function(x, indent) {
  n <- length(x)
  width <- line_width(indent)
  out <- x[[1L]]
  for (k in seq_len(n)) {
    line <- paste0(
      paste(x[seq_len(k)], collapse = ", "),
      if (k < n) paste0(", and ", n - k, " more")
    )
    if (k > 1L && nchar(line, type = "width") > width) {
      break
    }
    out <- line
  }
  out
} # /croissant::fit_items


# %% fit_width ----
#' Truncate text to the console width
#'
#' @param x Character: Text.
#' @param indent Integer: Column at which the text starts.
#'
#' @return Character: `x`, with each value longer than the remaining width
#' cut and ended with `...`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_width <- function(x, indent) {
  width <- line_width(indent)
  long <- nchar(x, type = "width") > width
  x[long] <- paste0(substr(x[long], 1L, width - 3L), "...")
  x
} # /croissant::fit_width


# %% line_width ----
#' Characters available after an indentation
#'
#' @param indent Integer: Column at which the text starts.
#'
#' @return Integer scalar: `getOption("width")` minus `indent`, at least 20.
#'
#' @author EDG
#' @keywords internal
#' @noRd
line_width <- function(indent) {
  max(getOption("width", 80L) - indent, 20L)
} # /croissant::line_width


# %% one_line ----
#' Collapse whitespace runs, including newlines, to single spaces
#'
#' @param x Character: Text.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
one_line <- function(x) {
  trimws(gsub("\\s+", " ", x))
} # /croissant::one_line


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
