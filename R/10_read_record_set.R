# 10_read_record_set.R
# ::croissant::
# 2026- EDG rtemis.org

# Records of a record set as a table.
# spec: croissant/read-write#record-sets
#
# The procedure follows mlcroissant 1.1.1 (operation_graph/operations/*.py):
# each field's source names an origin, a file or another record set; the
# origins are read into tables and left-joined through `references`; each
# field's values are extracted, transformed, and cast to its data type. Each
# record set table has one column per leaf field, named by field `@id`, which
# is how another record set references it; `read_record_set()` renames the
# columns of the table it returns with `column_names()`.

# %% read_record_set ----
#' Read the records of a record set
#'
#' Reads the files a record set draws from, and returns its records as a
#' table with one column per field. A column is named by the field's `@id`
#' without the record set's `@id` and `/`: field `default/age` of record set
#' `default` is column `age`, and subfield `images/bbox/x` of record set
#' `images` is column `bbox/x`. A field `@id` without that prefix names its
#' column whole. When two names coincide, every column keeps its full `@id`.
#'
#' A text field that `references` a field of an enumeration record set (data
#' type `sc:Enumeration`) is a factor whose levels are the enumeration's
#' values in record order, as [croissant_from_data()] writes a factor. A
#' column holding a value outside the enumeration stays character, with a
#' warning.
#'
#' Files with an `http(s)` `contentUrl` are downloaded into `cache_dir`, and
#' their `sha256` or `md5` is verified. Files of a Hugging Face dataset
#' repository (a `git+https` `FileObject` with a Hub `contentUrl`, as in the
#' metadata [read_croissant_hf()] reads, or an `hf://datasets/` path) are
#' downloaded from the resolved commit and verified against the Hub's
#' listing, as [hf_download()] does. Zip and tar archives are extracted
#' into `cache_dir`. Relative `contentUrl`s and `FileSet` globs resolve
#' against `base`. Columns are read from CSV, TSV, JSON, JSON Lines, and
#' Parquet files; Parquet requires the 'arrow' package. A field extracting
#' the `content` of an image, audio, or video file holds the file's local
#' path; with a text data type it holds the file's text.
#'
#' Values are cast by the field's first data type with an R counterpart:
#' `sc:Integer` to integer, `sc:Float` and `sc:Number` to double,
#' `sc:Boolean` to logical, `sc:Date` to Date, `sc:DateTime` to POSIXct
#' (UTC), and `sc:Text` and `sc:URL` to character. A repeated field, or one
#' split by a `separator` transform, is a list column.
#'
#' @param x `Croissant` object, or Character: Path or URL of a Croissant
#' JSON-LD file.
#' @param record_set Character: `@id` or name of the record set.
#' @param output Character \{"data.table", "tibble", "data.frame", "arrow"\}:
#' Class of the result. `"tibble"` requires the 'tibble' package and
#' `"arrow"` the 'arrow' package.
#' @param base Optional Character: Directory or URL that relative
#' `contentUrl`s and `FileSet` globs resolve against. `NULL` uses the `base`
#' of the `Croissant` object: the directory of the path, URL, or
#' `hf://datasets/` path of a file that [read_croissant()] read. A relative
#' `contentUrl` under an `hf://datasets/` directory names a file of that
#' repository at the same revision.
#' @param cache_dir Character: Directory for downloaded and extracted files.
#' @param n_max Numeric \[1, Inf\]: Most records to return. Files of a
#' `FileSet` are read in order until the first origin holds `n_max` records,
#' so later files are not downloaded.
#' @param token Optional Character: Hugging Face token, for files of gated or
#' private Hub datasets. `NULL` reads `HF_TOKEN`.
#' @param verbosity Integer: Verbosity level; `1L` reports the record set
#' before reading, each file downloaded or read from the cache, each archive
#' extracted or whose cached extraction is reused, and the number of rows and
#' columns read, with the elapsed time.
#'
#' @return A data.table, tibble, data.frame, or arrow Table, by `output`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines(c("name,age", "Alice,22", "Bob,23"), file.path(dir, "people.csv"))
#' x <- Croissant(
#'   name = "people",
#'   distribution = list(
#'     FileObject(id = "people.csv", content_url = "people.csv", encoding_format = "text/csv")
#'   ),
#'   record_set = list(RecordSet(
#'     id = "people",
#'     field = list(
#'       Field(
#'         id = "people/name",
#'         data_type = "sc:Text",
#'         source = Source(file_object = "people.csv", extract = Extract(column = "name"))
#'       ),
#'       Field(
#'         id = "people/age",
#'         data_type = "sc:Integer",
#'         source = Source(file_object = "people.csv", extract = Extract(column = "age"))
#'       )
#'     )
#'   ))
#' )
#' read_record_set(x, "people", base = dir)
#' unlink(dir, recursive = TRUE)
read_record_set <- function(
  x,
  record_set,
  output = c("data.table", "tibble", "data.frame", "arrow"),
  base = NULL,
  cache_dir = getOption(
    "croissant.cache_dir",
    tools::R_user_dir("croissant", "cache")
  ),
  n_max = Inf,
  token = NULL,
  verbosity = 1L
) {
  output <- match.arg(output)
  check_character_scalar(record_set)
  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) || n_max < 1) {
    abort(
      "`n_max` must be a number of records, at least 1, or Inf.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_optional_character_scalar(base)
  check_character_scalar(cache_dir)
  if (output == "tibble") {
    check_dependencies("tibble")
  }
  if (output == "arrow") {
    check_dependencies("arrow")
  }
  if (!S7_inherits(x, Croissant)) {
    check_character_scalar(x)
    x <- read_croissant(x, verbosity = 0L)
  }
  base <- base %||% x@base
  ctx <- new_load_context(x, base, cache_dir, verbosity, hf_token(token))
  rs <- find_record_set(x, record_set)
  start <- Sys.time()
  msg0(
    bold(highlight("\u25B6")),
    " Reading record set ",
    highlight(node_key(rs)),
    if (!is.null(x@name)) paste0(" of ", first_text(x@name)),
    "...",
    verbosity = verbosity
  )
  out <- record_set_table(ctx, rs, n_max)
  enumeration_factors(ctx, rs, out)
  data.table::setnames(out, column_names(rs, names(out)))
  msg(
    "Read in",
    highlightbig(nrow(out)),
    "x",
    highlightbig(ncol(out)),
    if (nrow(out) >= n_max) "(n_max reached)",
    sprintf(
      "in %.1f s",
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    ),
    verbosity = verbosity
  )
  table_output(out, output)
} # /croissant::read_record_set


# %% table_output ----
#' A data.table in the class `output` names
#'
#' @param x data.table.
#' @param output Character \{"data.table", "tibble", "data.frame", "arrow"\}:
#' Class of the result. `"tibble"` requires the 'tibble' package and `"arrow"`
#' the 'arrow' package.
#'
#' @return A data.table, tibble, data.frame, or arrow Table, by `output`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
table_output <- function(x, output) {
  if (output %in% c("tibble", "arrow")) {
    check_dependencies(output)
  }
  switch(
    output,
    data.table = x,
    data.frame = as.data.frame(x),
    tibble = tibble::as_tibble(x),
    arrow = arrow::as_arrow_table(as.data.frame(x))
  )
} # /croissant::table_output


# %% find_record_set ----
#' Find a record set by `@id` or name
#'
#' @param x `Croissant` object.
#' @param record_set Character: `@id` or name.
#'
#' @return `RecordSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
find_record_set <- function(x, record_set) {
  for (rs in x@record_set) {
    if (identical(rs@id, record_set) || record_set %in% rs@name) {
      return(rs)
    }
  }
  ids <- vapply(x@record_set, \(rs) node_ref(rs), character(1L))
  abort(
    "The dataset has no record set \"",
    record_set,
    "\". Its record sets are: ",
    paste(ids, collapse = ", "),
    ".",
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /croissant::find_record_set


# %% column_names ----
#' Column names of a record set table
#'
#' A field `@id` that starts with the `@id` of the record set (its name when
#' it has no `@id`) followed by `/` loses that prefix; any other `@id` is kept
#' whole. When two shortened names coincide, or one is empty, every column
#' keeps its full `@id`.
#'
#' @param rs `RecordSet` object.
#' @param ids Character: Field `@id`s, one per column.
#'
#' @return Character: Column names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
column_names <- function(rs, ids) {
  rs_id <- rs@id %||% first_text(rs@name)
  if (is.null(rs_id)) {
    return(ids)
  }
  prefix <- paste0(rs_id, "/")
  short <- ifelse(
    startsWith(ids, prefix),
    substring(ids, nchar(prefix) + 1L),
    ids
  )
  if (anyDuplicated(short) > 0L || !all(nzchar(short))) {
    return(ids)
  }
  short
} # /croissant::column_names


# %% enumeration_factors ----
#' Convert fields that reference an enumeration to factors
#'
#' A character column whose field `references` a field of a record set with
#' data type `sc:Enumeration` becomes a factor. Its levels are the values of
#' the referenced field in record order: the order of the enumeration's
#' inline `data`, or of its file. A column holding a value outside the
#' enumeration stays character, with a warning.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param rs `RecordSet` object.
#' @param tab data.table: Records of `rs`, columns named by field `@id`;
#' modified in place.
#'
#' @return `tab`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
enumeration_factors <- function(ctx, rs, tab) {
  for (leaf in leaf_fields(rs@field)) {
    f <- leaf[["field"]]
    ref <- f@references
    values <- tab[[f@id]]
    if (is.null(ref) || is.null(ref@field) || !is.character(values)) {
      next
    }
    enum_id <- ctx[["field_rs"]][ref@field]
    if (is.na(enum_id)) {
      next
    }
    enum <- find_record_set(ctx[["x"]], unname(enum_id))
    if (!"sc:Enumeration" %in% enum@data_type) {
      next
    }
    levels <- unique(as.character(record_set_table(ctx, enum)[[ref@field]]))
    levels <- levels[!is.na(levels)]
    outside <- setdiff(unique(values[!is.na(values)]), levels)
    if (length(outside) > 0L) {
      warn(
        "Field ",
        f@id,
        " stays character: ",
        paste(utils::head(outside, 5L), collapse = ", "),
        if (length(outside) > 5L) ", ..." else "",
        " not among the levels of enumeration ",
        unname(enum_id),
        ".",
        use_warning = TRUE
      )
      next
    }
    data.table::set(tab, j = f@id, value = factor(values, levels = levels))
  }
  invisible(tab)
} # /croissant::enumeration_factors


# %% record_set_table ----
#' Records of a record set, one column per leaf field
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param rs `RecordSet` object.
#' @param n_max Numeric: Most records to return.
#'
#' @return data.table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_set_table <- function(ctx, rs, n_max = Inf) {
  key <- rs@id %||% node_ref(rs)
  if (!is.null(ctx[["tables"]][[key]])) {
    return(utils::head(ctx[["tables"]][[key]], n_max))
  }
  leaves <- leaf_fields(rs@field)
  out <- if (!is.null(rs@data)) {
    inline_table(rs, leaves)
  } else {
    sourced_table(ctx, rs, leaves, n_max)
  }
  if (is.infinite(n_max)) {
    ctx[["tables"]][[key]] <- out
  }
  utils::head(out, n_max)
} # /croissant::record_set_table


# %% leaf_fields ----
#' Fields without subfields, depth first
#'
#' @param fields List: `Field` objects.
#' @param repeated Logical: Whether an enclosing field is repeated.
#'
#' @return List of lists with `field` and `repeated`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
leaf_fields <- function(fields, repeated = FALSE) {
  out <- list()
  for (f in fields) {
    rep_here <- repeated || isTRUE(f@repeated) || isTRUE(f@is_array)
    if (length(f@sub_field) > 0L) {
      out <- c(out, leaf_fields(f@sub_field, rep_here))
    } else {
      out[[length(out) + 1L]] <- list(field = f, repeated = rep_here)
    }
  }
  out
} # /croissant::leaf_fields


# %% inline_table ----
#' Table of a record set's inline `data`
#'
#' @param rs `RecordSet` object.
#' @param leaves List from `leaf_fields()`.
#'
#' @return data.table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
inline_table <- function(rs, leaves) {
  records <- as_array(rs@data)
  cols <- lapply(leaves, function(leaf) {
    f <- leaf[["field"]]
    values <- lapply(records, \(r) r[[f@id]])
    cast_values(values, f, leaf[["repeated"]])
  })
  names(cols) <- vapply(leaves, \(l) l[["field"]]@id, character(1L))
  data.table::as.data.table(cols)
} # /croissant::inline_table


# %% sourced_table ----
#' Table of a record set whose fields read from files or record sets
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param rs `RecordSet` object.
#' @param leaves List from `leaf_fields()`.
#' @param n_max Numeric: Most records of the first origin to read.
#'
#' @return data.table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sourced_table <- function(ctx, rs, leaves, n_max = Inf) {
  origins <- list()
  for (leaf in leaves) {
    s <- leaf[["field"]]@source
    if (is.null(s)) {
      next
    }
    o <- source_origin(ctx, s)
    if (is.null(origins[[o[["key"]]]])) {
      origins[[o[["key"]]]] <- c(o, list(sources = list()))
    }
    origins[[o[["key"]]]][["sources"]] <- c(
      origins[[o[["key"]]]][["sources"]],
      list(s)
    )
  }
  # A join adds the origin a field `references`.
  for (leaf in leaves) {
    r <- leaf[["field"]]@references
    if (is.null(r) || is.null(leaf[["field"]]@source)) {
      next
    }
    o <- source_origin(ctx, r)
    if (is.null(origins[[o[["key"]]]])) {
      origins[[o[["key"]]]] <- c(o, list(sources = list()))
    }
    origins[[o[["key"]]]][["sources"]] <- c(
      origins[[o[["key"]]]][["sources"]],
      list(r)
    )
  }
  if (length(origins) == 0L) {
    n <- 1L
    tab <- data.table::data.table(.row = 1L)
  } else {
    # Only the left origin is limited: a join keeps every left row, so its
    # first `n_max` rows give the first `n_max` records.
    tables <- lapply(seq_along(origins), function(i) {
      origin_table(ctx, origins[[i]], if (i == 1L) n_max else Inf)
    })
    names(tables) <- names(origins)
    tab <- join_origins(ctx, rs, leaves, origins, tables)
    n <- nrow(tab)
  }
  cols <- lapply(leaves, function(leaf) {
    f <- leaf[["field"]]
    if (is.null(f@source)) {
      return(rep(list(f@value), n) |> cast_values(f, leaf[["repeated"]]))
    }
    col <- source_column(ctx, f@source)
    if (!col %in% names(tab)) {
      abort(
        "The field \"",
        f@id,
        "\" reads ",
        sub("^[^|]*\\|", "", col),
        ", which its source does not have. Available: ",
        paste(sub("^[^|]*\\|", "", names(tab)), collapse = ", "),
        ".",
        class = c("croissant_record_error", "rtemis_input_error")
      )
    }
    values <- apply_transforms(tab[[col]], f@source@transform)
    property <- if (is.null(f@source@extract)) {
      NULL
    } else {
      f@source@extract@file_property
    }
    if (identical(property, "content") && is_text_type(f)) {
      values <- vapply(values, read_text_file, character(1L))
    }
    cast_values(values, f, leaf[["repeated"]])
  })
  names(cols) <- vapply(leaves, \(l) l[["field"]]@id, character(1L))
  data.table::as.data.table(cols)
} # /croissant::sourced_table


# %% source_origin ----
#' The file or record set a source reads from
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param s `Source` object.
#'
#' @return List with `key`, `type` (`"file"` or `"record_set"`), and `node`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
source_origin <- function(ctx, s) {
  id <- s@file_object %||% s@file_set %||% s@field %||% s@record_set
  if (is.null(id)) {
    abort(
      "A source names none of fileObject, fileSet, field, or recordSet.",
      class = c("croissant_record_error", "rtemis_input_error")
    )
  }
  file <- ctx[["files"]][[id]]
  if (!is.null(file)) {
    return(list(key = paste0("file:", id), type = "file", node = file))
  }
  rs_id <- if (!is.null(s@field)) ctx[["field_rs"]][s@field] else s@record_set
  if (is.na(rs_id) || is.null(rs_id)) {
    abort(
      "A source names \"",
      id,
      "\", which is neither a file nor a field of this dataset.",
      class = c("croissant_record_error", "rtemis_input_error")
    )
  }
  rs <- find_record_set(ctx[["x"]], unname(rs_id))
  list(key = paste0("rs:", rs_id), type = "record_set", node = rs)
} # /croissant::source_origin


# %% source_column ----
#' Column of an origin table that holds a source's values
#'
#' A record set origin names its columns by field `@id`. A file origin names
#' them `<origin key>|<column>`, `<origin key>|$<JSONPath>`, or
#' `<origin key>|@<file property>`.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param s `Source` object.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
source_column <- function(ctx, s) {
  o <- source_origin(ctx, s)
  if (o[["type"]] == "record_set") {
    return(s@field)
  }
  e <- s@extract
  name <- if (is.null(e)) {
    "@content"
  } else if (!is.null(e@column)) {
    e@column
  } else if (!is.null(e@json_path)) {
    paste0("$", e@json_path)
  } else if (!is.null(e@file_property)) {
    paste0("@", e@file_property)
  } else {
    "@content"
  }
  paste0(o[["key"]], "|", name)
} # /croissant::source_column


# %% origin_table ----
#' Read an origin into a table
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param o List: Origin from `source_origin()`, with the `sources` that read
#' from it.
#' @param n_max Numeric: Rows after which no further file is read.
#'
#' @return data.table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
origin_table <- function(ctx, o, n_max = Inf) {
  if (o[["type"]] == "record_set") {
    return(data.table::copy(record_set_table(ctx, o[["node"]])))
  }
  node <- o[["node"]]
  files <- if (S7_inherits(node, FileSet)) {
    fileset_files(ctx, node)
  } else {
    path <- local_file(ctx, node)
    data.table::data.table(
      filename = basename(path),
      filepath = path,
      fullpath = node@content_url %||% basename(path),
      hf = NA_character_
    )
  }
  extracts <- lapply(o[["sources"]], \(s) s@extract)
  props <- unique(unlist(lapply(extracts, \(e) {
    if (!is.null(e)) e@file_property
  })))
  columns <- unique(unlist(lapply(extracts, \(e) if (!is.null(e)) e@column)))
  paths <- unique(unlist(lapply(extracts, \(e) if (!is.null(e)) e@json_path)))
  prefix <- paste0(o[["key"]], "|")
  # Files are read in order until `n_max` rows are held.
  read_files <- function(read_one) {
    parts <- list()
    held <- 0
    for (i in seq_len(nrow(files))) {
      part <- read_one(i)
      parts[[length(parts) + 1L]] <- part
      held <- held + nrow(part)
      if (held >= n_max) break
    }
    data.table::rbindlist(parts, fill = TRUE)
  }
  if (length(columns) > 0L || length(paths) > 0L) {
    tab <- read_files(function(i) {
      path <- file_path_at(ctx, files, i)
      content <- read_content(path, node@encoding_format)
      part <- content_table(content, columns, paths)
      part[["@filename"]] <- rep(files[["filename"]][[i]], nrow(part))
      part[["@filepath"]] <- rep(path, nrow(part))
      part[["@fullpath"]] <- rep(files[["fullpath"]][[i]], nrow(part))
      part
    })
  } else if (any(c("lines", "lineNumbers") %in% props)) {
    tab <- read_files(function(i) {
      path <- file_path_at(ctx, files, i)
      lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
      data.table::data.table(
        `@filename` = rep(files[["filename"]][[i]], length(lines)),
        `@filepath` = rep(path, length(lines)),
        `@fullpath` = rep(files[["fullpath"]][[i]], length(lines)),
        `@lines` = lines,
        `@lineNumbers` = seq_along(lines) - 1L
      )
    })
  } else {
    files <- utils::head(files, n_max)
    # A file of a Hub repository is downloaded only when a field reads its
    # local path or content.
    if (any(c("filepath", "content") %in% props) || is.null(unlist(props))) {
      files[["filepath"]] <- vapply(
        seq_len(nrow(files)),
        \(i) file_path_at(ctx, files, i),
        character(1L)
      )
    }
    tab <- data.table::data.table(
      `@filename` = files[["filename"]],
      `@filepath` = files[["filepath"]],
      `@fullpath` = files[["fullpath"]]
    )
  }
  if ("@filepath" %in% names(tab)) {
    tab[["@content"]] <- tab[["@filepath"]]
  }
  data.table::setnames(tab, paste0(prefix, names(tab)))
  tab
} # /croissant::origin_table


# %% content_table ----
#' Columns read from one parsed file
#'
#' @param content List from `read_content()`.
#' @param columns Character: Column names to extract.
#' @param paths Character: JSONPath expressions to evaluate.
#'
#' @return data.table with a column per name in `columns`, and a column
#' `$<path>` per JSONPath.
#'
#' @author EDG
#' @keywords internal
#' @noRd
content_table <- function(content, columns, paths) {
  data <- content[["data"]]
  if (content[["kind"]] == "table") {
    tab <- data.table::as.data.table(data)
    for (p in paths) {
      abort(
        "A JSONPath (",
        p,
        ") extracts from a tabular file. Extract a column instead.",
        class = c("croissant_record_error", "rtemis_input_error")
      )
    }
    return(tab)
  }
  rows <- if (content[["kind"]] == "rows") data else NULL
  cols <- list()
  if (!is.null(rows)) {
    for (cl in columns) {
      cols[[cl]] <- lapply(rows, \(r) r[[cl]])
    }
    for (p in paths) {
      cols[[paste0("$", p)]] <- lapply(rows, function(r) {
        m <- jsonpath(r, p)
        if (length(m) == 0L) NULL else m[[1L]]
      })
    }
  } else {
    records <- if (is.list(data) && is.null(names(data))) data else list(data)
    for (cl in columns) {
      cols[[cl]] <- lapply(records, \(r) r[[cl]])
    }
    for (p in paths) {
      cols[[paste0("$", p)]] <- jsonpath(data, p)
    }
  }
  lens <- unique(lengths(cols))
  if (length(lens) > 1L) {
    abort(
      "The JSONPath and column extractions of one file return different ",
      "numbers of values (",
      paste(lens, collapse = ", "),
      "), so they cannot form records.",
      class = c("croissant_record_error", "rtemis_input_error")
    )
  }
  data.table::as.data.table(lapply(cols, \(v) I(v)))
} # /croissant::content_table


# %% join_origins ----
#' Left-join the origins of a record set
#'
#' The origin of the first field is the left table. Each further origin joins
#' through a field that reads from one joined origin and `references` the
#' other, as in mlcroissant's `Join` operation: the field's transforms apply
#' to the left key, keys compare as strings, and every left row is kept.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param rs `RecordSet` object.
#' @param leaves List from `leaf_fields()`.
#' @param origins List: Origins, named by key.
#' @param tables List: Origin tables, named by key.
#'
#' @return data.table.
#'
#' @author EDG
#' @keywords internal
#' @noRd
join_origins <- function(ctx, rs, leaves, origins, tables) {
  tab <- tables[[1L]]
  joined <- names(tables)[[1L]]
  pending <- setdiff(names(tables), joined)
  while (length(pending) > 0L) {
    progressed <- FALSE
    for (leaf in leaves) {
      f <- leaf[["field"]]
      if (is.null(f@source) || is.null(f@references)) {
        next
      }
      left <- source_origin(ctx, f@source)[["key"]]
      right <- source_origin(ctx, f@references)[["key"]]
      if (left %in% joined && right %in% pending) {
        l_key <- apply_transforms(
          tab[[source_column(ctx, f@source)]],
          f@source@transform
        )
        r_tab <- tables[[right]]
        r_key <- r_tab[[source_column(ctx, f@references)]]
        tab <- left_join(tab, r_tab, key_string(l_key), key_string(r_key))
        joined <- c(joined, right)
        pending <- setdiff(pending, right)
        progressed <- TRUE
      }
    }
    if (!progressed) {
      abort(
        "The record set \"",
        node_ref(rs),
        "\" reads from ",
        paste(sub("^[a-z_]+:", "", c(joined, pending)), collapse = " and "),
        " without a join to ",
        paste(sub("^[a-z_]+:", "", pending), collapse = ", "),
        ". Add references to a field that reads from one of them.",
        class = c("croissant_record_error", "rtemis_input_error")
      )
    }
  }
  tab
} # /croissant::join_origins


# %% left_join ----
#' Left join two tables on key vectors
#'
#' @param left data.table.
#' @param right data.table.
#' @param l_key Character: Key of each left row.
#' @param r_key Character: Key of each right row.
#'
#' @return data.table: Every left row, in order, with the columns of each
#' matching right row; a left row with several matches repeats.
#'
#' @author EDG
#' @keywords internal
#' @noRd
left_join <- function(left, right, l_key, r_key) {
  pairs <- lapply(seq_along(l_key), function(i) {
    m <- which(r_key == l_key[[i]] & !is.na(r_key))
    if (length(m) == 0L || is.na(l_key[[i]])) NA_integer_ else m
  })
  li <- rep(seq_along(pairs), lengths(pairs))
  ri <- unlist(pairs)
  right <- right[, setdiff(names(right), names(left)), with = FALSE]
  cbind(left[li], right[ri])
} # /croissant::left_join


# %% key_string ----
#' Join keys as strings
#'
#' @param x Vector or list of key values.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
key_string <- function(x) {
  if (is.list(x)) {
    x <- vapply(
      x,
      \(v) if (length(v) == 0L) NA_character_ else as.character(v[[1L]]),
      character(1L)
    )
  }
  out <- as.character(x)
  # 1 and 1.0 join as the same key, as pandas compares them.
  num <- suppressWarnings(as.numeric(out))
  whole <- !is.na(num) & num == round(num)
  out[whole] <- format(num[whole], scientific = FALSE, trim = TRUE)
  out
} # /croissant::key_string


# %% read_text_file ----
#' Read a whole file as text
#'
#' @param path Character: File path.
#'
#' @return Character scalar: The file's text, or `path` itself when the file
#' holds a NUL byte and so is not text.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_text_file <- function(path) {
  if (is.na(path)) {
    return(NA_character_)
  }
  bytes <- readBin(path, "raw", n = file.info(path)[["size"]])
  if (any(bytes == as.raw(0L))) {
    return(path)
  }
  out <- rawToChar(bytes)
  Encoding(out) <- "UTF-8"
  out
} # /croissant::read_text_file
