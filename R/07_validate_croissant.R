# 07_validate_croissant.R
# ::croissant::
# 2026- EDG rtemis.org

# Structural checks of Croissant metadata.
# spec: croissant/read-write#validation
#
# The checks follow those of mlcroissant 1.1.1 (structure_graph/nodes/*.py
# and structure_graph/graph.py): mandatory and recommended properties,
# identifiers, references, sources, joins, data types, inline data, arrays,
# and transforms. Each check logs to the issue log and continues.

# %% validate_croissant ----
#' Validate Croissant metadata
#'
#' Reads `x` when it is a path, URL, or JSON string, then checks the
#' metadata, and reports every problem found in one `CroissantIssues` object.
#' Problems that [read_croissant()] raises as an error, such as a value of the
#' wrong JSON type, are reported here as issues.
#'
#' Errors cover: values of the wrong JSON type; node types; mandatory
#' properties (`encodingFormat` and `contentUrl` of a `FileObject`,
#' `encodingFormat` and `includes` of a `FileSet`); a checksum (`md5` or
#' `sha256`) on each
#' `FileObject` that is not contained in another file; `@id`s that contain
#' whitespace or repeat; references to nodes that do not exist; sources that
#' do not name exactly one `field`, `fileObject`, or `fileSet`; fields with
#' neither `source` nor `value`; fields without a data type of their own or
#' of their source field; record sets that combine several sources without a
#' join (`references`); inline `data` whose records do not match the fields;
#' `arrayShape` without `isArray` or with non-integer dimensions; and
#' transforms that define no transformation, or more than one string
#' transformation.
#'
#' Warnings cover the recommended dataset properties `citeAs`,
#' `datePublished`, `license`, and `version`; a missing `conformsTo`; nodes
#' without `@id`, which no other node can reference; record set keys that
#' name no field; and Croissant terms used as keys without a definition in
#' the `@context`.
#'
#' @param x `Croissant` object, or Character: Path to a Croissant JSON-LD
#' file, an `http(s)` URL, or a JSON string.
#'
#' @return `CroissantIssues` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- Croissant(
#'   name = "example",
#'   distribution = list(FileObject(id = "data.csv", encoding_format = "text/csv"))
#' )
#' issues <- validate_croissant(x)
#' issues
#' is_valid(issues)
validate_croissant <- function(x) {
  log <- new_issue_log()
  if (!S7_inherits(x, Croissant)) {
    x <- tryCatch(
      read_document(x, log),
      croissant_version_error = function(e) {
        log_issue(log, "error", "version", "Croissant", conditionMessage(e))
        NULL
      },
      rtemis_input_error = function(e) {
        log_issue(log, "error", "input", "Croissant", conditionMessage(e))
        NULL
      }
    )
  }
  if (!is.null(x)) {
    check_croissant(x, log)
  }
  as_issues(log)
} # /croissant::validate_croissant


# %% check_croissant ----
#' Run every structural check on a `Croissant` object
#'
#' @param x `Croissant` object.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_croissant <- function(x, log) {
  index <- node_index(x)
  check_ids(index, log)
  recommended <- list(
    citeAs = x@cite_as,
    datePublished = x@date_published,
    license = x@license,
    version = x@dataset_version
  )
  for (key in names(recommended)) {
    if (is.null(recommended[[key]])) {
      log_issue(
        log,
        "warning",
        "recommended_property",
        node_ref(x),
        "The recommended property ",
        key,
        " is absent."
      )
    }
  }
  live <- isTRUE(x@is_live_dataset)
  for (file in x@distribution) {
    check_file(file, index, live, log)
  }
  for (rs in x@record_set) {
    check_record_set(rs, index, log)
  }
  invisible(NULL)
} # /croissant::check_croissant


# %% node_index ----
#' Index every identified node of a dataset
#'
#' @param x `Croissant` object.
#'
#' @return data.frame with one row per node: `id`, `class`, and `parent` (the
#' `@id` of the record set a field belongs to, `NA` otherwise).
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_index <- function(x) {
  rows <- list()
  add <- function(node, parent) {
    rows[[length(rows) + 1L]] <<- data.frame(
      id = node@id %||% NA_character_,
      class = S7_class(node)@name,
      parent = parent
    )
  }
  add_fields <- function(fields, parent) {
    for (f in fields) {
      add(f, parent)
      add_fields(f@sub_field, parent)
    }
  }
  for (file in x@distribution) {
    add(file, NA_character_)
  }
  for (rs in x@record_set) {
    add(rs, NA_character_)
    add_fields(rs@field, rs@id %||% NA_character_)
  }
  if (length(rows) == 0L) {
    return(data.frame(
      id = character(),
      class = character(),
      parent = character()
    ))
  }
  do.call(rbind, rows)
} # /croissant::node_index


# %% check_ids ----
#' Check that `@id`s contain no whitespace and do not repeat
#'
#' @param index data.frame from `node_index()`.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_ids <- function(index, log) {
  ids <- index[["id"]][!is.na(index[["id"]])]
  for (id in ids[grepl("\\s", ids)]) {
    log_issue(
      log,
      "error",
      "invalid_id",
      id,
      "The @id \"",
      id,
      "\" contains whitespace. Use an @id without spaces, such as \"",
      gsub("\\s+", "-", id),
      "\"."
    )
  }
  for (id in unique(ids[duplicated(ids)])) {
    log_issue(
      log,
      "error",
      "duplicate_id",
      id,
      sum(ids == id),
      " nodes have the @id \"",
      id,
      "\". Give each node a unique @id."
    )
  }
  invisible(NULL)
} # /croissant::check_ids


# %% check_file ----
#' Check a `FileObject` or `FileSet`
#'
#' @param file `FileObject` or `FileSet` object.
#' @param index data.frame from `node_index()`.
#' @param live Logical: Whether the dataset is a live dataset, whose files
#' need no checksum.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_file <- function(file, index, live, log) {
  cls <- S7_class(file)@name
  node <- node_ref(file)
  check_id(file, node, log)
  check_mandatory(file, c(encoding_format = "encodingFormat"), node, log)
  contained_in <- c(file@contained_in, other_contained_in(file))
  for (ref in contained_in) {
    check_reference(ref, index, node, "containedIn", log)
  }
  if (cls == "FileSet") {
    check_mandatory(file, c(includes = "includes"), node, log)
    return(invisible(NULL))
  }
  if (length(contained_in) == 0L) {
    check_mandatory(file, c(content_url = "contentUrl"), node, log)
    if (!live && is.null(file@md5) && is.null(file@sha256)) {
      log_issue(
        log,
        "error",
        "missing_checksum",
        node,
        "The FileObject has neither md5 nor sha256. Add the sha256 checksum ",
        "of the file at contentUrl."
      )
    }
  }
  invisible(NULL)
} # /croissant::check_file


# %% other_contained_in ----
#' `@id`s named by a `containedIn` kept in `other_properties`
#'
#' A 1.1 `containedIn` may be a source with a transform, which stays in
#' `other_properties`; its `fileObject` or `fileSet` names the container.
#'
#' @param file `FileObject` or `FileSet` object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
other_contained_in <- function(file) {
  other <- file@other_properties
  value <- other[["containedIn"]] %||% other[["sc:containedIn"]]
  if (is.null(value)) {
    return(character())
  }
  ids <- unlist(lapply(as_array(value), function(v) {
    if (is_ref(v)) {
      return(read_ref(v))
    }
    if (!is.list(v)) {
      return(NULL)
    }
    refs <- c(v[["fileObject"]], v[["fileSet"]])
    if (is_ref(refs)) read_ref(refs) else NULL
  }))
  as.character(ids)
} # /croissant::other_contained_in


# %% check_record_set ----
#' Check a `RecordSet` and its fields
#'
#' @param rs `RecordSet` object.
#' @param index data.frame from `node_index()`.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_record_set <- function(rs, index, log) {
  node <- node_ref(rs)
  check_id(rs, node, log)
  field_ids <- vapply(rs@field, \(f) f@id %||% NA_character_, character(1L))
  field_names <- vapply(
    rs@field,
    \(f) first_text(f@name) %||% NA_character_,
    character(1L)
  )
  # A key names a field by @id, by @id relative to the record set
  # ("movie_id" for "ratings/movie_id"), or by name.
  for (key in rs@key) {
    known <- c(field_ids, field_names)
    if (!key %in% known && !paste0(rs@id %||% "", "/", key) %in% field_ids) {
      log_issue(
        log,
        "warning",
        "unresolved_key",
        node,
        "The key names \"",
        key,
        "\", which is not a field of this record set. Name the @id of one of ",
        "its fields."
      )
    }
  }
  if (!is.null(rs@data)) {
    check_inline_data(rs@data, field_ids, node, log)
  }
  for (f in rs@field) {
    check_field(f, rs, index, log)
  }
  check_joins(rs, index, node, log)
  invisible(NULL)
} # /croissant::check_record_set


# %% check_inline_data ----
#' Check that inline records match the fields of their record set
#'
#' @param data List: Parsed `data` of the record set.
#' @param field_ids Character: `@id`s of the record set's fields.
#' @param node Character: `@id` of the record set.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_inline_data <- function(data, field_ids, node, log) {
  records <- as_array(data)
  if (length(records) == 0L) {
    log_issue(
      log,
      "error",
      "inline_data",
      node,
      "The data is an empty array. Give at least one record, or remove data."
    )
  }
  expected <- sort(field_ids[!is.na(field_ids)])
  for (i in seq_along(records)) {
    rec <- records[[i]]
    if (!is.list(rec) || is.null(names(rec))) {
      log_issue(
        log,
        "error",
        "inline_data",
        node,
        "Record ",
        i,
        " of data is not a JSON object. Give each record as an object keyed by ",
        "field @id."
      )
      next
    }
    if (!identical(sort(names(rec)), expected)) {
      log_issue(
        log,
        "error",
        "inline_data",
        node,
        "Record ",
        i,
        " of data has the keys ",
        paste(sort(names(rec)), collapse = ", "),
        "; the fields are ",
        paste(expected, collapse = ", "),
        ". Key each record by exactly the field @ids."
      )
    }
  }
  invisible(NULL)
} # /croissant::check_inline_data


# %% check_field ----
#' Check a `Field` and its subfields
#'
#' @param f `Field` object.
#' @param rs `RecordSet` object that holds the field.
#' @param index data.frame from `node_index()`.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_field <- function(f, rs, index, log) {
  node <- node_ref(f)
  check_id(f, node, log)
  has_value <- !is.null(f@value)
  has_source <- !is.null(f@source)
  if (has_value && has_source) {
    log_issue(
      log,
      "error",
      "missing_source",
      node,
      "The field defines both source and value. Keep exactly one of them."
    )
  }
  if (
    !has_value && !has_source && length(f@sub_field) == 0L && is.null(rs@data)
  ) {
    log_issue(
      log,
      "error",
      "missing_source",
      node,
      "The field defines neither source nor value. Add a source that names ",
      "the file or field its values come from, or a constant value."
    )
  }
  if (has_value && is.null(f@data_type)) {
    log_issue(
      log,
      "error",
      "missing_data_type",
      node,
      "The field defines a constant value but no dataType. Add its dataType."
    )
  }
  if (has_source) {
    check_source(f@source, index, node, "source", log)
  }
  if (!is.null(f@references)) {
    check_source(f@references, index, node, "references", log)
  }
  if (length(f@sub_field) == 0L && is.null(f@data_type) && !has_value) {
    source_field <- if (has_source) f@source@field else NULL
    field_ids <- index[["id"]][index[["class"]] == "Field"]
    if (is.null(source_field) || !source_field %in% field_ids) {
      log_issue(
        log,
        "error",
        "missing_data_type",
        node,
        "The field has no dataType, and its source is not a field to take ",
        "one from. Add a dataType, such as sc:Text or sc:Integer."
      )
    }
  }
  if (!is.null(f@array_shape)) {
    if (!isTRUE(f@is_array)) {
      log_issue(
        log,
        "error",
        "array_shape",
        node,
        "The field defines arrayShape but not isArray: true. Set isArray to ",
        "true, or remove arrayShape."
      )
    }
    dims <- trimws(strsplit(f@array_shape, ",", fixed = TRUE)[[1L]])
    if (length(dims) == 0L || !all(grepl("^-?[0-9]+$", dims))) {
      log_issue(
        log,
        "error",
        "array_shape",
        node,
        "The arrayShape \"",
        f@array_shape,
        "\" is not a comma-separated list of integers, such as \"28,28\" or ",
        "\"-1,3\"."
      )
    }
  }
  for (sub in f@sub_field) {
    check_field(sub, rs, index, log)
  }
  invisible(NULL)
} # /croissant::check_field


# %% check_source ----
#' Check a `Source`: one target that exists, and valid transforms
#'
#' @param s `Source` object.
#' @param index data.frame from `node_index()`.
#' @param node Character: `@id` of the field that holds the source.
#' @param key Character: Property that holds the source, for messages.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_source <- function(s, index, node, key, log) {
  targets <- c(
    fileObject = s@file_object,
    fileSet = s@file_set,
    field = s@field
  )
  if (length(targets) != 1L) {
    log_issue(
      log,
      "error",
      "source_target",
      node,
      "The ",
      key,
      " names ",
      if (length(targets) == 0L) {
        "none"
      } else {
        paste(names(targets), collapse = ", ")
      },
      " of fileObject, fileSet, and field. Name exactly one of them."
    )
  }
  for (ref in c(targets, s@record_set)) {
    check_reference(ref, index, node, key, log)
  }
  for (t in s@transform) {
    check_transform(t, node, log)
  }
  invisible(NULL)
} # /croissant::check_source


# %% check_transform ----
#' Check that a `Transform` defines exactly one transformation
#'
#' @param t `Transform` object.
#' @param node Character: `@id` of the field that holds the transform.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_transform <- function(t, node, log) {
  strings <- c(
    format = !is.null(t@format),
    jsonPath = !is.null(t@json_path),
    regex = !is.null(t@regex),
    replace = !is.null(t@replace),
    separator = !is.null(t@separator)
  )
  booleans <- isTRUE(t@read_lines) || isTRUE(t@un_archive)
  if (sum(strings) > 1L) {
    log_issue(
      log,
      "error",
      "transform",
      node,
      "A transform defines ",
      paste(names(strings)[strings], collapse = ", "),
      ". Give each of these in its own transform."
    )
  }
  if (sum(strings) == 0L && !booleans) {
    log_issue(
      log,
      "error",
      "transform",
      node,
      "A transform defines no transformation. Give one of format, jsonPath, ",
      "regex, replace, separator, readLines, or unArchive."
    )
  }
  invisible(NULL)
} # /croissant::check_transform


# %% check_joins ----
#' Check that a record set that combines sources declares their joins
#'
#' The source of a field is the file it reads, or the record set of the
#' field it reads. Two sources are joined when a field takes its values from
#' one and `references` the other.
#'
#' @param rs `RecordSet` object.
#' @param index data.frame from `node_index()`.
#' @param node Character: `@id` of the record set.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_joins <- function(rs, index, node, log) {
  origin <- function(s) {
    if (is.null(s)) {
      return(NA_character_)
    }
    id <- s@field %||% s@file_object %||% s@file_set %||% s@record_set
    if (is.null(id)) {
      return(NA_character_)
    }
    row <- match(id, index[["id"]])
    if (is.na(row)) {
      return(NA_character_)
    }
    if (index[["class"]][[row]] == "Field") index[["parent"]][[row]] else id
  }
  sources <- character()
  joins <- character()
  for (f in rs@field) {
    from <- origin(f@source)
    to <- origin(f@references)
    if (!is.na(from)) {
      sources <- c(sources, from)
      if (!is.na(to)) {
        joins <- c(
          joins,
          paste(from, to, sep = "\r"),
          paste(to, from, sep = "\r")
        )
      }
    }
  }
  sources <- sort(unique(sources))
  if (length(sources) < 2L) {
    return(invisible(NULL))
  }
  pairs <- utils::combn(sources, 2L, simplify = FALSE)
  for (p in pairs) {
    if (!paste(p[[1L]], p[[2L]], sep = "\r") %in% joins) {
      log_issue(
        log,
        "error",
        "undeclared_join",
        node,
        "The fields read from both ",
        p[[1L]],
        " and ",
        p[[2L]],
        " without a join between them. Add references to a field that reads ",
        "from one of them, naming the matching field of the other."
      )
    }
  }
  invisible(NULL)
} # /croissant::check_joins


# %% check_mandatory ----
#' Log each mandatory property a node omits
#'
#' @param x Node object.
#' @param props Named character: JSON-LD keys, named by S7 property.
#' @param node Character: `@id` of the node, or its type.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_mandatory <- function(x, props, node, log) {
  for (p in names(props)) {
    if (is.null(prop(x, p))) {
      log_issue(
        log,
        "error",
        "missing_property",
        node,
        "The ",
        S7_class(x)@name,
        " has no ",
        props[[p]],
        ", which is mandatory."
      )
    }
  }
  invisible(NULL)
} # /croissant::check_mandatory


# %% check_id ----
#' Log a node without `@id`
#'
#' A node without `@id` is read, but no other node can reference it.
#'
#' @param x Node object.
#' @param node Character: Label of the node, from `node_ref()`.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_id <- function(x, node, log) {
  if (is.null(x@id)) {
    log_issue(
      log,
      "warning",
      "missing_id",
      node,
      "The ",
      S7_class(x)@name,
      " has no @id, so no other node can reference it. Add a unique @id."
    )
  }
  invisible(NULL)
} # /croissant::check_id


# %% node_ref ----
#' Label a node for an issue
#'
#' @param x Node object.
#'
#' @return Character scalar: The `@id`, or the class and name of a node
#' without `@id`, such as `"FileSet(images)"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_ref <- function(x) {
  if (!is.null(x@id)) {
    return(x@id)
  }
  cls <- S7_class(x)@name
  name <- first_text(x@name)
  if (is.null(name)) cls else paste0(cls, "(", name, ")")
} # /croissant::node_ref


# %% check_reference ----
#' Log a reference to a node that does not exist
#'
#' @param ref Character: Referenced `@id`.
#' @param index data.frame from `node_index()`.
#' @param node Character: `@id` of the referencing node.
#' @param key Character: Property that holds the reference.
#' @param log Environment: Issue log from `new_issue_log()`.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_reference <- function(ref, index, node, key, log) {
  if (!ref %in% index[["id"]]) {
    log_issue(
      log,
      "error",
      "unresolved_reference",
      node,
      "The ",
      key,
      " names \"",
      ref,
      "\", but no node has that @id. Name the @id of a FileObject, FileSet, ",
      "RecordSet, or Field of this dataset."
    )
  }
  invisible(NULL)
} # /croissant::check_reference
