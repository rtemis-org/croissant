# 09_files.R
# ::croissant::
# 2026- EDG rtemis.org

# Locating, downloading, extracting, and reading the files a dataset names.
# spec: croissant/read-write#record-sets
#
# A relative `contentUrl` resolves against the location of the metadata
# document. Downloads and extracted archives are kept under the cache
# directory, keyed by checksum or URL, and reused by later reads. Files on
# local disk are read in place.

# %% new_load_context ----
#' Create the state shared by one `read_record_set()` call
#'
#' @param x `Croissant` object.
#' @param base Optional Character: Directory or URL that relative
#' `contentUrl`s resolve against.
#' @param cache_dir Character: Cache directory.
#' @param verbosity Integer: Verbosity level.
#' @param token Character: Hugging Face token, or `""`.
#'
#' @return Environment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_load_context <- function(x, base, cache_dir, verbosity, token = "") {
  ctx <- new.env(parent = emptyenv())
  ctx[["x"]] <- x
  ctx[["base"]] <- base
  ctx[["cache_dir"]] <- cache_dir
  ctx[["verbosity"]] <- verbosity
  ctx[["token"]] <- token
  ctx[["tables"]] <- new.env(parent = emptyenv())
  ctx[["dirs"]] <- new.env(parent = emptyenv())
  ctx[["hf"]] <- new.env(parent = emptyenv())
  files <- list()
  for (file in x@distribution) {
    if (!is.null(file@id)) files[[file@id]] <- file
  }
  ctx[["files"]] <- files
  field_rs <- character()
  add_fields <- function(fields, rs_id) {
    for (f in fields) {
      if (!is.null(f@id)) {
        field_rs[[f@id]] <<- rs_id
      }
      add_fields(f@sub_field, rs_id)
    }
  }
  for (rs in x@record_set) {
    add_fields(rs@field, rs@id %||% "")
  }
  ctx[["field_rs"]] <- field_rs
  ctx
} # /croissant::new_load_context


# %% local_file ----
#' Local path of a `FileObject`, downloading or extracting it as needed
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param file `FileObject` object.
#'
#' @return Character: Path of the file on local disk.
#'
#' @author EDG
#' @keywords internal
#' @noRd
local_file <- function(ctx, file) {
  container <- container_of(ctx, file)
  if (!is.null(container)) {
    rev <- hf_container(ctx, container)
    if (!is.null(rev)) {
      return(hf_fetch(
        rev,
        file@content_url,
        ctx[["cache_dir"]],
        ctx[["token"]],
        ctx[["verbosity"]]
      ))
    }
    path <- file.path(container_dir(ctx, container), file@content_url)
    if (!file.exists(path)) {
      abort(
        "The FileObject \"",
        file@id,
        "\" names ",
        file@content_url,
        " inside \"",
        container@id,
        "\", which does not hold that file.",
        class = c("croissant_file_error", "rtemis_input_error")
      )
    }
    return(path)
  }
  if (is.null(file@content_url)) {
    abort(
      "The FileObject \"",
      file@id,
      "\" has no contentUrl, so its file cannot be located.",
      class = c("croissant_file_error", "rtemis_input_error")
    )
  }
  fetch(ctx, file@content_url, sha256 = file@sha256, md5 = file@md5)
} # /croissant::local_file


# %% container_of ----
#' The `FileObject` or `FileSet` a file is contained in
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param file `FileObject` or `FileSet` object.
#'
#' @return The container node, or `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
container_of <- function(ctx, file) {
  ids <- c(file@contained_in, other_contained_in(file))
  if (length(ids) == 0L) {
    return(NULL)
  }
  container <- ctx[["files"]][[ids[[1L]]]]
  if (is.null(container)) {
    abort(
      "The ",
      S7_class(file)@name,
      " \"",
      file@id,
      "\" is contained in \"",
      ids[[1L]],
      "\", which is not in the distribution.",
      class = c("croissant_file_error", "rtemis_input_error")
    )
  }
  container
} # /croissant::container_of


# %% fetch ----
#' Resolve a `contentUrl` to a local path
#'
#' An absolute `http(s)` URL, or a relative path when the metadata was read
#' from a URL, is downloaded into the cache. A relative path resolves against
#' the directory of the metadata document.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param url Character: `contentUrl`.
#' @param sha256 Optional Character: Expected SHA-256 checksum.
#' @param md5 Optional Character: Expected MD5 checksum.
#'
#' @return Character: Local path.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fetch <- function(ctx, url, sha256 = NULL, md5 = NULL) {
  if (is_hf(url)) {
    ref <- parse_hf(url)
    if (is.null(ref[["file"]])) {
      abort(
        "The contentUrl ",
        url,
        " names a repository, not a file.",
        class = c("croissant_file_error", "rtemis_input_error")
      )
    }
    rev <- hf_revision(ref[["repo"]], ref[["revision"]], ctx[["token"]])
    return(hf_fetch(
      rev,
      ref[["file"]],
      ctx[["cache_dir"]],
      ctx[["token"]],
      ctx[["verbosity"]]
    ))
  }
  if (grepl("^git\\+", url)) {
    abort(
      "The contentUrl ",
      url,
      " is a git repository, which read_record_set() does not clone. ",
      "Clone it and pass its directory as `base`.",
      class = c("croissant_file_error", "rtemis_input_error")
    )
  }
  base <- ctx[["base"]]
  if (!grepl("^[A-Za-z][A-Za-z0-9+.-]*://", url) && !startsWith(url, "/")) {
    if (!is.null(base) && grepl("^https?://", base)) {
      url <- paste0(sub("/+$", "", base), "/", url)
    } else {
      path <- file.path(base %||% getwd(), url)
      if (!file.exists(path)) {
        abort(
          "The contentUrl ",
          url,
          " resolves to ",
          path,
          ", which does not exist. Pass the directory the paths are relative ",
          "to as `base`.",
          class = c("croissant_file_error", "rtemis_input_error")
        )
      }
      return(normalizePath(path))
    }
  }
  if (startsWith(url, "file://")) {
    return(sub("^file://", "", url))
  }
  if (startsWith(url, "/")) {
    return(url)
  }
  download_cached(ctx, url, sha256, md5)
} # /croissant::fetch


# %% download_cached ----
#' Download a file into the cache and verify its checksum
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param url Character: URL.
#' @param sha256 Optional Character: Expected SHA-256 checksum.
#' @param md5 Optional Character: Expected MD5 checksum.
#'
#' @return Character: Path of the cached file.
#'
#' @author EDG
#' @keywords internal
#' @noRd
download_cached <- function(ctx, url, sha256, md5) {
  key <- sha256 %||%
    md5 %||%
    sha256_string(url)
  name <- basename(sub("[?#].*$", "", url))
  if (!nzchar(name)) {
    name <- "file"
  }
  dest <- file.path(ctx[["cache_dir"]], "downloads", key, name)
  if (file.exists(dest) && checksum_ok(dest, sha256, md5)) {
    return(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  info("Downloading ", url, verbosity = ctx[["verbosity"]])
  status <- tryCatch(
    utils::download.file(url, dest, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !identical(status, 0L)) {
    unlink(dest)
    abort(
      "Downloading ",
      url,
      " failed",
      if (inherits(status, "error")) {
        paste0(": ", conditionMessage(status))
      } else {
        ""
      },
      ".",
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  if (!checksum_ok(dest, sha256, md5)) {
    unlink(dest)
    abort(
      "The file downloaded from ",
      url,
      " does not match its ",
      if (!is.null(sha256)) "sha256" else "md5",
      " in the metadata. The file at the URL differs from the one the ",
      "metadata describes.",
      class = c("croissant_checksum_error", "rtemis_input_error")
    )
  }
  dest
} # /croissant::download_cached


# %% checksum_ok ----
#' Test a file against its expected checksum
#'
#' @param path Character: File path.
#' @param sha256 Optional Character: Expected SHA-256 checksum.
#' @param md5 Optional Character: Expected MD5 checksum.
#'
#' @return Logical scalar: `TRUE` when the checksum matches or none is given.
#'
#' @author EDG
#' @keywords internal
#' @noRd
checksum_ok <- function(path, sha256, md5) {
  if (!is.null(sha256)) {
    return(identical(
      sha256_file(path),
      tolower(unclass(sha256))
    ))
  }
  if (!is.null(md5)) {
    return(identical(unname(tools::md5sum(path)), tolower(unclass(md5))))
  }
  TRUE
} # /croissant::checksum_ok


# %% sha256_file ----
#' SHA-256 checksum of a file
#'
#' @param path Character: File path.
#'
#' @return Character scalar: Lowercase hexadecimal checksum.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sha256_file <- function(path) {
  unclass(as.character(openssl::sha256(file(path, raw = TRUE))))
} # /croissant::sha256_file


# %% sha256_string ----
#' SHA-256 checksum of a string's UTF-8 bytes
#'
#' @param x Character scalar.
#'
#' @return Character scalar: Lowercase hexadecimal checksum.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sha256_string <- function(x) {
  unclass(as.character(openssl::sha256(charToRaw(enc2utf8(x)))))
} # /croissant::sha256_string


# %% container_dir ----
#' Local directory holding the content of an archive or directory
#'
#' A zip archive is extracted with `utils::unzip()`, and a tar archive,
#' compressed or not, with `utils::untar()`, into the cache. A local
#' directory is used in place.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param container `FileObject` object.
#'
#' @return Character: Directory path.
#'
#' @author EDG
#' @keywords internal
#' @noRd
container_dir <- function(ctx, container) {
  id <- container@id
  if (!is.null(ctx[["dirs"]][[id]])) {
    return(ctx[["dirs"]][[id]])
  }
  path <- local_file(ctx, container)
  if (dir.exists(path)) {
    ctx[["dirs"]][[id]] <- path
    return(path)
  }
  info <- file.info(path)
  key <- sha256_string(
    paste(normalizePath(path), info[["size"]], as.numeric(info[["mtime"]]))
  )
  exdir <- file.path(ctx[["cache_dir"]], "extracted", key)
  if (!dir.exists(exdir)) {
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    formats <- container@encoding_format %||% character()
    is_zip <- any(grepl("zip", formats)) || grepl("\\.zip$", path)
    ok <- tryCatch(
      {
        if (is_zip) {
          safe_unzip(path, exdir)
        } else {
          utils::untar(path, exdir = exdir)
        }
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!ok) {
      unlink(exdir, recursive = TRUE)
      abort(
        "The FileObject \"",
        id,
        "\" (",
        path,
        ") could not be extracted as a ",
        if (is_zip) "zip" else "tar",
        " archive.",
        class = c("croissant_file_error", "rtemis_input_error")
      )
    }
  }
  ctx[["dirs"]][[id]] <- exdir
  exdir
} # /croissant::container_dir


# %% hf_container ----
#' Resolved revision of a Hugging Face repository `FileObject`
#'
#' A `FileObject` with encoding format `git+https` whose `contentUrl` is a Hub
#' dataset URL names the repository at a revision. Its files are listed and
#' downloaded through the Hub's HTTP API, one at a time as they are read.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param container `FileObject` object.
#'
#' @return List from `hf_revision()`, or `NULL` when `container` is not a Hub
#' repository.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_container <- function(ctx, container) {
  if (!S7_inherits(container, FileObject)) {
    return(NULL)
  }
  ref <- if (is_hf(container@content_url)) {
    parse_hf(container@content_url)
  } else if ("git+https" %in% container@encoding_format) {
    hf_repo_url(container@content_url)
  }
  if (is.null(ref)) {
    return(NULL)
  }
  key <- container@id %||% container@content_url
  if (is.null(ctx[["hf"]][[key]])) {
    ctx[["hf"]][[key]] <- hf_revision(
      ref[["repo"]],
      ref[["revision"]],
      ctx[["token"]]
    )
  }
  ctx[["hf"]][[key]]
} # /croissant::hf_container


# %% file_path_at ----
#' Local path of row `i` of a file listing, downloading a Hub file
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param files data.table from `fileset_files()`.
#' @param i Integer: Row.
#'
#' @return Character: Local path.
#'
#' @author EDG
#' @keywords internal
#' @noRd
file_path_at <- function(ctx, files, i) {
  path <- files[["filepath"]][[i]]
  if (!is.na(path)) {
    return(path)
  }
  rev <- ctx[["hf"]][[files[["hf"]][[i]]]]
  hf_fetch(
    rev,
    files[["fullpath"]][[i]],
    ctx[["cache_dir"]],
    ctx[["token"]],
    ctx[["verbosity"]]
  )
} # /croissant::file_path_at


# %% safe_unzip ----
#' Extract a zip archive, keeping every entry inside the target directory
#'
#' An entry name with `..` segments or a leading `/` is extracted at its
#' name with those parts removed, as Python's `zipfile` extracts it.
#'
#' @param path Character: Zip archive.
#' @param exdir Character: Target directory.
#'
#' @return `NULL`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
safe_unzip <- function(path, exdir) {
  entries <- utils::unzip(path, list = TRUE)[["Name"]]
  entries <- entries[!endsWith(entries, "/")]
  for (entry in entries) {
    parts <- strsplit(entry, "/", fixed = TRUE)[[1L]]
    parts <- parts[nzchar(parts) & parts != ".." & parts != "."]
    target <- do.call(file.path, as.list(c(exdir, parts)))
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, files = entry, exdir = dirname(target), junkpaths = TRUE)
  }
  invisible(NULL)
} # /croissant::safe_unzip


# %% fileset_files ----
#' Files of a `FileSet`
#'
#' Lists the files under each of the set's containers, in the order the
#' containers are named, or under the directory of the metadata document when
#' it has none, whose paths relative to that root match an `includes` glob and
#' no `excludes` glob.
#'
#' @param ctx Environment: Load context from `new_load_context()`.
#' @param fs `FileSet` object.
#'
#' @return data.table with columns `filename`, `filepath`, `fullpath` (the
#' path relative to the root), and `hf`, sorted by `fullpath` within each
#' root. A file of a Hugging Face repository has `filepath` `NA` until
#' `file_path_at()` downloads it, and `hf` the `@id` of its repository
#' `FileObject`; `hf` is `NA` for other files.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fileset_files <- function(ctx, fs) {
  ids <- c(fs@contained_in, other_contained_in(fs))
  containers <- lapply(ids, function(id) {
    container <- ctx[["files"]][[id]]
    if (is.null(container)) {
      abort(
        "The FileSet \"",
        fs@id,
        "\" is contained in \"",
        id,
        "\", which is not in the distribution.",
        class = c("croissant_file_error", "rtemis_input_error")
      )
    }
    container
  })
  if (length(containers) == 0L) {
    base <- ctx[["base"]]
    if (!is.null(base) && grepl("^https?://", base)) {
      abort(
        "The FileSet \"",
        fs@id,
        "\" lists files relative to ",
        base,
        ", which is a URL. Download the files and pass their directory as `base`.",
        class = c("croissant_file_error", "rtemis_input_error")
      )
    }
    containers <- list(NULL)
  }
  parts <- lapply(containers, function(container) {
    rev <- if (!is.null(container)) hf_container(ctx, container)
    root <- NULL
    rel <- if (!is.null(rev)) {
      sort(rev[["files"]][["rfilename"]])
    } else {
      root <- if (is.null(container)) {
        ctx[["base"]] %||% getwd()
      } else {
        container_dir(ctx, container)
      }
      sort(list.files(root, recursive = TRUE, all.files = FALSE))
    }
    keep <- rep(FALSE, length(rel))
    for (glob in fs@includes %||% "*") {
      keep <- keep | grepl(glob_to_regex(glob), rel, perl = TRUE)
    }
    for (glob in fs@excludes) {
      keep <- keep & !grepl(glob_to_regex(glob), rel, perl = TRUE)
    }
    rel <- rel[keep]
    data.table::data.table(
      filename = basename(rel),
      filepath = if (is.null(root)) {
        rep(NA_character_, length(rel))
      } else {
        normalizePath(file.path(root, rel))
      },
      fullpath = rel,
      hf = rep(if (is.null(rev)) NA_character_ else container@id, length(rel))
    )
  })
  data.table::rbindlist(parts)
} # /croissant::fileset_files


# %% glob_to_regex ----
#' Convert a glob to an anchored regular expression, as `fnmatch` does
#'
#' mlcroissant matches `includes` with Python's `fnmatch.translate()`: `*`
#' matches any characters, `/` included, `?` one character, and `[...]` a
#' character class.
#'
#' @param glob Character: Glob pattern.
#'
#' @return Character: Perl regular expression.
#'
#' @author EDG
#' @keywords internal
#' @noRd
glob_to_regex <- function(glob) {
  chars <- strsplit(glob, "", fixed = TRUE)[[1L]]
  out <- character()
  i <- 1L
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (ch == "[") {
      close <- which(chars == "]" & seq_along(chars) > i + 1L)
      if (length(close) > 0L) {
        inner <- paste(chars[(i + 1L):(close[[1L]] - 1L)], collapse = "")
        inner <- sub("^!", "^", inner)
        out <- c(out, paste0("[", gsub("\\\\", "\\\\\\\\", inner), "]"))
        i <- close[[1L]] + 1L
        next
      }
    }
    out <- c(
      out,
      switch(
        ch,
        "*" = ".*",
        "?" = ".",
        if (grepl("[.^$+(){}|\\[\\]\\\\]", ch)) paste0("\\", ch) else ch
      )
    )
    i <- i + 1L
  }
  paste0("^(?s:", paste(out, collapse = ""), ")$")
} # /croissant::glob_to_regex


# %% read_content ----
#' Read a file by its encoding format
#'
#' @param path Character: Local file path.
#' @param formats Character: Encoding formats of the file.
#'
#' @return List with `kind` (`"table"`, `"json"`, or `"rows"`) and `data`: a
#' data.table, a parsed JSON document, or a list of parsed JSON Lines rows.
#'
#' @author EDG
#' @keywords internal
#' @noRd
read_content <- function(path, formats) {
  formats <- tolower(formats %||% character())
  if (
    any(formats %in% c("text/csv", "text/tab-separated-values", "text/plain"))
  ) {
    sep <- if ("text/tab-separated-values" %in% formats) "\t" else ","
    return(list(
      kind = "table",
      data = data.table::fread(
        path,
        sep = sep,
        colClasses = "character",
        na.strings = .na_strings,
        encoding = "UTF-8",
        showProgress = FALSE
      )
    ))
  }
  if ("application/json" %in% formats) {
    return(list(
      kind = "json",
      data = jsonlite::read_json(path, simplifyVector = FALSE)
    ))
  }
  if (
    any(
      formats %in%
        c(
          "application/jsonlines",
          "application/x-jsonlines",
          "application/jsonl",
          "application/x-ndjson"
        )
    )
  ) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    return(list(
      kind = "rows",
      data = lapply(lines, jsonlite::parse_json, simplifyVector = FALSE)
    ))
  }
  if (
    any(
      formats %in%
        c(
          "application/x-parquet",
          "application/vnd.apache.parquet",
          "application/parquet"
        )
    )
  ) {
    check_dependencies("arrow")
    df <- as.data.frame(arrow::read_parquet(path))
    # A struct column reads as a data.frame column; each value becomes a
    # named list, as a JSON object, so a jsonPath transform can select from
    # it.
    for (col in names(df)[vapply(df, is.data.frame, logical(1L))]) {
      inner <- df[[col]]
      df[[col]] <- lapply(seq_len(nrow(inner)), \(i) lapply(inner, `[[`, i))
    }
    return(list(kind = "table", data = data.table::as.data.table(df)))
  }
  abort(
    "read_record_set() reads columns from CSV, TSV, JSON, JSON Lines, and ",
    "Parquet files; ",
    path,
    " has encoding format ",
    paste(formats, collapse = ", "),
    ". Extract a file property (content, filename, filepath, fullpath, or ",
    "lines) from it instead.",
    class = c(
      "croissant_format_error",
      "rtemis_value_error",
      "rtemis_input_error"
    )
  )
} # /croissant::read_content


# Strings that pandas reads as missing, so that CSV values compare with
# mlcroissant's.
.na_strings <- c(
  "",
  "#N/A",
  "#N/A N/A",
  "#NA",
  "-1.#IND",
  "-1.#QNAN",
  "-NaN",
  "-nan",
  "1.#IND",
  "1.#QNAN",
  "<NA>",
  "N/A",
  "NA",
  "NULL",
  "NaN",
  "None",
  "n/a",
  "nan",
  "null"
)
