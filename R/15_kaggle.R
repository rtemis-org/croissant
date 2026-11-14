# 15_kaggle.R
# ::croissant::
# 2026- EDG rtemis.org

# Kaggle datasets.
# spec: croissant/read-write#kaggle

# %% kaggle_dataset ----
#' Kaggle dataset named by a slug or a dataset URL
#'
#' @param dataset Character: `<owner>/<slug>`, or a URL
#' `https://www.kaggle.com/datasets/<owner>/<slug>`, optionally followed by
#' further path segments, a query, or a fragment.
#'
#' @return Character: `<owner>/<slug>`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_dataset <- function(dataset) {
  slug <- "([A-Za-z0-9][A-Za-z0-9_-]*/[A-Za-z0-9][A-Za-z0-9._-]*)"
  url <- paste0(
    "^https?://(?:www\\.)?kaggle\\.com/datasets/",
    slug,
    "(?:[/?#].*)?$"
  )
  m <- regmatches(dataset, regexec(url, dataset, perl = TRUE))[[1L]]
  if (length(m) > 0L) {
    return(m[[2L]])
  }
  if (grepl(paste0("^", slug, "$"), dataset, perl = TRUE)) {
    return(dataset)
  }
  abort(
    "`dataset` names a Kaggle dataset as `<owner>/<slug>` or as its URL, ",
    "`https://www.kaggle.com/datasets/<owner>/<slug>`. Received: `",
    dataset,
    "`.",
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /croissant::kaggle_dataset


# %% kaggle_download_parts ----
#' Parts of a Kaggle API download URL
#'
#' @param url Character: URL.
#'
#' @return List: `root`, the URL
#' `https://www.kaggle.com/api/v1/datasets/download/<owner>/<slug>`, and
#' `path`, the decoded file path after it, `""` when there is none; `NULL`
#' when `url` is not a Kaggle API download URL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_download_parts <- function(url) {
  pattern <- paste0(
    "^(https://www\\.kaggle\\.com/api/v1/datasets/download/",
    "[^/?#]+/[^/?#]+)(?:/([^?#]*))?(?:[?#].*)?$"
  )
  m <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
  if (length(m) == 0L) {
    return(NULL)
  }
  list(root = m[[2L]], path = utils::URLdecode(m[[3L]]))
} # /croissant::kaggle_download_parts


# %% kaggle_file_url ----
#' Kaggle API download URL of a file of a dataset
#'
#' The Kaggle API serves a file in a folder only when the `/` of its path is
#' written `%2F`.
#'
#' @param root Character: `https://www.kaggle.com/api/v1/datasets/download/<owner>/<slug>`.
#' @param path Character: File path within the dataset.
#'
#' @return Character: URL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_file_url <- function(root, path) {
  paste0(root, "/", utils::URLencode(path, reserved = TRUE))
} # /croissant::kaggle_file_url


# %% kaggle_document_base ----
#' Base of a document read from a Kaggle API download URL
#'
#' @param url Character: Kaggle API download URL of a file.
#'
#' @return Character: The dataset's download root, followed by the encoded
#' folder of the file when the file is in a folder.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_document_base <- function(url) {
  parts <- kaggle_download_parts(url)
  dir <- dirname(parts[["path"]])
  if (dir %in% c(".", "")) {
    parts[["root"]]
  } else {
    kaggle_file_url(parts[["root"]], dir)
  }
} # /croissant::kaggle_document_base


# %% kaggle_resolve ----
#' Resolve a relative `contentUrl` against a Kaggle API download base
#'
#' @param base Character: Base from `kaggle_document_base()`.
#' @param url Character: Relative `contentUrl`.
#'
#' @return Character: Kaggle API download URL of the file.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_resolve <- function(base, url) {
  parts <- kaggle_download_parts(base)
  dir <- sub("/+$", "", parts[["path"]])
  path <- if (nzchar(dir)) paste0(dir, "/", url) else url
  kaggle_file_url(parts[["root"]], path)
} # /croissant::kaggle_resolve


# %% kaggle_croissant ----
#' Croissant metadata Kaggle generates for a dataset
#'
#' @param dataset Character: `<owner>/<slug>`.
#'
#' @return List: The parsed JSON-LD document.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_croissant <- function(dataset) {
  page <- paste0("https://www.kaggle.com/datasets/", dataset)
  dest <- tempfile("croissant_kaggle_", fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  failed <- kaggle_get(paste0(page, "/croissant/download"), dest)
  doc <- if (is.null(failed) && file.exists(dest)) {
    tryCatch(
      jsonlite::read_json(dest, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (!is.list(doc) || is.null(names(doc))) {
    abort(
      "Kaggle returned no Croissant metadata for `",
      dataset,
      "`. The dataset may not exist, be private, or have no Croissant ",
      "metadata (",
      page,
      ").",
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  doc
} # /croissant::kaggle_croissant


# %% kaggle_get ----
#' Download a URL from Kaggle
#'
#' @param url Character: URL.
#' @param dest Character: Destination file.
#'
#' @return The condition when the download fails, otherwise `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_get <- function(url, dest) {
  status <- tryCatch(
    utils::download.file(url, dest, mode = "wb", quiet = TRUE),
    error = identity,
    warning = identity
  )
  if (inherits(status, "condition")) status else NULL
} # /croissant::kaggle_get


# %% read_croissant_kaggle ----
#' Read the Croissant metadata of a Kaggle dataset
#'
#' With `file = NULL`, downloads the metadata Kaggle generates for a public
#' dataset (`https://www.kaggle.com/datasets/<owner>/<slug>/croissant/download`)
#' and reads it with [read_croissant()]. The metadata describes the dataset's
#' current version. Its record sets read through [read_record_set()], which
#' downloads the version's zip archive from the Kaggle API, checks its MD5,
#' and extracts the files the record set reads.
#'
#' With `file`, reads a Croissant file stored in the dataset, from the Kaggle
#' API (`https://www.kaggle.com/api/v1/datasets/download/<owner>/<slug>/<file>`).
#' Its relative `contentUrl`s resolve to the dataset's files on the Kaggle API,
#' so [read_record_set()] downloads each file a record set reads.
#'
#' @param dataset Character: Dataset, as `<owner>/<slug>` or as its URL,
#' `https://www.kaggle.com/datasets/<owner>/<slug>`.
#' @param file Optional Character: Path of a Croissant file in the dataset,
#' such as `"croissant.json"`. If `NULL`, the metadata Kaggle generates is
#' read.
#' @param verbosity Integer: Verbosity level; `1L` reports the dataset before
#' reading, and the dataset name and counts of record sets, enumerations, and
#' files after.
#'
#' @return `Croissant` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' # The metadata Kaggle generates
#' x <- read_croissant_kaggle("heptapod/titanic")
#' x
#' cache_dir <- tempfile()
#' head(read_record_set(
#'   x,
#'   "train_and_test2.csv",
#'   n_max = 5L,
#'   cache_dir = cache_dir
#' ))
#' # A Croissant file stored in the dataset
#' y <- read_croissant_kaggle("rtemisorg/rtemis-demo", file = "croissant.json")
#' read_record_set(y, "classification", cache_dir = cache_dir)
#' unlink(cache_dir, recursive = TRUE)
read_croissant_kaggle <- function(dataset, file = NULL, verbosity = 1L) {
  check_character_scalar(dataset)
  check_nullable_character_scalar(file)
  dataset <- kaggle_dataset(dataset)
  if (!is.null(file)) {
    root <- paste0("https://www.kaggle.com/api/v1/datasets/download/", dataset)
    return(read_croissant(kaggle_file_url(root, file), verbosity = verbosity))
  }
  msg0(
    bold(highlight("\u25B6")),
    " Reading ",
    highlight(paste0("https://www.kaggle.com/datasets/", dataset)),
    "...",
    verbosity = verbosity
  )
  doc <- kaggle_croissant(dataset)
  out <- read_croissant(to_json_text(doc, pretty = FALSE), verbosity = 0L)
  msg0(
    "Read ",
    highlight(first_text(out@name) %||% "(unnamed)"),
    ": ",
    croissant_counts(out),
    verbosity = verbosity
  )
  out
} # /croissant::read_croissant_kaggle
