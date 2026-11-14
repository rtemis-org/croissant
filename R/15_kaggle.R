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
      "). Kaggle serves its generated metadata for public datasets only; ",
      "a Croissant file stored in the dataset reads with `file`.",
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  doc
} # /croissant::kaggle_croissant


# %% resolve_kaggle_token ----
#' Kaggle API token
#'
#' Resolves the token from the sources the Kaggle command-line tool and
#' 'kagglehub' read, in their order: `token`; the `KAGGLE_API_TOKEN`
#' environment variable, holding the token or the path of a file that holds
#' it; and the file `~/.kaggle/access_token`, or `~/.kaggle/access_token.txt`.
#'
#' @param token Optional Character: Token.
#'
#' @return Character: The token, or `""` when there is none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_kaggle_token <- function(token = NULL) {
  if (!is.null(token)) {
    return(if (is.na(token)) "" else token)
  }
  from_file <- function(path) {
    if (!nzchar(path) || !file.exists(path) || dir.exists(path)) {
      return("")
    }
    trimws(paste(readLines(path, warn = FALSE), collapse = ""))
  }
  env <- Sys.getenv("KAGGLE_API_TOKEN", "")
  if (nzchar(env)) {
    return(if (file.exists(env)) from_file(env) else env)
  }
  for (path in c("~/.kaggle/access_token", "~/.kaggle/access_token.txt")) {
    out <- from_file(path.expand(path))
    if (nzchar(out)) {
      return(out)
    }
  }
  ""
} # /croissant::resolve_kaggle_token


# %% is_kaggle_api ----
#' Test whether a URL is on the Kaggle API
#'
#' @param url Character: URL.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_kaggle_api <- function(url) {
  startsWith(url, "https://www.kaggle.com/api/")
} # /croissant::is_kaggle_api


# %% kaggle_headers ----
#' Request headers for a Kaggle API URL
#'
#' The token is sent as a Bearer `Authorization` header to the Kaggle API
#' only. 'libcurl' drops the header when a redirect leaves the host, as the
#' Kaggle API's redirect to Google Cloud Storage does.
#'
#' @param url Character: URL.
#' @param token Character: Kaggle API token, or `""`.
#'
#' @return Named character vector, or `NULL` when `url` is not on the Kaggle
#' API or `token` is `""`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_headers <- function(url, token) {
  if (nzchar(token) && is_kaggle_api(url)) {
    c(Authorization = paste("Bearer", token))
  }
} # /croissant::kaggle_headers


# %% kaggle_get ----
#' Download a URL from Kaggle
#'
#' @param url Character: URL.
#' @param dest Character: Destination file.
#' @param token Character: Kaggle API token, or `""`.
#'
#' @return The condition when the download fails, otherwise `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_get <- function(url, dest, token = "") {
  download_url(url, dest, kaggle_headers(url, token))
} # /croissant::kaggle_get


# %% kaggle_stored_croissant ----
#' Croissant file stored in a Kaggle dataset
#'
#' @param dataset Character: `<owner>/<slug>`.
#' @param url Character: Kaggle API download URL of the file.
#' @param token Character: Kaggle API token, or `""`.
#'
#' @return Character: The JSON text of the file.
#'
#' @author EDG
#' @keywords internal
#' @noRd
kaggle_stored_croissant <- function(dataset, url, token) {
  dest <- tempfile("croissant_kaggle_", fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  failed <- kaggle_get(url, dest, token)
  txt <- if (is.null(failed) && file.exists(dest)) {
    paste(readLines(dest, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  if (is.null(txt) || !grepl("^\\s*\\{", txt)) {
    abort(
      "Kaggle returned no file at ",
      url,
      ". The dataset `",
      dataset,
      "` or the file may not exist",
      if (nzchar(token)) {
        ", or the token may not grant access to the dataset."
      } else {
        paste0(
          ". A private dataset requires a Kaggle API token: pass `token`, ",
          "set `KAGGLE_API_TOKEN`, or save the token to ~/.kaggle/access_token."
        )
      },
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  txt
} # /croissant::kaggle_stored_croissant


# %% read_croissant_kaggle ----
#' Read the Croissant metadata of a Kaggle dataset
#'
#' With `file = NULL`, downloads the metadata Kaggle generates for a public
#' dataset (`https://www.kaggle.com/datasets/<owner>/<slug>/croissant/download`),
#' the document the dataset page exports as Croissant, and reads it with
#' [read_croissant()]. Kaggle serves this metadata for public datasets.
#' The metadata describes the dataset's current version. Its record sets read
#' through [read_record_set()], which downloads the version's zip archive from
#' the Kaggle API, checks its MD5, and extracts the files the record set reads.
#'
#' With `file`, reads a Croissant file stored in the dataset, from the Kaggle
#' API (`https://www.kaggle.com/api/v1/datasets/download/<owner>/<slug>/<file>`).
#' Its relative `contentUrl`s resolve to the dataset's files on the Kaggle API,
#' so [read_record_set()] downloads each file a record set reads.
#'
#' Requests to the Kaggle API carry the Kaggle API token when there is one,
#' which a private dataset requires. The token is resolved as the Kaggle
#' command-line tool and 'kagglehub' resolve it: `token`; the
#' `KAGGLE_API_TOKEN` environment variable, holding the token or the path of a
#' file that holds it; and the file `~/.kaggle/access_token`. A token is
#' created at <https://www.kaggle.com/settings/api>.
#'
#' @param dataset Character: Dataset, as `<owner>/<slug>` or as its URL,
#' `https://www.kaggle.com/datasets/<owner>/<slug>`.
#' @param file Optional Character: Path of a Croissant file in the dataset,
#' such as `"croissant.json"`. If `NULL`, the metadata Kaggle generates is
#' read.
#' @param token Optional Character: Kaggle API token, used with `file`. `NULL`
#' reads `KAGGLE_API_TOKEN`, then `~/.kaggle/access_token`.
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
read_croissant_kaggle <- function(
  dataset,
  file = NULL,
  token = NULL,
  verbosity = 1L
) {
  check_character_scalar(dataset)
  check_nullable_character_scalar(file)
  check_nullable_character_scalar(token)
  dataset <- kaggle_dataset(dataset)
  url <- if (is.null(file)) {
    paste0("https://www.kaggle.com/datasets/", dataset)
  } else {
    kaggle_file_url(
      paste0("https://www.kaggle.com/api/v1/datasets/download/", dataset),
      file
    )
  }
  msg0(
    bold(highlight("\u25B6")),
    " Reading ",
    highlight(url),
    "...",
    verbosity = verbosity
  )
  if (is.null(file)) {
    txt <- to_json_text(kaggle_croissant(dataset), pretty = FALSE)
    out <- read_croissant(txt, verbosity = 0L)
  } else {
    txt <- kaggle_stored_croissant(dataset, url, resolve_kaggle_token(token))
    out <- read_croissant(txt, verbosity = 0L)
    out@base <- kaggle_document_base(url)
  }
  msg0(
    "Read ",
    highlight(first_text(out@name) %||% "(unnamed)"),
    ": ",
    croissant_counts(out),
    verbosity = verbosity
  )
  out
} # /croissant::read_croissant_kaggle
