# 13_huggingface.R
# ::croissant::
# 2026- EDG rtemis.org

# Hugging Face Hub datasets.
# spec: croissant/read-write#hugging-face
#
# Paths and verification follow rtemis's contract (spec: rtemis/read-urls,
# "Hugging Face datasets"), so that rtemis can call these functions in place
# of its own: `hf://datasets/<owner>/<name>[@<revision>][/<file>]`, the Hub at
# `HF_ENDPOINT`, a revision resolved to a commit by the revision API, files
# downloaded from `resolve/<commit>/`, and each file checked against the
# listed size and LFS SHA-256 or git blob SHA-1. A token, from `HF_TOKEN` by
# default, is sent as an `Authorization: Bearer` header and appears in no
# message or condition.

# %% is_hf ----
#' Test whether a path is a Hugging Face `hf://` path
#'
#' @param path Value to test.
#'
#' @return Logical scalar: `TRUE` for a character scalar starting with
#' `hf://`, in any case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_hf <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    grepl("^hf://", path, ignore.case = TRUE)
} # /croissant::is_hf


# %% parse_hf ----
#' Parse an `hf://datasets/` path
#'
#' @param path Character: `hf://datasets/<owner>/<name>[@<revision>][/<file>]`.
#'
#' @return List with `repo` (`<owner>/<name>`), `revision` (default
#' `"main"`), and `file` (`NULL` when the path names the repository only).
#'
#' @author EDG
#' @keywords internal
#' @noRd
parse_hf <- function(path) {
  if (!grepl("^hf://datasets/", path, ignore.case = TRUE)) {
    abort(
      "`hf://` paths name datasets only: `",
      path,
      "`. Use `hf://datasets/<owner>/<name>/<file>`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  pattern <- paste0(
    "^hf://datasets/",
    "([A-Za-z0-9][A-Za-z0-9._-]*)/([A-Za-z0-9][A-Za-z0-9._-]*)",
    "(?:@([A-Za-z0-9._%-]+))?",
    "(?:/(.*))?$"
  )
  parts <- regmatches(
    path,
    regexec(pattern, path, perl = TRUE, ignore.case = TRUE)
  )[[1L]]
  if (length(parts) == 0L) {
    abort(
      "Cannot parse the Hugging Face path `",
      path,
      "`. Use `hf://datasets/<owner>/<name>[@<revision>][/<file>]`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  file <- sub("/+$", "", parts[[5L]])
  if (nzchar(file)) {
    segments <- strsplit(file, "/", fixed = TRUE)[[1L]]
    if (any(segments %in% c("", ".", ".."))) {
      abort(
        "The file part of `",
        path,
        "` has an empty, `.`, or `..` segment. Name the file by its path in ",
        "the repository, as `data/x.parquet`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(
    repo = paste0(parts[[2L]], "/", parts[[3L]]),
    revision = if (nzchar(parts[[4L]])) {
      utils::URLdecode(parts[[4L]])
    } else {
      "main"
    },
    file = if (nzchar(file)) file
  )
} # /croissant::parse_hf


# %% hf_endpoint ----
#' Hugging Face Hub endpoint
#'
#' @return Character: `HF_ENDPOINT`, or `https://huggingface.co`, without a
#' trailing slash.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_endpoint <- function() {
  endpoint <- sub(
    "/+$",
    "",
    Sys.getenv("HF_ENDPOINT", "https://huggingface.co")
  )
  if (!grepl("^https?://", endpoint)) {
    abort(
      "The `HF_ENDPOINT` environment variable must be an http:// or https:// ",
      "URL. Got: `",
      endpoint,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  endpoint
} # /croissant::hf_endpoint


# %% hf_repo_url ----
#' Repository and revision of a Hub dataset URL
#'
#' Recognizes `<endpoint>/datasets/<owner>/<name>`, optionally followed by
#' `/tree/<revision>` with the revision URL-encoded, the form Hugging Face
#' writes as the `contentUrl` of a dataset's `git+https` `FileObject`.
#'
#' @param url Character: URL.
#'
#' @return List with `repo` and `revision`, or `NULL` when `url` is not a Hub
#' dataset URL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_repo_url <- function(url) {
  if (is.null(url)) {
    return(NULL)
  }
  hosts <- unique(c(hf_endpoint(), "https://huggingface.co"))
  for (host in hosts) {
    prefix <- paste0(host, "/datasets/")
    if (!startsWith(url, prefix)) {
      next
    }
    rest <- sub("/+$", "", substring(url, nchar(prefix) + 1L))
    m <- regmatches(
      rest,
      regexec("^([^/]+/[^/]+)(?:/tree/([^/]+))?$", rest, perl = TRUE)
    )[[1L]]
    if (length(m) == 0L) {
      return(NULL)
    }
    return(list(
      repo = m[[2L]],
      revision = if (nzchar(m[[3L]])) utils::URLdecode(m[[3L]]) else "main"
    ))
  }
  NULL
} # /croissant::hf_repo_url


# %% hf_token ----
#' Hugging Face token
#'
#' @param token Optional Character: Token. `NULL` reads `HF_TOKEN`.
#'
#' @return Character: The token, or `""` when there is none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_token <- function(token = NULL) {
  token <- token %||% Sys.getenv("HF_TOKEN", "")
  if (is.na(token)) "" else token
} # /croissant::hf_token


# %% hf_get ----
#' Download a URL from the Hub, with the token when there is one
#'
#' @param url Character: URL.
#' @param dest Character: Destination file.
#' @param token Character: Token, or `""`.
#'
#' @return The condition when the download fails, otherwise `NULL`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_get <- function(url, dest, token) {
  headers <- if (nzchar(token)) c(Authorization = paste("Bearer", token))
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(
    utils::download.file(
      url,
      dest,
      mode = "wb",
      quiet = TRUE,
      headers = headers
    ),
    error = identity,
    warning = identity
  )
  if (inherits(status, "condition")) status else NULL
} # /croissant::hf_get


# %% hf_json ----
#' Read a JSON document from the Hub API
#'
#' @param url Character: API URL.
#' @param token Character: Token, or `""`.
#' @param what Character: Description of the document, for messages.
#'
#' @return Parsed JSON.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_json <- function(url, token, what) {
  dest <- tempfile("croissant_hf_", fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  failed <- hf_get(url, dest, token)
  if (!is.null(failed) || !file.exists(dest)) {
    abort(
      "The Hugging Face Hub returned no ",
      what,
      ". The dataset may not exist, or be private or gated, which needs a ",
      "token with access in `HF_TOKEN`.",
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  jsonlite::read_json(dest, simplifyVector = FALSE)
} # /croissant::hf_json


# %% hf_revision ----
#' Resolve a dataset revision to a commit and its files
#'
#' @param repo Character: `<owner>/<name>`.
#' @param revision Character: Branch, tag, or commit.
#' @param token Character: Token, or `""`.
#'
#' @return List with `repo`, `revision`, `commit`, and `files`, a data.frame
#' with `rfilename`, `size`, `lfs_sha256`, and `blob_id`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_revision <- function(repo, revision, token) {
  info <- hf_json(
    paste0(
      hf_endpoint(),
      "/api/datasets/",
      repo,
      "/revision/",
      utils::URLencode(revision, reserved = TRUE),
      "?blobs=true"
    ),
    token,
    paste0("dataset `", repo, "` at revision `", revision, "`")
  )
  commit <- info[["sha"]]
  if (!is.character(commit) || !grepl("^[0-9a-f]{40}$", commit)) {
    abort(
      "The Hugging Face Hub response for `",
      repo,
      "` carries no commit SHA.",
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  if (
    !isFALSE(info[["gated"]]) && !is.null(info[["gated"]]) && !nzchar(token)
  ) {
    abort(
      "The dataset `",
      repo,
      "` is gated. Accept its terms on the Hub and set `HF_TOKEN` to a ",
      "token with access.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  siblings <- info[["siblings"]] %||% list()
  files <- data.frame(
    rfilename = vapply(siblings, \(s) s[["rfilename"]], character(1L)),
    size = vapply(siblings, \(s) as.numeric(s[["size"]] %||% NA), numeric(1L)),
    lfs_sha256 = vapply(
      siblings,
      \(s) s[["lfs"]][["sha256"]] %||% NA_character_,
      character(1L)
    ),
    blob_id = vapply(
      siblings,
      \(s) s[["blobId"]] %||% NA_character_,
      character(1L)
    )
  )
  list(repo = repo, revision = revision, commit = commit, files = files)
} # /croissant::hf_revision


# %% hf_fetch ----
#' Download one file of a resolved revision into the cache, verified
#'
#' A cached file of the listed size is reused. A new download must match the
#' listed size and the LFS SHA-256, or for a file outside LFS the git blob
#' SHA-1 of `blob <size>`, a NUL byte, and the content; a mismatch removes the
#' file and raises `croissant_checksum_error`.
#'
#' @param rev List from `hf_revision()`.
#' @param file Character: Path of the file in the repository.
#' @param cache_dir Character: Cache directory.
#' @param token Character: Token, or `""`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character: Local path.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_fetch <- function(rev, file, cache_dir, token, verbosity) {
  row <- match(file, rev[["files"]][["rfilename"]])
  if (is.na(row)) {
    abort(
      "The dataset `",
      rev[["repo"]],
      "` at commit ",
      substr(rev[["commit"]], 1L, 7L),
      " has no file `",
      file,
      "`.",
      class = c("croissant_file_error", "rtemis_input_error")
    )
  }
  listed <- rev[["files"]][row, ]
  dest <- file.path(
    cache_dir,
    "hf",
    "datasets",
    rev[["repo"]],
    rev[["commit"]],
    file
  )
  if (file.exists(dest) && isTRUE(file.size(dest) == listed[["size"]])) {
    return(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  url <- paste0(
    hf_endpoint(),
    "/datasets/",
    rev[["repo"]],
    "/resolve/",
    rev[["commit"]],
    "/",
    utils::URLencode(file, reserved = FALSE)
  )
  info(
    "Downloading ",
    rev[["repo"]],
    "@",
    substr(rev[["commit"]], 1L, 7L),
    " ",
    file,
    verbosity = verbosity
  )
  failed <- hf_get(url, dest, token)
  if (!is.null(failed)) {
    unlink(dest)
    abort(
      "Downloading `",
      file,
      "` from `",
      rev[["repo"]],
      "` failed: ",
      conditionMessage(failed),
      class = c("croissant_download_error", "rtemis_input_error")
    )
  }
  mismatch <- hf_verify(dest, listed)
  actual <- mismatch[["actual"]]
  expected <- mismatch[["expected"]]
  if (!is.null(actual)) {
    unlink(dest)
    abort(
      "The file `",
      file,
      "` downloaded from `",
      rev[["repo"]],
      "` at commit ",
      substr(rev[["commit"]], 1L, 7L),
      " has ",
      actual,
      " where the Hub lists ",
      expected,
      ". Download it again.",
      class = c("croissant_checksum_error", "rtemis_input_error")
    )
  }
  dest
} # /croissant::hf_fetch


# %% hf_verify ----
#' Check a downloaded file against its Hub listing
#'
#' @param path Character: Local file.
#' @param listed data.frame row from `hf_revision()` with `size`,
#' `lfs_sha256`, and `blob_id`.
#'
#' @return `NULL` when the file matches; otherwise a list with `actual` and
#' `expected`, describing the first mismatch: size, LFS SHA-256, or, for a
#' file outside LFS, the git blob SHA-1 of `blob <size>`, a NUL byte, and the
#' content.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_verify <- function(path, listed) {
  size <- file.size(path)
  if (!isTRUE(size == listed[["size"]])) {
    return(list(actual = paste("size", size), expected = listed[["size"]]))
  }
  if (!is.na(listed[["lfs_sha256"]])) {
    sha <- sha256_file(path)
    if (!identical(sha, listed[["lfs_sha256"]])) {
      return(list(
        actual = paste("SHA-256", sha),
        expected = listed[["lfs_sha256"]]
      ))
    }
    return(NULL)
  }
  if (!is.na(listed[["blob_id"]])) {
    sha <- unclass(as.character(openssl::sha1(c(
      charToRaw(paste0("blob ", format(size, scientific = FALSE))),
      as.raw(0L),
      readBin(path, "raw", n = size)
    ))))
    if (!identical(sha, listed[["blob_id"]])) {
      return(list(
        actual = paste("git blob SHA-1", sha),
        expected = listed[["blob_id"]]
      ))
    }
  }
  NULL
} # /croissant::hf_verify


# %% hf_data_files ----
#' Data files among a repository's files
#'
#' @param files Character: Repository file paths.
#'
#' @return Character: The paths with a data file extension.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_data_files <- function(files) {
  files[grepl(
    "\\.(parquet|csv|tsv|csv\\.gz|tsv\\.gz|rds|xlsx|dta|arff|fasta)$",
    files,
    ignore.case = TRUE
  )]
} # /croissant::hf_data_files


# %% hf_download ----
#' Download a file from a Hugging Face dataset
#'
#' Resolves an `hf://datasets/` path to a commit, downloads the file from that
#' commit into `cache_dir`, and verifies its size and its LFS SHA-256 or git
#' blob SHA-1 against the Hub's listing. A cached file of the listed size is
#' reused; a commit's files do not change. A path that names the repository
#' only reads its one data file (`.parquet`, `.csv`, `.tsv`, `.csv.gz`,
#' `.tsv.gz`, `.rds`, `.xlsx`, `.dta`, `.arff`, or `.fasta`).
#'
#' A gated or private dataset requires a token with access, read from
#' `HF_TOKEN` by default. The Hub is at `HF_ENDPOINT`, or
#' `https://huggingface.co`.
#'
#' @param path Character:
#' `hf://datasets/<owner>/<name>[@<revision>][/<file>]`. A revision that
#' contains `/` is URL-encoded, as `refs%2Fconvert%2Fparquet`.
#' @param cache_dir Character: Cache directory.
#' @param token Optional Character: Hugging Face token. `NULL` reads
#' `HF_TOKEN`.
#' @param verbosity Integer: Verbosity level; `1L` reports downloads.
#'
#' @return Character: Local path of the file, with attribute `hf`, the path
#' pinned to the commit (`hf://datasets/<owner>/<name>@<commit>/<file>`).
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' path <- hf_download("hf://datasets/ylecun/mnist@refs%2Fconvert%2Fparquet/mnist/test/0000.parquet")
#' attr(path, "hf")
hf_download <- function(
  path,
  cache_dir = getOption(
    "croissant.cache_dir",
    tools::R_user_dir("croissant", "cache")
  ),
  token = NULL,
  verbosity = 1L
) {
  check_character_scalar(path)
  check_character_scalar(cache_dir)
  ref <- parse_hf(path)
  token <- hf_token(token)
  rev <- hf_revision(ref[["repo"]], ref[["revision"]], token)
  file <- ref[["file"]]
  if (is.null(file)) {
    candidates <- hf_data_files(rev[["files"]][["rfilename"]])
    if (length(candidates) != 1L) {
      abort(
        "The dataset `",
        ref[["repo"]],
        "` at commit ",
        substr(rev[["commit"]], 1L, 7L),
        if (length(candidates) == 0L) {
          " has no data file."
        } else {
          paste0(
            " has ",
            length(candidates),
            " data files: ",
            paste0("`", candidates, "`", collapse = ", "),
            ". Name one, as `hf://datasets/",
            ref[["repo"]],
            "/",
            candidates[[1L]],
            "`."
          )
        },
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    file <- candidates[[1L]]
  }
  out <- hf_fetch(rev, file, cache_dir, token, verbosity)
  attr(out, "hf") <- paste0(
    "hf://datasets/",
    ref[["repo"]],
    "@",
    rev[["commit"]],
    "/",
    file
  )
  out
} # /croissant::hf_download


# %% read_croissant_hf ----
#' Read the Croissant metadata of a Hugging Face dataset
#'
#' Downloads the metadata the Hub generates for a dataset
#' (`<endpoint>/api/datasets/<owner>/<name>/croissant`) and reads it with
#' [read_croissant()]. Its record sets read through [read_record_set()],
#' which downloads the Parquet files they name from the dataset's
#' `refs/convert/parquet` branch.
#'
#' @param repo Character: Dataset, as `<owner>/<name>` or
#' `hf://datasets/<owner>/<name>`.
#' @param token Optional Character: Hugging Face token, needed for gated and
#' private datasets. `NULL` reads `HF_TOKEN`.
#'
#' @return `Croissant` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' x <- read_croissant_hf("ylecun/mnist")
#' x
#' head(read_record_set(x, "mnist", n_max = 5L))
read_croissant_hf <- function(repo, token = NULL) {
  check_character_scalar(repo)
  if (is_hf(repo)) {
    ref <- parse_hf(repo)
    repo <- ref[["repo"]]
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*/[A-Za-z0-9][A-Za-z0-9._-]*$", repo)) {
    abort(
      "`repo` names a dataset as `<owner>/<name>`. Received: `",
      repo,
      "`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  doc <- hf_json(
    paste0(hf_endpoint(), "/api/datasets/", repo, "/croissant"),
    hf_token(token),
    paste0("Croissant metadata for `", repo, "`")
  )
  read_croissant(to_json_text(doc, pretty = FALSE))
} # /croissant::read_croissant_hf
