# helper-local.R
# ::croissant::
# 2026- EDG rtemis.org

# Registers the call `cl` to run when the frame of `envir` exits, before calls
# registered earlier. on.exit() registers on the frame it is evaluated in, so
# `cl` must carry its values, not names local to the caller.
on_exit_of <- function(cl, envir) {
  do.call(on.exit, list(cl, add = TRUE, after = FALSE), envir = envir)
}

# A new directory, removed when the frame of `envir` exits.
local_tempdir <- function(envir = parent.frame()) {
  dir <- tempfile("croissant-test-")
  dir.create(dir)
  on_exit_of(call("unlink", dir, recursive = TRUE), envir)
  dir
}

# A temporary file path, removed when the frame of `envir` exits.
local_tempfile <- function(fileext = "", envir = parent.frame()) {
  path <- tempfile("croissant-test-", fileext = fileext)
  on_exit_of(call("unlink", path), envir)
  path
}

# Sets options, restored when the frame of `envir` exits.
local_options <- function(..., envir = parent.frame()) {
  old <- options(...)
  on_exit_of(as.call(list(options, old)), envir)
  invisible(old)
}

# Sets environment variables, restored when the frame of `envir` exits. An
# `NA` value unsets the variable.
local_envvar <- function(..., envir = parent.frame()) {
  new <- c(...)
  old <- Sys.getenv(names(new), unset = NA, names = TRUE)
  set_envvar(new)
  on_exit_of(as.call(list(set_envvar, old)), envir)
  invisible(old)
}

set_envvar <- function(x) {
  unset <- is.na(x)
  if (any(unset)) {
    Sys.unsetenv(names(x)[unset])
  }
  if (any(!unset)) {
    do.call(Sys.setenv, as.list(x[!unset]))
  }
}
