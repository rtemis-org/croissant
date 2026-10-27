# 00_context.R
# ::croissant::
# 2026- EDG rtemis.org

# JSON-LD context of the Croissant vocabulary.
# spec: croissant/read-write#json-ld-processing
#
# A Croissant file names each property by a term of its `@context`. Every term
# the package recognizes has exactly one definition, in `.croissant_terms`. A
# file whose context defines only these terms, each as defined here, is read by
# plain key lookup; any other context is compacted against
# `croissant_context()` by `jsonld` first (see `01_jsonld.R`).

# %% Vocabulary ----
# Namespace IRIs, by prefix.
.croissant_prefixes <- c(
  sc = "https://schema.org/",
  cr = "http://mlcommons.org/croissant/",
  rai = "http://mlcommons.org/croissant/RAI/",
  dct = "http://purl.org/dc/terms/",
  wd = "https://www.wikidata.org/wiki/",
  prov = "http://www.w3.org/ns/prov#"
)

.croissant_vocab <- "https://schema.org/"

# Term definitions, as written in a context. Terms first defined in 1.1 are
# listed in `.croissant_terms_1_1`.
.croissant_terms <- list(
  citeAs = "cr:citeAs",
  column = "cr:column",
  conformsTo = "dct:conformsTo",
  data = list(`@id` = "cr:data", `@type` = "@json"),
  dataBiases = "cr:dataBiases",
  dataCollection = "cr:dataCollection",
  dataType = list(`@id` = "cr:dataType", `@type` = "@vocab"),
  equivalentProperty = "cr:equivalentProperty",
  examples = list(`@id` = "cr:examples", `@type` = "@json"),
  extract = "cr:extract",
  field = "cr:field",
  fileObject = "cr:fileObject",
  fileProperty = "cr:fileProperty",
  fileSet = "cr:fileSet",
  format = "cr:format",
  includes = "cr:includes",
  isLiveDataset = "cr:isLiveDataset",
  jsonPath = "cr:jsonPath",
  key = "cr:key",
  md5 = "cr:md5",
  parentField = "cr:parentField",
  path = "cr:path",
  personalSensitiveInformation = "cr:personalSensitiveInformation",
  recordSet = "cr:recordSet",
  references = "cr:references",
  regex = "cr:regex",
  repeated = "cr:repeated",
  replace = "cr:replace",
  samplingRate = "cr:samplingRate",
  separator = "cr:separator",
  source = "cr:source",
  subField = "cr:subField",
  transform = "cr:transform"
)

.croissant_terms_1_1 <- list(
  annotation = "cr:annotation",
  arrayShape = "cr:arrayShape",
  containedIn = "cr:containedIn",
  description = list(`@container` = "@language"),
  isArray = "cr:isArray",
  name = list(`@container` = "@language"),
  readLines = "cr:readLines",
  sdVersion = "cr:sdVersion",
  unArchive = "cr:unArchive",
  value = "cr:value"
)

.croissant_versions <- c("1.0", "1.1")


# %% croissant_context ----
#' Croissant JSON-LD context
#'
#' Returns the `@context` that [write_croissant()] writes for a Croissant
#' version.
#'
#' @param version Character \{"1.0", "1.1"\}: Croissant version.
#'
#' @return Named list: The context, ready for `jsonlite::toJSON()`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' names(croissant_context("1.1"))
croissant_context <- function(version = "1.1") {
  check_enum(version, .croissant_versions)
  terms <- .croissant_terms
  if (version == "1.1") {
    terms <- c(terms, .croissant_terms_1_1)
  }
  prefixes <- .croissant_prefixes
  if (version == "1.0") {
    prefixes <- prefixes[names(prefixes) != "prov"]
  }
  ctx <- c(
    list(`@language` = "en", `@vocab` = .croissant_vocab),
    as.list(prefixes),
    terms
  )
  ctx[c(1L, 2L, 2L + order(names(ctx)[-(1:2)]))]
} # /croissant::croissant_context


# %% is_croissant_context ----
#' Test whether a context defines only Croissant terms
#'
#' A context qualifies when it is a JSON object, defines `@language` and
#' `@vocab` as `croissant_context()` does or not at all, and defines every
#' other entry exactly as `croissant_context("1.1")` does. Entry order and
#' omitted entries do not matter.
#'
#' @param ctx Parsed `@context` value.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_croissant_context <- function(ctx) {
  if (!is.list(ctx) || (length(ctx) > 0L && is.null(names(ctx)))) {
    return(FALSE)
  }
  known <- croissant_context("1.1")
  for (term in names(ctx)) {
    if (!term %in% names(known) || !identical(ctx[[term]], known[[term]])) {
      return(FALSE)
    }
  }
  TRUE
} # /croissant::is_croissant_context


# %% expand_iri ----
#' Expand a term or compact IRI to an absolute IRI
#'
#' Resolves `x` as JSON-LD does: a keyword (`@id`) is returned unchanged, a
#' term defined in `terms` is replaced by its definition, a compact IRI
#' (`cr:field`) by its prefix's namespace, and any other relative name by
#' `@vocab` when `vocab` is `TRUE`. A Croissant term that `terms` omits is a
#' relative name: `dataType` without a definition expands to
#' `https://schema.org/dataType`.
#'
#' @param x Character scalar: Term, compact IRI, or IRI.
#' @param terms Named list: Term definitions in effect, as in a context.
#' @param vocab Logical scalar: Whether to resolve a relative name against
#' `@vocab`.
#'
#' @return Character scalar: The expanded IRI.
#'
#' @author EDG
#' @keywords internal
#' @noRd
expand_iri <- function(x, terms = .all_terms(), vocab = TRUE) {
  if (startsWith(x, "@")) {
    return(x)
  }
  def <- terms[[x]]
  if (!is.null(def)) {
    id <- if (is.list(def)) def[["@id"]] else def
    if (!is.null(id)) {
      return(expand_iri(id, terms = list(), vocab = FALSE))
    }
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*:", x)) {
    prefix <- sub(":.*$", "", x)
    ns <- .croissant_prefixes[prefix]
    if (!is.na(ns)) {
      return(paste0(ns, substring(x, nchar(prefix) + 2L)))
    }
    return(x)
  }
  if (vocab) paste0(.croissant_vocab, x) else x
} # /croissant::expand_iri


# %% compact_key ----
#' Compact an absolute IRI to the key Croissant writes for it
#'
#' Returns the term that `croissant_context()` defines for `iri`, the local
#' name for a schema.org IRI, or a compact IRI with the longest matching
#' namespace. Any other IRI is returned unchanged. A schema.org IRI whose
#' local name is a Croissant term for another IRI compacts to `sc:<name>`, so
#' `https://schema.org/dataType` stays distinct from `dataType`.
#'
#' @param iri Character scalar: Absolute IRI.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compact_key <- function(iri) {
  if (startsWith(iri, "@")) {
    return(iri)
  }
  by_iri <- .term_by_iri()
  term <- by_iri[iri]
  if (!is.na(term)) {
    return(unname(term))
  }
  local <- substring(iri, nchar(.croissant_vocab) + 1L)
  if (
    startsWith(iri, .croissant_vocab) &&
      !grepl("[/#]", local) &&
      !local %in% by_iri
  ) {
    return(local)
  }
  compact_value(iri)
} # /croissant::compact_key


# %% compact_value ----
#' Compact an absolute IRI to a compact IRI
#'
#' Uses the longest namespace in `.croissant_prefixes` that `iri` starts with,
#' so `https://schema.org/Text` compacts to `sc:Text` and a RAI IRI to `rai:`
#' rather than `cr:`.
#'
#' @param iri Character scalar: Absolute IRI.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
compact_value <- function(iri) {
  hits <- .croissant_prefixes[startsWith(iri, .croissant_prefixes)]
  if (length(hits) == 0L) {
    return(iri)
  }
  best <- which.max(nchar(hits))
  paste0(names(hits)[best], ":", substring(iri, nchar(hits[best]) + 1L))
} # /croissant::compact_value


# %% .term_by_iri ----
#' Croissant terms keyed by their absolute IRI
#'
#' @return Named character vector: Term names, named by IRI.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.term_by_iri <- function() {
  terms <- .all_terms()
  ids <- vapply(
    terms,
    function(def) {
      id <- if (is.list(def)) def[["@id"]] else def
      if (is.null(id)) {
        NA_character_
      } else {
        expand_iri(id, terms = list(), vocab = FALSE)
      }
    },
    character(1L)
  )
  keep <- !is.na(ids)
  stats::setNames(names(terms)[keep], ids[keep])
} # /croissant::.term_by_iri


# %% .all_terms ----
#' Every Croissant term definition, 1.0 and 1.1
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.all_terms <- function() {
  c(.croissant_terms, .croissant_terms_1_1)
} # /croissant::.all_terms


# %% context_terms ----
#' Term definitions of a parsed `@context`
#'
#' @param ctx Named list: Parsed `@context`.
#'
#' @return Named list: The entries that are neither keywords nor namespace
#' prefixes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
context_terms <- function(ctx) {
  ctx[
    !startsWith(names(ctx), "@") & !names(ctx) %in% names(.croissant_prefixes)
  ]
} # /croissant::context_terms
