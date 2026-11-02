# test-repr.R
# ::croissant::
# 2026- EDG rtemis.org

plain <- function(x) {
  local_options(width = 80L)
  repr(x, output_type = "plain")
}

test_that("a Croissant lists record sets, enumerations, files, and usage in order", {
  x <- read_croissant(
    test_path("fixtures", "1.0", "titanic.json"),
    verbosity = 0L
  )
  out <- plain(x)
  expect_match(out, "^<Croissant>\n  Titanic\n")
  expect_match(out, "passengers  14 fields: name, gender, age", fixed = TRUE)
  expect_match(
    out,
    "Enumerations (2)\n    genders, embarkation_ports",
    fixed = TRUE
  )
  expect_match(out, "passengers.csv         text/csv, 117.7 kB", fixed = TRUE)
  expect_match(out, "version          Croissant 1.0", fixed = TRUE)
  expect_match(out, "fields(x, \"passengers\")", fixed = TRUE)
  expect_match(out, "read_record_set(x, \"passengers\")", fixed = TRUE)
  sections <- c("Record sets", "Enumerations", "Files", "Metadata", "Usage")
  expect_identical(order(vapply(sections, \(s) regexpr(s, out), 1L)), 1:5)
  expect_true(all(nchar(strsplit(out, "\n")[[1L]]) <= 80L))
})

test_that("a Croissant counts the fields and enumerations it does not show", {
  df <- as.data.frame(matrix(
    1,
    1,
    40,
    dimnames = list(NULL, paste0("x", 1:40))
  ))
  for (i in 1:12) {
    df[[i]] <- factor("a")
  }
  dir <- local_tempdir()
  x <- croissant_from_data(df, file = file.path(dir, "d.csv"), name = "wide")
  out <- plain(x)
  expect_match(out, "default  40 fields: x1, x2, .*, and [0-9]+ more")
  expect_match(out, "Enumerations (12)", fixed = TRUE)
  expect_match(out, "default_x1_levels \\(1\\), .*, and [0-9]+ more")
})

test_that("Hugging Face splits are enumerations and usage names the data", {
  x <- suppressWarnings(read_croissant(
    test_path("fixtures", "1.1", "huggingface-squad_v2.json"),
    verbosity = 0L
  ))
  out <- plain(x)
  expect_match(out, "Record sets (1)\n    squad_v2  6 fields", fixed = TRUE)
  expect_match(out, "squad_v2_splits (2)", fixed = TRUE)
  expect_match(out, "read_record_set(x, \"squad_v2\")", fixed = TRUE)
})

test_that("a record set shows its key, file, values, and field rows", {
  x <- read_croissant(
    test_path("fixtures", "1.0", "titanic.json"),
    verbosity = 0L
  )
  out <- plain(x@record_set$passengers)
  expect_match(out, "^<RecordSet>\n  passengers\n  The list of passengers")
  expect_match(out, "files  passengers.csv", fixed = TRUE)
  expect_match(out, "Fields (14)", fixed = TRUE)
  expect_match(out, "gender  +Text +column sex → genders/")
  expect_match(out, "... and 2 more", fixed = TRUE)
  dir <- local_tempdir()
  m <- croissant_from_data(
    data.frame(f = factor(c("lo", "hi"), levels = c("lo", "hi"))),
    file = file.path(dir, "f.csv"),
    name = "f"
  )
  out <- plain(m@record_set$default_f_levels)
  expect_match(out, "type    Enumeration", fixed = TRUE)
  expect_match(out, "values  lo, hi", fixed = TRUE)
})

test_that("a field shows its type, source, and reference", {
  x <- read_croissant(
    test_path("fixtures", "1.0", "titanic.json"),
    verbosity = 0L
  )
  out <- plain(x@record_set$passengers@field[[2L]])
  expect_match(out, "^<Field>\n  passengers/gender\n")
  expect_match(out, "type +Text")
  expect_match(out, "source +passengers.csv, column sex")
  expect_match(out, "references +genders/")
})

test_that("files, sources, and agents show only the properties that are set", {
  out <- plain(FileObject(
    id = "a.zip",
    content_url = "https://example.org/a.zip",
    content_size = "2500000 B"
  ))
  expect_identical(
    out,
    paste0(
      "<FileObject>\n  a.zip\n",
      "  url   https://example.org/a.zip\n",
      "  size  2.5 MB (2500000 B)\n"
    )
  )
  expect_identical(
    plain(FileSet(id = "imgs", includes = "*.jpg", contained_in = "a.zip")),
    "<FileSet>\n  imgs\n  includes      *.jpg\n  contained in  a.zip\n"
  )
  s <- Source(
    file_set = "imgs",
    extract = Extract(file_property = "filename"),
    transform = list(Transform(regex = "^(\\d+)"))
  )
  expect_identical(
    plain(s),
    "<Source>\n  imgs, file property filename, regex ^(\\d+)\n"
  )
  expect_identical(plain(Extract(column = "age")), "<Extract>\n  column age\n")
  expect_identical(
    plain(Transform(separator = ",")),
    "<Transform>\n  separator \",\"\n"
  )
  expect_match(
    plain(ParentField(references = Source(field = "a/id"))),
    "references  a/id",
    fixed = TRUE
  )
  expect_identical(
    plain(Person(name = "Ada Lovelace", email = "ada@example.org")),
    "<Person>\n  Ada Lovelace\n  email  ada@example.org\n"
  )
  expect_identical(
    plain(Organization(name = "MLCommons")),
    "<Organization>\n  MLCommons\n"
  )
  expect_match(
    plain(CreativeWork(name = "CC BY 4.0", url = "https://example.org")),
    "url  https://example.org",
    fixed = TRUE
  )
})

test_that("print writes repr and returns its argument", {
  x <- Extract(column = "age")
  expect_output(y <- print(x), "column age")
  expect_identical(y, x)
})
