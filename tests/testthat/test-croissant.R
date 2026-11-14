# test-croissant.R

test_that("package namespace loads", {
  expect_true(isNamespaceLoaded("croissant"))
})

test_that("every class reference of the node classes resolves", {
  expect_true(rtemis.core::check_class_references(asNamespace("croissant")))
})

test_that("a field holds fields of its own class", {
  inner <- Field(id = "rs/a/b", name = "b", data_type = "sc:Text")
  outer <- Field(id = "rs/a", name = "a", sub_field = list(inner))
  expect_identical(outer@sub_field[[1L]], inner)
  expect_error(Field(sub_field = list(Source())), "must be a Field object")
  expect_error(Field(annotation = list(inner, "b")), "must be a Field object")
})
