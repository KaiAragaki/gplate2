test_that("updating well works", {
  expect_equal(
    update_well(new_well(1, 1, list()), list(a = "a")),
    new_well(1, 1, list(a = "a"))
  )
  expect_equal(
    update_well(new_well(1, 1, list(a = "a")), list(a = "b")),
    new_well(1, 1, list(a = "b"))
  )
  expect_error(
    update_well(new_well(1, 1, list(a = "a")), list(a = "b")),
    "New contents will overwrite old and overwrite = FALSE"
  )
})
