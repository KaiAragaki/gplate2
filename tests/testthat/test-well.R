test_that("updating well works", {
  empty_well <- new_well(1, 1, list())
  full_well <- new_well(1, 1, list(a = "a"))
  expect_equal(
    update_well(empty_well, list(a = "a")),
    new_well(1, 1, list(a = "a"))
  )
  expect_equal(
    update_well(full_well, list(a = "b")),
    new_well(1, 1, list(a = "b"))
  )
  expect_error(
    update_well(full_well, list(a = "b"), overwrite = FALSE),
    "New contents will overwrite old and overwrite = FALSE"
  )
})
