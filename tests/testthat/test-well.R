test_that("creating well works", {
  expect_equal(
    well(1, 1, list()),
    new_well(1, 1, list())
  )
  expect_equal(
    well(1, 1),
    new_well(1, 1, list())
  )
  expect_equal(
    well(1, 1, NULL),
    new_well(1, 1, list())
  )
  expect_error(
    well(1, 1, list(1)),
    "All items of `content` must be named"
  )
  expect_error(
    well(1, 1, list(1, a = "2")),
    "All items of `content` must be named"
  )
})

test_that("updating well works", {
  empty_well <- well(1, 1, list())
  full_well <- well(1, 1, list(a = "a"))
  expect_equal(
    update_well(empty_well, list(a = "a")),
    well(1, 1, list(a = "a"))
  )
  expect_equal(
    update_well(full_well, list(a = "b")),
    well(1, 1, list(a = "b"))
  )
  expect_error(
    update_well(full_well, list(a = "b"), overwrite = FALSE),
    "New contents will overwrite old and overwrite = FALSE"
  )
})

test_that("selecting well works", {
  empty_well <- well(1, 1, list())
  full_well <- well(1, 1, list(a = "a"))
  expect_equal(
    select_well(full_well, -a), empty_well
  )
  expect_equal(
    select_well(full_well, a), full_well
  )
})

test_that("position works", {
  well <- well(1, 2, list())

  expect_equal(position(well), c(1, 2))
  expect_equal(
    {
      position(well) <- c(2, 3)
      position(well)
    },
    c(2, 3)
  )
})
