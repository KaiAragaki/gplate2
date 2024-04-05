test_that("plate creation works", {
  expect_equal(
    plate(well(1, 1)),
    new_plate(wells = list(well(1, 1)))
  )
  expect_equal(
    plate(list(well(1, 1), well(1, 2))),
    new_plate(wells = list(well(1, 1), well(1, 2)))
  )
})
