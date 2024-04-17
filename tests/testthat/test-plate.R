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

test_that("can create plate from data.frame", {
  expect_equal(
    # 1 3
    # 2 4
    plate(data.frame(a = 1:2, b = 3:4), "v"),
    new_plate(
      wells = list(
        well(1, 2, list(v = 1)),
        well(1, 1, list(v = 2)),
        well(2, 2, list(v = 3)),
        well(2, 1, list(v = 4))
      )
    )
  )
})

test_that("can transpose plate", {
  expect_equal(
    # 4 3
    # 2 1
    t(plate(data.frame(a = 1:2, b = 3:4), "v")),
    new_plate(
      wells = list(
        well(2, 1, list(v = 1)),
        well(1, 1, list(v = 2)),
        well(2, 2, list(v = 3)),
        well(1, 2, list(v = 4))
      )
    )
  )
})
