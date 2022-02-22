test_that("viz_poster correct output", {
  # viz_poster expects dataframe
  expect_identical(viz_poster(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_poster(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_poster(c(1, 2, 3)), list(error = "Input not a dataframe"))

  # error should be the same as the one prepare_data gives
  expect_identical(viz_poster(prepare_data(c(1, 2, 3)))[[1]],
                   prepare_data(c(1, 2, 3)))
  expect_identical(viz_poster(prepare_data(patientdata[, -1]))[[1]],
                   prepare_data(patientdata[, -1]))

  # correct output viz_poster is a svg string
  # expect_identical(class(viz_poster(prepare_data(patientdata))[[1]])[2],
  #                  "svg")
})
