test_that("viz_slider correct output", {
  # viz_slider expects dataframe
  expect_identical(viz_slider(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_slider(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_slider(c(1, 2, 3)), list(error = "Input not a dataframe"))
  
  # error should be the same as the one prepare_data gives
  expect_identical(viz_slider(prepare_data(c(1, 2, 3)))[[1]],
                   prepare_data(c(1, 2, 3)))
  expect_identical(viz_slider(prepare_data(patientdata[, -1]))[[1]],
                   prepare_data(patientdata[, -1]))
  
  # correct output viz_slider is a svg string
  # expect_identical(class(viz_slider(prepare_data(patientdata))[[1]])[2],
  #                  "svg")
})
