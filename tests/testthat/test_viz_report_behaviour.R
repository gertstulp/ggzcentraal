test_that("viz_report_behaviour correct output", {
  # viz_report_behaviour expects dataframe
  expect_identical(viz_report_behaviour(NULL), list(error = "Input not a dataframe"))
  expect_identical(viz_report_behaviour(NA), list(error = "Input not a dataframe"))
  expect_identical(viz_report_behaviour(c(1, 2, 3)), list(error = "Input not a dataframe"))
  
  # error should be the same as the one prepare_data gives
  expect_identical(viz_report_behaviour(prepare_data(c(1, 2, 3)))[[1]], 
                   prepare_data(c(1, 2, 3)))
  expect_identical(viz_report_behaviour(prepare_data(patientdata[, -1]))[[1]], 
                   prepare_data(patientdata[, -1]))
  
  # correct output viz_report_behaviour is a svg string
  # expect_identical(class(viz_report_behaviour(prepare_data(patientdata))[[1]])[2], 
  #                  "svg")
})
