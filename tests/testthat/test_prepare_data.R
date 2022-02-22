test_that("prepare_data not in correct data format", {
  # Input should be dataframe
  expect_identical(prepare_data(NULL), "Data is not a dataset")
  expect_identical(prepare_data(NA), "Data is not a dataset")
  expect_identical(prepare_data(c(1, 2, 3)), "Data is not a dataset")

    # Dataset should have correct number of variables
  expect_match(prepare_data(patientdata[, -1]), 
                   "Dataset does not have right number of variables")

    # Dataset should have correct variable names
  patientdata2 <- patientdata
  names(patientdata2)[names(patientdata2) == 'patient_id'] <- 'patientid' 
  # expect_identical(prepare_data(patientdata2),
  #                  "Dataset does not have prespecified variable list")
 
   # Dataset should have too many missings
  patientdata2 <- patientdata
  # Add more than 90% missing, a response on csp_dna_non_response means missing
  # a NA on csp_dna_non_response means not-missing
  # Below code generates 60 out of 63 nonresponses
  patientdata2$csp_dna_non_response <- sample(c(rep(110749242, 60), rep(NA, 3)), 
                                                nrow(patientdata))
  # expect_identical(prepare_data(patientdata2),
  #                  "Too many non-responses")
  
  # Dataset should contain minimally 20 measurements
  # expect_identical(prepare_data(patientdata[1:19, ]),
  #                  "Dataset has fewer than 20 rows")

})

test_that("prepare_data gives correct output", {
  # expect_identical(class(prepare_data(patientdata))[3],
  #                  "data.frame")
})
