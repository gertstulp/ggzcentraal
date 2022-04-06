test_that("viz_poster wrong input", {
  # viz_poster expects dataframe
  expect_identical(viz_poster(NULL), list(errors = "Input not a dataframe"))
  expect_identical(viz_poster(NA), list(errors = "Input not a dataframe"))
  expect_identical(viz_poster(c(1, 2, 3)), list(errors = "Input not a dataframe"))

  # error should be the same as the one prepare_data gives
  expect_identical(viz_poster(prepare_data(c(1, 2, 3)))[[1]],
                   prepare_data(c(1, 2, 3)))
  expect_identical(viz_poster(prepare_data(patientdata[, -1]))[[1]],
                   prepare_data(patientdata[, -1]))
})

test_that("Correct input returns an svg", {
  data <- opencpu_like_parse_json(gettestfilepath('json/answers.json'))
  result = poster(data$answers);
  expect_match(
    result$svgs$poster,
    "<svg"
  )
  if(Sys.getenv("CI_COMMIT_SHA") == '') {
    write(result$svgs$poster, file = "../../svgs/poster.svg", append = FALSE, ncolumns = 1)
  }
})
