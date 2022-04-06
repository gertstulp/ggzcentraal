#' Create poster of all measurements, from roqua inputs
#' @param answers {csp_dna: [{id: 1, .. }]}, see tests/testthat/jsons/answers.json
#' @export
poster <- function(answers) {
  viz_poster(prepare_data(JSON_to_DF(answers)))
};
