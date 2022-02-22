#' get testfile path
#' tese functions are created by Roqua not by Gert Stulp, creator package
#' @param filename the name of the file
#' @return complete path
#' @importFrom stringr str_c
gettestfilepath <- function(filename) {
  if (tail(strsplit(getwd(), '[/\\]')[[1]], n=1) != "testthat")
    normalizePath(str_c("tests/testthat/", filename))
  else
    normalizePath(filename)
}


# trying to be like https://github.com/jeroenooms/opencpu/blob/058eef69e811d2059f850e9977717159e8e91fde/R/parse_post.R
opencpu_like_parse_json <- function(filename) {
  obj <- as.list(jsonlite::fromJSON(filename))
  
  return(lapply(obj, function(x){
    if(isTRUE(is.atomic(x) && length(x) == 1)){
      #primitives as expressions
      return(deparse(x))
    } else {
      return(I(x))
    }
  }));
}
