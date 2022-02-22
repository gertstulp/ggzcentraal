#! /usr/bin/env Rscript

message(R.version.string)
options(repos=structure(c(CRAN="https://cran.uni-muenster.de/")))
install.packages('packrat')
packrat::restore()
