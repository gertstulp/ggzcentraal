#! /usr/bin/env Rscript

message(R.version.string)
options(repos=structure(c(CRAN="https://ftp.belnet.be/mirror/CRAN")))
install.packages('packrat')
packrat::restore()
