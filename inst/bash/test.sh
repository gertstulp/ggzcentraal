R --no-save --quiet -e 'devtools::document()'; R CMD build .; R CMD check `ls *.gz | tail -1` --no-manual --no-build-vignettes
