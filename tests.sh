#!/bin/sh

cd /app
R CMD check --no-clean /app/`ls *.gz | tail -1` --no-manual --no-build-vignettes
