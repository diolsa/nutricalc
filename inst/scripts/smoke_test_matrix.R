#!/usr/bin/env Rscript

source("R/constants.R")
source("R/build_matrix.R")
source("R/utils.R")
source("R/solver.R")

target <- setNames(rep(0.1, length(CANONICAL_NUTRIENTS)), CANONICAL_NUTRIENTS)
importance <- setNames(rep(0, length(CANONICAL_NUTRIENTS)), CANONICAL_NUTRIENTS)

res_one <- optimize_nutrients(nutrient_matrix, target, importance)
res_two <- two_stage_optimize_nutrients(nutrient_matrix, target, importance)

stopifnot(identical(names(res_one$achieved), names(target)))
stopifnot(identical(names(res_two$achieved), names(target)))

cat("nutrient_matrix dim:", paste(dim(nutrient_matrix), collapse = " x "), "\n")
