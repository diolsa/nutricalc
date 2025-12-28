#!/usr/bin/env Rscript

source("R/constants.R")
source("R/build_matrix.R")
source("R/utils.R")
source("R/ph.R")
source("R/solver.R")

target <- setNames(rep(0.1, length(CANONICAL_NUTRIENTS)), CANONICAL_NUTRIENTS)
importance <- setNames(rep(0, length(CANONICAL_NUTRIENTS)), CANONICAL_NUTRIENTS)

res_one <- optimize_nutrients(nutrient_matrix, target, importance)
res_two <- two_stage_optimize_nutrients(nutrient_matrix, target, importance)

stopifnot(identical(names(res_one$achieved), names(target)))
stopifnot(identical(names(res_two$achieved), names(target)))

chelate_salt <- "C10H12N2O8FeNa·3H2O"
nm_chelate <- nutrient_matrix[chelate_salt, , drop = FALSE]
target_chelate <- setNames(c(0.01), "Fe")
importance_chelate <- setNames(0, "Fe")

res_chelate <- optimize_nutrients(nm_chelate, target_chelate, importance_chelate)
ph_res_chelate <- ph_from_achieved(res_chelate$achieved_full, debug = FALSE)
stopifnot("EDTA" %in% names(ph_res_chelate$charge_breakdown$fixed_anions_meq))
stopifnot(ph_res_chelate$charge_breakdown$fixed_anions_meq[["EDTA"]] > 0)

cat("nutrient_matrix dim:", paste(dim(nutrient_matrix), collapse = " x "), "\n")
