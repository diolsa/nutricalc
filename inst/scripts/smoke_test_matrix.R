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

achieved_ph <- res_one$achieved
achieved_ph[["Fe"]] <- 0.01
achieved_ph[["EDTA"]] <- 0.02

ph_res <- ph_from_achieved(achieved_ph, debug = FALSE)
stopifnot("EDTA" %in% names(ph_res$charge_breakdown$fixed_anions_meq))
stopifnot(ph_res$charge_breakdown$fixed_anions_meq[["EDTA"]] > 0)

cat("nutrient_matrix dim:", paste(dim(nutrient_matrix), collapse = " x "), "\n")
