# Canonical nutrient order (mmol/L inside the file)
nutrients <- c("NO3_N","NH4_N","P","K","Ca","Mg","S","Na","Cl",
               "Fe","Mn","Zn","B","Cu","Mo","Si")


recipes <- list(
  knop_1865 = list(
    name = "Knop (1865)",
    notes = "1g Ca(NO3)2, 0.25g MgSO4 x 7 H2O, 0.25g KH2PO4, 0.25g KNO3 and a trace of FeSO4 per 1 l water",
    unit  = "mg/L",
    targets = c(  NO3_N=206
                , NH4_N=0
                , P=57
                , K=168
                , Ca=244
                , Mg=24
                , S=32
                , Na=0
                , Cl=0
                , Fe=0
                , Mn=0
                , Zn=0
                , B=0
                , Cu=0
                , Mo=0
                , Si=0
                )),


    shive_1915 = list(
      name = "Shive (1915)",
      notes = "2",
      unit  = "mg/L",
      targets = c(  NO3_N=148
                    , NH4_N=0
                    , P=448
                    , K=562
                    , Ca=208
                    , Mg=484
                    , S=640
                    , Na=0
                    , Cl=0
                    , Fe=0
                    , Mn=0
                    , Zn=0
                    , B=0
                    , Cu=0
                    , Mo=0
                    , Si=0
      )),
    hoagland = list(
        name = "Hoagland (1915)",
        notes = "3",
        unit  = "mg/L",
        targets = c(  NO3_N=158
                      , NH4_N=0
                      , P=44
                      , K=284
                      , Ca=200
                      , Mg=99
                      , S=0
                      , Na=12
                      , Cl=0
                      , Fe=0
                      , Mn=0
                      , Zn=0
                      , B=0
                      , Cu=0
                      , Mo=0
                      , Si=0
        )),
      huhu = list(
          name = "Knop (1865)",
          notes = "1",
          unit  = "mmol/L",
          targets = c(  NO3_N=0
                        , NH4_N=0
                        , P=0
                        , K=0
                        , Ca=0
                        , Mg=0
                        , S=0
                        , Na=0
                        , Cl=0
                        , Fe=0
                        , Mn=0
                        , Zn=0
                        , B=0
                        , Cu=0
                        , Mo=0
                        , Si=0
          )),
  lettuce = list(
    name = "Lettuce (example)",
    notes = "Demo crop-specific setting.",
    unit  = "mmol/L",
    targets = c(NO3_N=14, NH4_N=0.5, P=1.8, K=8, Ca=3, Mg=1.2,
                S=1.2, Na=0, Cl=0, Fe=0.012, Mn=0.008, Zn=0.004,
                B=0.012, Cu=0.0006, Mo=0.0005, Si=0)
  )
)

# Minimal validation (names + coverage)
validate_recipe <- function(x, nutrients) {
  stopifnot(is.list(x), is.character(x$name), nzchar(x$name))
  stopifnot(is.character(x$unit), x$unit %in% c("mmol/L","mg/L"))
  stopifnot(is.numeric(x$targets), !is.null(names(x$targets)))
  miss <- setdiff(nutrients, names(x$targets))
  if (length(miss)) stop("Recipe missing: ", paste(miss, collapse=", "))
  invisible(TRUE)
}
invisible(lapply(recipes, validate_recipe, nutrients = nutrients))

# Save ONE object named 'recipes' into the .rda
save(recipes, file = "recipes.rda")
