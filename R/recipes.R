# Canonical nutrient order (mmol/L inside file)
nutrients <- CANONICAL_NUTRIENTS

# ------------------------------------------------------------------------------

#' Nutrient solution recipes used by NutriCalc
#'
#' A named list of recipe definitions. Each element is a list with:
#' \describe{
#'   \item{name}{Display name}
#'   \item{category}{Category such as "Olericulture", "Fruticulture", ...}
#'   \item{notes}{Free text notes / citation}
#'   \item{unit}{Either "mmol/L" or "mg/L"}
#'   \item{salts}{TRUE (all salts) or a character vector of salt formulas}
#'   \item{targets}{Named numeric vector of nutrient targets}
#' }
#'
#' @format A named list of recipe lists.
#' @export


recipes <- list(



  ### Standard Formulations


  HU_standard = list(
    name  = "HU Standard",
    category = "Standard Formulations",
    notes = "Standard solution of the Humboldt University of Berlin, Faculty of plant nutrition.",
    unit  = "mmol/L",
    salts = c(
    "KH2PO4", "K2SO4", "MgSO4\u00b77H2O", "KCl",
    "CuSO4\u00b75H2O", "H3BO3", "MnSO4\u00b7H2O",
    "ZnSO4\u00b77H2O", "NH4H2PO4", "Ca(NO3)2\u00b74H2O", "C10H12N2O8FeNa\u00b73H2O",
    "((NH4)6)Mo7O24\u00b74H2O" ),

    targets = c(
      NO3_N=5, NH4_N=0, P=0.5, K=1.6, Ca=2.5, Mg=0.6, S=1.1,
      Na=0.2, Cl=0.1, Fe=0.2, Mn=0.0005, Zn=0.0003, B=0.001, Cu=0.0001, Mo=0.000005, Si=0
    )
  ),

  knop_1865 = list(
    name  = "Knop (1865)",
    category = "Standard Formulations",
    notes = "1 g Ca(NO3)2, 0.25 g MgSO4\u00b77H2O, 0.25 g KH2PO4, 0.25 g KNO3 per 1 L water",
    unit  = "mg/L",
    salts = c("KNO3","Ca(NO3)2","KH2PO4","MgSO4\u00b77H2O"),
    targets = c(
      NO3_N=206, NH4_N=0, P=57, K=168, Ca=244, Mg=24, S=32,
      Na=0, Cl=0, Fe=0, Mn=0, Zn=0, B=0, Cu=0, Mo=0, Si=0
    )
  ),

  shive_1915_1 = list(
    name  = "Shive (1915) [Wheat]",
    category = "Standard Formulations",
    notes = "Historic formulation

The solution giving the best growth of wheat tops contained the
three salts in the following volume-molecular partial concentrations:
KH2PO4, .OI80m.; Ca(NO3)2, .oo52m.; MgSO4, .OI50m. (Shive 1915)

note: For MgSO4 Heptahydrate is selected here

",
    unit  = "mmol/L",
    salts = c("Ca(NO3)2","KH2PO4","MgSO4\u00b77H2O"),
    targets = c(
      NO3_N=10.4, NH4_N=0, P=18, K=18, Ca=5.2, Mg=15, S=15,
      Na=0, Cl=0, Fe=0, Mn=0, Zn=0, B=0, Cu=0, Mo=0, Si=0
    )
  ),

  shive_1915_2 = list(
    name  = "Shive (1915) [Buckwheat]",
    category = "Standard Formulations",
    notes = "Historic formulation known as Shive\u00b4s Solution and reported as such by Hewitt.

In the solution yielding the greatest dry weight of buckwheat tops the volume-
molecular partial concentrations of the three salts were: KH2PO4,
.oI44m.; Ca(NO3)2, .oo52m.; MgSO4, .0200m. (Shive 1915)

note: For MgSO4 Heptahydrate is selected here


",
    unit  = "mmol/L",
    salts = c("Ca(NO3)2","KH2PO4","MgSO4\u00b77H2O"),
    targets = c(
      NO3_N=10.4, NH4_N=0, P=14.4, K=14.4, Ca=5.2, Mg=20, S=20,
      Na=0, Cl=0, Fe=0, Mn=0, Zn=0, B=0, Cu=0, Mo=0, Si=0
    )
  ),


  hoagland_1919 = list(
    name  = "Hoagland (1919)",
    category = "Standard Formulations",
    notes = "Historic formulation (Hoagland 1919).",
    unit  = "mg/L",
    salts =  TRUE,
    targets = c(
      NO3_N=158, NH4_N=0, P=44, K=284, Ca=200, Mg=99, S=125,
      Na=12, Cl=18, Fe=0, Mn=0, Zn=0, B=0, Cu=0, Mo=0, Si=0
    )
  ),

  Jones_and_shive_1921 = list(
    name  = "Jones and Shive (1921)",
    category = "Standard Formulations",
    notes = "Historic formulation (Shive and Jones (1921).",
    unit  = "mg/L",
    salts =  TRUE,
    targets = c(
      NO3_N=204, NH4_N=39, P=65, K=102, Ca=292, Mg=172, S=227,
      Na=0, Cl=0, Fe=0.8, Mn=0, Zn=0,
      B=0, Cu=0, Mo=0, Si=0
    )
  ),



  resh_2015 = list(
    name  = "Resh (2015)",
    category = "Standard Formulations",
    notes = "from: Resh, H. M. (2015). Hydroponics for the home grower. CRC press. S. 46-47",
    unit  = "mg/L",
    salts =  TRUE,
    targets = c(
      NO3_N=140, NH4_N=0, P=50, K=352, Ca=180, Mg=50, S=168,
      Na=0, Cl=0, Fe=5, Mn=0.8, Zn=0.2,
      B=0.3, Cu=0.07, Mo=0.03, Si=0
    )
  ),



#  name = list(
#    name  = "name",
#    category = "Standard Formulations",
#    notes = "here just put appendix name and citation",
#    unit  = "mmol/L",
#    salts =  TRUE,
#    targets = c(
#      NO3_N=0, NH4_N=0, P=0, K=0, Ca=0, Mg=0, S=0,
#      Na=0, Cl=0, Fe=0, Mn=0, Zn=0,
#      B=0, Cu=0, Mo=0, Si=0
#    )
#  ),

  # Pomology

  strawberry_peaty_substrate = list(
    name  = "\U0001f353 Strawberry in Peaty Substrates",
    category = "Fruticulture",
    notes = "EC (25\u00b0C): 1.7 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 11.5, NH4_N = 1.0, P = 1.0, K = 5.5, Ca = 3.25, Mg = 1.25, S = 1.5,
      Na = 0, Cl = 0, Fe = 0.02, Mn = 0.01, Zn = 0.007,
      B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),

  strawberry_recirculating_water = list(
    name  = "\U0001f353 Strawberry in Recirculating Water",
    category = "Fruticulture",
    notes = "EC (25\u00b0C): 1.5 Appendix B; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 10.0, NH4_N = 0.5, P = 1.25, K = 5.25, Ca = 2.75, Mg = 1.125, S = 1.125,
      Na = 0, Cl = 0, Fe = 0.02, Mn = 0.01, Zn = 0.004,
      B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),
  melon_rockwool = list(
    name  = "\U0001f348 Melon in Rockwool",
    category = "Fruticulture",
    notes = "EC (25\u00b0C): 2.2 Appendix B; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 16.25, NH4_N = 1.0, P = 1.25, K = 7.5, Ca = 4.75, Mg = 1.25, S = 1.5,
      Na = 0, Cl = 0, Fe = 0.01, Mn = 0.01, Zn = 0.004,
      B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0.75
    )
  ),


  # Olericulture

  endive_recirculating_water = list(
    name  = "\U0001f96c Endive in Recirculating Water",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.6 Appendix C; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 19.0, NH4_N = 1.25, P = 2.0, K = 9.0, Ca = 5.0, Mg = 1.5, S = 1.125,
      Na = 0, Cl = 0, Fe = 0.04, Mn = 0.005, Zn = 0.004,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),

  eggplant_rockwool = list(
    name  = "\U0001f346 Eggplant in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.1 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 15.5, NH4_N = 1.5, P = 1.25, K = 6.75, Ca = 3.25, Mg = 2.5, S = 1.5,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.010, Zn = 0.005,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),

  eggplant_rockwool_reuse_drainage = list(
    name  = "\U0001f346 Eggplant in Rockwool (Reuse Drainage Water)",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 1.7 Appendix B; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 11.75, NH4_N = 1.0, P = 1, K = 6.5, Ca = 2.25, Mg = 1.5, S = 1.125,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.010, Zn = 0.005,
      B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),
  bean_rockwool = list(
    name  = "\U0001fad8 Bean in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 1.7 Appendix B; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 12.0, NH4_N = 1.0, P = 1.25, K = 5.5, Ca = 3.25, Mg = 1.25, S = 1.125,
      Na = 0, Cl = 0, Fe = 0.01, Mn = 0.01, Zn = 0.004,
      B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
    )
  ),
  courgette_rockwool = list(
    name  = "\U0001f952 Courgette in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.2 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 16.0, NH4_N = 1.25, P = 1.25, K = 7.25, Ca = 3.625, Mg = 2.0, S = 1.25,
      Na = 0, Cl = 0, Fe = 0.01, Mn = 0.01, Zn = 0.005,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),
  cucumber_rockwool = list(
    name  = "\U0001f952 Cucumber in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.2 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 16.0, NH4_N = 1.25, P = 1.25, K = 8.0, Ca = 4.0, Mg = 1.375, S = 1.375,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.01, Zn = 0.005,
      B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0.75
    )
  ),
  cucumber_rockwool_reuse_drainage = list(
    name  = "\U0001f952 Cucumber in Rockwool (Reuse Drainage Water)",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 1.7 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 11.75, NH4_N = 1.0, P = 1.25, K = 6.5, Ca = 2.75, Mg = 1.0, S = 1.0,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.01, Zn = 0.005,
      B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0.75
    )
  ),


  propagation_rockwool = list(
    name  = "\U0001f33f Propagation vegetable plants in rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.4 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 16.75, NH4_N = 1.25, P = 1.25, K = 6.75, Ca = 4.5, Mg = 3.0, S = 2.5,
      Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.005,
      B = 0.035, Cu = 0.001, Mo = 0.0005, Si = 0
    )
  ),

  sweet_pepper_rockwool = list(
    name  = "\U0001fad1 Sweet Pepper in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.2 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 15.5, NH4_N = 1.25, P = 1.25, K = 6.5, Ca = 4.75, Mg = 1.5, S = 1.75,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.010, Zn = 0.005,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),

  sweet_pepper_rockwool_reuse_drainage = list(
    name  = "\U0001fad1 Sweet Pepper in Rockwool (Reuse Drainage Water)",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 1.7 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 12.75, NH4_N = 1.25, P = 1.0, K = 5.75, Ca = 3.25, Mg = 1.125, S = 1.0,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.010, Zn = 0.004,
      B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),

  lettuce_recirculating_water = list(
    name  = "\U0001f96c Lettuce in Recirculating Water",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.6 Appendix A; if peat cubes are used no manganese will be added, otherwise 5 umol is
  advisable. from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 19.0, NH4_N = 1.25, P = 2.0, K = 11.0, Ca = 4.5, Mg = 1.0, S = 1.125,
      Na = 0, Cl = 0, Fe = 0.04, Mn = 0.005, Zn = 0.004,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0.5
    )
  ),

lettuce_bht = list(
  name  = "\U0001f96c Lettuce BHT",
  category = "Olericulture",
  notes = "Demo crop-specific setting.",
  unit  = "mmol/L",
  salts =  TRUE,
  targets = c(
    NO3_N=10.7, NH4_N=0.35, P=1.3, K=6, Ca=3.5, Mg=1.0, S=1.3,
    Na=0, Cl=1.3, Fe=0.04, Mn=0.0035, Zn=0.0035,
    B=0.035, Cu=0.0007, Mo=0.0035, Si=0.0035
  )
),

  tomato_rockwool = list(
    name  = "\U0001f345 Tomato in Rockwool",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 2.3 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 13.75, NH4_N = 1.25, P = 1.25, K = 8.75, Ca = 4.25, Mg = 2.0, S = 3.75,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.01, Zn = 0.005,
      B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  ),


  tomato_rockwool_drainage_water = list(
    name  = "\U0001f345 Tomato in Rockwool (Reuse Drainage Water)",
    category = "Olericulture",
    notes = "EC (25\u00b0C): 1.6 Appendix A; from: (Sonneveld and Straver, 1994)",
    unit  = "mmol/L",
    salts = TRUE,
    targets = c(
      NO3_N = 10.75, NH4_N = 1.0, P = 1.25, K = 6.5, Ca = 2.75, Mg = 1.0, S = 1.5,
      Na = 0, Cl = 0, Fe = 0.015, Mn = 0.01, Zn = 0.004,
      B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
    )
  )
,

# Floriculture



alstroemeria_rockwool = list(
  name  = "\U0001f490 Alstroemeria in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.7 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 11.25, NH4_N = 1.25, P = 1.25, K = 6.0, Ca = 2.875, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.004,
    B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


alstroemeria_rockwool_reuse_drainage = list(
  name  = "\U0001f490 Alstroemeria in Rockwool (Reuse Drainage Water)",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.1 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 7.5, NH4_N = 0.75, P = 1.0, K = 4.75, Ca = 2.0, Mg = 0.75, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.005, Zn = 0.004,
    B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


anemone_rockwool = list(
  name  = "\U0001f490 Anemone in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.9 Appendix C; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 13.0, NH4_N = 1.0, P = 1.5, K = 6.5, Ca = 3.75, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.035, Mn = 0.005, Zn = 0.004,
    B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


carnation_rockwool_or_peat = list(
  name  = "\U0001f490 Carnation in Rockwool or Peat",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.8 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 13.0, NH4_N = 1.0, P = 1.25, K = 6.25, Ca = 3.75, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.004,
    B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),

carnation_rockwool__reuse_drainage = list(
  name  = "\U0001f490 Carnation in Rockwool (reuse drainage)",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.1 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 7.0, NH4_N = 0.75, P = 0.8, K = 4.0, Ca = 1.625, Mg = 0.6, S = 0.7,
    Na = 0, Cl = 0, Fe = 0.02, Mn = 0.005, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),

anthurium_andreanum_rockwool_or_peat = list(
  name  = "\U0001f490 Anthurium andreanum in Rockwool or Peat",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 0.8 Appendix A; Mn is given as 0 \u00b5mol, with an optional 3 \u00b5mol if necessary; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 4.5, NH4_N = 0.8, P = 0.7, K = 3.0, Ca = 1.0, Mg = 0.7, S = 1.0,
    Na = 0, Cl = 0, Fe = 0.015, Mn = 0.003, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),


aster_rockwool_or_peat = list(
  name  = "\U0001f490 Aster in Rockwool or Peat",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.8 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 13.0, NH4_N = 1.0, P = 1.25, K = 6.25, Ca = 3.75, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.004,
    B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


bouvardia_rockwool = list(
  name  = "\U0001f490 Bouvardia in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.9 Appendix C; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 13.0, NH4_N = 1.25, P = 1.75, K = 6.0, Ca = 4.25, Mg = 1.0, S = 1.5,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.005, Zn = 0.0035,
    B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


bouvardia_rockwool_reuse_drainage = list(
  name  = "\U0001f490 Bouvardia in Rockwool (Reuse Drainage Water)",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.2 Appendix C; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 8.0, NH4_N = 1.0, P = 1.5, K = 4.0, Ca = 2.5, Mg = 0.5, S = 0.75,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.005, Zn = 0.0035,
    B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


chrysanthemum_recirculating_water = list(
  name  = "\U0001f490 Chrysanthemum in Recirculating Water",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.8 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 12.75, NH4_N = 1.25, P = 1.0, K = 7.5, Ca = 2.5, Mg = 1.0, S = 1.0,
    Na = 0, Cl = 0, Fe = 0.06, Mn = 0.02, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),

cymbidium_phenol_foam = list(
  name  = "\U0001f490 Cymbidium in phenol foam",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 0.8 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 4.5, NH4_N = 0.5, P = 0.8, K = 3.0, Ca = 1.2, Mg = 0.75, S = 1.05,
    Na = 0, Cl = 0, Fe = 0.008, Mn = 0.01, Zn = 0.004,
    B = 0.02, Cu = 0.0004, Mo = 0.0004, Si = 0
  )
),

cymbidium_rockwool_urethane_foam = list(
  name  = "\U0001f490 Cymbidium in rockwool/urethane foam",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 0.8 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 4.0, NH4_N = 1.0, P = 0.8, K = 2.8, Ca = 1.0, Mg = 0.75, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.008, Mn = 0.01, Zn = 0.004,
    B = 0.02, Cu = 0.0004, Mo = 0.0004, Si = 0
  )
),


euphorbia_fulgens_rockwool = list(
  name  = "\U0001f490 Euphorbia fulgens in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.7 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 11.5, NH4_N = 1.0, P = 1.5, K = 6.0, Ca = 3.5, Mg = 1.0, S = 1.5,
    Na = 0, Cl = 0, Fe = 0.035, Mn = 0.01, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),


freesia_rockwool_or_sand = list(
  name  = "\U0001f490 Freesia in Rockwool or Sand",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 2.1 Appendix C; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 14.5, NH4_N = 1.25, P = 1.25, K = 7.75, Ca = 3.375, Mg = 1.5, S = 1.5,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.004,
    B = 0.025, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


gerbera_rockwool = list(
  name  = "\U0001f3f5\ufe0f Gerbera in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.7 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 11.25, NH4_N = 1.5, P = 1.25, K = 5.5, Ca = 3.0, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.035, Mn = 0.005, Zn = 0.004,
    B = 0.03, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),


gerbera_rockwool_reuse_drainage = list(
  name  = "\U0001f3f5\ufe0f Gerbera in Rockwool (Reuse Drainage Water)",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.1 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 7.25, NH4_N = 0.75, P = 0.6, K = 4.5, Ca = 1.6, Mg = 0.4, S = 0.7,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.005, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),



gypsophila_rockwool = list(
  name  = "\U0001f490 Gypsophila in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 2.2 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 17.0, NH4_N = 1.25, P = 1.25, K = 4.0, Ca = 6.0, Mg = 1.7, S = 1.2,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.01, Zn = 0.004,
    B = 0.03, Cu = 0.0008, Mo = 0.0005, Si = 0
  )
),


hippeastrum_pumice = list(
  name  = "\U0001f490 Hippeastrum in Pumice",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.9 Appendix B; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 13.0, NH4_N = 1.0, P = 1.25, K = 7.5, Ca = 3.125, Mg = 1.0, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.01, Mn = 0.01, Zn = 0.005,
    B = 0.03, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),



pot_plants_expanded_clay = list(
  name  = "\U0001fab4 Pot Plants in Expanded Clay",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.6 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 10.6, NH4_N = 1.1, P = 1.5, K = 5.5, Ca = 3.0, Mg = 0.75, S = 1.0,
    Na = 0, Cl = 0, Fe = 0.02, Mn = 0.01, Zn = 0.003,
    B = 0.02, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),


rose_rockwool = list(
  name  = "\U0001f339 Rose in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.6 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 11.0, NH4_N = 1.5, P = 1.25, K = 4.5, Ca = 3.25, Mg = 1.125, S = 1.25,
    Na = 0, Cl = 0, Fe = 0.025, Mn = 0.005, Zn = 0.0035,
    B = 0.02, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),

rose_rockwool__reuse_drainage = list(
  name  = "\U0001f339 Rose in Rockwool (Reuse Drainage Water)",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 0.7 Appendix A; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 4.3, NH4_N = 0.85, P = 0.5, K = 2.15, Ca = 0.9, Mg = 0.5, S = 0.5,
    Na = 0, Cl = 0, Fe = 0.015, Mn = 0.005, Zn = 0.003,
    B = 0.015, Cu = 0.0005, Mo = 0.0005, Si = 0
  )
),

statice_rockwool = list(
  name  = "\U0001f490 Statice in Rockwool",
  category = "Floriculture",
  notes = "EC (25\u00b0C): 1.7 Appendix C; from: (Sonneveld and Straver, 1994)",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 12.0, NH4_N = 1.0, P = 1.0, K = 6.0, Ca = 3.0, Mg = 1.0, S = 1.0,
    Na = 0, Cl = 0, Fe = 0.015, Mn = 0.01, Zn = 0.005,
    B = 0.0025, Cu = 0.00075, Mo = 0.0005, Si = 0
  )
),

dicots = list(
  name  = "Bugbee [Dicots]",
  category = "Standard Formulations",
  notes = "Water use efficiency 3 g l-1 (ambient 44ppm Co2); from: (Bugbee, ????). o.1 \u00b5mmol/l Ni has to be added and N +1 mmol as ph control",
  unit  = "mmol/L",
  salts = TRUE,
  targets = c(
    NO3_N = 6, NH4_N = 0, P = 0.4, K = 3.6, Ca = 1.5, Mg = 0.8, S = 0.8,
    Na = 0.021, Cl = 0.0062, Fe = 0.007, Mn = 0.003, Zn = 0.003,
    B = 0.040, Cu = 0.004, Mo = 0.0001, Si = 0.6
  )
)
)


# ------------------------------------------------------------------------------
# Validation and normalization helpers
normalize_targets <- function(x, nutrients) {
  out <- stats::setNames(numeric(length(nutrients)), nutrients)
  stopifnot(is.numeric(x$targets), !is.null(names(x$targets)))
  out[names(x$targets)] <- as.numeric(x$targets)
  out[is.na(out)] <- 0
  pmax(out, 0)
}

validate_recipe <- function(x, nutrients) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$name), length(x$name) == 1L, nzchar(x$name))
  stopifnot(is.character(x$unit), x$unit %in% c("mmol/L","mg/L"))
  stopifnot(is.numeric(x$targets), !is.null(names(x$targets)))
  miss <- setdiff(nutrients, names(x$targets))
  if (length(miss)) stop("Recipe missing: ", paste(miss, collapse = ", "))
  if (!is.null(x$salts) && !isTRUE(x$salts)) {
    stopifnot(is.character(x$salts))
  }
  invisible(TRUE)
}

recipes <- lapply(recipes, function(r) {
  validate_recipe(r, nutrients)
  r$targets <- normalize_targets(r, nutrients)
  r
})
