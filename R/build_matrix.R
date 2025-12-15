nutrient_matrix <- matrix(c(
#NO3 NH4 P  K   Ca Mg S  Na Cl Fe Mn Zn B  Cu Mo SI
  2,  0, 0, 0,  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # Ca(NO3)2
  1,  1, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # NH4NO3
  0,  0, 1, 1,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # KH2PO4
  1,  0, 0, 1,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # KNO3
  0,  0, 0, 2,  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # K2SO4
  0,  0, 0, 0,  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # MgSO4·7H2O
  0,  0, 0, 0,  1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,  # CaCl2·2H2O
  0,  0, 0, 1,  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  # KCl
  0,  0, 0, 0,  1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # CaSO4·2H2O
  0,  0, 0, 0,  0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,  # CuSO4·5H2O
  0,  2, 0, 0,  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # (NH4)2SO4
  0,  0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,  # H3BO3
  0,  0, 0, 0,  0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,  # MnSO4·H2O
  0,  0, 0, 0,  0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0,  # Na2MoO4·2H2O
  0,  0, 0, 0,  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  # ZnSO4·7H2O
  0,  1, 1, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # NH4H2PO4
  2,  0, 0, 0,  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # Ca(NO3)2·4H2O
  2,  0, 0, 0,  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # Mg(NO3)2·6H2O
  0,  0, 0, 0,  0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,  # C10H12N2O8FeNa·3H2O
  0,  0, 0, 0,  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,  # ZnSO4·6H2O
  0,  0, 0, 0,  0, 0, 0, 2, 0, 0, 0, 0, 4, 0, 0, 0,  # Na2B4O7·10H2O
  0,  0, 0, 0,  0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,  # C18H16N2O6FeNa
  0,  0, 0, 2,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  # K2SiO3
  0,  0, 0, 0,  0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,  # NaCl
  0,  2, 1, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # (NH4)2HPO4
  0,  0, 0, 0,  0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0,  # CuCl2
  0,  6, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0,  # ((NH4)6)Mo7O24·4H2O
  0,  0, 0, 0,  0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0,  # ZnCl2
  0,  0, 0, 0,  0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0,  # MnCl2·4H2O
  0,  1, 0, 0,  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  # NH4Cl
  1,  0, 0, 0,  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  # NaNO3
  11, 1, 0, 0,  5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # 5Ca(NO3)2·NH4NO3·10H2O
  1,  0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # HNO3
  0,  0, 0, 1,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # KOH
  0,  0, 0, 0,  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # H2SO4
  0,  0, 0, 0,  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  # HCL
  0,  0, 1, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # H3PO4
  0,  0, 0, 0,  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  # NaOH
  0,  1, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  # NH4OH
  0,  0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0   # Ca(OH)2




),
nrow = 40,
byrow = TRUE,
dimnames = list(
  c("Ca(NO3)2", "NH4NO3", "KH2PO4", "KNO3", "K2SO4", "MgSO4·7H2O", "CaCl2·2H2O", "KCl",
    "CaSO4·2H2O", "CuSO4·5H2O", "(NH4)2SO4", "H3BO3", "MnSO4·H2O", "Na2MoO4·2H2O",
    "ZnSO4·7H2O", "NH4H2PO4", "Ca(NO3)2·4H2O", "Mg(NO3)2·6H2O", "C10H12N2O8FeNa·3H2O",
    "ZnSO4·6H2O", "Na2B4O7·10H2O", "C18H16N2O6FeNa","K2SiO3","NaCl","(NH4)2HPO4","CuCl2",
    "((NH4)6)Mo7O24·4H2O","ZnCl2","MnCl2·4H2O","NH4Cl","NaNO3","5Ca(NO3)2·NH4NO3·10H2O",
    "HNO3","KOH","H2SO4", "HCL","H3PO4","NaOH","NH4OH","Ca(OH)2"),
  c("NO3_N", "NH4_N", "P", "K", "Ca", "Mg", "S", "Na", "Cl", "Fe", "Mn", "Zn", "B", "Cu", "Mo", "Si")
))

salt_names <- rownames(nutrient_matrix)

salt_info <- data.frame(
  salt     = salt_names,
  category = NA_character_,
  stringsAsFactors = FALSE
)

salt_info$category[salt_info$salt == "Ca(NO3)2"]               <- "Calcium nitrate"
salt_info$category[salt_info$salt == "NH4NO3"]                 <- "Ammonium nitrate"
salt_info$category[salt_info$salt == "KH2PO4"]                 <- "Monopotassium dihydrogen phosphate"
salt_info$category[salt_info$salt == "KNO3"]                   <- "Potassium nitrate"
salt_info$category[salt_info$salt == "K2SO4"]                  <- "Potassium sulfate"
salt_info$category[salt_info$salt == "MgSO4·7H2O"]             <- "Magnesium sulfate heptahydrate"
salt_info$category[salt_info$salt == "CaCl2·2H2O"]             <- "Calcium chloride dihydrate"
salt_info$category[salt_info$salt == "KCl"]                    <- "Potassium chloride"
salt_info$category[salt_info$salt == "CaSO4·2H2O"]             <- "Calcium sulfate dihydrate"
salt_info$category[salt_info$salt == "CuSO4·5H2O"]             <- "Copper(II) sulfate pentahydrate"
salt_info$category[salt_info$salt == "(NH4)2SO4"]              <- "Ammonium sulfate"
salt_info$category[salt_info$salt == "H3BO3"]                  <- "Boric acid"
salt_info$category[salt_info$salt == "MnSO4·H2O"]              <- "Manganese(II) sulfate monohydrate"
salt_info$category[salt_info$salt == "Na2MoO4·2H2O"]           <- "Sodium molybdate dihydrate"
salt_info$category[salt_info$salt == "ZnSO4·7H2O"]             <- "Zinc sulfate heptahydrate"
salt_info$category[salt_info$salt == "NH4H2PO4"]               <- "Ammonium dihydrogen phosphate"
salt_info$category[salt_info$salt == "Ca(NO3)2·4H2O"]          <- "Calcium nitrate tetrahydrate"
salt_info$category[salt_info$salt == "Mg(NO3)2·6H2O"]          <- "Magnesium nitrate hexahydrate"
salt_info$category[salt_info$salt == "C10H12N2O8FeNa·3H2O"]    <- "Iron(III) sodium EDTA trihydrate"
salt_info$category[salt_info$salt == "ZnSO4·6H2O"]             <- "Zinc sulfate hexahydrate"
salt_info$category[salt_info$salt == "Na2B4O7·10H2O"]          <- "Sodium tetraborate decahydrate (borax)"
salt_info$category[salt_info$salt == "C18H16N2O6FeNa"]         <- "Iron(III) sodium EDDHA"
salt_info$category[salt_info$salt == "K2SiO3"]                 <- "Potassium silicate"
salt_info$category[salt_info$salt == "NaCl"]                   <- "Sodium chloride"
salt_info$category[salt_info$salt == "(NH4)2HPO4"]             <- "Diammonium hydrogen phosphate"
salt_info$category[salt_info$salt == "CuCl2"]                  <- "Copper(II) chloride"
salt_info$category[salt_info$salt == "((NH4)6)Mo7O24·4H2O"]    <- "Ammonium heptamolybdate tetrahydrate"
salt_info$category[salt_info$salt == "ZnCl2"]                  <- "Zinc chloride"
salt_info$category[salt_info$salt == "MnCl2·4H2O"]             <- "Manganese(II) chloride tetrahydrate"
salt_info$category[salt_info$salt == "NH4Cl"]                  <- "Ammonium chloride"
salt_info$category[salt_info$salt == "NaNO3"]                  <- "Sodium nitrate"
salt_info$category[salt_info$salt == "5Ca(NO3)2·NH4NO3·10H2O"] <- "Calcium nitrate–ammonium nitrate double salt decahydrate (5:1)"
salt_info$category[salt_info$salt == "HNO3"]                   <- "Nitric acid"
salt_info$category[salt_info$salt == "KOH"]                    <- "Potassium hydroxide"
salt_info$category[salt_info$salt == "H2SO4"]                  <- "Sulfuric acid"
salt_info$category[salt_info$salt == "HCL"]                    <- "Hydrochloric acid"
salt_info$category[salt_info$salt == "H3PO4"]                  <- "Phosphoric acid"
salt_info$category[salt_info$salt == "NaOH"]                   <- "Sodium hydroxide"
salt_info$category[salt_info$salt == "NH4OH"]                  <- "Ammonium hydroxide"
salt_info$category[salt_info$salt == "Ca(OH)2"]                <- "Calcium hydroxide"


acid_base_compounds <- c("HNO3", "H2SO4", "KOH", "HCL", "H3PO4", "NaOH", "NH4OH", "Ca(OH)2")

salt_info$is_acid_base <- salt_info$salt %in% acid_base_compounds

