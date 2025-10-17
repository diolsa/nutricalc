water_matrix <- matrix(c(
  0.06, # N-NO3 umrechnen
  0,    # NH4
  0.2,  # P in mg l
  0.15, # K
  3.15, # Ca
  0.56, # Mg
  0.395, # S  1.58 SO4  1.58*compute_molar_mass("S")/compute_molar_mass("S04")
  1.68, # Na
  0,    # Cl
  0,    # Fe
  0,    # Mn
  0,    # Zn
  0,    # B
  0,    # Cu
  0,    # Mo
  0     # Si
  # 10997

),
nrow = 1,
byrow = TRUE,
dimnames = list(
  c("10997"),
  c("NO3_N", "NH4_N", "P", "K", "Ca", "Mg", "S", "Na", "Cl", "Fe", "Mn", "Zn", "B", "Cu", "Mo", "Si")
))


1.58*compute_molar_mass("S")/compute_molar_mass("S04")
