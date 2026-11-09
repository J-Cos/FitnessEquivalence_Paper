test_that("organism stats respect METE lower bound (e >= 1), basal-area scaling, and reference tree b_size = 1", {
  org <- compute_organism_stats(toy_comm, toy_traits, toy_env)
  expect_equal(nrow(org), nrow(toy_comm))
  expect_equal(org$e, (toy_comm$DBH / DBH_MIN_CM)^2, tolerance = 1e-12)
  expect_equal(min(org$e), 1, tolerance = 1e-12)
  expect_true(all(org$b > 0 & org$b_rho > 0 & org$b_size > 0))
  # Stems at DBH_min (10 cm) have b_size == 1 identically
  at_min <- org$DBH == DBH_MIN_CM
  expect_equal(org$b_size[at_min], rep(1, sum(at_min)), tolerance = 1e-12)
})

test_that("interleaved plot rows are immune to BIOMASS::computeE duplicate-coordinate row scrambling", {
  perm <- c(1, 4, 7, 2, 5, 8, 3, 6)   # interleaves J1, P1, S1
  org_grouped     <- compute_organism_stats(toy_comm, toy_traits, toy_env)
  org_interleaved <- compute_organism_stats(toy_comm[perm, ], toy_traits, toy_env)
  expect_equal(org_interleaved$agb_Mg, org_grouped$agb_Mg[perm], tolerance = 1e-14)
  expect_equal(org_interleaved$b,      org_grouped$b[perm],      tolerance = 1e-14)
  expect_equal(org_interleaved$b_rho,  org_grouped$b_rho[perm],  tolerance = 1e-14)
  expect_equal(org_interleaved$b_size, org_grouped$b_size[perm], tolerance = 1e-14)
  # Cross-check single stem against single-point vector call computeAGB(D, WD, coord = c(lon, lat))
  j1_single <- BIOMASS::computeAGB(D = 20, WD = 0.7, coord = c(JURUA_CENTROID$lon, JURUA_CENTROID$lat))
  expect_equal(org_grouped$agb_Mg[2], j1_single, tolerance = 1e-14)
})

test_that("comm row-index column X does not collide with env longitude column X", {
  comm_with_x <- dplyr::mutate(toy_comm, X = dplyr::row_number(), .before = 1)
  org1 <- compute_organism_stats(toy_comm, toy_traits, toy_env)
  org2 <- compute_organism_stats(comm_with_x, toy_traits, toy_env)
  expect_equal(org1$b, org2$b, tolerance = 1e-14)
})

test_that("Jurua plots missing from env receive JURUA_CENTROID, while missing non-Jurua coords fail loudly", {
  coords <- prepare_plot_coords(toy_comm, toy_env)
  j1 <- dplyr::filter(coords, Region_Plot == "J1")
  expect_equal(j1$lon, JURUA_CENTROID$lon)
  expect_equal(j1$lat, JURUA_CENTROID$lat)
  expect_error(prepare_plot_coords(toy_comm, toy_env[-1, ]), "Missing or non-finite coordinates")
})

test_that("stems below dbh_min or with missing/non-positive WD fail loudly", {
  bad_dbh <- toy_comm; bad_dbh$DBH[1] <- 9.5
  expect_error(compute_organism_stats(bad_dbh, toy_traits, toy_env), "DBH >= dbh_min")
  expect_error(compute_organism_stats(toy_comm, toy_traits[-1, ], toy_env), "Wood density")
  bad_wd <- toy_traits; bad_wd$WD[1] <- 0
  expect_error(compute_organism_stats(toy_comm, bad_wd, toy_env), "Wood density")
})

test_that("plot macrostates and 3-run multiplicative decomposition multiply out exactly", {
  org <- compute_organism_stats(toy_comm, toy_traits, toy_env)
  ms <- summarise_macrostates(org)
  expect_equal(nrow(ms), 3)
  expect_equal(sum(ms$N), nrow(toy_comm))
  j1 <- dplyr::filter(ms, Region_Plot == "J1")
  expect_equal(j1$N, 3)
  expect_equal(j1$S, 2)
  expect_equal(j1$E, sum((c(10, 20, 35) / 10)^2), tolerance = 1e-12)
  expect_equal(ms$AGB_Mg, ms$B * ms$AGB_ref_Mg, tolerance = 1e-12)
  expect_equal(ms$Q_size * ms$W_H * ms$W_rho, ms$Q_obs, tolerance = 1e-14)
})

test_that("AGBmonteCarlo biomass CV propagates WD + height + allometric error deterministically", {
  sp_map <- c(a = "Eschweilera coriacea", b = "Protium hebetatum",
              c = "Cecropia sciadophylla", d = "Bertholletia excelsa")
  mc_comm   <- dplyr::mutate(toy_comm, Binomial_correct = unname(sp_map[Binomial_correct]))
  mc_traits <- dplyr::mutate(toy_traits, Binomial_correct = unname(sp_map[Binomial_correct]))
  cv1 <- compute_biomass_mc_cv(mc_comm, mc_traits, toy_env, n_sim = 100, seed = 42)
  cv2 <- compute_biomass_mc_cv(mc_comm, mc_traits, toy_env, n_sim = 100, seed = 42)
  expect_equal(nrow(cv1), 3)
  expect_false(anyNA(cv1$B_cv))
  expect_true(all(cv1$B_cv > 0))
  expect_equal(cv1$B_cv, cv1$AGB_mc_sd_Mg / cv1$AGB_mc_mean_Mg, tolerance = 1e-14)
  expect_equal(cv1$B_cv, cv2$B_cv, tolerance = 1e-14)
})
