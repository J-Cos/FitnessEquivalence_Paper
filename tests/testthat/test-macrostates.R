test_that("every scaling respects METE's bound (min(e) = 1) and gives positive masses", {
  for (s in SCALING_SUFFIXES) {
    org <- compute_organism_stats(toy_comm, toy_traits, s)
    expect_equal(min(org$e), 1, tolerance = 1e-12, info = s)
    expect_true(all(org$m > 0), info = s)
    expect_equal(nrow(org), nrow(toy_comm), info = s)
  }
})

test_that("biomass-first option (mte_wd): e proportional to (D^(8/3) * centred WD)^(3/4), rescaled to min 1, m = e^(4/3)", {
  wd_c <- center_wd(left_join(toy_comm, toy_traits, by = "Binomial_correct")$WD)
  org <- compute_organism_stats(toy_comm, toy_traits, "mte_wd")
  raw <- ((toy_comm$DBH / 10)^(8/3) * wd_c)^(3/4)
  expect_equal(org$e, raw / min(raw))
  expect_equal(org$m, org$e^(4/3))
})

test_that("a scaling that breaks METE's lower bound is rejected", {
  bad <- SCALINGS; SCALINGS$bad <<- function(d) dplyr::mutate(d, e = DBH / max(DBH), m = e)
  on.exit(SCALINGS <<- bad)
  expect_error(compute_organism_stats(toy_comm, toy_traits, "bad"), "violates METE's bound")
})

test_that("centred wood density has stem-weighted mean exactly 1 and rejects NA", {
  wd <- left_join(toy_comm, toy_traits, by = "Binomial_correct")$WD
  expect_equal(mean(center_wd(wd)), 1)
  expect_error(center_wd(c(0.5, NA)), "NA")
})

test_that("no-WD options follow m = e^(4/3); WD options scale by centred WD", {
  wd_c <- center_wd(left_join(toy_comm, toy_traits, by = "Binomial_correct")$WD)
  for (s in c("harte_nowd", "quad_nowd")) {
    org <- compute_organism_stats(toy_comm, toy_traits, s)
    expect_equal(org$m, org$e^(4/3), info = s)
  }
  for (s in c("harte_wd", "quad_wd")) {
    org <- compute_organism_stats(toy_comm, toy_traits, s)
    expect_equal(org$m, org$e^(4/3) * wd_c, info = s)
  }
  lin <- compute_organism_stats(toy_comm, toy_traits, "harte_nowd")
  quad <- compute_organism_stats(toy_comm, toy_traits, "quad_nowd")
  expect_equal(quad$e, lin$e^2)
})

test_that("Chave option uses DBH in cm: m / e^(4/3) equals centred WD^0.976", {
  wd_c <- center_wd(left_join(toy_comm, toy_traits, by = "Binomial_correct")$WD)
  org <- compute_organism_stats(toy_comm, toy_traits, "chave")
  expect_equal(org$m / org$e^(4/3), wd_c^0.976, tolerance = 1e-12)
  # mass ratio between a 100 cm and a 10 cm stem follows Chave et al. (2014) eq. 7 in cm
  expect_equal(chave_mass(100, 1) / chave_mass(10, 1),
               exp(2.673 * log(10) - 0.0299 * (log(100)^2 - log(10)^2)))
})

test_that("macrostates are correct sums per plot", {
  org <- compute_organism_stats(toy_comm, toy_traits, "harte_nowd")
  ms <- summarise_macrostates(org)
  j1 <- filter(ms, Region_Plot == "J1")
  expect_equal(j1$N, 3); expect_equal(j1$S, 2)
  expect_equal(j1$E, (10 + 20 + 35) / 10)
  expect_equal(j1$B, sum(c(1, 2, 3.5)^(4/3)))
  expect_equal(sum(ms$N), nrow(toy_comm))
})

test_that("unknown scaling and missing traits fail loudly", {
  expect_error(compute_organism_stats(toy_comm, toy_traits, "nope"), "Unknown scaling")
  expect_error(compute_organism_stats(toy_comm, toy_traits[-1, ], "harte_wd"), "NA")
})
