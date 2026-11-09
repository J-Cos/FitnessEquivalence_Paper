test_that("extract_legend and make_region_legend return grobs without creating Rplots.pdf", {
  tmp_dir <- tempfile("plot_test_")
  dir.create(tmp_dir)
  old_wd <- setwd(tmp_dir)
  on.exit({
    setwd(old_wd)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  df <- tibble(
    Region = factor(REGIONS, levels = REGIONS),
    x      = c(0.1, 0.3, 0.6, 0.8),
    y      = c(0.05, -0.02, -0.10, -0.18)
  )
  leg <- make_region_legend(df, position = "bottom")
  expect_false(is.null(leg))
  expect_s3_class(leg, "grob")

  p_bar <- plot_proportion_mediated(list(prop_mediated = 0.75))
  expect_s3_class(p_bar, "ggplot")

  expect_false(file.exists(file.path(tmp_dir, "Rplots.pdf")))
})
