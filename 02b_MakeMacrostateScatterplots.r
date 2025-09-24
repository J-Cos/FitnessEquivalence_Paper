#Make EEOS Plots

library(ggplot2)
library(cowplot)
library(tidyverse)
library(ggpubr)

pairwise_lower_triangle <- function(data, vars) {
  # ---- Input checks ----
  if (length(vars) != 4) {
    stop("Please supply exactly four variable names (as strings).")
  }
  if (!all(vars %in% names(data))) {
    missing_vars <- vars[!(vars %in% names(data))]
    stop("The following variables are not in your data frame: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # ---- Helper to create a “blank” panel ----
  blank_panel <- function() {
    ggplot() + theme_void()
  }
  
  # ---- Build a list of 4×4 = 16 plots, row‐major order ----
  # For row = 1..4, col = 1..4:
  #   if row > col → create scatter (x = vars[col], y = vars[row])
  #   else          → blank panel
  plot_list <- vector("list", length = 16)
  idx <- 1
  for (i in seq_along(vars)) {      # i = row index (1..4)
    for (j in seq_along(vars)) {    # j = col index (1..4)
      if (i > j) {
        # Lower‐triangle: scatter y = vars[i] vs x = vars[j]
        plot_list[[idx]] <-
          ggplot(data, aes_string(x = vars[j], y = vars[i], color="Region")) +
          geom_point(alpha = 0.6) +
          geom_smooth(method="lm")+
          theme_minimal(base_size = 10) +
          labs(
            x = vars[j],
            y = vars[i]
          )+
          theme(legend.position = "none")+
          ggsci::scale_color_npg()

      } else {
        # Diagonal or upper‐triangle: leave blank
        plot_list[[idx]] <- blank_panel()
      }
      idx <- idx + 1
    }
  }

  # ----  Extract the legend from a “dummy” plot ----
  # Use the first lower‐triangle combination (vars[2] vs vars[1]) 
  # to build a small ggplot that has color = color_var. 
  # That way we guarantee there's a legend to pull.
  dummy_plot <- ggplot(data, 
                       aes_string(x = vars[1], 
                                  y = vars[2], 
                                  color = "Region")) +
    geom_point()+
          ggsci::scale_color_npg()

  # Extract the legend GROG
  the_legend <- cowplot::get_legend(dummy_plot)

  # Wrap the legend grob into a ggplot‐compatible object
  legend_panel <- cowplot::ggdraw(the_legend)

  plot_list<-c(plot_list[c(5:7, 9:10)], list(legend_panel), plot_list[c(13:15)])



  # ---- Combine into a 4×4 grid ----
  combined <- plot_grid(
    plotlist = plot_list,
    ncol     = 3,
    align    = "hv"
  )
  
  return(combined)
}


d<-read.csv("Outputs/EEOSPredsHarteWD.csv") %>% as_tibble

pairwise_lower_triangle(d, c("N", "S", "B", "E"))
ggsave("Figures/FigureS1.png", bg="white", height=8, width=8)