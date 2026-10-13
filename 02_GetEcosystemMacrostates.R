# --------------------------------------------
# Purpose: Calculates ecosystem macrostates (B, S, N E) 
# Input Origin Scripts: 01_CombinePinho&JuaraData.R
# Output: csv of ecosystem macrostates
# ---------------------------------------------------

library(tidyverse)

comm<-read.csv("Outputs/comm.csv") %>% as_tibble
traits<-read.csv("Outputs/traits.csv") %>% as_tibble 


# get macrostates, following Harte, making use of wood density trait

# Option 4: Quadratic + WD (Metabolism-First)
OrganismStats_quad_wd <- comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH/minDBH)^2) %>%
    mutate(m = e^(4/3) * (WD / min(WD, na.rm = TRUE))) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)

Macrostates_quad_wd <- OrganismStats_quad_wd %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m, na.rm=TRUE),
        E=sum(e, na.rm=TRUE),
        .groups = "drop"
    )

write.csv(Macrostates_quad_wd, "Outputs/MacrostatesHarteWD_quad_wd.csv", row.names = FALSE)

# Option 3: Quadratic, No WD
OrganismStats_quad_nowd <- comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH/minDBH)^2) %>%
    mutate(m = e^(4/3)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)

Macrostates_quad_nowd <- OrganismStats_quad_nowd %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m, na.rm=TRUE),
        E=sum(e, na.rm=TRUE),
        .groups = "drop"
    )

write.csv(Macrostates_quad_nowd, "Outputs/MacrostatesHarteWD_quad_nowd.csv", row.names = FALSE)

# Option 6: Volume-Corrected Chave Hybrid
OrganismStats_chave <- comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(D = DBH / minDBH) %>%
    mutate(m = ((WD / min(WD, na.rm = TRUE))^0.976) * D^2.673 * exp(-0.0299 * (log(D))^2)) %>%
    mutate(e = D^(2.673 * 0.75) * exp(-0.0299 * 0.75 * (log(D))^2)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)

Macrostates_chave <- OrganismStats_chave %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m, na.rm=TRUE),
        E=sum(e, na.rm=TRUE),
        .groups = "drop"
    )

write.csv(Macrostates_chave, "Outputs/MacrostatesHarteWD_chave.csv", row.names = FALSE)