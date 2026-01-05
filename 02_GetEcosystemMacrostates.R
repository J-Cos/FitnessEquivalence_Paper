# --------------------------------------------
# Purpose: Calculates ecosystem macrostates (B, S, N E) 
# Input Origin Scripts: 01_CombinePinho&JuaraData.R
# Output: csv of ecosystem macrostates
# ---------------------------------------------------

library(tidyverse)

comm<-read.csv("Outputs/comm.csv") %>% as_tibble
traits<-read.csv("Outputs/traits.csv") %>% as_tibble 


# get macrostates, following Harte, making use of wood density trait

OrganismStatsHarteWD<-comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD)) %>%
    mutate(minDBH=min(DBH)) %>%
    mutate(e=   DBH/minDBH ) %>%    #
    mutate(meanWD=mean(WD)) %>%
    mutate(scaledWD=scale(WD, scale=FALSE, center=(mean(WD)-1))) %>% 
    mutate(m=(e^(4/3)*scaledWD)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)

MacrostatesHarteWD<-OrganismStatsHarteWD %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m),
        E=sum(e)
    ) %>%
    ungroup

write.csv(MacrostatesHarteWD, "Outputs/MacrostatesHarteWD.csv")