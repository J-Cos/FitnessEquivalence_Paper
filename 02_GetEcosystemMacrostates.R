#------------------------------------
# Script calculates ecosystem macrostates (B, S, N E) in various alternatives ways
# each is output as csv, MacrostatesHarteWD is taken forward for back comparability and incorporation of troopical wood density consideration
#----------------------------------

library(tidyverse)

comm<-read.csv("Outputs/comm.csv") %>% as_tibble
traits<-read.csv("Outputs/traits.csv") %>% as_tibble 


# get macrostates making use of wood density trait
OrganismStats<-comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD)) %>%
    mutate(relativeMass=   (pi*(DBH/2)^2 / 4) * WD ) %>%    # pirsquared /4 for taper - assumes height dbh ratio is constant across trees
    mutate(minRelMass=min(relativeMass)) %>%
    mutate(m=relativeMass/minRelMass) %>%
    mutate(e=m^(3/4)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e)

Macrostates<-OrganismStats %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m),
        E=sum(e)
    ) %>%
    ungroup

write.csv(Macrostates, "Outputs/Macrostates.csv")


# get macrostates as harte et al.
OrganismStatsHarte<-comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD)) %>%
    mutate(minDBH=min(DBH)) %>%
    mutate(e=   DBH/minDBH ) %>%    #
    mutate(m=e^(4/3)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e)

MacrostatesHarte<-OrganismStatsHarte %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m),
        E=sum(e)
    ) %>%
    ungroup

write.csv(MacrostatesHarte, "Outputs/MacrostatesHarte.csv")

# get macrostates as harte et al. with WD
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


# get macrostates as harte et al. with WD and LMA
OrganismStatsHarteWDLMA<-comm %>%
    left_join(select(traits, Binomial_correct, Hmax, WD, LMA)) %>%
    mutate(e_unscaled=   DBH*LMA ) %>%    #
    mutate(m_unscaled=DBH^(4/3)*WD) %>%
    mutate(min_e=min(e_unscaled), min_m=min(m_unscaled)) %>%
    mutate(e=e_unscaled/min_e, m=m_unscaled/min_m) %>%
    select(Region, Region_Plot, Binomial_correct, m, e)

MacrostatesHarteWDLMA<-OrganismStatsHarteWDLMA %>%
    group_by(Region, Region_Plot) %>%
    summarise(
        N=n(), 
        S=length(unique(Binomial_correct)),
        B=sum(m),
        E=sum(e)
    ) %>%
    ungroup

write.csv(MacrostatesHarteWDLMA, "Outputs/MacrostatesHarteWDLMA.csv")



