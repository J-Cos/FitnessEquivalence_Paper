# --------------------------------------------
# Sript fits CATS regressions per plot (poisson only, negbinom only exploratory see Warton et al., 2014)
# Outputs a csv with these records
# ---------------------------------------------------



library(tidyverse)

comm<-read.csv("Outputs/comm.csv") %>% as_tibble
traits<-read.csv("Outputs/traits.csv") %>% as_tibble 

OrganismStats<-comm %>%
    group_by(Region, Region_Plot, Binomial_correct) %>%
    summarise(n=n()) %>%
    left_join(select(traits, -DS))

CATS_df<-OrganismStats %>% summarise() %>% ungroup
CATS_df$CATS_pois<-NA
CATS_df$CATS_nb <-NA

for (i in 1:length(CATS_df$Region_Plot)) {
    plotDat<-OrganismStats %>%
        filter(Region_Plot==CATS_df$Region_Plot[i]) %>%
        mutate(rel=n/sum(n))
    
    metaStats<-OrganismStats %>%
        filter(Region == plotDat$Region[1]) %>%
        #filter(Binomial_correct %in% unique(plotDat$Binomial_correct)) %>%
        group_by(Binomial_correct)%>%
        summarise(n=n()) %>%
        mutate(meta=n/sum(n)) %>%
        select(-n)
    

    fittingDat<-left_join( metaStats, plotDat)%>%
        select(Binomial_correct, meta, rel) %>%
        left_join(select(traits, -DS)) %>%
        mutate(rel=replace_na(rel, 0))


        #formula<-paste0("n ~ ", trait)
        #res<-coef(lm(formula, plotDat))[2]
        #res<-cor(plotDat$n ,plotDat[trait], use="complete.obs")

        my_glm_model<-(glm(rel~ WD+ lnSM+ LMA+ Hmax +offset(log(meta)), family="poisson", data=fittingDat))

        #plot(density(fittingDat$rel))
        my_glmnb_model<-try(MASS::glm.nb(rel~ WD+ lnSM+ LMA+ Hmax +offset(log(meta)), data=fittingDat, start = coef(my_glm_model)))

        deviance_r_squared <- 1 - (my_glm_model$deviance / my_glm_model$null.deviance)
        deviance_r_squared_nb <- try(1 - (my_glmnb_model$deviance / my_glmnb_model$null.deviance))

        CATS_df$CATS_pois[i]<-deviance_r_squared
        CATS_df$CATS_nb[i]<-deviance_r_squared_nb

}


CATS_df<-CATS_df %>%
    mutate(CATS_nb=as.numeric(CATS_nb))

write.csv(CATS_df, "Outputs/CatsOutput.csv")


CATS_df %>%
    ggplot(aes(x=CATS_pois, y=CATS_nb))+
        geom_point(aes( color=Region))+
        geom_smooth()+
        geom_smooth(method="lm")

cor.test(CATS_df$CATS_pois, CATS_df$CATS_nb)