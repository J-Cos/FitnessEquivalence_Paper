# --------------------------------------------
# Sript fits overall sloan models (Poisson random sampling fits best) and extracts non neutral abundance
# Output is 2 figures (example sloan model fits and violins of the sloan average fits); and a csv of the nonneutral abundances
# ---------------------------------------------------

library(tidyverse)
source("Code/Functions/runPermutationTest().r")

plotSubsets_l<-readRDS("Outputs/PlotSubsets.RDS")
sampleSize=29#37 - 30 is the number expected from Juara, 37 is the number in Balbina
    # 2.1) get 100 replicate neutral model fits
    df<-data.frame(seed=NA, region=NA, r2=NA, m=NA)
    df_taxa<-data.frame(sp=NA, Neutrality=NA, region=NA)
    row<-1
    for (i in 1:100){
        set.seed(i)
        source("Code/Functions/sncm.fit().R")
        comm<-read.csv("Outputs/comm.csv") %>% as_tibble %>%
            group_by(Region, Region_Plot, Binomial_correct) %>%
            summarise(value=n()) %>%
            ungroup %>%
            pivot_wider(names_from=Binomial_correct, values_from=value, values_fill=0)  %>%
            filter(Region %in% c("Balbina", "PGM", "STM", "Jurua")) %>%
            filter(Region_Plot %in% plotSubsets_l[[i]])

        regions<-read.csv("Outputs/comm.csv") %>% as_tibble  %>% 
            filter(Region %in% c("Balbina", "PGM", "STM", "Jurua")) %>% 
            group_by(Region) %>% 
            summarise()
        comm_l<-lapply(regions$Region, function(item){filter(comm, Region==item) %>% select(-Region, -Region_Plot) %>% as.matrix})
        names(comm_l)<-regions$Region

        sloan_l<-lapply(comm_l, sncm.fit)


        plot_l<-dat_l<-list()
        for (region in names(sloan_l)){
            test<-sloan_l[[region]]
            dat<-test[["vals"]]
            dat$sp<-rownames(test[["vals"]])
            dat_l[[region]]<-dat %>%
                    as_tibble %>%
                    mutate(Neutrality=case_when(freq<pois.lwr~"LFHA",
                            freq>pois.upr~"HFLA",
                            freq>=pois.lwr & freq<=pois.upr~"Neutral")) %>% 
                    select(sp, Neutrality) %>% 
                    arrange(sp)
            
            df_taxa<-df_taxa %>% rbind(., cbind(dat_l[[region]], region))

            df[row, "seed"]<-i
            df[row, "region"]<-region
            df[row, "r2"]<-test[["stats"]]$Rsqr
            df[row, "r2binom"]<-test[["stats"]]$Rsqr.bino
            df[row, "r2pois"]<-test[["stats"]]$Rsqr.pois
            df[row, "dAIC_v_binom"]<-test[["stats"]]$AIC-test[["stats"]]$AIC.bino
            df[row, "dAIC_v_pois"]<-test[["stats"]]$AIC-test[["stats"]]$AIC.pois
            df[row, "m"]<-test[["stats"]]$m
            row<-row+1

                plot_l[[region]]<-test[["vals"]] %>%
                    ggplot(aes(y=freq, x=log(p)))+
                        geom_point(alpha=0.3)+
                        geom_line(aes(y=pois.upr), color="red")+
                        #geom_point(aes(y=freq.pred), color="grey")+
                        geom_line(aes(y=pois.lwr), color="red")+
                        annotate("text", x = -6, y = 0.9, label = paste0(  "R2=",signif(test[["stats"]]$Rsqr.pois,2), "\n"#,
                                                                            #"dAIC=",signif(test[["stats"]]$AIC-(test[["stats"]]$AIC.bino+test[["stats"]]$AIC.pois)/2,2), "\n",
                                                                            #"m=", signif(test[["stats"]]$m,2)
                                                                            ))+
                        #labs(title=paste0(Region), subtitle=paste("n=", length(sample_names(psReg[[sppFraction]][[replicate]]))))+
                        #coord_cartesian(xlim = c(-15.5, -4))+
                        theme_classic()+
                        ggtitle(region)
        }

        print(i)
    }

    # 2.2) make visualisations
        # binomial model fit examples
        cowplot::plot_grid(plotlist=plot_l)
        ggsave("Figures/Figure_ExampleSLoanFits.png")


    df %>%
        as_tibble %>% 
        group_by(region) %>%
        summarise_all(mean)

    saveRDS(df, "Outputs/SloanOutput.RDS")


    df %>% select(dAIC_v_binom, dAIC_v_pois) %>% summary

        # difference in model fit for all random subsamples
        df %>%
            #mutate(Biome=case_when(region %in% c("Balbina", "PGM", "STM")~ "Amazonia", .default="Atlantic")) %>%
            mutate(order=case_when( region %in% c("PB", "STM")~ 2,
                                    region %in% c("SGD", "PGM")~ 1,
                                    region %in% c("Balbina")~3, 
                                    .default=4)) %>%
            #mutate(r2best= case_when(region %in% c("PB", "SGD") ~ r2, .default = r2binom)) %>%
            ggplot(aes(x=reorder(region, order), y=r2pois, color=region, fill=region))+
                geom_violin( alpha=0.5)+
                #geom_jitter()+
                #facet_wrap(~Biome, ncol=1, scales="free")+
                theme_classic()+
                theme(
                    axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")),
                    axis.title.x = element_text(angle = 0)) +
                ggsci::scale_color_npg()+
                ggsci::scale_fill_npg()+
                labs(x = "More Degraded                                                                   Less Degraded")
            ggsave("Figures/Figure_SloanAvgFit.png", height=8, width=8)

    # test
    df %>%
              #mutate(Biome=case_when(region %in% c("Balbina", "PGM", "STM")~ "Amazonia", .default="Atlantic")) %>%
              mutate(order=case_when( region %in% c("PB", "STM")~ 1,
                                      region %in% c("SGD", "PGM")~ 1,
                                      region %in% c("Balbina")~2, 
                                      .default=3)) %>%
              mutate(dv=r2binom) %>%
              runPermutationTest(., 1000)





    # 2.3 taxonomic stats and plots
        tax_summary_long<-df_taxa %>%
            filter(complete.cases(.)) %>% 
            group_by(region, sp,Neutrality ) %>%
            summarise(n=n()) %>%
            mutate(totaln=sum(n)) %>%
            mutate(proportion=n/totaln) %>%
            ungroup()

        tax_summary<-tax_summary_long %>%
            pivot_wider(names_from="region", values_from=proportion, values_fill=0)  %>%
            mutate(AmazoniaSum = rowSums(across(c(Balbina, PGM, STM, Jurua)))/4) %>%
            #mutate(AtlanticSum = rowSums(across(c(PB, SGD, Una)))/3) %>%
        # pivot_longer(AmazoniaSum:AtlanticSum, names_to="region", values_to="sum") %>%
            left_join(., read.csv("Data/traits.csv"), by = join_by(sp == Binomial_correct))



    tax_summary %>% 
        group_by(sp) %>%
        summarise_all(first) %>%
        summarise(
            WD_mean=mean(WD, na.rm=TRUE), 
            lnSM_mean=mean(lnSM, na.rm=TRUE),
            LMA_mean=mean(LMA, na.rm=TRUE),
            Hmax_mean=mean(Hmax, na.rm=TRUE)
            )
    NonNeutrals<-tax_summary %>%
        filter(Neutrality!="Neutral") %>%
        filter(Balbina==1 | Jurua==1 | STM==1 | PGM==1) %>%
        select(-n, -totaln, -AmazoniaSum)

    NonNeutrals %>%
        mutate(sharedByRegion=Balbina+Jurua+PGM+STM) %>%
        filter(sharedByRegion>1) %>%
        summarise(
            WD_mean=mean(WD, na.rm=TRUE), 
            lnSM_mean=mean(lnSM, na.rm=TRUE),
            LMA_mean=mean(LMA, na.rm=TRUE),
            Hmax_mean=mean(Hmax, na.rm=TRUE)
            )


    NonNeutrals %>% 
        pivot_longer(
            cols = c(Balbina, Jurua, PGM, STM),
            names_to = "RegionNeutral",
            values_to = "NeutralScore") %>%
        filter(NeutralScore==1) %>%
        mutate(WD=scale(WD), lnSM=scale(lnSM), LMA=scale(LMA), Hmax=scale(Hmax)) %>%
        pivot_longer(
            cols = c(WD, lnSM, LMA, Hmax),
            names_to = "TraitName",
            values_to = "TraitScore") %>%
        mutate(region= factor(region, levels = c("Jur", "Bal","STM", "PGM"))) %>%
        ggplot(data=.)+
            geom_boxplot(aes(x=TraitName, y=TraitScore, fill=RegionNeutral))+
            ggsci::scale_fill_npg()+
            geom_hline(yintercept=0, linetype=2)+
            theme_classic()

    ggsave("Figures/Figure_traitsOfNonneutrals.png", height=8, width=8)

    #save a table
    NonNeutrals %>% 
        select(sp, Balbina, Jurua, PGM, STM) %>%
        rename(Binomial=sp) %>%
        write.csv("Outputs/NonNeutralTaxaTable.csv")

    NonNeutral_df<-NonNeutrals %>%
        select(sp, Balbina, Jurua, PGM, STM) %>% 
        rename(Binomial_correct=sp)

comm<-read.csv("Outputs/comm.csv") %>% left_join(NonNeutral_df)



NonNeutralProportions<-comm %>%
    group_by( Region, Region_Plot) %>%
    summarise(
        Jn=sum(Jurua, na.rm=TRUE)/n(),
        Bn=sum(Balbina, na.rm=TRUE)/n(),
        Sn=sum(STM, na.rm=TRUE)/n(),
        Pn=sum(PGM, na.rm=TRUE)/n(),
    ) %>%
    mutate(
        proportionNonNeutral = case_when(
        Region == "Jurua" ~ Jn,
        Region == "Balbina" ~ Bn,
        Region == "STM" ~ Sn,
        Region == "PGM" ~ Pn,
        TRUE ~ NA_real_ # Default case if no condition matches
        )
    ) %>%
    ungroup()%>%
    select(Region_Plot, proportionNonNeutral)

write.csv(NonNeutralProportions, "Outputs/NonNeutralProportions.csv")
