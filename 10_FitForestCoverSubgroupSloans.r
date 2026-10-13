# --------------------------------------------
# Purpose: Fits sloan models to forest cover subgroups (in this case poisson fits best)
# Input Origin Scripts: 01_CombinePinho&JuaraData.R; 07_getPlotSubsets.r
# Output: sloan fits for each forest cover subgroup (csv and visualisations)
# ---------------------------------------------------

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
suffix <- if (length(args) > 0) args[1] else "harte_wd"

# 1) load data
orderedPlotSubsets_l<-readRDS("Outputs/NonOverlappingOrderedPlotSubsets.RDS")

# 2) fit sloan models
    df<-data.frame(seed=NA, region=NA, r2=NA, dAIC=NA, m=NA)
    df_taxa<-data.frame(sp=NA, Neutrality=NA, region=NA)
    row<-1
    #for (i in 1:length(orderedPlotSubsets_l)){
        source("Code/Functions/sncm.fit().R")
        comm<-read.csv("Outputs/comm.csv") %>% as_tibble %>%
            group_by(Region, Region_Plot, Binomial_correct) %>%
            summarise(value=n()) %>%
            ungroup %>%
            pivot_wider(names_from=Binomial_correct, values_from=value, values_fill=0)  %>%
            filter(Region %in% c("Balbina", "PGM", "STM", "Jurua"))

        comm_l<-lapply(orderedPlotSubsets_l, function(item){filter(comm, Region_Plot %in% item) %>% select(-Region, -Region_Plot) %>% as.matrix})
        names(comm_l)<-names(orderedPlotSubsets_l)

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

            df[row, "seed"]<-region
            df[row, "region"]<-substr(region,1, 3)
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

        #print(i)
    #}

    # 2.2) make visualisations
        # binomial model fit examples
        cowplot::plot_grid(plotlist=plot_l)
        ggsave("Figures/Figure_sloansubsets_supplement.png",  height=20, width=20)

    df %>% select(dAIC_v_binom, dAIC_v_pois) %>% summary


    df %>%
        as_tibble %>% 
        group_by(region) %>%
        select(-seed) %>%
        summarise_all(median)

df$fcmedian<-NULL
df$devmedian<-NULL
df$RMSE<-NULL
df$R2<-NULL

for (i in 1:nrow(df)) {
    df[i, "fcmedian"]<-read.csv("Data/env.csv") %>% as_tibble %>%
        filter(Region_Plot %in% orderedPlotSubsets_l[[i]]) %>%
        summarise(fcmedian=median(fc2000))
    df[i, "devmedian"]<-read.csv(paste0("Outputs/EEOSPredsHarteWD_", suffix, ".csv")) %>% as_tibble %>%
        filter(Region_Plot %in% orderedPlotSubsets_l[[i]]) %>%
        summarise(devmedian=median(dev))
    df[i, "RMSE"]<-read.csv(paste0("Outputs/EEOSPredsHarteWD_", suffix, ".csv")) %>% as_tibble %>%
        filter(Region_Plot %in% orderedPlotSubsets_l[[i]]) %>%
        mutate(error_sq=dev^2) %>%
        summarise(RMSE=sqrt(mean(error_sq)))
    df[i, "R2"]<-read.csv(paste0("Outputs/EEOSPredsHarteWD_", suffix, ".csv")) %>% as_tibble %>%
        filter(Region_Plot %in% orderedPlotSubsets_l[[i]]) %>%
        mutate(error=(B_pred-B)) %>%
        mutate(meanobs=mean(B)) %>%
        mutate(totalsquares=(B-meanobs)^2) %>%
        mutate(residualsquares=(error)^2) %>% 
        summarise(R2=1-(sum(residualsquares)/sum(totalsquares)))
}

df[is.na(df$fcmedian),]$fcmedian<-1

        # difference in model fit for all random subsamples
        df %>%
            group_by(region) %>% #print(n=999)
            mutate(counter=row_number()) %>% #print(n=999)
            #mutate(Biome=case_when(region %in% c("Balbina", "PGM", "STM")~ "Amazonia", .default="Atlantic")) %>%
            #mutate(r2best= case_when(region %in% c("PB", "SGD") ~ r2, .default = r2binom)) %>%
            ggplot(aes(x=fcmedian, y=r2pois, color=region))+
                geom_point()+
                geom_smooth(method="lm")+
                #facet_wrap(~Biome, ncol=1, scales="free")+
                theme_classic()+
                theme(
                    axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")),
                    axis.title.x = element_text(angle = 0)) +
                ggsci::scale_color_npg()


write.csv(df, paste0("Outputs/ForestCoverSubgroupSloanFits_", suffix, ".csv"), row.names = FALSE)
if (suffix == "harte_wd") {
    write.csv(df, "Outputs/ForestCoverSubgroupSloanFits.csv", row.names = FALSE)
}


