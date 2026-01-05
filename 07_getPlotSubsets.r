# --------------------------------------------
# Purpose: Gets plot subsets for random and ordered non overlapping
# Input Origin Scripts: 01_CombinePinho&JuaraData.R
# Output: RDS of plot subsets
# ---------------------------------------------------

library(tidyverse)

sampleSize<-29


#random
plotSubsets_l<-list()
    for (i in 1:100){
        set.seed(i)
        plotSubsets_l[[i]]<-read.csv("Outputs/comm.csv") %>% as_tibble %>%
            group_by(Region, Region_Plot) %>%
            summarise(value=n()) %>%
            group_by(Region) %>% 
            #sample_n(17) %>%
            sample_n(sampleSize)%>%
            ungroup() %>%
            pull(Region_Plot)
    }
saveRDS(plotSubsets_l, "Outputs/PlotSubsets.RDS")


# -------------------------------------------
#ordered non overlapping
# ----------------------------------------------
getNonOverlappingPlotSubsamples<-function(orderedPlots) {
    grouping_factor <- ceiling(seq_along(orderedPlots$Region_Plot) / nonOverlappingSampleSize)

    # Split the vector based on the grouping factor
    split_vector <- split(orderedPlots$Region_Plot, grouping_factor)

    #remove groups less than desired smaple size
    split_vector_samesizes<-split_vector[lapply(split_vector, length)==nonOverlappingSampleSize]
    return(split_vector_samesizes)
}

nonOverlappingSampleSize<-5

NonoverlappingOrderedPlotSubsets_l<-list()
for (region in c("Balbina", "PGM", "STM")){
    orderedPlots<-read.csv("Data/env.csv") %>% as_tibble %>%
        filter(Region_Plot %in% read.csv("Outputs/comm.csv")$Region_Plot)  %>%
        filter(Region==region) %>%
        arrange(fc2000)

    NonoverlappingOrderedPlotSubsets_l[[region]]<-getNonOverlappingPlotSubsamples(orderedPlots)
}


orderedPlots<-read.csv("Outputs/comm.csv") %>% as_tibble %>%
                filter(Region=="Jurua") %>%
                group_by(Region, Region_Plot) %>%
                summarise(value=n())

NonoverlappingOrderedPlotSubsets_l[["Jurua"]]<-getNonOverlappingPlotSubsamples(orderedPlots)

saveRDS(
    unlist(NonoverlappingOrderedPlotSubsets_l, recursive=FALSE), 
    "Outputs/NonOverlappingOrderedPlotSubsets.RDS")



