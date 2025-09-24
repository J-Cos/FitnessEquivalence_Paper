library(tidyverse)

sampleSize<-21


#random
plotSubsets_l<-list()
    for (i in 1:100){
        set.seed(i)
        plotSubsets_l[[i]]<-read.csv("Outputs/comm_outlierRemoved.csv") %>% as_tibble %>%
            group_by(Region, Region_Plot) %>%
            summarise(value=n()) %>%
            group_by(Region) %>% 
            #sample_n(17) %>%
            sample_n(sampleSize)%>%
            ungroup() %>%
            pull(Region_Plot)
    }
saveRDS(plotSubsets_l, "Outputs/PlotSubsets.RDS")




#ordered
env<-read.csv("Data/env.csv") %>% as_tibble %>%
    filter(Region_Plot %in% read.csv("Outputs/comm_outlierRemoved.csv")$Region_Plot)
orderedPlotSubsets_l<-list()
for (region in c("Balbina", "PGM", "STM")){
    orderedPlots<-env %>%
        filter(Region==region) %>%
        arrange(fc2000)
    for (i in 1:100){
        if (i %in% 1:(nrow(orderedPlots)-20)){
            orderedPlotSubsets_l[[paste0(region, i)]]<-orderedPlots[i:(i+20),] %>%
                pull(Region_Plot)
            print(paste0(region, i))
        }
        else {
            orderedPlotSubsets_l[[paste0(region, i)]]<-orderedPlots %>%
                sample_n(sampleSize) %>%
                pull(Region_Plot)
            print(paste0(region, i))  
        }
    }
}

for (i in 1:100){
    orderedPlotSubsets_l[[paste0("Jurua", i)]]   <-read.csv("Outputs/comm_outlierRemoved.csv") %>% as_tibble %>%
                filter(Region=="Jurua") %>%
                group_by(Region, Region_Plot) %>%
                summarise(value=n()) %>%
                group_by(Region) %>% 
                #sample_n(17) %>%
                sample_n(sampleSize)%>%
                ungroup() %>%
                pull(Region_Plot)
}
    
saveRDS(orderedPlotSubsets_l, "Outputs/OrderedPlotSubsets.RDS")



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
        filter(Region_Plot %in% read.csv("Outputs/comm_outlierRemoved.csv")$Region_Plot)  %>%
        filter(Region==region) %>%
        arrange(fc2000)

    NonoverlappingOrderedPlotSubsets_l[[region]]<-getNonOverlappingPlotSubsamples(orderedPlots)
}


orderedPlots<-read.csv("Outputs/comm_outlierRemoved.csv") %>% as_tibble %>%
                filter(Region=="Jurua") %>%
                group_by(Region, Region_Plot) %>%
                summarise(value=n())

NonoverlappingOrderedPlotSubsets_l[["Jurua"]]<-getNonOverlappingPlotSubsamples(orderedPlots)

saveRDS(
    unlist(NonoverlappingOrderedPlotSubsets_l, recursive=FALSE), 
    "Outputs/NonOverlappingOrderedPlotSubsets.RDS")



