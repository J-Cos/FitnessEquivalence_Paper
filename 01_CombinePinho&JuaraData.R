# ---------------------
# Script combines the Jurua data with the Pinho et al data, applying various data merging validations
# Output is new comm and traits csvs in the Outputs directory.
# ----------------------------
library(tidyverse)

#Functions
# Function to compute Hamming distance (character-wise differences)
hamming_distance <- function(a, b) {
  if (nchar(a) != nchar(b)) return(Inf)
  sum(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])
}

# Main functionto get similar strings
strings_with_n_diff <- function(vec, n) {
  # Create all combinations of string pairs
  combs <- expand.grid(a = vec, b = vec, stringsAsFactors = FALSE) %>%
    filter(a != b) # remove self-comparisons
  
  # Compute Hamming distances
  result <- combs %>%
    rowwise() %>%
    mutate(dist = hamming_distance(a, b)) %>%
    ungroup() %>%
    filter(dist == n)
  
  # Return unique strings that matched with any other
  unique(c(result$a, result$b))
}


j<-read.csv("Data/Jurua_Flora_FullDB.csv") %>% 
        as_tibble %>%
        filter(DBH>=10)


comm<-read.csv("Data/comm.csv") %>% as_tibble %>% filter(Region %in% c("Balbina", "PGM", "STM"))

traits<-read.csv("Data/traits.csv") %>% as_tibble


str(j)
str(comm)

##########################################################
# comm
#########################################################
CommMatchingColumns<-j %>% 
    mutate(X=NA, Region = "Jurua") %>%
    select(X, Region, parcel, species, genus, family, DBH) %>%
    rename(Region_Plot=parcel, Binomial_correct=species, Genus=genus)

commSubcolumns<-comm %>%
    select(X, Region, Region_Plot, Binomial_correct, Genus, family, DBH)


comm_new<-rbind(commSubcolumns, CommMatchingColumns)


############################
# check and fix inconsistencies in each column
##############

# 1/ unique ID #########################
comm_new$X<-1:dim(comm_new)[1]

# 2/ region and region plots #################
comm_new$Region %>% table
comm_new$Region_Plot %>% table

# 3/ binomial #############################
binoms <- comm_new$Binomial_correct %>% str_trim 
vec<- binoms %>% unique
potentialTypos<-strings_with_n_diff(vec,1)

# match low frequency similar variants to their more common versions
binoms[binoms %in% potentialTypos] %>% table
binoms[binoms=="Copaifera guyanensis"]<-"Copaifera guianensis"
binoms[binoms=="Licaria macrophylla"]<-"Licania macrophylla"
binoms[binoms=="Licaria rodriguesii"]<-"Licania rodriguesii"

#fix
comm_new$Binomial_correct<- binoms

# 4/ Genus #############################################
genera <- comm_new$Genus %>% str_trim 
vec<- genera %>% unique
potentialTypos<-strings_with_n_diff(vec,1)
genera[genera %in% potentialTypos] %>% table

# 5/ family #####################################
families <- comm_new$family %>% str_trim 
vec<- families %>% unique
potentialTypos<-strings_with_n_diff(vec,1)
families %>% table

#6 / DBH ###############################
comm_new$DBH %>% summary
comm_new[comm_new$DBH>120,]

# 7/ save new comm ###############################################
write.csv(comm_new, row.names=FALSE, "Outputs/comm.csv")

#################################################
# traits
##############################################

str(j)
str(traits)

# 1/ sort out juara data ##########################################
# get matching columns only
TraitMatchingColumns<-j %>% 
    mutate(Hmax=NA, DS=NA, lnSM=log(SM)) %>%
    select(species, WD, lnSM, LMA, Hmax, DS) %>%
    rename(Binomial_correct=species )

#fix names using problems identified in comm
binoms <- TraitMatchingColumns$Binomial_correct %>% str_trim 
binoms[binoms=="Copaifera guyanensis"]<-"Copaifera guianensis"
binoms[binoms=="Licaria macrophylla"]<-"Licania macrophylla"
binoms[binoms=="Licaria rodriguesii"]<-"Licania rodriguesii"
TraitMatchingColumns$Binomial_correct<- binoms


#find taxa not presnent in traits
TraitMatchingColumns_needed<-TraitMatchingColumns[!TraitMatchingColumns$Binomial_correct %in% traits$Binomial_correct,]
TraitMatchingColumns_needed$Binomial_correct %>% unique

# identify taxa without matching traits across the juara databse

# 22 lack Seed mass data
TraitMatchingColumns_needed %>%
    group_by(Binomial_correct) %>%
    summarise_all(mean) %>%
    filter(is.na(WD) | is.na(lnSM) | is.na(LMA)) %>%
    print(n=999)


# 39 have non perfectly matching trait data, for which the median 
#will be taken (this includes 3 of those with no seed mass data)
TraitMatchingColumns_needed %>%
    group_by(Binomial_correct) %>%
    summarise_all(sd) %>%
   # filter(!is.na(WD) | !is.na(lnSM) | !is.na(LMA)) %>%
    filter(!WD==0 | !lnSM==0 | !LMA==0) %>%
    print(n=999)

# extract trait medians for all needed taxa
traits_needed<-TraitMatchingColumns_needed %>%
    group_by(Binomial_correct) %>%
    summarise_all(median) 

# 2/ filter out a problem trait row from the pinho data #######################################################
# remove problem imputed traits for one of the eroneous taxa in the pinho dataset
traits_filtered<-traits %>% filter (!Binomial_correct=="Licaria rodriguesii")

# 3/ combine and check ##################################################
traits_new<-rbind(traits_filtered, traits_needed)

#double check no issues with names
binoms <- traits_new$Binomial_correct
vec<- binoms
potentialTypos<-strings_with_n_diff(vec,1)


# 4 / save new traits
write.csv(traits_new, row.names=FALSE, "Outputs/traits.csv")
