#This script assigns species at BCI to four PFTs:
#light demanding, drought intolerant (LD-DI)
#light demanding, drought tolerant (LD-DT)
#shade tolerant, drought intolerant (ST-DI)
#and shade tolerant, drought tolerant (ST-DT)  

print("assigning species to pfts")
source('utils/supporting_funcs.R')
library(tidyverse)
create_csv_of_pfts <- T

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_benchmarking_data
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"

#loading BCI census and trait data 
load(paste0(path_to_benchmarking_data,"bcifull.RData")) #full census data
load(paste0(path_to_benchmarking_data,"wsg.ctfs.Rdata")) #wood specific gravity data
load(paste0(path_to_benchmarking_data,"bci.spptable.rdata")) #species table for BCI


#The drought tolerance indices come from Engelbrecht et al. (2007) which is based on earlier work started by Engelbrecht and Kursar (2003)
d_indices <- read.csv(paste0(path_to_benchmarking_data,"drought_indices_Engelbrecht_2007.csv"))
#Engelbrecht BMJ, Comita LS, Condit R, Kursar T, Tyree MT, Turner BL, Hubbell SP. 2007. 
#Drought sensitivity shapes species distribution patterns in tropical forests. Nature 447: 
#80–82.

#The within-site (BCI) habitat association data comes from Harms et al., 2001 
harms_pft <- read.csv(paste0(path_to_benchmarking_data,"harms_habitat_associations.csv"))
#Harms KE, Condit R, Hubbell SP, Foster RB. 2001. Habitat associations of trees and shrubs in
#a 50-ha neotropical forest plot. Journal of Ecology 89: 947–959.

#The regional moisture gradient assocations are from Condit et al., 2013 
moistr_resp <- read.table(paste0(path_to_benchmarking_data,"TreeCommunityDrySeasonSpeciesResponse.txt"),
                          sep = '\t', header = T)

#Condit R, Engelbrecht BMJ, Pino D, Pérez R, Turner BL. 2013. Species distributions in 
#response to individual soil nutrients and seasonal drought across a community of tropical 
#trees. Proceedings of the National Academy of Sciences of the United States of America 110: 
#5064–8.


#The growth form data is from expert opinion at BCI
g.forms <- read_csv(paste0(path_to_observational_data,"bci50splistwrepthresh.csv")) %>%
  rename(sp = sp6)
names(g.forms)

#Function to convert Latin names to "sp code names"
Latin2sp <- function(Latin.name){
  if(is.character(Latin.name) != T){Latin.name <- as.character(Latin.name)} 
  sp <- sp_code[sp_code$Latin == Latin.name,]$sp
  if(Latin.name %in% sp_code$Latin == F){return(Latin.name)}else{
    return(sp)}
}

#Extracting a list of wood specific gravity for all species in the bci.spptable
wsg <- wsg.ctfs3 %>%
  mutate(Latin = paste(genus,species)) %>%
  rename(sp_wsg = sp) %>%
  left_join(bci.spptable, by = "Latin") %>%
  select(Latin,wsg,sp) %>%
  drop_na(sp,wsg) %>%
  distinct(Latin,sp,wsg)

#Determining which species get larger than 20 cm dbh, but that are still understory specialists
understory_greater_than_20cm <- bci.full %>% 
  select(sp, dbh) %>% 
  filter(dbh > 200) %>% 
  left_join(g.forms, by = "sp") %>%
  filter(grform == "U") %>%
  #filter(grform != "U" & grform != "M") %>%
  select(sp) %>% distinct(sp) %>% left_join(bci.spptable) %>% pull(Latin) %>% tibble() %>% drop_na()
names(understory_greater_than_20cm) <- "Latin"
#write_csv(x = understory_greater_than_20cm, path = "understory_greater_than_20cm.csv")


#Defining canopy species according to Powell et al. 2018
#Canopy species are ones that can grow larger than 20 cm dbh
canopy_species_bci <- bci.full %>% 
  select(sp, dbh) %>% 
  filter(dbh > 200) %>% #dbh in mm
  left_join(g.forms, by = "sp") %>%
  filter(grform != "U") %>%
  #filter(grform != "U" & grform != "M") %>%
  pull(sp) %>% unique() %>% as.data.frame()
names(canopy_species_bci) <- "sp"

#Powell TL, Koven CD, Johnson DJ, Faybishenko B, Fisher RA, Knox RG, McDowell NG, Condit R, Hubbell SP, Wright SJ, et al. 2018. 
#Variation in hydroclimate sustains tropical forest biomass and promotes functional diversity. New Phytologist 219: 932–946.


###############################################################
##assigning species to light demanding vs. shade tolerant PFTs#
###############################################################

#We assign species to light demanding vs. shade tolerant species
#similar to Powell et al., 2018 using a threshold for wood density
cs2 <- canopy_species_bci %>%
  left_join(wsg, by = "sp") %>% #adding wsg to the list of canopy species
  drop_na(Latin) %>%
  mutate(pft = case_when(
    wsg >= 0.49 ~ "ST",
    wsg < 0.49 ~ "LD"
  )) %>%
  rename(l.pft = pft) %>%
  select(sp,Latin,l.pft)


###############################################
##assigning drought tolerant vs. intolerant####
###############################################

#cleaning the habitat associations data from Harms et al. 2001
harms_pft$sp <- gsub(pattern = "\n", replacement = " ", x = harms_pft$sp)
harms_pft$stat <- as.character(harms_pft$stat)
names(harms_pft) <- c("Latin", "harms_pft")

#cleaning moisture response data from Condit 2013
moistr_resp <- moistr_resp %>% select(Latin, occur, Inter, Moist, Moist.2)
moistr_resp$Latin <- lapply(X = strsplit(x = as.character(moistr_resp$Latin), split = " (", fixed = T), `[[`, 1) %>% unlist(.)
condit2013 <- moistr_resp %>% select(Latin,Inter,Moist,Moist.2) 

#function used to calculate the moisture optima for each species using statistical parameters from Condit et al. 2013
moisture.optimum.func <- function(Inter,Moist,Moist.2){
  b0 = Inter
  b1 = Moist
  b2 = Moist.2
  x = seq(-3,3,.01)
  y = b0 + b1*x + b2*x^2
  p = exp(y)/(1+exp(y))
  #plot(x,p)
  z = data.frame(cbind(x,p))
  moisture.optimum = z$x[z$p==max(z$p)]
  return(moisture.optimum)
}
#map the above function over the data
moisture_optima <- unlist(pmap(condit2013 %>% select(-Latin),moisture.optimum.func))

#moisture optima quantiles
med_optima <- median(moisture_optima)
optima_quantiles <- quantile(moisture_optima, probs = c(.33, .66), na.rm = T)
condit2013 <- condit2013 %>%
  add_column(moisture_optima) 


#determining the 33% percentile and 66% percent of drought indices from the Engelbrecht data
Dquantiles <- quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)


#Joining all of the pft assignments data together to finalize pft assignments
cs3 <- cs2 %>%
  left_join(d_indices) %>% #adding drought indices from Engelbrecht
  mutate(engelbrecht_pft = case_when(
          d_index  >=  Dquantiles[2] ~ "DI",
          d_index  <  Dquantiles[1] ~ "DT" # lower drought index means more drought tolerant because drought index was calculated as (S_irrigated - S_dry)/ S_irrigated × 100 
           )) %>%
  left_join(harms_pft) %>%
  mutate_at(.vars = "harms_pft", .funs = toupper) %>%
  left_join(condit2013) %>%
  mutate(condit_pft = case_when(
    moisture_optima <= optima_quantiles[2] ~ "DT", #lower optima means more drought tolerant because predictor variable in Condit et al. 2013 was dry season moisture (P - ET)
    moisture_optima > optima_quantiles[1] ~ "DI"
  )) %>%
  mutate(condit_pft2 = case_when(
    moisture_optima <= med_optima ~ "DT", #lower optima means more drought tolerant because predictor variable in Condit et al. 2013 was dry season moisture (P - ET)
    moisture_optima > med_optima ~ "DI"
  )) %>%
  mutate(d.pft = case_when(
    !is.na(engelbrecht_pft) ~ engelbrecht_pft,
    is.na(engelbrecht_pft) & !is.na(harms_pft) ~ harms_pft,
    is.na(engelbrecht_pft) & is.na(harms_pft) & !is.na(condit_pft) ~ condit_pft,
    is.na(engelbrecht_pft) & is.na(harms_pft) & is.na(condit_pft) & !is.na(condit_pft2) ~ condit_pft2
    #is.na(engelbrecht_pft) & is.na(harms_pft) & is.na(Moist) ~  c("DT","DI")[round(runif(1,1,2))]
    #TRUE ~ c("DT","DI")[round(runif(1,1,2))]
  )) 


#There are 9 species for which there is no drought information
#Assing these to drought tolerant vs. intolerant pfts randomly
set.seed(7)
cs3$d.pft[is.na(cs3$d.pft)] <- c("DT","DI")[round(runif(9,1,2))] #assigning them randomly
cs4 <- cs3 %>% mutate(pft = paste0(l.pft,"_",d.pft))
pft_list <- cs4 %>% select(Latin, sp, pft)

#output the pft assignments to a csv file
if(create_csv_of_pfts == T){
  write.csv(pft_list, file = "benchmarking/pft_assignments.csv")
}

print("finished assigning species to pfts")
pfts_nov_2018 <- pft_list #some subsequent scripts still call the pft list by this name



