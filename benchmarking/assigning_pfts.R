print("assigning species to pfts")
library(tidyverse)
create_csv_of_pfts <- T

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_benchmarking_data
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"

#import Powell's pft assignments according to Wright's (2010) growth mortality tradeoff
powell.pfts <- read_csv(paste0(path_to_observations,"wright_2010_powell_manipulated_pfts.csv")) %>%
  select(GENUS, SPECIES, powell_pft) %>%
  mutate(Latin = paste(GENUS,SPECIES)) %>%
  mutate(powell_pft = case_when(
    powell_pft %in% c("e","me")  ~ "LD",
    powell_pft %in% c("l","ml")  ~ "ST"
  )) %>%
  select(Latin,powell_pft)

#import Ruger's (2020) pft assignments
ruger.pfts <- read_csv(paste0(path_to_observations,"ruger_pft_assignments.csv")) %>%
  select(Genus, Species, PFT_1axis) %>%
  mutate(Latin = paste(Genus, Species)) %>%
  filter(PFT_1axis != 2) %>%
  mutate(ruger_pft = case_when(
    PFT_1axis == 1  ~ "LD",
    PFT_1axis == 3  ~ "ST"
  )) %>%
  select(Latin, ruger_pft)

#loading the bci census and drought tolerance data
load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"wsg.ctfs.Rdata")) 
load(paste0(path_to_benchmarking_data,"bci.spptable.rdata"))
d_indices <- read.csv(paste0(path_to_benchmarking_data,"drought_indices_Engelbrecht_2007.csv"))
harms_pft <- read.csv(paste0(path_to_benchmarking_data,"harms_habitat_associations.csv"))
moistr_resp <- read.table(paste0(path_to_benchmarking_data,"TreeCommunityDrySeasonSpeciesResponse.txt"),
                          sep = '\t', header = T)


#function to convert Latin names to "sp code names"
Latin2sp <- function(Latin.name){
  if(is.character(Latin.name) != T){Latin.name <- as.character(Latin.name)} 
  sp <- sp_code[sp_code$Latin == Latin.name,]$sp
  if(Latin.name %in% sp_code$Latin == F){return(Latin.name)}else{
    return(sp)}
}

#geting a list of wood specific gravity for all species in the bci.spptable
wsg <- wsg.ctfs3 %>%
  mutate(Latin = paste(genus,species)) %>%
  rename(sp_wsg = sp) %>%
  left_join(bci.spptable, by = "Latin") %>%
  select(Latin,wsg,sp) %>%
  drop_na(sp,wsg) %>%
  distinct(Latin,sp,wsg)

#defining canopy species according to Powell et al. 2018
canopy_species_bci <- bci.full %>% 
  select(sp, dbh) %>% 
  filter(dbh > 200) %>% 
  pull(sp) %>% unique() %>% as.data.frame()
names(canopy_species_bci) <- "sp"


###############################################
##assigning light demanding vs. shade tolerant#
###############################################
cs2 <- canopy_species_bci %>%
  left_join(wsg, by = "sp") %>% #adding wsg to the list of canopy species
  mutate(Latin = case_when(
    sp == "picrla" ~ "Picramnia latifolia", #this species did not have wsg gravity data but it does have a pft assignment from Powell's assignments
    TRUE ~ Latin)) %>%
  drop_na(Latin) %>%
  left_join(powell.pfts) %>%
  left_join(ruger.pfts) %>%
  mutate(wsg_pft = case_when(
    wsg >= 0.49 ~ "ST",
    wsg < 0.49 ~ "LD"
  )) %>%
  mutate(pft = case_when(
    !is.na(powell_pft) ~ powell_pft,
    is.na(powell_pft) & !is.na(ruger_pft) ~ ruger_pft,
    is.na(powell_pft) & is.na(ruger_pft)  ~ wsg_pft
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
condit2013 <- moistr_resp %>% select(Latin,Moist) #just selecting the first order coefficient


#determining the 33% percentile and 66% percent of drought indices
Dquantiles <- quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)

cs3 <- cs2 %>%
  left_join(d_indices) %>% #adding drought indices from Engelbrecht
  mutate(engelbrecht_pft = case_when(
          d_index  >  Dquantiles[2] ~ "DI",
          d_index  <  Dquantiles[2] ~ "DT"
           )) %>%
  left_join(harms_pft) %>%
  mutate_at(.vars = "harms_pft", .funs = toupper) %>%
  left_join(condit2013) %>%
  mutate(condit_pft = case_when(
    Moist < 0 ~ "DI",
    Moist >= 0 ~ "DT"
  )) %>%
  mutate(d.pft = case_when(
    !is.na(engelbrecht_pft) ~ engelbrecht_pft,
    is.na(engelbrecht_pft) & !is.na(harms_pft) ~ harms_pft,
    is.na(engelbrecht_pft) & is.na(harms_pft) & !is.na(condit_pft) ~ condit_pft
    #is.na(engelbrecht_pft) & is.na(harms_pft) & is.na(Moist) ~  c("DT","DI")[round(runif(1,1,2))]
    #TRUE ~ c("DT","DI")[round(runif(1,1,2))]
  )) 



#there are 9 species for which there is no drought information
set.seed(7)
cs3$d.pft[is.na(cs3$d.pft)] <- c("DT","DI")[round(runif(9,1,2))] #assigning them randomly
cs4 <- cs3 %>% mutate(pft = paste0(l.pft,"_",d.pft))

pft_list <- cs4 %>% select(Latin, sp, pft)

if(create_csv_of_pfts == T){
  write.csv(pft_list, file = "benchmarking/pft_assignments.csv")
}

print("finished assigning species to pfts")
pfts_nov_2018 <- pft_list #some subsequent scripts still call the pft list by this name

pft_list %>%
  group_by(pft) %>%
  summarise(n = length(pft))


