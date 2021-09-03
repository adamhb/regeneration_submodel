print("assigning species to pfts")
source('utils/supporting_funcs.R')
library(tidyverse)
create_csv_of_pfts <- T

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_benchmarking_data
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"

#import Powell's pft assignments according to Wright's (2010) growth mortality tradeoff
# powell.pfts <- read_csv(paste0(path_to_observations,"wright_2010_powell_manipulated_pfts.csv")) %>%
#   select(GENUS, SPECIES, powell_pft) %>%
#   mutate(Latin = paste(GENUS,SPECIES)) %>%
#   mutate(powell_pft = case_when(
#     powell_pft %in% c("e","me")  ~ "LD",
#     powell_pft %in% c("l","ml")  ~ "ST"
#   )) %>%
#   select(Latin,powell_pft)

#import Ruger's (2020) pft assignments
# ruger.pfts <- read_csv(paste0(path_to_observations,"ruger_pft_assignments.csv")) %>%
#   select(Genus, Species, PFT_1axis) %>%
#   mutate(Latin = paste(Genus, Species)) %>%
#   filter(PFT_1axis != 2) %>%
#   mutate(ruger_pft = case_when(
#     PFT_1axis == 1  ~ "LD",
#     PFT_1axis == 3  ~ "ST"
#   )) %>%
#   select(Latin, ruger_pft)

#loading the bci census and drought tolerance data
load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"wsg.ctfs.Rdata")) 
load(paste0(path_to_benchmarking_data,"bci.spptable.rdata"))

#these indices come from Engelbrecht 2007's Nature paper which is based on earlier work from Engelbrecht and Kursar 2005
# 10.1038/nature05747 
d_indices <- read.csv(paste0(path_to_benchmarking_data,"drought_indices_Engelbrecht_2007.csv"))

harms_pft <- read.csv(paste0(path_to_benchmarking_data,"harms_habitat_associations.csv"))
moistr_resp <- read.table(paste0(path_to_benchmarking_data,"TreeCommunityDrySeasonSpeciesResponse.txt"),
                          sep = '\t', header = T)

#load the growth form data from expert opinion
g.forms <- read_csv(paste0(path_to_observational_data,"bci50splistwrepthresh.csv")) %>%
  rename(sp = sp6)

names(g.forms)

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


understory_greater_than_20cm <- bci.full %>% 
  select(sp, dbh) %>% 
  filter(dbh > 200) %>% 
  left_join(g.forms, by = "sp") %>%
  filter(grform == "U") %>%
  #filter(grform != "U" & grform != "M") %>%
  select(sp) %>% distinct(sp) %>% left_join(bci.spptable) %>% pull(Latin) %>% tibble() %>% drop_na()
names(understory_greater_than_20cm) <- "Latin"

write_csv(x = understory_greater_than_20cm, path = "understory_greater_than_20cm.csv")


#defining canopy species according to Powell et al. 2018
canopy_species_bci <- bci.full %>% 
  select(sp, dbh) %>% 
  filter(dbh > 200) %>% 
  left_join(g.forms, by = "sp") %>%
  filter(grform != "U") %>%
  #filter(grform != "U" & grform != "M") %>%
  pull(sp) %>% unique() %>% as.data.frame()
names(canopy_species_bci) <- "sp"


###############################################
##assigning light demanding vs. shade tolerant#
###############################################
cs2 <- canopy_species_bci %>%
  left_join(wsg, by = "sp") %>% #adding wsg to the list of canopy species
  # mutate(Latin = case_when(
  #   sp == "picrla" ~ "Picramnia latifolia", #this species did not have wsg gravity data but it does have a pft assignment from Powell's assignments
  #   TRUE ~ Latin)) %>%
  drop_na(Latin) %>%
  #left_join(powell.pfts) %>%
  #left_join(ruger.pfts) %>%
  mutate(pft = case_when(
    wsg >= 0.49 ~ "ST",
    wsg < 0.49 ~ "LD"
  )) %>%
  # mutate(pft = case_when(
  #   !is.na(powell_pft) ~ powell_pft,
  #   is.na(powell_pft) & !is.na(ruger_pft) ~ ruger_pft,
  #   is.na(powell_pft) & is.na(ruger_pft)  ~ wsg_pft
  # )) %>%
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

#create moisture optima for each species
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
moisture_optima <- unlist(pmap(condit2013 %>% select(-Latin),moisture.optimum.func))

#moisture optima quantiles
med_optima <- median(moisture_optima) #median
optima_quantiles <- quantile(moisture_optima, probs = c(.33, .66), na.rm = T)

condit2013 <- condit2013 %>%
  add_column(moisture_optima) 
 
#determining the 33% percentile and 66% percent of drought indices
Dquantiles <- quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)

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


#cs3 %>% select(Latin,engelbrecht_pft,harms_pft,condit_pft,d.pft)


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


