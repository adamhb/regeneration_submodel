
create_csv_of_pfts <- T

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"

#loading the census and other observational data
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



#defining canopy species by saying that any species greater than 20 cm can be a pft
canopy_species_bci <- bci.full %>% select(sp, dbh) %>% filter(dbh > 200) %>% .$sp %>% unique(.)
canopy_species_bci <- as.data.frame(canopy_species_bci)
names(canopy_species_bci) <- "sp"


wsg.ctfs3$Latin <- paste(wsg.ctfs3$genus, wsg.ctfs3$species)
temp <- merge(wsg.ctfs3,bci.spptable, by = "Latin")[,c(1,2,5)]
names(temp)[3] <- "sp"
temp[temp$Latin == "Trema integerrima",]$sp <- "tremin"
canopy_species <- merge(as.data.frame(canopy_species_bci),temp, by = "sp") %>% na.omit(.)
#write.csv(canopy_species, "canopy_sp_pfts_9_20_2018.csv")




#adding early versus late accoring to Powell 2018
pft <- rep(NA, length(canopy_species$sp))
pft[canopy_species$wsg >= 0.49] <- "ST"
pft[canopy_species$wsg < 0.49] <- "LD"
canopy_species$pft <- pft


#adding drought tolerant versus intolerant from Engelbrecht drought indices

pfts_sept_2018 <- merge(d_indices, canopy_species, all.y = T)

#adding data from Harms 2001 on whats positively or negatively associated with the plateau

#importing the habitat associations from the harms 2001 paper

harms_pft$sp <- gsub(pattern = "\n", replacement = " ", x = harms_pft$sp)
harms_pft$stat <- as.character(harms_pft$stat)
names(harms_pft) <- c("Latin", "dpft")

#merging the values with the dpft data from Harmin
pfts_sept_2018 <- merge(pfts_sept_2018, harms_pft, by = "Latin", all.x = T)


#adding the values from the PNAS paper

# importing the moisture response data from the PNAS paper

moistr_resp <- moistr_resp %>% select(Latin, occur, Inter, Moist, Moist.2)
moistr_resp$Latin <- lapply(X = strsplit(x = as.character(moistr_resp$Latin), split = " (", fixed = T), `[[`, 1) %>% unlist(.)


#merging this new list back with the pft data
pfts_sept_2018 <- merge(pfts_sept_2018, y = moistr_resp[,c("Latin", "Moist")], by = "Latin", all.x = T)
pfts_sept_2018$engel_dpft <- rep(0, length(pfts_sept_2018$Latin))

quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)

pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(2:4), funs(replace(., is.na(.), 0)))

pfts_sept_2018[pfts_sept_2018$d_index < 14.2 & pfts_sept_2018$d_index > 0,]$engel_dpft <- "DT"
pfts_sept_2018[pfts_sept_2018$d_index > 33.35,]$engel_dpft <- "DI"

names(pfts_sept_2018)[c(5,6,8)] <- c("LD_vs_ST","harms_dt_vs_di", "engel_dt_vs_di")

pfts_sept_2018 <- pfts_sept_2018 %>% select(Latin, sp, wsg, LD_vs_ST, d_index, engel_dt_vs_di, harms_dt_vs_di, Moist)


#adding the PNAS categorization

quantile(pfts_sept_2018$Moist, na.rm = T)
pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(7:8), funs(replace(., is.na(.), 0)))

PNAS_dt_vs_di <- rep(0, length(pfts_sept_2018$Latin))

PNAS_dt_vs_di[pfts_sept_2018$Moist < -0.0001] <- "DI"
PNAS_dt_vs_di[pfts_sept_2018$Moist > 0] <- "DT"


pfts_sept_2018$PNAS_dt_vs_di <- PNAS_dt_vs_di


#creating final dt_vs_di pfts
dpft <- c()
for(i in 1:length(pfts_sept_2018$Latin)){
  if(pfts_sept_2018$engel_dt_vs_di[i] != 0){dpft[i] <- pfts_sept_2018$engel_dt_vs_di[i]}else{
    if(pfts_sept_2018$harms_dt_vs_di != 0){dpft[i] <- pfts_sept_2018$harms_dt_vs_di[i]}else{
      dpft[i] <- pfts_sept_2018$PNAS_dt_vs_di[i]
    }
  }
}


pfts_sept_2018$dpft <- dpft

#write.csv(pfts_sept_2018, paste0(path_to_benchmarking_output,"pfts_9_20_2018a.csv"))

#acknowledging the ones that we don't have drought tolerance data for
no_drought_tolerance_data <- pfts_sept_2018[pfts_sept_2018$dpft == "0",]

#randomly assigning those few trees to either drought tolerance or intolerant

pfts_sept_2018[pfts_sept_2018$dpft == 0,]$dpft <- runif(n = 9,min = 0,max = 1)


change <- as.logical((is.na(as.numeric(pfts_sept_2018$dpft))*-1)+1)

pfts_sept_2018$dpft[change][1:4] <- "DT"
pfts_sept_2018$dpft[change][5:9] <- "DI" 

#write.csv(pfts_sept_2018, file = paste0(path_to_benchmarking_output,"pfts_9_20_2018b.csv"))

#the pft list we use in November 2018
pfts_nov_2018 <- pfts_sept_2018 %>% mutate(pft = paste0(LD_vs_ST,"_",dpft)) %>% select(Latin, sp, pft)


if(create_csv_of_pfts == T){
  write.csv(pfts_nov_2018, file = "benchmarking/pft_assignments.csv")
}



