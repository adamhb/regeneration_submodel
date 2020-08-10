##DMAX##
#The Dmax is the mean of the six largest trees in each species across all censuses in BCI.
rm(list = ls())
gc()

library(tidyverse)

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"
load(paste0(path_to_benchmarking_data,"bcifull.RData"))

pft_file_exists <- file.exists("benchmarking/pft_assignments.csv")

if(pft_file_exists == FALSE){
  source("benchmarking/assigning_pfts.R")
}

pft_assignments <- read_csv(file = "benchmarking/pft_assignments.csv")


#getting basal area of each species to weight by it
source("benchmarking/forestgeo_benchmark_driver_AHB.r")


ba_per_sp <- tibble()

for(i in 1:8){
  
  ba <- sp.BAlist[[i]]$ba
  ba <- ba$all
  sp <- attributes(sp.BAlist[[i]]$ba)$row.names
  
  tmp <- tibble(ba = ba, sp = sp)
  
  ba_per_sp <- rbind(ba_per_sp,tmp)
}

total_ba_per_pft <- ba_per_sp %>% 
  group_by(sp) %>% 
  summarise(ba = mean(ba)) %>%
  right_join(pft_assignments) %>%
  group_by(pft) %>%
  summarise(total_ba = sum(ba))


ba_fraction_per_sp <- ba_per_sp %>% 
  group_by(sp) %>% 
  summarise(ba = mean(ba)) %>%
  right_join(pft_assignments) %>%
  left_join(total_ba_per_pft) %>%
  mutate(ba_fraction = ba / total_ba)

  
#getting pft-specific dmax values
dmax_vals <- bci.full %>%
  select(dbh, sp) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>% 
  summarise(dmax = mean(dbh)) %>%
  right_join(pft_assignments) %>%
  left_join(ba_fraction_per_sp) %>%
  mutate(tmp = ba_fraction * dmax) %>% 
  group_by(pft) %>%
  summarise(dmax_val = mean(dmax))


print("Values for DMax are:")
print(dmax_vals)




