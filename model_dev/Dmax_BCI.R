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

#getting pft-specific dmax values
dmax_vals <- bci.full %>%
  select(dbh, sp) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>%
  summarise(dmax = mean(dbh)) %>%
  merge(., pft_assignments, by = "sp") %>%
  group_by(pft) %>%
  summarise(dmax_val = mean(dmax))

print("Values for DMax are:")
print(dmax_vals)




