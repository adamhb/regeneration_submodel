library(lme4)
library(tidyverse)

path_to_observations <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"


pft_assignments <- read_csv(file = "benchmarking/pft_assignments.csv")

seed_dyn <- readRDS(paste0(path_to_observations,"bci_seeding_data_for_Adam.RDS"))

seed_dyn$start.date <- as.Date(seed_dyn$start, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))
seed_dyn$stop.date <- as.Date(seed_dyn$stop, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))


pft_specific_annual_mort_rate <- seed_dyn %>%
  mutate_at(.vars = "SPP", .funs = tolower) %>% 
  rename(sp = SPP) %>%
  filter(habitat == "slope") %>%
  filter(sp %in% unique(pft_assignments$sp)) %>%
  mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
  merge(., pft_assignments, by = "sp") %>%
  group_by(pft) %>%
  summarise(mort_rate = (sum(mort)/length(mort)), mean_cen_length = mean(census_length_days)) %>%
  mutate(annual_mort_rate = (mort_rate / mean_cen_length)*365) %>%
  select(pft,annual_mort_rate)

mean_annual_mort_rate <- seed_dyn %>%
  mutate_at(.vars = "SPP", .funs = tolower) %>% 
  rename(sp = SPP) %>%
  filter(habitat == "slope") %>%
  filter(sp %in% unique(pft_assignments$sp)) %>%
  mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
  merge(., pft_assignments, by = "sp") %>%
  mutate(mort_rate = (sum(mort)/length(mort)), mean_cen_length = mean(census_length_days)) %>%
  mutate(annual_mort_rate = (mort_rate / mean_cen_length)*365) %>%
  pull(annual_mort_rate) %>% mean()

print("mean seedling mortality rate is:")
print(mean_annual_mort_rate)

print("PFT-level seedling mortality rate is:")
print(pft_specific_annual_mort_rate)





#visualizing seedling mortality rates
# seed_dyn %>%
#   filter(habitat == "slope") %>%
#   filter(sp %in% unique(pfts_sept_2018$sp)) %>%
#   mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
#   merge(., pfts_sept_2018, by = "sp") %>%
#   filter(dpft != "0") %>%
#   mutate(pft = paste0(e_vs_l,dpft)) %>%
#   group_by(pft, census) %>%
#   summarise(mort_rate = (sum(mort)/length(mort)), mean_cen_length = mean(census_length_days)) %>%
#   mutate(annual_mort_rate = (mort_rate / mean_cen_length)*365) %>%
#   ggplot(mapping = aes(x = census, y = annual_mort_rate, color = pft))+ geom_line()



