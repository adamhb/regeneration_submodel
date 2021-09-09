path_to_observations <- "~/cloud/gdrive/rec_submodel/data/observations/"

la.sel.sol.rad <- read_csv(file = paste0(path_to_observations,"la_selva_solar_rad.csv"))

la.sel.sol.rad %>%
  filter(Year == 1994) %>%
  rename(rad = `DailyLSPyr MJ/d`) %>%
  pull(rad) %>% mean() 

#14.11947 MJ per day