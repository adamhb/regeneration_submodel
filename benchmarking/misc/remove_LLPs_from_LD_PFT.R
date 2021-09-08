source("utils/system_settings.R")
ruger_pfts <- read_csv(paste0(path_to_observational_data,'ruger_pft_assignments.csv')) #clean up where this data is coming from. Its in both the benchmarking folder and the observational folder
my_pfts <-  read_csv('benchmarking/pft_assignments.csv')
ruger_pfts$sp <- tolower(ruger_pfts$sp) 

pfts_without_LLPs <- my_pfts %>%
  left_join(ruger_pfts, by = "sp") %>%
  select(Latin,sp,pft,PFT_2axes) %>%
  filter(PFT_2axes != 3)

LLPs <- my_pfts %>%
  left_join(ruger_pfts, by = "sp") %>%
  select(Latin,sp,pft,PFT_2axes) %>%
  filter(PFT_2axes == 3)

write_csv(LLPs,path = "benchmarking/LLPs.csv")
write_csv(pfts_without_LLPs,path = "benchmarking/pfts_without_LLPs.csv")
