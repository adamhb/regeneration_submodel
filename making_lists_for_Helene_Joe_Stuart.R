source("model_dev/Dmax_BCI.R")

ruger_pfts <- read_csv("benchmarking/ruger_pft_assignments.csv")

ruger_pfts <- ruger_pfts %>%
  select(sp,Genus,Species,PFT_1axis, PFT_2axes,wd) %>%
  mutate_at(.vars = "sp",.funs = tolower) 

dmax_vals_Helene_and_Joe <- bci.full %>%
  select(dbh, sp) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>% 
  summarise(dmax = mean(dbh)) %>%
  right_join(pft_assignments) %>%
  left_join(ba_fraction_per_sp) %>%
  left_join(ruger_pfts) %>%
  select(-X1,-Species,-Genus) %>%
  rename(total_ba_per_pft = total_ba,
         Ruger_PFT_1axis = PFT_1axis) %>%
  arrange(pft,desc(ba)) %>%
  mutate(LLP = case_when(
    PFT_2axes == 3 ~ TRUE,
    TRUE ~ FALSE
  ))


write_csv(dmax_vals_Helene_and_Joe,path = "temp/Dmax_and_BA.csv")


dmax_vals_Helene_and_Joe_no_LLP <- dmax_vals_Helene_and_Joe %>%
  filter(LLP == FALSE)

write_csv(dmax_vals_Helene_and_Joe_no_LLP,path = "temp/Dmax_and_BA_no_LLP.csv")







