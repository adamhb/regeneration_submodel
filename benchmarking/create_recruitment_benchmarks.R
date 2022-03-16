#This script creates benchmarking data for recruitment at BCI

#The calculations for recruitment rates from the BCI census data are based on
#prior work by Kohyama et al., 2018.

#Kohyama TS, Kohyama TI, Sheil D. 2018. Definition and estimation of vital rates from repeated 
#censuses: Choices, comparisons and bias corrections focusing on trees. Methods in Ecology and 
#Evolution 9: 809–821.
m2_per_ha <- 1e4
write_benchmark_csv <- T

library(tidyverse)
library(lubridate)

source("create_output/figure_formatting.R")
source("utils/supporting_funcs.R")
source("utils/system_settings.R")

path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"
path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
  
load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"bci.full8.rdata"))
pfts <- read_csv('benchmarking/pft_assignments.csv') %>%
  select(sp, pft, Latin)

#Clean the census data to prepare it for calculating recruitment
#according to Kohyama et al. (2018) Eqn 11.
bci.tree8.ahb <- bci.tree8 %>% mutate(bid = 8) %>%
  select(bid,sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb <- bci.full %>%
  select(bid, sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb.1 <- bci.full.ahb %>%
  rbind(bci.tree8.ahb) %>%
  left_join(pfts, by = "sp") %>% 
  select(-8) %>%
  mutate_at(.vars = "dbh",.funs = function(x){x/10}) %>% #convert to cm
  drop_na(Latin)

#calculate per area mortality and recruitment rates
cen_ints <- length(unique(bci.full.ahb.1$bid)) - 1
species <- unique(bci.full.ahb.1$sp)
dem_data <- tibble()

for(i in 1:cen_ints){
  for(spp in species){
    
    cen_start <- bci.full.ahb.1 %>%
      filter(bid == i, status == "A", sp == spp, dbh < 5)
    cen_end <- bci.full.ahb.1 %>%
      filter(bid == i+1, status == "A", sp == spp, dbh < 5)
    
    N1 <- nrow(cen_start)
    N2 <- nrow(cen_end)
    
    IDs_alive1 <- cen_start %>% pull(treeID)
    IDs_alive2 <- cen_end %>% pull(treeID)
    Ns <- sum(IDs_alive2 %in% IDs_alive1)
    
    interval_start <- cen_start %>% pull(ExactDate) %>% ymd() %>% mean(na.rm = T)
    interval_end <- cen_end %>% pull(ExactDate) %>% ymd() %>% mean(na.rm = T)
    interval_length <- as.numeric(difftime(time1 = interval_end, time2 = interval_start, units = "auto") / 365)
    
    Stmp <- tibble(sp = spp, 
                   Ns = Ns, 
                   N1 = N1, 
                   N2 = N2,
                   int_start = interval_start,
                   int_end =  interval_end,
                   interval_length = interval_length,
                   int = i)
    dem_data <- rbind(dem_data,Stmp)
  }
  print(paste("done",i))
}

write_csv(dem_data,"benchmarking/dem_data_raw.csv")

mort_Kohyama <- function(N1,Ns,int_length,A = 5e5){
  M = (N1/A) * ( 1- (Ns / N1)^(1/int_length) ) 
  return(M)
}

rec_Kohyama <- function(Ma,N2,Ns,N1){
  R <- Ma*(N2-Ns)/(N1-Ns)
  return(R)
}

dem_data1 <- dem_data %>%
  mutate(M_Koh = unlist(pmap(list(N1 = N1, Ns = Ns, int_length = interval_length), .f = mort_Kohyama))) %>%
  mutate(R_Koh = unlist(pmap(list(Ma = M_Koh, N2 = N2, Ns = Ns, N1 = N1), .f = rec_Kohyama))) %>% 
  mutate(Nd = N1 - Ns) %>%
  mutate(Nr = N2 + Nd - N1) %>%
  select(int,sp,N1,N2,Ns,Nd,Nr,int_start,int_end,interval_length, M_Koh, R_Koh) %>%
  drop_na(M_Koh, R_Koh)

dem_data2 <- dem_data1 %>%
  left_join(pfts,by = "sp") %>%
  drop_na(pft) 

write_csv(dem_data2,"benchmarking/dem_data_clean.csv")

dem_data2  <- read_csv('benchmarking/dem_data_clean.csv')

#correlation between Koh rates and basic r rates.
ggplot(dem_data2,aes((Nr/interval_length),R_Koh * 5e5)) + geom_point() 


per_ha_rec_rates_per_pft_per_int <- dem_data2 %>%
  mutate(r_basic_per_ha_per_yr = Nr / (50*interval_length)) %>%
  group_by(pft,int) %>%
  summarise(R_Koh_ha_yr = sum(R_Koh, na.rm = T) * m2_per_ha,
            R_no_mort_adjust_ha_yr = sum(r_basic_per_ha_per_yr))

write_csv(per_ha_rec_rates_per_pft_per_int, "benchmarking/obs_per_ha_rec_rates_per_pft_per_int.csv")

per_ha_rec_rates_per_pft_per_int_2005_2015 <- per_ha_rec_rates_per_pft_per_int %>%
  ungroup() %>%
  filter(int %in% c(6,7)) %>%
  group_by(pft) %>%
  summarise(R = mean(R_Koh_ha_yr),
            se_R = sd(R_Koh_ha_yr) / sqrt(length(R_Koh_ha_yr)),
            R_no_mort_adjust_ha_yr = mean(R_no_mort_adjust_ha_yr)) %>%
  mutate(model = "BCI obs.")
  
write_csv(per_ha_rec_rates_per_pft_per_int_2005_2015, "benchmarking/per_ha_rec_rates_per_pft_per_int_2005_2015.csv")


rec_benchmarks_over_time <- per_ha_rec_rates_per_pft_per_int %>% 
  ggplot(mapping = aes(x = int, y = R_Koh_ha_yr, color = pft)) +
  geom_point(size = 5) +
  geom_point(aes(int,R_no_mort_adjust_ha_yr,color = pft)) +
  geom_line() +
  scale_color_manual(values = pft.cols) +
  ylab(label = "recruitment rate (# ind. per ha yr)") +
  xlab(label = "census interval") +
  theme_minimal() +
  adams_theme

makePNG(fig = rec_benchmarks_over_time, path_to_output.x = path_to_benchmarking_output, file_name = "rec_benchmarks")




#reshaping the recruitment benchmarks to be in a long format with a time axis so that
#it can be plotted alongside model output.

# intDates <- dem_data2 %>%
#   group_by(int) %>%
#   summarise(int_start = mean.Date(int_start),
#             int_end = mean.Date(int_end)) 
# 
# 
# dates1 <- seq.Date(intDates$int_start[1],intDates$int_end[7],by = "day")
# pft_id <- c(rep(pft_names[1],length(dates1)),rep(pft_names[2],length(dates1)),rep(pft_names[3],length(dates1)),rep(pft_names[4],length(dates1)))
# dates2 <- rep(dates1,4)
# 
# rec_benchmarks_with_dates_long <- tibble(date = dates2, pft = pft_id) %>%
#   mutate(int = case_when(
#     date %in% seq.Date(intDates$int_start[1],intDates$int_end[1],by = "day") ~ 1,
#     date %in% seq.Date(intDates$int_start[2],intDates$int_end[2],by = "day") ~ 2,
#     date %in% seq.Date(intDates$int_start[3],intDates$int_end[3],by = "day") ~ 3,
#     date %in% seq.Date(intDates$int_start[4],intDates$int_end[4],by = "day") ~ 4,
#     date %in% seq.Date(intDates$int_start[5],intDates$int_end[5],by = "day") ~ 5,
#     date %in% seq.Date(intDates$int_start[6],intDates$int_end[6],by = "day") ~ 6,
#     date %in% seq.Date(intDates$int_start[2],intDates$int_end[7],by = "day") ~ 7,
#   )) %>%
#   left_join(per_ha_rec_rates_per_pft_per_int, by = c("pft","int")) %>%
#   mutate(rec_rate = R_Koh_ha_yr)
# 
# write_csv(rec_benchmarks_with_dates_long, file = "benchmarking/bci_rec_benchmarks_long.csv")
# 
# print("created benchmarks")