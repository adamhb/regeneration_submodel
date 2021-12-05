#This script creates benchmarking data for recruitment at BCI
#The calculations first rely on running the forestgeo benchmark driver developed by
#Dan Johnson and Ryan Knox

#The calculations for recruitment rates from the BCI census data are based on
#prior work by Kohyama et al., 2018.

#Kohyama TS, Kohyama TI, Sheil D. 2018. Definition and estimation of vital rates from repeated 
#censuses: Choices, comparisons and bias corrections focusing on trees. Methods in Ecology and 
#Evolution 9: 809–821.

write_benchmark_csv <- T

library(tidyverse)
library(lubridate)

#make sure that when the script 'forestgeo_benchmark_driver_AHB.r' is run that the ctfs_at_settings.xml file has the appropriate settings
source("benchmarking/forestgeo_benchmark_driver_AHB.r")

source("benchmarking/assigning_pfts.R")
source("create_output/figure_formatting.R")
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"
  
load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"bci.full8.rdata"))
pfts <- read_csv('benchmarking/pft_assignments.csv')



#START cleaning the census data to prepare it for calculating recruitment
#according to Kohyama et al. (2018) Eqn 11.
bci.tree8.ahb <- bci.tree8 %>% mutate(bid = 8) %>%
  select(bid,sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb <- bci.full %>%
  select(bid, sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb.1 <- bci.full.ahb %>%
  rbind(bci.tree8.ahb) %>%
  #filter(bid > 5) %>%
  left_join(pfts, by = "sp") %>% 
  select(-8) %>%
  mutate_at(.vars = "dbh",.funs = function(x){x/10}) %>% #convert to cm
  drop_na(Latin)

rec_data <- data.frame()
  
  for(i in 1:7){
    
    Si <- tibble()
    
    for(spp in names(sp.Rlist_mindbh[[i]]$R)){
      
      N1 <- bci.full.ahb.1 %>%
        filter(bid == i, status == "A", sp == spp, dbh < 5) %>% nrow()
      
      IDs_alive1 <- bci.full.ahb.1 %>%
        filter(bid == i, status == "A", sp == spp, dbh < 5) %>% pull(treeID)
      
      N2 <- bci.full.ahb.1 %>%
        filter(bid == i+1, status == "A", sp == spp, dbh < 5) %>% nrow()
      
      IDs_alive2 <- bci.full.ahb.1 %>%
        filter(bid == i+1, status == "A", sp == spp, dbh < 5) %>% pull(treeID)
      
      Stmp <- tibble(sp = spp, Ns = sum(IDs_alive2 %in% IDs_alive1), N1 = N1, N2 = N2)
      
      Si <- rbind(Si,Stmp)
    }
    
    rec_data <- rbind(rec_data, data.frame(int = rep(i, length(sp.Rlist_mindbh[[i]]$R)), 
                                           sp = names(sp.Rlist_mindbh[[i]]$R), 
                                           R = sp.Rlist_mindbh[[i]]$R, 
                                           int_length = sp.Rlist_mindbh[[i]]$time, 
                                           M_rate = sp.Mlist[[i]]$rate[1:300,1],#))
                                           #N1 = as.numeric(sp.Alist[[i]]$abund$all[1:300]),
                                           #N2 = as.numeric(sp.Alist[[i+1]]$abund$all[1:300]),
                                           S = Si$Ns,
                                           N1 = Si$N1,
                                           N2 = Si$N2))
    print(paste("done",i))
  }
  #END cleaning the census data
  

write_csv(x = rec_data,file = "temp/raw_rec_data.R")


#calculating Mrate from N1, N2, and S to compare with Mrates from the benchmarking driver

  #Function to calculate recruitment rates
  rec_Kohyama <- function(Ma,Nt,Nst,N0){
      R <- Ma*(Nt-Nst)/(N0-Nst)
      return(R*1e4)
    }
 
  
  t <- rec_data %>% 
    mutate(M_rate2 = (log(N1) - log(S)) / int_length) %>%
    ggplot(mapping = aes(M_rate, M_rate2)) +
    geom_point()
  
  #Merging the recruitment observations dataframe with the species for which we have pft 
  #assignments
  rec_data1 <- rec_data %>% 
    #rename(inter_length = int_length) %>%
    mutate(M_rate2 = (N1 - S) / (N1 * int_length)) %>%
    left_join(pfts_nov_2018, by = "sp") %>%
    drop_na(pft) %>% 
    #mutate(M_rate_area = M_rate * N1 / 5e4) %>%
    mutate(M_rate_area_Koh = (N1/(50*10000))*(1-(S/N1)^(1 / int_length))) %>%
    mutate(M_rate_area = M_rate2 * N1 / 5e4) %>%
    #mutate(M_rate_area = M_rate * N1 / (5e4*int_length)) %>% 
    mutate(R_Koh = unlist(pmap(list(Ma = M_rate_area_Koh, Nt = N2, Nst = S, N0 = N1), .f = rec_Kohyama))) %>%
    #mutate(R_ha_yr = R / (50*int_length)) %>%
    mutate(R_ha_yr = R / (50)) %>%
    filter(R_Koh != Inf) %>% str()
    group_by(int,pft) %>%
    summarise(R_Koh = sum(R_Koh, na.rm = T),
              R_ha_yr = sum(R_ha_yr, na.rm = T)) %>%
    mutate(rec_rate = R_Koh)
    
 
  

#plotting the recruitment observations
rec_benchmarks <- rec_data1 %>% ggplot(mapping = aes(x = int, y = R_Koh, color = pft)) +
  geom_point(size = 5) +
  geom_line() +
  scale_color_manual(values = pft.cols) +
  ylab(label = "recruitment rate (# ind. per ha yr)") +
  xlab(label = "census interval") +
  theme_minimal() +
  adams_theme

makePNG(fig = rec_benchmarks, path_to_output.x = path_to_benchmarking_output, file_name = "rec_benchmarks")


#reshaping the recruitment benchmarks to be in a long format with a time axis so that
#it can be plotted alongside model output.
if(write_benchmark_csv == T){
  write_csv(rec_data1,paste0(path_to_benchmarking_output,"rec_benchmarks_bci.csv"))  
}

int <- c()
int_start <- c()
int_end <- c()

for(i in 1:7){
  int[i] <- i
  int_start[i] <- mean(sp.Rlist_mindbh[[i]]$date1,na.rm = T)
  int_end[i] <- mean(sp.Rlist_mindbh[[i]]$date2,na.rm = T)
}

rec_benchmarks_with_dates <- tibble(int = int, 
                                    int_start = as.Date(int_start,origin = "1960-01-01"), 
                                    int_end = as.Date(int_end,origin = "1960-01-01"))


int <- c()
int_start <- c()
int_end <- c()

dates_tib <- tibble()
for(i in 1:(ncens-1)){
  date.t <-seq.Date(from = rec_benchmarks_with_dates$int_start[i], to = rec_benchmarks_with_dates$int_end[i], by = 1)
  int.t <- rep(i,length(date))
  tmp <- tibble(date = date.t,
                int = int.t)
  dates_tib <- rbind(dates_tib,tmp)
}  

rec_benchmarks_with_dates_long <- dates_tib %>%
  left_join(rec_data1, by = "int") 

if(write_benchmark_csv == T){
  write_csv(rec_benchmarks_with_dates_long, path = "benchmarking/bci_rec_benchmarks_long.csv")
}

print("created benchmarks")

