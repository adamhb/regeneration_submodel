#benchmarks 11/18/2018

#first step is to run the forestgeo_benchmark_driver_AHB.r script
#make sure that when that script is run the ctfs_at_settings.xml file has the correct settings
#the above commands create the sp.Mlist and sp.Rlist_mindbh lists which are used for benchmarking
#but those lists have all species in the inventory

#step1
#select just the species that we have pft designations for. These are all "canopy trees" according to Powell et al., 2018

write_benchmark_csv <- T

path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"


  library(tidyverse)
  library(lubridate)
  source("benchmarking/forestgeo_benchmark_driver_AHB.r")
  source("benchmarking/assigning_pfts.R")
  source("create_output/figure_formatting.R")
  
load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"bci.full8.rdata"))
pfts <- read_csv('benchmarking/pft_assignments.csv')


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
  select(-X1) %>%
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
  
  rec_Kohyama <- function(Ma,Nt,Nst,N0){
      R <- Ma*(Nt-Nst)/(N0-Nst)
      return(R*1e4)
    }
 
  #adjusting the recruitment to account for recruits that may have died before the census
  #rec_data <- rec_data %>% mutate(R_adjust = R / (1 - M_rate))
  
  #merging with the species for which we have pft designations
  rec_data1 <- rec_data %>% 
    left_join(pfts_nov_2018, by = "sp") %>%
    drop_na(pft) %>% 
    mutate(M_rate_area = M_rate * N1 / (5e4*int_length)) %>% 
    mutate(R_Koh = unlist(pmap(list(Ma = M_rate_area, Nt = N2, Nst = S, N0 = N1), .f = rec_Kohyama))) %>%
    mutate(R_ha_yr = R / (50*int_length)) %>%
    filter(R_Koh != Inf) %>%
    group_by(int,pft) %>%
    summarise(R_Koh = sum(R_Koh, na.rm = T),
              R_ha_yr = sum(R_ha_yr, na.rm = T)) %>%
    mutate(rec_rate = R_Koh)
    
 
  rec_data <- rec_data1
  
  
# str(rec_data1)
#   
#   mean_M_rate_area_per_pft <- rec_data1 %>%
#     filter(int %in% c(6,7)) %>%
#     drop_na(M_rate_area) %>%
#     filter(M_rate_area != Inf) %>%
#     group_by(pft) %>%
#     summarise(Ma_pft = mean(M_rate_area, na.rm = T))
# 
#   
#   
#  rec_data2 <- rec_data1 %>%
#     left_join(mean_M_rate_area_per_pft, by = "pft") %>%
#     group_by(int,pft) %>%
#     summarise(N1 = sum(N1),
#               N2 = sum(N2),
#               S = sum(S),
#               int_length = mean(int_length, na.rm = T),
#               M = sum(Ma_pft),
#               R = sum(R)) %>%
#    mutate(R_ha_yr = R / (50*int_length)) %>%
#     mutate(R_Koh = unlist(pmap(list(Ma = M, Nt = N2, Nst = S, N0 = N1), .f = rec_Kohyama)))
#     
#  
# 
#  
#  
#  
#  
#  
#  
#  
#  
#  
#  
#  
#  
#   
#   
#   
#   
#   
#   mean_M_rate_area_per_sp <- rec_data1 %>%
#     #filter(int %in% c(6,7)) %>%
#     group_by(Latin) %>%
#     summarise(Ma = mean(M_rate_area)) %>%
#     left_join(pfts, by = "Latin") %>%
#     left_join(mean_M_rate_area_per_pft, by = "pft") %>%
#     mutate(Ma2 = case_when(
#       (Ma == Inf) ~ Ma_pft,
#       is.na(Ma) ~ Ma_pft,
#       TRUE ~ Ma
#     )) %>%
#     select(sp,Ma2)
#   
#   
# #recruitment based on Kohyama  
# # rec_Kohyama <- function(Ma,Nt,Nst,N0,t){
# #     R <- Ma*(Nt-Nst)*(N0/Nst)^(1/t) / (N0-Nst)
# #   return(R)
# #   }  
#   
# rec_Kohyama <- function(Ma,Nt,Nst,N0){
#   R <- Ma*(Nt-Nst)/(N0-Nst)
#   return(R*1e4)
# }
# 
# rec_data1 %>%
#   left_join(mean_M_rate_area_per_sp, by = "sp") %>%
#   mutate(R_ha_yr_original = R/(50*int_length)) %>% 
#   mutate(R_Kohyama_ha_yr = unlist(pmap(list(Ma = Ma2, Nt = N2, Nst = S, N0 = N1), .f = rec_Kohyama))) %>% 
#   filter(R_Kohyama_ha_yr != Inf) %>%
#   group_by(int,pft) %>% 
#   summarise(R_Kohyama_ha_yr_pft = sum(R_Kohyama_ha_yr, na.rm = T))
#   
# 
# 
# rec_data1 %>%
#   left_join(mean_M_rate_area_per_sp, by = "sp") %>%
#   mutate(R_a = pmap(list(Ma = Ma2, Nt = N2, Nst = S, N0 = N1, t = int_length), .f = rec_Kohyama))
#   
#   
#   
# df4 <- 
#   tribble(~mean, ~sd, ~n,
#           1,  0.03, 2,
#           10, 0.1,  4,
#           5,  0.1,  4)
#   
#   
#   #pft-specific mortality rates
#   # pft.level.M <- rec_data %>%
#   #   filter(M_rate != Inf) %>%
#   #   group_by(pft) %>%
#   #   summarise(mrate.pft = mean(M_rate, na.rm = T))
#   # 
#   # write.csv(x = pft.level.M, "temp/mort_rates.csv")
#   
#  # N_dead_per_year_per_pft <- rec_data %>% group_by(int,pft) %>%
#  #    drop_na(a,M_rate,R,R_adjust) %>%
#  #    filter(M_rate != Inf) %>%
#  #    summarise(a = sum(a)/50,
#  #              m = mean(M_rate),
#  #              r = sum(R)/50,
#  #              ra = sum(R_adjust)/50) %>%
#  #    mutate(n_dead = m*a) %>%
#  #    group_by(pft) %>%
#  #    summarise_if(.predicate = is.numeric, .funs = mean) #%>% pull(n_dead) %>% median()
#  #              
#  # write.csv( x = N_dead_per_year_per_pft , "temp/mort_rates.csv")
#  # 
#  
#   
#   #aggregating recruits to the pft level
#   rec_data <- rec_data %>% group_by(int, pft) %>%
#     summarise(R = sum(R_adjust, na.rm = T), time = mean(int_length, na.rm = T)) %>%
#     mutate(rec_rate = R / (time * 50)) # to get units of recruits per ha per year
# 


#plotting
rec_benchmarks <- rec_data1 %>% ggplot(mapping = aes(x = int, y = R_Koh, color = pft)) +
  geom_point(size = 5) +
  geom_line() +
  scale_color_manual(values = pft.cols) +
  ylab(label = "recruitment rate (# ind. per ha yr)") +
  xlab(label = "census interval") +
  theme_minimal() +
  adams_theme

makePNG(fig = rec_benchmarks, path_to_output.x = path_to_benchmarking_output, file_name = "rec_benchmarks")

  
if(write_benchmark_csv == T){
  write_csv(rec_data,paste0(path_to_benchmarking_output,"rec_benchmarks_bci.csv"))  
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

#expanding this
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
  left_join(rec_data, by = "int") 

if(write_benchmark_csv == T){
  write_csv(rec_benchmarks_with_dates_long, path = "benchmarking/bci_rec_benchmarks_long.csv")
}

print("created benchmarks")
# 
# 
# head(rec_data)
# 
# #converting to a format that can be graphed with output data from the submodel
# 
# dates <- as.Date(full_output$date)
# pfts_in_output <- full_output$pft
# length(pfts_in_output)
# 
# ints <- c()
# 
# ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) < 2005] <- 5
# ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) >= 2005 & as.numeric(substr(as.character(dates),start = 1, stop = 4)) < 2010] <- 6
# ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) >= 2010] <- 7
# 
# length(ints)
# 
# graphable_bench_data <- data.frame(date = dates, int = ints, pft = pfts_in_output)
# graphable_bench_data <- merge(graphable_bench_data, rec_data, by = c("int","pft"))
# 
# graphable_bench_data_all_pfts <- graphable_bench_data %>% group_by(date) %>% summarise(R_total = sum(R), time = mean(time)) %>% mutate(total_rec = R_total/ (time * 50))
# 
# 
# str(graphable_bench_data)
# 
# ggplot(data = graphable_bench_data, mapping = aes(x = date, y = rec_rate, color = pft)) + geom_point()



