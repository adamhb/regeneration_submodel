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
  
  
  rec_data <- data.frame()
  
  for(i in 1:7){
    rec_data <- rbind(rec_data, data.frame(int = rep(i, length(sp.Rlist_mindbh[[i]]$R)), 
                                           sp = names(sp.Rlist_mindbh[[i]]$R), 
                                           R = sp.Rlist_mindbh[[i]]$R, 
                                           int_length = sp.Rlist_mindbh[[i]]$time, 
                                           M_rate = sp.Mlist[[i]]$rate[1:300,1],#))
                                           a = as.numeric(sp.Alist[[i]]$abund$all[1:300])))
    print(paste("done",i))
  }
  
  
  #adjusting the recruitment to account for recruits that may have died before the census
  rec_data <- rec_data %>% mutate(R_adjust = R / (1 - M_rate))
  
  #merging with the species for which we have pft designations
  rec_data <- merge(pfts_nov_2018, rec_data, by = "sp") 
  
  
  
  #pft-specific mortality rates
  pft.level.M <- rec_data %>%
    filter(M_rate != Inf) %>%
    group_by(pft) %>%
    summarise(mrate.pft = mean(M_rate, na.rm = T))
  
  
 N_dead_per_year_per_pft <- rec_data %>% group_by(int,pft) %>%
    drop_na(a,M_rate,R,R_adjust) %>%
    filter(M_rate != Inf) %>%
    summarise(a = sum(a)/50,
              m = mean(M_rate),
              r = sum(R)/50,
              ra = sum(R_adjust)/50) %>%
    mutate(n_dead = m*a) %>%
    group_by(pft) %>%
    summarise_if(.predicate = is.numeric, .funs = mean) #%>% pull(n_dead) %>% median()
              
  
 
 
  
  #aggregating recruits to the pft level
  rec_data <- rec_data %>% group_by(int, pft) %>%
    summarise(R = sum(R_adjust, na.rm = T), time = mean(int_length, na.rm = T)) %>%
    mutate(rec_rate = R / (time * 50)) # to get units of recruits per ha per year



#plotting
rec_benchmarks <- rec_data %>% ggplot(mapping = aes(x = int, y = rec_rate, color = pft)) +
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



