#benchmarks 11/18/2018

#first step is to run the forestgeo_benchmark_driver_AHB.r script
#make sure that when that script is run the ctfs_at_settings.xml file has the correct settings
#the above commands create the sp.Mlist and sp.Rlist_mindbh lists which are used for benchmarking
#but those lists have all species in the inventory

#step1
#select just the species that we have pft designations for. These are all "canopy trees" according to Powell et al., 2018

rec_data <- data.frame()
for(i in 1:7){
  rec_data <- rbind(rec_data, data.frame(int = rep(i, length(sp.Rlist_mindbh[[i]]$R)), sp = names(sp.Rlist_mindbh[[i]]$R), R = sp.Rlist_mindbh[[i]]$R, int_length = sp.Rlist_mindbh[[i]]$time, M_rate = sp.Mlist[[i]]$rate[1:300,1]))
  print(paste("done",i))
}

#adjusting the recruitment to account for recruits that may have died before the census
rec_data <- rec_data %>% mutate(R_adjust = R / (1 - M_rate))

#merging with the species for which we have pft designations
rec_data <- merge(pfts_nov_2018, rec_data, by = "sp") #this creates 182 species

#aggregating recruits to the pft level
rec_data <- rec_data %>% group_by(int, pft) %>%
  summarise(R = sum(R_adjust, na.rm = T), time = mean(int_length, na.rm = T)) %>%
  mutate(rec_rate = R / (time * 50))


#plotting
rec_data %>% ggplot(mapping = aes(x = int, y = rec_rate, color = pft)) +
  geom_line()


#converting to a format that can be graphed with output data
str(full_output)

dates <- as.Date(full_output$date)
pfts_in_output <- full_output$pft
length(pfts_in_output)

ints <- c()

ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) < 2005] <- 5
ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) >= 2005 & as.numeric(substr(as.character(dates),start = 1, stop = 4)) < 2010] <- 6
ints[as.numeric(substr(as.character(dates),start = 1, stop = 4)) >= 2010] <- 7

length(ints)

graphable_bench_data <- data.frame(date = dates, int = ints, pft = pfts_in_output)
graphable_bench_data <- merge(graphable_bench_data, rec_data, by = c("int","pft"))

graphable_bench_data_all_pfts <- graphable_bench_data %>% group_by(date) %>% summarise(R_total = sum(R), time = mean(time)) %>% mutate(total_rec = R_total/ (time * 50))


str(graphable_bench_data)

ggplot(data = graphable_bench_data, mapping = aes(x = date, y = rec_rate, color = pft)) + geom_point()



