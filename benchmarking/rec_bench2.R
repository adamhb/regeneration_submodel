# sp.Mlist[[1]]$N
# sp.Mlist[[1]]$D[2,]
# sp.Mlist[[1]]$rate[2,]
# sp.Mlist[[1]]$
m2_per_ha <- 1e4
pfts <- read_csv('benchmarking/pft_assignments.csv')  

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
  select(int,sp,N1,N2,Ns,Nd,Nr,interval_length, M_Koh, R_Koh) %>%
  drop_na(M_Koh, R_Koh)

dem_data2 <- dem_data1 %>%
  left_join(pfts,by = "sp") %>%
  drop_na(pft) 

write_csv(dem_data2,"benchmarking/dem_data.csv")

per_area_rec_rates <- dem_data2 %>%
  group_by(pft) %>%
  summarise(R_Koh_m2_yr = sum(R_Koh, na.rm = T)) %>% #ind. per m2 per year
  mutate(R_Koh_ha_yr = R_Koh_m2_yr * m2_per_ha) #ind. per ha per year

write_csv(per_area_rec_rates, "benchmarking/obs_per_area_rec_rates.csv")

rec_benchmarks <- dem_data2 %>% 
  ggplot(mapping = aes(x = int, y = R_Koh, color = pft)) +
  geom_point(size = 5) +
  geom_line() +
  scale_color_manual(values = pft.cols) +
  ylab(label = "recruitment rate (# ind. per ha yr)") +
  xlab(label = "census interval") +
  theme_minimal() +
  adams_theme


