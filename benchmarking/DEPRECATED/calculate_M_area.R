source("benchmarking/forestgeo_benchmark_driver_AHB.r")

#calculating the per area mortality rate for each species

M_perCap <- sp.Mlist[[7]]$rate[1:300,1]
M_perCapnames <- names(M_perCap)
M_perCapDF <- tibble(M_perCap,M_perCapnames)
names(M_perCapDF) <- c("M","sp")

A <- sp.Alist[[7]]$abund$all
sp.names.A <- rownames(sp.Alist[[7]]$abund)
abund <- tibble(sp.names.A,A)
names(abund) <- c("sp","A")

M_area_2010 <- M_perCapDF %>%
  drop_na(M) %>%
  left_join(abund, by = "sp") %>%
  mutate(M_per_m2 = M * A / (5e4*5))

write_csv(M_area_2010, path = "benchmarking/M_area_2010.csv")

#recruitment rate (2010-2015)
R_names <- names(sp.Rlist_mindbh[[7]]$R)
R <- sp.Rlist_mindbh[[7]]$R
R_df <- tibble(sp = R_names, R= R)

write_csv(R_df, path = "benchmarking/R_2010_2015_benchmarking_driver.csv")




