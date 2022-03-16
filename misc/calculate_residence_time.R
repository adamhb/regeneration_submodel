
source("create_output/figures_for_MS/benchmark_figure_BCI_2008to2014.R")

total_res_time <- function(pft.x){
  
  stock <- full_output %>% filter(yr > 2005, pft == pft.x) %>% pull(seedbank) %>% mean(na.rm = T)
  f_out_trans <- (full_output %>% filter(yr > 2005, pft == pft.x) %>% pull(frac_rec.t) %>% mean(na.rm = T)) * stock
  f_out_mort <- ((full_output %>% filter(yr > 2005, pft == pft.x) %>% pull(light_mort_rate) %>% mean(na.rm = T)) * stock) +
    ((M_background[pft.x] / 365) * stock)
  f_out_total <- f_out_trans + f_out_mort
  T_mean_total <- stock / f_out_total
  T_mean_trans <- stock / f_out_trans
  
  return(c(T_mean_total,T_mean_trans))
  
}

out <- tibble()
n <- c("total","transitioning")
for(p in pft_names){
  for(i in 1:2){
    t <- tibble(pft = p, type = n[i], total_res_time(pft.x = p)[i])
    out <- rbind(out,t)
  }
}

write_csv(out,"misc/residenceTimes.csv")

#full_output$H20_mort_rate %>% summary()

# full_output %>%
#   ggplot(aes(day,seedbank)) +
#   geom_line() +
#   facet_wrap(~pft)

