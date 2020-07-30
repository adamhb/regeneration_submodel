source("parameter_files/parameters_ED2_run.R")


#mean(input_vars$FSDS) / 1e6 * 0.03 * 183
#mean solar radiation accumulation over 6 months at the forest floor at BCI = 92.26 MJ
avg_SMP <- -60326 
avg_l <- 92.26 # I've said 61 in the past

light_regimes <- 20:200
light_rec_data <- tibble()

for(i in pft_names[c(1,3)]){
  PFT <- i
  temp <- tibble(
    annual_transition_rate = c(
      rec_func(l = 20:200,
               a_rec.x = a_rec[PFT], 
               b_rec.x = b_rec[PFT], 
               avg_l.x = avg_l, 
               seedpool.x = 1, 
               SMP.x = avg_SMP)$frac_rec * 365
    ),
    pft = rep(i,length(light_regimes)),
    light = light_regimes)
    
  light_rec_data <- rbind(light_rec_data,temp)  
}


light_rec_data %>%
  mutate(pft = case_when(
    pft == "earlydi" ~ "early",
    pft == "latedi" ~ "late"
  )) %>%
  ggplot(aes(x = light, y = annual_transition_rate, color = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = c(pft.cols[2],pft.cols[4])) +
  #scale_linetype_manual(values = c("solid","solid","solid")) +
  ylab(label = "annual seedling to \n sapling transition rate") +
  xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  annotate("text", x = 92, y = 0.09, label = "BCI avg. \n at 3 % light") +
  geom_vline(xintercept = avg_l) +
  annotate("text", x = avg_l, y = 0.06, label = "avg_l") +
  geom_vline(xintercept = 61) +
  annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  adams_theme 




