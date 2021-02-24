source("parameter_files/parameters_ED2_run.R")
source("model/old_funcs/rec_func_fall_2018.R")

path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"

mean(input_vars$FSDS) / 1e6 * 0.58 * 183
#mean solar radiation accumulation over 6 months at the forest floor at BCI = 92.26 MJ
avg_SMP <- -60326 
avg_l <- 105 # I've said 61 in the past; 1783 is what it should be according to Kobe

light_regimes <- 20:200
light_rec_data <- tibble()


for(i in pft_names[c(1,3)]){
  PFT <- i
  temp <- tibble(
    annual_transition_rate =
      rec_func(l = light_regimes,
               a_rec.x = a_rec[PFT], 
               b_rec.x = b_rec[PFT], 
               avg_l.x = avg_l, 
               seedpool.x = 1, 
               SMP.x = avg_SMP)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
    )
    
  light_rec_data <- rbind(light_rec_data,temp)  
}


light_rec_fig <- light_rec_data %>%
  mutate(pft = case_when(
    pft == "earlydi" ~ "early",
    pft == "latedi" ~ "late"
  )) %>%
  ggplot(aes(x = light, y = annual_transition_rate * 365, color = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = c(pft.cols[2],pft.cols[4])) +
  #scale_linetype_manual(values = c("solid","solid","solid")) +
  ylab(label = "annual seedling to \n sapling transition rate") +
  xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  annotate("text", x = 170, y = 0.15, label = paste0("avg_l = ", avg_l)) +
  annotate("text", x = 92, y = 0.16, label = "BCI avg. \n at 3 % light") +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  adams_theme 


makePNG(light_rec_fig, path_to_output.x = path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/", file_name = "transition_vs_light")