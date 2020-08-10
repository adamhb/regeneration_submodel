source("parameter_files/parameters_ED2_run.R")
source("model/old_funcs/rec_func_fall_2018.R")


a_rec.yy <- c(rep(0.437e-6,2),rep(1.737e-6,2))
names(a_rec.yy) <- pft_names
b_rec.yy <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_rec.yy) <- pft_names


new_rec_func <- function(a_rec.y = a_rec.yy[PFT], b_rec.y = b_rec[PFT], l) {
  log10_frac_rec <- log10(a_rec.y) + b_rec.y*log10(l)
  frac_rec <- (10^log10_frac_rec)
  return(frac_rec)
}

#mean(input_vars$FSDS) / 1e6 * 0.03 * 183
#mean solar radiation accumulation over 6 months at the forest floor at BCI = 92.26 MJ
avg_SMP <- -60326 
avg_l <- 92.26 # I've said 61 in the past

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
    light = light_regimes,
    annual_transition_rate_new_func = 
      new_rec_func(
        a_rec.y = a_rec.yy[PFT],
        b_rec.y = b_rec[PFT],
        l = light_regimes
      )
    )
    
  light_rec_data <- rbind(light_rec_data,temp)  
}


light_rec_data %>%
  mutate(pft = case_when(
    pft == "earlydi" ~ "early",
    pft == "latedi" ~ "late"
  )) %>%
  dplyr::select(pft, light, annual_transition_rate, annual_transition_rate_new_func) %>%
  rename(old_func = annual_transition_rate, new_func = annual_transition_rate_new_func) %>%
  gather(old_func:new_func,key = "func_type", value = "trans_rate") %>%
  ggplot(aes(x = light, y = trans_rate, color = pft, line_type = func_type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(pft.cols[2],pft.cols[4])) +
  #scale_linetype_manual(values = c("solid","solid","solid")) +
  ylab(label = "annual seedling to \n sapling transition rate") +
  xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  #annotate("text", x = 92, y = 0.09, label = "BCI avg. \n at 3 % light") +
  geom_vline(xintercept = avg_l) +
  #annotate("text", x = avg_l, y = 0.06, label = "avg_l") +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  adams_theme 




PFT <- "earlydi"
l <- 98

log10_frac_rec <- log10(a_rec[PFT]) + b_rec[PFT]*log10(l/avg_l) 

frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day

if(SMP.x < thresh.xx[PFT]){
  frac_rec <- 0
}

C_rec <- frac_rec * seedpool.x

N_rec <- C_rec / Z0

out <- list(frac_rec,C_rec, N_rec)










x <- 20:200
plot(x, x^1.05) 

PFT <- "latedi"

new_rec_func <- function(a_rec.y = -115.34, b_rec.y = 1.05, light) {
  log10_frac_rec <- log10(a_rec.y) + b_rec.y*log10(light)
  frac_rec <- (10^log10_frac_rec)
  return(frac_rec)
}

plot(20:200, new_rec_func(a_rec.x = 1.737e-6, b_rec.x = 1.05, light = 92))




plot(x, new_rec_func(light = 92))


light_rec_data %>%
       mutate(pft = case_when(
         pft == "earlydi" ~ "early",
         pft == "latedi" ~ "late"
       )) 



rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x, SMP.x = avg_SMP){
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}

