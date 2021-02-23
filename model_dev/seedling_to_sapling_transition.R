#derive seedling to sapling transition function
source("utils/supporting_funcs.R")

BCImeanTOC <- 16.7 #MJ m-2 day-1
BCImeanUnderstory <- BCImeanTOC* 0.02 #MJ m-2 day-1
BCImeanUnderstory.W_TR <- BCImeanUnderstory * W_TR # MJ m-2 W_TR-1

#deriving a.TR: the mean transition rate under mean light
a.TR.LD <- (0.5/365) %>% #the default fraction of the seed bank that emerges in FATES and CLM(ED)
  `/` (BCImeanUnderstory^b_TR["LD_DI"]) 
a.TR.ST <- (0.5/365) %>% #the default fraction of the seed bank that emerges in FATES and CLM(ED)
  `/` (BCImeanUnderstory^b_TR["ST_DI"]) 

a_TR <- c(rep(a.TR.LD,2),rep(a.TR.ST,2))
names(a_TR) <- pft_names
print("a.TR =")
print(a_TR)

#rec function
rec_func <- function(a_TR.x = a_TR[PFT], b_TR.x = b_TR[PFT], l, SMP.x, seedpool.x){
  frac_rec <- a_TR.x * l^b_TR.x
  if(SMP.x < psi_crit[PFT]){
    frac_rec <- 0
  }
  C_rec <- frac_rec * seedpool.x
  N_rec <- C_rec / Z0
  out <- list(frac_rec,C_rec, N_rec)
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out) 
}

#visualize rec function
#creating range of light levels from 0 to TOC at BCI
RIs <- seq(from = 0, to = 100, length.out = 100)/100
light_MJ_per_m2_day <- BCImeanTOC * RIs

#use rec function to create range of transition rates

light_rec_data3 <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    daily_transition_rate =
      rec_func(l = light_MJ_per_m2_day,
                a_TR.x = a_TR[PFT], 
                b_TR.x = b_TR[PFT], 
                seedpool.x = 1, 
                SMP.x = -0.001)$frac_rec,
    pft = rep(i,length(light_MJ_per_m2_day)),
    light = light_MJ_per_m2_day
  )
  light_rec_data3 <- rbind(light_rec_data3,temp)  
}

light_rec_fig <- light_rec_data3 %>%
  ggplot(aes(x = light, y = daily_transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition rate") +
  scale_x_continuous(limits = c(0,BCImeanTOC * 0.25)) +
  scale_y_continuous(limits = c(0,0.7)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("solar rad. at seedling layer", paste("[MJ m"^"-2"," day"^"-1","]")))) +
  labs(title = "Seedling to sapling transition") +
  geom_vline(xintercept = BCImeanTOC * 0.2, linetype = "dashed") +
  #annotate("text", x = 2, y = 0.1, label = "mean light at \n BCI seedling layer", size = 5) +
  geom_vline(xintercept = BCImeanTOC * 0.02, linetype = "dotted") +
  theme_minimal() +
  multipanel_theme

print("made light_rec_fig")
