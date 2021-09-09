#this script derives parameters for the seedling to sapling transition function

source('utils/supporting_funcs.R')
source('model/process_funcs.R')
source('runs/generate_input_data.R')

#determine light levels at BCI
BCImeanTOC <- 7.524879 #MJ m-2 day-1 of PAR at the top of canopy
BCImeanUnderstory <- BCImeanTOC* 0.02 #MJ m-2 day-1
BCImean_20_pct_gap <- BCImeanTOC* 0.2


#aTR is a coefficient in the seedling to sapling transition rate power function (Eqn 8). Its 
#value was back-calculated using Eqn 8 and assuming the mean transition rate was equal to the
#current transition out of the seed bank to the seedling pool in VDMs (Fisher et al., 2015) 
#under mean light levels at BCI. Although this is based on a different demographic stage, this parameter is not equivalent to a transition probability because individual seedlings are not tracked. We use this parameter here as a starting point, but it should be better constrained in future work. 
#See discussion of main text for more discussion on this.

# Fisher RA, Muszala S, Verteinstein M, Lawrence P, Xu C, McDowell NG, Knox RG, Koven C, Holm
#J, Rogers BM, et al. 2015. Taking off the training wheels: The properties of a dynamic 
#vegetation model without climate envelopes, CLM4.5(ED). Geoscientific Model Development 8: 
#3593–3619.

annual_transition_prob_in_FATES <- 0.5

a.TR.LD <- (annual_transition_prob_in_FATES/365) %>% #the default fraction of the seed bank that emerges in FATES and CLM(ED)
  `/` (BCImeanUnderstory^b_TR["LD_DI"]) 
a.TR.ST <- (annual_transition_prob_in_FATES/365) %>% #the default fraction of the seed bank that emerges in FATES and CLM(ED)
  `/` (BCImeanUnderstory^b_TR["ST_DI"]) 

a_TR <- c(rep(a.TR.LD,2),rep(a.TR.ST,2))
names(a_TR) <- pft_names
print("a.TR =")
print(a_TR)

#Note that the values for b_TR were taken directly from Ruger et al., 2009
#Rüger N, Huth A, Hubbell SP, Condit R. 2009. Response of recruitment to light availability 
#across a tropical lowland rain forest community. Journal of Ecology 97: 1–3.


#create light data used to
#visualize the seedling to sapling transition function
#as a function of light.
RIs <- seq(from = 0, to = 100, length.out = 100)/100
light_MJ_per_m2_day <- BCImeanTOC * RIs #of PAR

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
  ggplot(aes(x = light, y = daily_transition_rate, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "daily seedling to \n sapling transition rate") +
  scale_x_continuous(limits = c(0,BCImeanTOC * 0.25)) +
  scale_y_continuous(limits = c(0,0.02)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop(paste("forest floor light [MJ PAR m"^"-2"," day"^"-1","]"),"mean over prior 2 months"))) +
  labs(title = "Light & \n sapling recruitment") +
  geom_vline(xintercept = BCImeanTOC * 0.2, linetype = "dashed") +
  #annotate("text", x = 2, y = 0.1, label = "mean light at \n BCI seedling layer", size = 5) +
  geom_vline(xintercept = BCImeanTOC * 0.02, linetype = "dotted") +
  theme_minimal() +
  multipanel_theme

print("made light_rec_fig")
