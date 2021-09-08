fileConn<-file("ED2_vars.txt")
sink('ED2_vars.txt')

writeLines(print(mydata1), fileConn)

print(head(mydata1, 250))

grep(pattern = "NPP", x = names(mydata1), value = T)

close(fileConn)

options(max.print=999999)



NPPDaily <- mydata1[["MMEAN_NPPDAILY"]][]

NPPcroot <- mydata1[["MMEAN_NPPCROOT"]][]
NPPfroot <- mydata1[["MMEAN_NPPFROOT"]][]
NPPleaf <- mydata1[["MMEAN_NPPLEAF"]][]
NPPsapwood <- mydata1[["MMEAN_NPPSAPWOOD"]][]
NPPseeds <- mydata1[["MMEAN_NPPSEEDS"]][]
NPPwood <- mydata1[["MMEAN_NPPWOOD"]][]

sum(NPPcroot, NPPfroot, NPPleaf, NPPsapwood,NPPseeds, NPPwood)

AGE            = mydata1[["SLZ"]][]





unique(summary_data$start_date)



input_data %>%
  group_by(pft) %>%
  summarise(dbh_pft = mean(dbh))

full_output %>%
  group_by(pft) %>%
  summarise(npp = mean(NPP))

ED2_data %>%
  group_by(pft) %>%
  summarise(agb= mean(agb_pft))


full_output %>%
  filter(pft == "earlydi",
         date > as.Date("2003-01-01"),
         date < as.Date("2005-01-01")) %>%
  select(day, date, seedbank) %>%
  arrange(date)
  

summary_data %>%
  filter(pft == "earlydi") %>%
  str()


patch_level_light %>%
  filter(bin < 11) %>% pull(lightZ0) %>% quantile(probs = c(.90,.99,.999))
  ggplot(mapping = aes(lightZ0)) +
  geom_histogram()

  
  lseq <- function(from=1, to=100000, length.out=6) {
    # logarithmic spaced sequence
    # blatantly stolen from library("emdbook"), because need only this
    exp(seq(log(from), log(to), length.out = length.out))
  }

  lseq(from = 0.05, to = 1, length.out = 10)
  
  
  
  patch_level_light %>%
    filter(bin < 11) %>%
    filter(patch_age < 1000) %>%
    
    #group_by(bin) %>%
    summarise(mean_patch_age = mean(patch_age), mean_light = mean(lightZ0)) %>%
    ggplot(aes(mean_patch_age,mean_light)) + geom_point()
  
  
  
  
  input_vars$nppseed_pft_day[1:10] / input_vars$NPP
  
  input_data$nppseed_pft_day[1:10] / input_data$NPP
  
  
  pfts_assgn <- read_csv('benchmarking/pft_assignments.csv') 
pfts_assgn %>%
  filter(pft == "earlydi" | pft == "earlydt") %>% print(n = 300)
  






full_output %>%
  filter(yr == "2010", PFT_record == "LD_DT") %>%
  pull(R) %>% sum() #39-











# h <- 500
# gridExtra::grid.arrange(F_repro_fig,viz_emerg,viz_light_mort,
#              light_rec_fig,viz_H20_mort,moisture_def_fig, 
#              layout_matrix = rbind(c(1,2,3),c(4,5,6)),
#              heights = c(4,4))
# 
# library(gtable)
# grob()
# 
# grob_Frepro <- ggplotGrob(F_repro_fig)
# grob_Fviz <- ggplotGrob(viz_emerg)
# grob_lightmort <- ggplotGrob(viz_light_mort)
# 
# q <- cbind(grob_Frepro,grob_Fviz,grob_lightmort, size = "first")
# plot(q)
# 
# png(filename = "temp/test.png", width = 8,height = 8, res = 100)
# q
# dev.off()
# 
# makePNG(fig = q, path_to_output.x = path_to_output, file_name = "viz_H20_mort")
# 
# q$widths <- unit.pmax(grob_Frepro$widths, grob_Frepro$widths)
# 
# 
# gtable
# 



nummies <- c(1,2,3,4)

modify_if(.x = nummies,.p = nummies==2,.f = function(x){x+10})


read_csv("temp/N_recs_per_yr_default_params.csv") %>%
  filter(pft == "ST_DT") %>%
  pull(submodel) %>% summary()


full_out



