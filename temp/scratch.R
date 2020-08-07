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
  
  
  
  
  
