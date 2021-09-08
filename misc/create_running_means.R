
W_emerg <- W_ML

SMPforRunning <- input_data %>% filter(pft == "LD_DT") %>% pull(SMP)
solarRadShortDir <- input_data %>% filter(pft == "LD_DT") %>% pull(light)

write_csv(tibble(sr = solarRadShortDir,smp = SMPforRunning),path = "~/cloud/gdrive/FATES/Earth-System-Model-Tools/data/ED_input_example.csv")

index <- 1:length(input_data %>% filter(pft == "LD_DT") %>% pull(SMP))

runningMean <- c()
for(i in index){
  if ((i-round(W_emerg)/2) > 1) {
    tmp <- mean(input_vars$SMP[(i-round(W_emerg)/2):i])
  } else {
    tmp <- 0
  }
  runningMean <- append(runningMean,tmp)
}

df <- tibble(x = index, rM = runningMean, smp = SMPforRunning)

ggplot(data = df, mapping = aes(index,smp)) +
  geom_line() +
  geom_line(data = df, mapping = aes(index,runningMean), color = "red") +
  scale_x_continuous(limits = c(0,500)) +
  theme_minimal()