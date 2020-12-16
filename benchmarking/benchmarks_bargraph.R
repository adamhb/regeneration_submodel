#showing the effect of removing midstory trees on recruitment

bench_data %>%
  ggplot(aes(pft,R)) +
  geom_bar(stat = "identity") +
  labs(title = "canopy only") +
  adams_theme

#write.csv(bench_data, file = "benchmarking/benchsummary_T_only.csv")

MandT <- read_csv("benchmarking/benchsummary_MandT.csv") %>%
  mutate(withM = "yes") %>%
  select(-X1)

bench_data %>% mutate(withM = "no") %>%
  rbind(MandT) %>%
  ggplot(aes(pft,R,fill=withM)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "include midstory?") +
  ylab("recruitment rate [ind. ha-1 yr-1]") +
  adams_theme
