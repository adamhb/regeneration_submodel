from_new_data <- TRUE
print("creating parameter sensitivity figure...")


if(file.exists('temp/param_sens_data.csv') == F | from_new_data == T){
  print("need to run parameter sensitivity analysis.R")
  source("runs/ED2_run_param_sensitivity.R")
}else(print("making figure with prior run's data"))

source("create_output/figure_formatting.R")

param_sens_data <- read_csv("temp/param_sens_data.csv")
#param_sens_data <- read_csv("temp/param_sens_data_ENSO.csv")

source("create_output/figure_formatting.R")

base_avg_rec <- param_sens_data %>%
  filter(param_changed == "dummy") %>%
  pull(R_avg) %>% sum()

param_sens_data1 <- param_sens_data %>%
  group_by(param_changed) %>%
  summarise(pct_change = (abs(sum(R_avg) - base_avg_rec) / base_avg_rec * 100)) %>%
  filter(param_changed != "dummy")

param_order <- param_sens_data1 %>%
  arrange(abs(pct_change)) %>% pull(param_changed)

param_sens_fig <- param_sens_data1 %>%
  mutate(param_changed = factor(param_changed, levels = 
                                  param_order)) %>%
  ggplot(aes(x=param_changed, y=pct_change)) +
  geom_bar(stat="identity", size = 0.1) +
  coord_flip() +
  theme_minimal() +
  adams_theme +
  xlab("parameter") +
  ylab("pct. change in recruitment rate")

makePNG(fig = param_sens_fig, path_to_output.x = path_to_MS_figures, file_name = "param_sens")
print("FINISHED making parameter sensitivity figure")



# scale_x_discrete(labels = parse(text= c("b_TR" = "b[TR]",
#                                         "b.ML" = "b[ML]",
#                                         "F_seed" = "F[seed]",
#                                         "F_repro" = "F[repro]",
#                                         "Dmax" = "Dmax"))) +
