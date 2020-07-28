
source("create_output/figure_formatting.R")

model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

#create folder to store output
path_to_this_run_output <- paste0(path_to_output,run_name,"_",sub(pattern = " ", replacement = "",x = model_run_time_stamp),"/")
dir.create(path = path_to_this_run_output)

base_avg_rec <- param_sens_data %>%
  filter(param_changed == "dummy") %>%
  pull(R_avg) %>% sum()

param_sens_data1 <- param_sens_data %>%
  group_by(param_changed) %>%
  summarise(pct_change = (sum(R_avg) - base_avg_rec) / base_avg_rec * 100) %>%
  filter(param_changed != "dummy") %>%
  mutate(param_changed = case_when(
    (param_changed == "window.x") ~ "H20_mort_window",
    (param_changed == "seed_frac") ~ "F_seed",
    (param_changed == "frac_repro") ~ "F_repro",
    (param_changed == "decay_rate") ~ "S_decay",
    (param_changed == "thresh.xx") ~ "moisture_threshold",
    (param_changed == "background_seedling_mort") ~ "M_background",
    TRUE ~ as.character(param_changed)
  ))

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



makePNG(fig = param_sens_fig, path_to_output.x = path_to_this_run_output, file_name = paste0("param_sens",basename(driver_data_path)))


