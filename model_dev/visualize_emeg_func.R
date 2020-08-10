source("create_output/figure_formatting.R")
source("parameter_files/parameters_ED2_run_benchmarking.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"

pft_names <- c("earlydi", "earlydt", "latedi", "latedt")
avg_SMP <- -60326 


emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
  
  
  log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
  
  frac_emerg <- 10^log10_frac_emerg 
  if(frac_emerg > 0.07){frac_emerg <- 0.07}
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


emerg <- c()
SMP_windows <- seq(from = -5000, to = 0, length.out = 100)
pft <- c()
for(p in pft_names){
  for(i in 1:length(SMP_windows)){
    PFT <- p
    SMP_window <- SMP_windows[i]
    tmp <- emerg_func(SMP.x = SMP_window, seedbank.x = 1)$frac_emerg
    emerg <- append(emerg, tmp) 
    pft <- append(pft, p) 
  }
}


viz_emerg <- tibble(pft = pft,
       SMP = rep(SMP_windows,4),
       emerg = emerg) %>%
  ggplot(aes(x = SMP / 1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "mean SMP (Mpa) \n in prior two weeks") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme

makePNG(fig = viz_emerg, path_to_output.x = path_to_output, file_name = "visualize_emergence")


