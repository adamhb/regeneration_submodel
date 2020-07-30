source("create_output/figure_formatting.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"

Dmax <- c(934.2857, 846.3182, 556.7179, 581.3786) #maximum diamater (mm), changed from 561 to 581 to jigger the line
names(Dmax) <- pft_names

frac_repro <- c(0.1,0.1,0.1,0.1)#the fraction of NPP that gets allocated to reproduction
names(frac_repro) <- pft_names

prob_repro <- function(k = 0.0125, size_mm, Dmax){
  y <- 1 / (1 + exp(-k*(size_mm - 0.5*Dmax)))
  return(y)
}

efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, Dmax = Dmax[PFT]) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * frac_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}


sizes <- 1:1000 
pft <- c()

for(i in pft_names){
  pft <- append(pft,rep(i,length(sizes)))
}

F_repro_fig <- tibble(F_alloc = c(efrac(N = 1000, co_dbh_ind = sizes, PFT = "earlydi"),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = "earlydt"),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = "latedi"),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = "latedt")),
       pft = pft,
       dbh = rep(sizes,4)) %>%
  ggplot(aes(x = dbh, y = F_alloc, color = pft)) +
  geom_line() +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","solid","solid")) +
  ylab(label = expression(F[repro])) +
  theme_minimal() +
  adams_theme

makePNG(fig = F_repro_fig, path_to_output.x = path_to_output, file_name = "F_repro_fig")
