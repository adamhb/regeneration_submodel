source("create_output/figure_formatting.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"

pft_names <- c("earlydi", "earlydt", "latedi", "latedt")


Dmax <- c(934.2857, 846.3182, 556.7179, 561.3786) #maximum diamater (mm), changed from 561 to 591 to jigger the line
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
       PFT = pft,
       dbh = rep(sizes,4)) %>%
  ggplot(aes(x = dbh, y = F_alloc, color = PFT, linetype = PFT)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "solid","solid","dashed")) +
  ylab(label = "fraction of NPP \n allocated to reproduction") +
  labs(title = "Allocation to reproduction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        strip.text.x = element_text(size = 20),
        axis.title.x = element_text (size = 22), # change the axis title
        axis.title.y = element_text (size = 22),
        axis.title.y.right = element_text (size = 25, color = pft.cols[2]),
        axis.text.x = element_text (size = 20, colour = "black"),
        axis.text.y = element_text (size = 20, colour = "black"),
        legend.position = c(.8,.4),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
        #legend.key.size = unit(2,"line")) +
  guides(color = guide_legend(override.aes = list(size=10)),
         fill=guide_legend(title="PFT"))
  
  


makePNG(fig = F_repro_fig, path_to_output.x = path_to_output, file_name = "F_repro_fig")
