
plot_srgr95_vs_sm25 <- function(spc) {
  #dev.off()  # Useful sometimes if existing graphics windows cause crankiness
  ids <- which(is.finite(spc$sm25[,1]))
  df <- data.frame(sm25   = spc$sm25[ids,1], 
                 srgr95 = spc$srgr95[ids,1], 
                 sm25_lci = spc$sm25[ids,2], 
                 sm25_uci = spc$sm25[ids,3],
                 srgr95_lci = spc$srgr95[ids,2],
                 srgr95_uci = spc$srgr95[ids,3],
                 pft_rx = spc$pft_rx[ids],
                 pft_ry = spc$pft_ry[ids])

  
  p <- ggplot(df, aes(x=sm25, y=srgr95)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = sm25_lci,xmax = sm25_uci)) + 
  geom_errorbar(aes(ymax = srgr95_lci, ymin = srgr95_uci)) + 
  geom_abline(intercept = 0.04686, slope = 2.48452) +
  geom_point(aes(x = pft_rx, y = pft_ry), colour='red', size=2) +
  coord_equal() + 
  xlab("M25 (saplings) [%/yr]") + 
  ylab("RGR95 (saplings) [cm cm-1 yr-1]") + 
  theme_bw()

  print(p)
  cat("Press any key to continue")
  line <- readline()
}

plot_argr95_vs_am25 <- function(spc) {
  #dev.off()    # Useful sometimes if existing graphics windows cause crankiness
  ids <- which(is.finite(spc$am25[,1]))
  df <- data.frame(am25   = 100 * spc$am25[ids,1], 
                  argr95 = spc$argr95[ids,1], 
                  am25_lci = 100 * spc$am25[ids,2], 
                  am25_uci = 100 * spc$am25[ids,3],
                  argr95_lci = spc$argr95[ids,2],
                  argr95_uci = spc$argr95[ids,3])
  p <- ggplot(df, aes(x=am25, y=argr95)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = am25_lci,xmax = am25_uci)) + 
  geom_errorbar(aes(ymax = argr95_lci, ymin = argr95_uci)) + 
  xlab("M25 (adults) [%/yr]") + 
  ylab("RGR95 (adults) [cm cm-1 yr-1]") + 
  theme_bw()
  print(p)
  cat("Press any key to continue")
  line <- readline()
}