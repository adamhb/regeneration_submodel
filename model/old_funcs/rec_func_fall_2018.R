rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x, SMP.x = avg_SMP){
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}