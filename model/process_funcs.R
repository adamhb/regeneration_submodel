


prob_repro <- function(size_mm,PFT.x){
  a_RA.x <- a_RA[PFT.x]
  b_RA.x <- b_RA[PFT.x]
  y <- (exp(b_RA.x+a_RA.x*size_mm) / (1 + exp(b_RA.x+a_RA.x*size_mm)))
  return(y)
}

efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, PFT.x = PFT) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * F_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}