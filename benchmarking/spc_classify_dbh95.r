# Find and classify species by the 95% percentile of their dbh

spc_classify_dbh95 <- function(spc, cnsdata) {
  
  nspc <- length(spc$names)          
  spc$dbh95 <- rep(0, times = nspc)  # allocate memory

  for (isp in seq_len(nspc)) {

    ids <- which((cnsdata[[1]]$sp %in% spc$names[isp]))
    
    for (i in seq_len(ncens)) {
      ids2 <- which(cnsdata[[i]]$status[ids] == "A")  # select only live trees
      tdbh95 <- quantile(cnsdata[[i]]$dbh[ids[ids2]], probs = 0.95, na.rm = TRUE)  # find 95% of dbh in each census
      if (!is.na(tdbh95[[1]] > 0)) {
        spc$dbh95[isp] <- max(spc$dbh95[isp], tdbh95[[1]])  # assign 95% dbh if census i values are greatest
      }
    }
    if(spc$dbh95[isp]==0){spc$dbh95[isp]=NA}
  }
  
  return(spc)   # returns spc data.frame with column for 95% percentile dbh
}
