# ============================================================================
# Identify forest geo species that are not hardwoods This script requires
# that the CTFS species table has been loaded
# ============================================================================

filter_spcs <- function(cnsdata, ncens, AllCTFSSpecies ) {

  
  
  ir <- 0
  rmlist <- rep(0, times = 10000)
  genera <- AllCTFSSpecies$Genus
  family <- AllCTFSSpecies$Family
  # Create species list of Ficus
  for (ig in seq_along(genera)) {
    if (match("Ficus", genera[ig], nomatch = 0) == 1) {
      ir <- ir + 1
      rmlist[ir] <- AllCTFSSpecies$sp[ig]
    }
  }
  # Create species list of Ficus
  for (im in seq_along(family)) {
    if (match("Arecaceae", family[im], nomatch = 0) == 1) {
      ir <- ir + 1
      rmlist[ir] <- AllCTFSSpecies$sp[im]
    }
  }
  
  # For black-listed species, change DBH to NA and status to D
  nrlist <- ir
  rm_ids <- which(( cnsdata[[1]]$sp %in% rmlist[1:nrlist]))

  for (i in seq_len(ncens)) {
    print(paste("Filtering census", i, "of", ncens))
    cnsdata[[i]]$status[rm_ids] <- "M"
    cnsdata[[i]]$dbh[rm_ids] <- NA
  }
  
  return(cnsdata)
}
