taper_correction <- function(cnsdata, site, ncens) {
  
  if (site == "bci") {
    print("Applying Taper Corrections to BCI")
  } else {
    stop("Taper correction is not available yet...Please set do_tapercorr to FALSE")
  }
  
  for (i in seq_len(ncens)) {
    
    print(paste("Census:", i, "of", ncens))
    if (site == "bci") {
      
      # ========================================================================================
      # Perform taper corrections (BCI) Apply the correction per Cushman et al.
      # 2014 (Methods in Ecology and Evolution doi: 10.1111/2041-210X.12187) and
      # Metcalf and Clark 2008 (Journal of Tropical Ecology,25(1)) log(b1) =
      # log(diameter)+log(hom)+spcor d_ = D exp(-b1*(h-1.3))
      # =======================================================================================
      
      taper_int <- -2.0205
      taper_dslp <- -0.5053
      taper_hslp <- 0.3748
      taper_spc <- c("alsebl", "anacex", "brosal", "cavapl", "ceibpe", 
                     "diptpa", "huracr", "quaras", "tab2ar", "tet2pa")
      taper_cor <- c(0.144, 0.4371, 0.3874, 0.7982, 0.0495, 0.015, 0.0086, 
                     0.4865, 0.3576, -0.5141)
      taper_cor <- taper_cor + taper_int
      
      # Default case, perform corrections on all Filter to see if the data has
      # finite values?
      tp_ids <- which(!(cnsdata[[i]]$sp %in% taper_spc) & (cnsdata[[i]]$hom > 
                                                             1.3001 | cnsdata[[i]]$hom < 1.2999))
      print(paste("Taper corrections for Non-Specifics ", "N=", length(tp_ids)))
      
      b1 <- exp(taper_dslp * log(cnsdata[[i]]$dbh[tp_ids]) + taper_hslp * 
                  log(cnsdata[[i]]$hom[tp_ids]) + taper_int)
      cnsdata[[i]]$dbh[tp_ids] <- cnsdata[[i]]$dbh[tp_ids] * exp(b1 * 
                                                                   (cnsdata[[i]]$hom[tp_ids] - 1.3))
      cnsdata[[i]]$hom[tp_ids] <- 1.3
      
      
      for (isp in seq_along(taper_spc)) {
        print(paste("Taper corrections for ", taper_spc[isp], "N=", length(tp_ids)))
        tp_ids <- which((cnsdata[[i]]$sp %in% taper_spc[isp]) & 
                          (cnsdata[[i]]$hom > 1.3001 | cnsdata[[i]]$hom < 1.2999))
        # Apply the correction as per Cushman et al. 2014 (Methods in Ecology and
        # Evolution doi: 10.1111/2041-210X.12187) and Metcalf and Clark 2008
        # (Journal of Tropical Ecology,25(1)) log(b1) = log(diameter)+log(hom)+spcor
        # d_ = D exp(-b1*(h-1.3))
        
        b1 <- exp(taper_dslp * log(cnsdata[[i]]$dbh[tp_ids]) + taper_hslp * 
                    log(cnsdata[[i]]$hom[tp_ids]) + taper_cor[isp])
        cnsdata[[i]]$dbh[tp_ids] <- cnsdata[[i]]$dbh[tp_ids] * 
          exp(b1 * (cnsdata[[i]]$hom[tp_ids] - 1.3))
        cnsdata[[i]]$hom[tp_ids] <- 1.3
      }
    }
  }
  return(cnsdata)
}
