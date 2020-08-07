# =======================================================================================
# annual mortality rate of the slowest growing 25% of each species per
# census interval
# =======================================================================================

mort25 <- function(cnsdata, ncens, spc, large_stature_dbh95, sapling_dbh, adult_dbh,climit) {
  
  # If confidence assessment is necessary:
  source("check_binomial_sample.r")

  # Parameters
  do_plot_binomial_pdf <- FALSE  # This will plot out the binomial pdf of the expected 
  
  # climit: The confidence limit, ie this is the maximum acceptable probability
  # that a random sample from a vector of alive and dead plants will deviate
  # more than max_dev
  max_dev <- 0.05  # The half-width of mortality rates within which it is 99% probable 
  # a random mortality rate will occur [/year]
  
  nspc <- length(spc$names)
  
  # Allocate memory
  
  spc$am25 <- matrix(NA, nrow = nspc, ncol = 3)  # <25th growth percentile mortality rates (adults)
  spc$sm25 <- matrix(NA, nrow = nspc, ncol = 3)  # <25th growth percentile mortality rate (saplings)
  
  spc$am25_ul <- rep(NA, times = nspc)  # upper confidence limit
  spc$sm25_ul <- rep(NA, times = nspc)  # upper confidence limit
  spc$am25_ll <- rep(NA, times = nspc)  # lower confidence limit
  spc$sm25_ll <- rep(NA, times = nspc)  # lower confidence limit
  
  # If confidence assessment is necessary:
  source("check_binomial_sample.r")
  
  # Check if there is enough census
  
  if (ncens <= 2) {
    print("Not enough censuses for calculation of M25, need at least 3. Exiting...")
    print("Acknowledge by pressing [enter] to continue")
    line <- readline()
    return(spc)
  }
  

  # Allocate M25 data for each species AND census
  spc$am25_ec <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # am25 for each census
  spc$sm25_ec <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # sm25 for each census
  
  spc$am25_ecul <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # uCI of am25 for each census
  spc$sm25_ecul <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # uCI of sm25 for each census
  spc$am25_ecll <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # lCI of am25 for each census
  spc$sm25_ecll <- matrix(NA, nrow = nspc, ncol = (ncens - 2))  # lCI of sm25 for each census
  
  for (isp in 1:length(spc$names)) {
    
    # First criteria, we are only calculating M25 for species with large maximum
    # stature this is because we use size filters for adult and sapling trees,
    # these size filters are not viable if we are working with bushes and
    # treelet type species
    
    if ( is.finite(spc$dbh95[isp]) & spc$dbh95[isp] >= large_stature_dbh95) {
      
      ids <- which((cnsdata[[1]]$sp %in% spc$names[isp]))
      
      for (i in 2:(ncens - 1)) {
        
        dbh1 <- cnsdata[[i - 1]]$dbh[ids]
        dbh2 <- cnsdata[[i]]$dbh[ids]
        dtime <- (cnsdata[[i]]$date[ids] - cnsdata[[i - 1]]$date[ids])/365.25
        meandtime <- mean(dtime, na.rm = TRUE)
        grate <- (dbh2 - dbh1)/dtime
        rgrate <- log(dbh2/dbh1)/dtime
        statneg1 <- cnsdata[[i - 1]]$status[ids]
        stat1 <- cnsdata[[i]]$status[ids]
        stat2 <- cnsdata[[i + 1]]$status[ids]
        intercept <- 0.9036/10
        slope <- 0.006214
        mindbh <- 1
        pomcut <- 0.05  # maximum acceptable fraction difference between POM in diameter measurements
        err.limit <- 4
        stdev.dbh1 <- slope * dbh1 + intercept
        bad.neggrow <- ifelse(stat1 == "A" & statneg1 == "A", which(dbh2 <= 
                                                                      (dbh1 - err.limit * stdev.dbh1)), FALSE)
        pomdiff <- abs(as.numeric(cnsdata[[i]]$pom[ids]) - as.numeric(cnsdata[[i - 
                                                                                 1]]$pom[ids]))/as.numeric(cnsdata[[i - 1]]$pom[ids])
        
        accept <- rep(FALSE, length(pomdiff))
        include <- which(dbh1 >= mindbh & dbh2 >= mindbh & cnsdata[[i]]$status[ids] == 
                           "A" & cnsdata[[i - 1]]$status[ids] == "A")
        accept[include] <- TRUE
        accept[pomdiff > pomcut] <- FALSE
        accept[bad.neggrow] <- FALSE
        accept[is.na(grate)] <- FALSE
        accept[is.na(dtime)] <- FALSE
        
        # Create Sapling Filter
        accept_sap25 <- accept
        accept_sap25[dbh2 > sapling_dbh] <- FALSE
        accept_sap25[rgrate > spc$srgr25[isp]] <- FALSE
        
        # Create Adult Filter
        accept_ad25 <- accept
        accept_ad25[dbh2 < adult_dbh] <- FALSE
        accept_ad25[rgrate > spc$argr25[isp]] <- FALSE
        
        
        # Calculate mortality rates for saplings m25 <- d25/((a25+d25)*meandtime)
        # ===============================================================================
        
        a25 <- length(which(stat1[accept_sap25] == "A"))
        d25 <- length(which(stat2[accept_sap25] == "D"))
        
        binconf <- check_binomial_sample(d25, n = a25 + d25, max_dev * 
                                           meandtime, climit, spc$names[isp], do_plot_binomial_pdf)
        
#        check_binomial_sample <- function(k_exp, n, max_dev, climit, spc_name, do_plot_binomial_pdf)
        
        # Save to memory if the binomial sample check is TRUE
        if (binconf$isvalid) {
          spc$sm25_ec[isp, (i - 1)] <- d25/((a25 + d25) * meandtime)
          spc$sm25_ecul[isp, (i - 1)] <- binconf$ulim/((a25 + binconf$ulim) * 
                                                         meandtime)
          spc$sm25_ecll[isp, (i - 1)] <- binconf$llim/((a25 + binconf$llim) * 
                                                         meandtime)
        }
        
        # Calculate mortality rates for adults m25 = d25/((a25+d25)*meandtime)
        # ===============================================================================
        
        a25 <- length(which(stat1[accept_ad25] == "A"))
        d25 <- length(which(stat2[accept_ad25] == "D"))
        
        binconf <- check_binomial_sample(d25, n = a25 + d25, max_dev * 
                                           meandtime, climit, spc$names[isp], do_plot_binomial_pdf)
        
        # Save to memory if the binomial sample check is TRUE
        if (binconf$isvalid) {
          spc$am25_ec[isp, (i - 1)] <- d25/((a25 + d25) * meandtime)
          spc$am25_ecul[isp, (i - 1)] <- binconf$ulim/((a25 + binconf$ulim) * 
                                                         meandtime)
          spc$am25_ecll[isp, (i - 1)] <- binconf$llim/((a25 + binconf$llim) * 
                                                         meandtime)
        }
      }
      
      # Save the mean m25
      sm25 <- mean(spc$sm25_ec[isp, ], na.rm = TRUE)
      if (!is.nan(sm25)) {
        spc$sm25[isp,1] <- sm25
        spc$sm25[isp,2] <- mean(spc$sm25_ecll[isp, ], na.rm = TRUE)
        spc$sm25[isp,3] <- mean(spc$sm25_ecul[isp, ], na.rm = TRUE)
      }
      am25 <- mean(spc$am25_ec[isp, ], na.rm = TRUE)
      if (!is.nan(am25)) {
        spc$am25[isp,1] <- am25
        spc$am25[isp,2] <- mean(spc$am25_ecll[isp, ], na.rm = TRUE)
        spc$am25[isp,3] <- mean(spc$am25_ecul[isp, ], na.rm = TRUE)
      }
    }
  }
  
  return(spc)
}
