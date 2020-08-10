spc_classify_rgr <- function(cnsdata, ncens, spc, large_stature_dbh95, sapling_dbh, 
                             tree_dbh, climit, sitename) {
  
  # Note we are using a flag called spc$valid to identify if this species is
  # amenable to provide traits space information to filters have already been
  # passed: 1) does the dbh95 of the plant exceed a threshold to indicate it
  # is not a bush or a treelet and thus has a broad range of dbh measurements?
  # and 2) does it have enough samples to enable confident statistics?
  
  nspc <- length(spc$names)
  spc$argr95 <- matrix(NA, nrow = nspc, ncol = 3) # Relative growth rates of trees (dbh>=10cm or whatever cutoff the user supplied)
  spc$argr90 <- matrix(NA, nrow = nspc, ncol = 3) # The three dimensions are the quantile and 2 CI's
  spc$argr50 <- matrix(NA, nrow = nspc, ncol = 3)
  spc$argr25 <- matrix(NA, nrow = nspc, ncol = 3)
  
  spc$srgr95 <- matrix(NA, nrow = nspc, ncol = 3)  # Relative growth rates of saplings trees (dbh<=5cm)
  spc$srgr90 <- matrix(NA, nrow = nspc, ncol = 3)
  spc$srgr50 <- matrix(NA, nrow = nspc, ncol = 3)
  spc$srgr25 <- matrix(NA, nrow = nspc, ncol = 3)
  
  do_bootstrap_ci <- TRUE   # Set to true if you want to boot-strap the samples for CIs on the order stats
  nboot <- 2000 # Number of bootstrap samples (samples will maintain original size)
  if(do_bootstrap_ci){
      bs95 <- rep(NA, time=nboot)
      bs90 <- rep(NA, time=nboot)
      bs50 <- rep(NA, time=nboot)
      bs25 <- rep(NA, time=nboot)}
  
  spc$argrN <- rep(NA, times = nspc)
  spc$srgrN <- rep(NA, times = nspc)
  
  #rare_n <- 100 * (ncens - 1)  # This is the size of the subset for rarefication
  #rens_n <- 100  # Number of realizations in the rarefication subset
  
  for (isp in seq_along(spc$names)) {
    
    if (spc$valid[isp]) {
      
      print(paste("Growth Rate Statistics for", spc$names[isp]))
      
      grate_ac <- c()  # growth rate [cm yr-1] _ac indicates 'all censuses'
      rgrate_ac <- c()  # relative growth rate [cm yr-1 cm-1]
      dbh_ac <- c()  # initial size [cm] associated with grates
      
      ids <- which((cnsdata[[1]]$sp %in% spc$names[isp]))
      for (i in seq_len(ncens - 1)) {
        
        # First order of business is to create a table similar to extract.growthdata
        # but for diameter growth rates (modified from CTFS source)
        dbh1 <- cnsdata[[i]]$dbh[ids]
        dbh2 <- cnsdata[[i + 1]]$dbh[ids]
        dtime <- (cnsdata[[i + 1]]$date[ids] - cnsdata[[i]]$date[ids]) / 365.25
        grate <- (dbh2 - dbh1) / dtime
        rgrate <- log(dbh2/dbh1) / dtime
        
        # from trim.growth (filter to appropriate plants)
        intercept <- 0.9036 / 10
        slope <- 0.006214
        err.limit <- 4
        maxgrow <- 7.5  # cm
        pomcut <- 0.05  # maximum acceptable fraction difference between POM in diameter measurements
        mindbh <- 1  # cm
        stdev.dbh1 <- slope * dbh1 + intercept
        bad.neggrow <- which(dbh2 <= (dbh1 - err.limit * stdev.dbh1))
        bad.posgrow <- which(grate > maxgrow)
        pomdiff <- abs(as.numeric(cnsdata[[i + 1]]$pom[ids]) - as.numeric(cnsdata[[i]]$pom[ids])) / 
          as.numeric(cnsdata[[i]]$pom[ids])
        accept <- rep(FALSE, length(pomdiff))
        include <- which(dbh1 >= mindbh & 
                           dbh2 >= mindbh & 
                           cnsdata[[i]]$status[ids] == "A" & 
                           cnsdata[[i + 1]]$status[ids] == "A")
        accept[include] <- TRUE
        accept[pomdiff > pomcut] <- FALSE
        accept[bad.neggrow] <- FALSE
        accept[bad.posgrow] <- FALSE
        accept[is.na(grate)] <- FALSE
        accept[cnsdata[[i + 1]]$stemID[ids] != cnsdata[[i]]$stemID[ids]] <- FALSE
        accept[cnsdata[[i + 1]]$dbh[ids] <= 0 | cnsdata[[i]]$dbh[ids] <= 0] <- FALSE
        
        # Also, if BCI, the first two censuses dont have precise sapling dbh
#        if(sitename == "bci"){
#          accept[cnsdata[[i]]$CensusID[ids]<3 && cnsdata[[i]]$dbh[ids] <= sapling_dbh ] <- FALSE
#        }
        
        grate_ac <- c(grate_ac, grate[accept])
        rgrate_ac <- c(rgrate_ac, rgrate[accept])
        dbh_ac <- c(dbh_ac, dbh1[accept])
        
        # # Contribute to a binned mean (incgrowth must be in a table...)
        # dbhclass=as.numeric(as.character(cut(dbh1,breaks=c(szclass_l,10000),right=F,labels=szclass_l)))
        # sumgrow = sumgrow+tapply(incgrowth,dbhclass,sum) ngrow =
        # ngrow+tapply(incgrowth,dbhclass,length)
        
      }
      
      # We are only interested in species with large maximum stature for the tree
      # and sapling 95 percentile growth rates
      
      if (is.finite(spc$dbh95[isp]) & spc$dbh95[isp] >= large_stature_dbh95) {
        
        # Trees
        ad_ids <- which(dbh_ac >= tree_dbh)
        argrate_ac <- rgrate_ac[ad_ids]
        spc$argrN[isp] <- length(argrate_ac)
        if (spc$argrN[isp] == 0) {
          print(paste(spc$names[isp], "has no valid growth points for non-saplings"))
        } else {
          aquants <- quantile(argrate_ac, probs = c(0.25, 0.5, 0.9, 0.95))
          spc$argr95[isp,1] <- aquants[[4]]  # 0.09878286
          spc$argr90[isp,1] <- aquants[[3]]
          spc$argr50[isp,1] <- aquants[[2]]  # 0.01381544
          spc$argr25[isp,1] <- aquants[[1]]  # 0.01381544
          
          if(do_bootstrap_ci & spc$argrN[isp]>100){
            for (ib in 1:nboot) {
              rids <- ceiling(runif(spc$argrN[isp],min=0,max=spc$argrN[isp]))
              bs_argrate_ac <-argrate_ac[rids]
              bs_aquants <- quantile(bs_argrate_ac, probs = c(0.25, 0.5, 0.9, 0.95))
              bs95[ib] <- bs_aquants[[4]]
              bs90[ib] <- bs_aquants[[3]]
              bs50[ib] <- bs_aquants[[2]]
              bs25[ib] <- bs_aquants[[1]]
            }
            #hist(bs95,main=paste(spc$names[isp],"N=",spc$srgrN[isp]))
            #cat("Press [enter] to continue, [X] to exit")
            #line <- readline()
            #if(line=='X'){stop()}
            spc$argr95[isp,2] <- quantile(bs95, probs = climit)
            spc$argr90[isp,2] <- quantile(bs90, probs = climit)
            spc$argr50[isp,2] <- quantile(bs50, probs = climit)
            spc$argr25[isp,2] <- quantile(bs25, probs = climit)
            spc$argr95[isp,3] <- quantile(bs95, probs = 1.0-climit)
            spc$argr90[isp,3] <- quantile(bs90, probs = 1.0-climit)
            spc$argr50[isp,3] <- quantile(bs50, probs = 1.0-climit)
            spc$argr25[isp,3] <- quantile(bs25, probs = 1.0-climit)
          }
          
        }
        
        # Saplings
        sa_ids <- which(dbh_ac <= sapling_dbh)
        srgrate_ac <- rgrate_ac[sa_ids]
        spc$srgrN[isp] <- length(srgrate_ac)
        if (spc$srgrN[isp] == 0) {
          print(paste(spc$names[isp], "has no valid growth points for saplings"))
        } else {
          squants <- quantile(srgrate_ac, probs = c(0.25, 0.5, 0.9, 0.95))
          spc$srgr95[isp] <- squants[[4]]  # 0.09878286
          spc$srgr90[isp] <- squants[[3]]
          spc$srgr50[isp] <- squants[[2]]  # 0.01381544
          spc$srgr25[isp] <- squants[[1]]  # 0.01381544
          
          if(do_bootstrap_ci & spc$srgrN[isp]>100){
            for (ib in 1:nboot) {
              rids <- ceiling(runif(spc$srgrN[isp],min=0,max=spc$srgrN[isp]))
              bs_srgrate_ac <-srgrate_ac[rids]
              bs_squants <- quantile(bs_srgrate_ac, probs = c(0.25, 0.5, 0.9, 0.95))
              bs95[ib] <- bs_squants[[4]]
              bs90[ib] <- bs_squants[[3]]
              bs50[ib] <- bs_squants[[2]]
              bs25[ib] <- bs_squants[[1]]
            }
            #hist(bs95,main=paste(spc$names[isp],"N=",spc$srgrN[isp]))
            #cat("Press [enter] to continue, [X] to exit")
            #line <- readline()
            #if(line=='X'){stop()}
            spc$srgr95[isp,2] <- quantile(bs95, probs = climit)
            spc$srgr90[isp,2] <- quantile(bs90, probs = climit)
            spc$srgr50[isp,2] <- quantile(bs50, probs = climit)
            spc$srgr25[isp,2] <- quantile(bs25, probs = climit)
            spc$srgr95[isp,3] <- quantile(bs95, probs = 1.0-climit)
            spc$srgr90[isp,3] <- quantile(bs90, probs = 1.0-climit)
            spc$srgr50[isp,3] <- quantile(bs50, probs = 1.0-climit)
            spc$srgr25[isp,3] <- quantile(bs25, probs = 1.0-climit)
          }
        }
      }
    }
  }
  
  return(spc)
} 
