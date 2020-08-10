# =======================================================================================
#
# Run Various pft classification algorithms on species indices.
#
# Algorithms:
#
# 1) All species are classified on the shade tolerance axis via Wright et al, Ecology.
#    ie the relative growth rate of the top 95-percentile saplings in each species 
#    is compared against the lowest 25-percentile mortality rates of those species. A regression
#    line is fit against the data, and species are associated with their closest point
#    on the regression line, which is the early-to-late axis
#
# =======================================================================================

classify_pfts <- function(spc,cnsdata,ncens,diameter_classes_cm,plot_spp) {
  
  # Parameters
  
  pft <- list()
  pft$itype <- c(1,2)
  pft$name  <- c('pioneer','shade tolerant')
  default_ipft <- 2
  shade_tol_rank <- 0.66  # Shade axis ranks below this are considered tolerant  
  do_classifier_barplots <- FALSE
  do_record_classes <- TRUE
  
  print('== RUNNING SPECIES->PFT CLASSIFIER ===')
  
  ids_rgr95 <- which(is.finite(spc$srgr95[,1]))
  ids_m25   <- which(is.finite(spc$sm25[,1]))
  ids_both  <- which(is.finite(spc$sm25[,1]) & is.finite(spc$srgr95[,1]) )
  ids_either<- which(is.finite(spc$sm25[,1]) | is.finite(spc$srgr95[,1]) )
  #  ids_either<- ids_both 
  ids_neither <- which(!(is.finite(spc$sm25[,1]) | is.finite(spc$srgr95[,1])) )
  
  shade_axis <- rep(NA, length(ids_either))
  
  
  predictor <- spc$sm25[ids_both,1]
  response  <- spc$srgr95[ids_both,1]
  lin_coeffs <- lm(response ~ predictor)
  
  # Determine the location of each pft on the regression line
  # If a the mortality is calculated, use both coordinates
  # if only the relative growth rate is available use that
  # if none are available, set it to the default pft
  
  intercept <- lin_coeffs$coefficients[[1]]
  slope <- lin_coeffs$coefficients[[2]]
  alpha <- atan(slope)
  
  for (id in 1:length(ids_either)) {
    
    isp <- ids_either[id]
    
    if(is.finite(spc$sm25[isp,1]) && is.finite(spc$srgr95[isp,1]) ){
      # Point on regression line
      # slope is 1/lin_coeffs[2]
      x1 <- spc$sm25[isp,1]
      y1 <- spc$srgr95[isp,1]
      
      x3 <- x1
      y3 <- intercept + x3*slope
      
      S3 <- y1-y3
      if(S3>0){
        S3 <- abs(S3)        
        S1 <- S3*cos(alpha)
        x2 <- x1 + S1*sin(alpha)
        y2 <- y1 - S1*cos(alpha)
      } else {
        S3 <- abs(S3)        
        S1 <- S3*cos(alpha)
        x2 <- x1 - S1*sin(alpha)
        y2 <- y1 + S1*cos(alpha)
      }
      shade_axis[id] <- x2
      
    } else if (is.finite(spc$srgr95[isp,1])) {
      
      # Point on regression line
      # slope is 1/lin_coeffs[2]
      
      y2 <- spc$srgr95[isp,1]
      x2 <- (y1-intercept)/slope
      spc$pft_rx[isp] <- x2
      spc$pft_ry[isp] <- y2
      shade_axis[id] <- x2
      
    }
  }
  
  # Check to see if these are finite
  check_ids <- which(is.finite(shade_axis))
  if( length(check_ids) != length(ids_either) )
  { print('FILTERS ARENT MATCHING')
    return(NA) }
  
  
  # Normalize the shade tolerance axis from 0 to 1
  
  shade_axis <- shade_axis-min(shade_axis)  
  shade_axis <- shade_axis/max(shade_axis)
  
  
  print(paste('Number of classified Species: ',length(shade_axis)))
  print(paste('Number of unclassified Species: ',length(spc$srgr95[,1])-length(shade_axis)))
  
  # Shade axis has been established, assign pft values based on
  # some threshold
  
  thresh_id <- as.integer(shade_tol_rank * length(shade_axis))
  
  sorted_shade_axis <- sort(shade_axis,index.return = TRUE)
  
  spc$ipft <- rep(3, length(spc$names))   # Unclassified
  
  shade_tolerant_spc_ids <- ids_either[sorted_shade_axis$ix[1:thresh_id-1]]
  
  spc$ipft[shade_tolerant_spc_ids] <- 2
  
  pioneer_spc_ids <- ids_either[sorted_shade_axis$ix[thresh_id:length(shade_axis)]]
  
  spc$ipft[pioneer_spc_ids] <- 1
  
  
  # Calculate Some Simple Statistics for the PFTs
  if(do_classifier_barplots){
    print('Applying classifications to each census to report BA stats')
    print('This may take a moment. Have a nice iced tea while you wait.')
    
    nclass <- length(diameter_classes_cm)
    
    ba_pft_sz <- matrix(0, nrow = nclass, ncol = 3)
    n_pft_sz  <- matrix(0, nrow = nclass, ncol = 3)
    
    ba_sum <- matrix(0,nrow=ncens,ncol=3)
    
    for (i in seq_len(ncens)) {
      
      ba_pft_sz1 <- matrix(0, nrow = nclass, ncol = 3)
      n_pft_sz1  <- matrix(0, nrow = nclass, ncol = 3)
      
      nplants <- length(cnsdata[[i]]$dbh)
      live_ids <- which( cnsdata[[i]]$dbh>0.0  )
      
      for (jj in seq(live_ids)) {
        
        j <- live_ids[jj]  
        
        isp  <- which(( spc$names %in% cnsdata[[i]]$sp[j] ))
        ipft <- spc$ipft[isp]
        isz  <- max(which(cnsdata[[i]]$dbh[j]>=diameter_classes_cm))
        
        ba <- 3.14159*(0.01*cnsdata[[i]]$dbh[j])*(0.01*cnsdata[[i]]$dbh[j])/4.0
        
        ba_pft_sz1[isz,ipft] = ba_pft_sz1[isz,ipft] + ba
        n_pft_sz1[isz,ipft] = n_pft_sz1[isz,ipft] + 1
        
      }
      
      ba_sum[i,1] <- sum(ba_pft_sz1[,1])/50
      ba_sum[i,2] <- sum(ba_pft_sz1[,2])/50
      ba_sum[i,3] <- sum(ba_pft_sz1[,3])/50
      
      ba_pft_sz <- ba_pft_sz + ba_pft_sz1
      n_pft_sz <- n_pft_sz + n_pft_sz1
    }
    
    ba_pft_sz <- ba_pft_sz / (ncens*50)
    n_pft_sz <- n_pft_sz / (ncens*50)
    
    ba_sz_data_frame = data.frame(ba_pft_sz,row.names=diameter_classes_cm)
    
    ba_cn_data_frame = data.frame(ba_sum,row.names=c("1983","1985","1992","1996","2001","2006","2011"))
    
    
    barplot(t(ba_sz_data_frame),xlab="size", main="Basal Area by PFT", ylab="BA", legend=c("early","late","unclassified"))
    barplot(t(ba_cn_data_frame),xlab="size", main="Basal Area by PFT", ylab="BA", legend=c("early","late","unclassified"))
    barplot()
  }
  
  # We must make a decision on the un-classified PFTs, 
  spc$ipft[ids_neither] <- default_ipft
  
  if(do_record_classes) {
    timestamp <- max(cnsdata[[i]]$date,na.rm=TRUE)
    class_filename<-paste(site.name,"_pft_classes_type1_","c",format(Sys.time(),"%Y-%m-%d"),".xml",sep="")
  
    sink(class_filename,append = FALSE)
    
    # Generate the header
    cat('<all> \n')
    
    cat('    <summary> </summary> \n')
    cat('    <parameters> pioneer ranks at gt 0.66 </parameters> \n')
    cat('    <preparer> rgknox@lbl.gov </preparer> \n')
    
    for (kk in seq(pft$name)) {
      
      cat(paste('    <pft name ="',str_trim(pft$name[kk],side="both"),'"> \n'))
      
      isps  <- which((spc$ipft %in% kk))
      for (ll in seq(isps)){
        pid<-isps[ll]
        cat(paste('        <spc code="',str_trim(spc$name[pid],side="both"),'"> ',plot_spp$Genus[pid],plot_spp$Species[pid],'</spc> \n'))
      }
      cat(paste('    </pft> \n'))
    }
    cat('</all>')
  
    sink()  
  }
  
  
  return(spc)
}
