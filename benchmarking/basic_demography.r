# process size based demographics

# Pull out species that reach d95 of large_stature_dbh95 threshold
# ======================================================================================
if(large_only){
  lrg<-spc$names[spc$dbh95>=large_stature_dbh95 & !is.na(spc$dbh95)]
  cn<-list()
  for (j in 1:ncens){
    cn[[j]]<-subset(cnsdata[[j]], cnsdata[[j]]$sp %in% lrg)
  }
}

mids<-vector()

# sp by dbh class growth rates per census interval
###############################################################################
sp.grate<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh){
  sp.Glist<-list()
  if(large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Glist[[i]] <- growth.eachspp(cn[[i]], cn[[(i + 1)]], 
                                      classbreak = diameter_classes_cm,dbhunit = "cm", mindbh= min_dbh)
      if(do_plot_spp_grate){
        Sys.sleep(.01)
        hist(sp.Glist[[i]]$rate, xlab = "Annual growth rates (cm)", 
             main = paste("Census Interval ",i, " to ", i + 1, sep = ""))}
      Sys.sleep(0)
    }
  }
  
  if(!large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Glist[[i]] <- growth.eachspp(cnsdata[[i]], cnsdata[[(i + 1)]], 
                                      classbreak = diameter_classes_cm,dbhunit = "cm", mindbh= min_dbh)
      if(do_plot_spp_grate){
        Sys.sleep(.01)
        hist(sp.Glist[[i]]$rate, xlab = "Annual growth rates (cm)", 
             main = paste("Census Interval ",i, " to ", i + 1, sep = ""))}
      Sys.sleep(0)
    }
    print("Calculating species level growth")
  }
  return(sp.Glist)
}

# sp by dbh class mortality rates per census interval
###############################################################################
sp.mrate<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh){
  sp.Mlist<-list()
  if(large_only){
    
    for (i in seq_len(ncens - 1)) {
      sp.Mlist[[i]] <- mortality.eachspp(cn[[i]], cn[[(i + 1)]], 
                                         classbreak = diameter_classes_cm)
      if(do_plot_spp_mrate){
        Sys.sleep(.01)
        hist(sp.Mlist[[i]]$rate, xlab = "Annual mortality rates ", 
             main = paste("Census Interval ", i, " to ", i + 1, sep = ""))}
      Sys.sleep(0)
    }
  }
  if(!large_only){    
    for (i in seq_len(ncens - 1)) {
      sp.Mlist[[i]] <- mortality.eachspp(cnsdata[[i]], cnsdata[[(i + 1)]], 
                                         classbreak = diameter_classes_cm)
      if(do_plot_spp_mrate){
        Sys.sleep(.01)
        hist(sp.Mlist[[i]]$rate, xlab = "Annual mortality rates ", 
             main = paste("Census Interval ", i, " to ", i + 1, sep = ""))}
      Sys.sleep(0)
    }
    print("Calculating species level mortality")
  }
  return(sp.Mlist)
}

# sp by dbh class recruitment rates per census interval
###############################################################################
sp.rec<-function(cnsdata, large_only, ncens, min_dbh){
  sp.Rlist_mindbh<-list()
  if(large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Rlist_mindbh[[i]] <- recruitment.eachspp(cn[[i]], 
                                                  cn[[(i + 1)]], mindbh = min_dbh)
    }
  }
  if(!large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Rlist_mindbh[[i]] <- recruitment.eachspp(cnsdata[[i]], 
                                                  cnsdata[[(i + 1)]], mindbh = min_dbh)
    }
    print("Calculating species level recruitment")
  }
  return(sp.Rlist_mindbh)
}

# Calculate annual recruitment of each species at 10 cm dbh
############################################################################
sp.rec10<-function(cnsdata, large_only, ncens){
  sp.Rlist_10cm<-list()
  
  if(large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Rlist_10cm[[i]] <- recruitment.eachspp(cn[[i]], 
                                                cn[[(i + 1)]], mindbh = 10)
    }
  }
  if(!large_only){
    for (i in seq_len(ncens - 1)) {
      sp.Rlist_10cm[[i]] <- recruitment.eachspp(cnsdata[[i]], 
                                                cnsdata[[(i + 1)]], mindbh = 10)
    }
  }
  return(sp.Rlist_10cm)
}

#Calculate annual growth rate based on size classes
#===================================================================================

grate<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh){
  Glist<-list()
  if(large_only){
    for (i in seq_len(ncens -1)){
      Glist[[i]] <- growth.dbh(cn[[i]],cn[[(i+1)]], mindbh=min_dbh, 
                               classbreak = diameter_classes_cm)}}
  
  if(!large_only){
    for (i in seq_len(ncens -1)){
      Glist[[i]] <- growth.dbh(cnsdata[[i]],cnsdata[[(i+1)]], mindbh=min_dbh, 
                               classbreak = diameter_classes_cm)
    }
    print("Calculating growth by diameter class")
  }
  return(Glist)
}

# Caclulate annual mortality based on diameter classes
# ======================================================================================
mrate<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh){  
  Mlist<-list()
  Mlist<-list()
  if(large_only){
    
    for (i in seq_len(ncens -1)){
      Mlist[[i]] <- mortality.dbh(cn[[i]],cn[[(i+1)]],  
                                  classbreak = diameter_classes_cm)
    }}
  if(!large_only){
    for (i in seq_len(ncens -1)){
      Mlist[[i]] <- mortality.dbh(cnsdata[[i]],cnsdata[[(i+1)]],  
                                  classbreak = diameter_classes_cm)
    }
    print("Calculating mortality by diameter class")
    
  }
  return(Mlist)
}


# Caclulate annual population change (# stems) based on diameter classes
# ======================================================================================
stem_abund<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh, plot_stem_abund){  
  Stemlist <- list()
  if(large_only){
    for (i in seq_len(ncens)) {
      Stemlist[[i]] <- abundance(cn[[i]], type = "abund", mindbh = min_dbh, 
                                 split1 = cut(cn[[i]]$dbh, diameter_classes_cm, 
                                              include.lowest=TRUE, right=FALSE), dbhunit = "cm")
    }
  }
  if(!large_only){
    for (i in seq_len(ncens)) {
      Stemlist[[i]] <- abundance(cnsdata[[i]], type = "abund", mindbh = min_dbh, 
                                 split1 = cut(cnsdata[[i]]$dbh, diameter_classes_cm, 
                                              include.lowest=TRUE, right=FALSE), dbhunit = "cm")
    }
  }
  if(do_plot_stem_abund){
    for (i in 1:(length(diameter_classes_cm)-1)){
      mids[i]<-(diameter_classes_cm[i]+diameter_classes_cm[i+1])/2
    }
    
    for (i in 1:ncens){
      c.ba<-(unname(unlist(Stemlist[[i]]))[1:(length(diameter_classes_cm)-1)])/50
      Sys.sleep(.01)
      barplot(c.ba, names.arg=mids, xlab="DBH class midpoints", ylab="stems [ha-1]", main=paste("Census ", i, sep=""))
      Sys.sleep(0)
    }
    print("Calculating abundance by diameter class")
    
  }
  return(Stemlist)
}



# Caclulate annual population change (basal area) based on diameter classes
# ======================================================================================
stem_ba<-function(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh, plot_stem_ba){  
  if(do_stem_ba){
    StemBAlist<-list()
    if(large_only){
      for (i in seq_len(ncens)){
        StemBAlist[[i]] <- abundance(cn[[i]], type = "ba", mindbh = min_dbh, 
                                     split1 = cut(cn[[i]]$dbh, diameter_classes_cm, 
                                                  include.lowest=TRUE, right=FALSE), dbhunit = "cm")}}
    if(!large_only){
      for (i in seq_len(ncens)){
        StemBAlist[[i]] <- abundance(cnsdata[[i]], type = "ba", mindbh = min_dbh, 
                                     split1 = cut(cnsdata[[i]]$dbh, diameter_classes_cm, 
                                                  include.lowest=TRUE, right=FALSE), dbhunit = "cm")
      }
    }
    if(do_plot_stem_ba){
      for (i in 1:(length(diameter_classes_cm)-1)){
        mids[i]<-(diameter_classes_cm[i]+diameter_classes_cm[i+1])/2
      }
      
      for (i in 1:ncens){
        c.ba<-(unname(unlist(StemBAlist[[i]]))[1:(length(diameter_classes_cm)-1)])/50
        Sys.sleep(.01)
        barplot(c.ba, names.arg=mids, xlab="DBH class midpoints", ylab="basal area [m2*ha-1]", main=paste("Census ", i, sep=""))
        Sys.sleep(0)
      }
    }
    print("Calculating basal area by diameter class")
    
  }
  return(StemBAlist)
}  

# Calculate total basal area/ha by census
#========================================================================
tba<-function(cnsdata, large_only, ncens){
  totba <- list()
  if(large_only){
    for (i in seq_len(ncens)) {
      totba[[i]] <- basum(cn[[i]]$dbh, dbhunit = "cm")/plotsize
    }
  }
  if(!large_only){
    for (i in seq_len(ncens)) {
      totba[[i]] <- basum(cnsdata[[i]]$dbh, dbhunit = "cm")/plotsize
    }
    print("Calculating total basal area/ha") 
  }  
  return(totba)
}

# Calulate species abundance
# =======================================================================================
sp_abund<-function(cnsdata, large_only, ncens){
  sp.Alist <- list()
  if(large_only){
    for (i in seq_len(ncens)) {
      sp.Alist[[i]] <- abundance(cn[[i]], type = "abund", mindbh = min_dbh, 
                                 split1 = cn[[i]]$sp, dbhunit = "cm")
    }
  }
  
  if(!large_only){
    for (i in seq_len(ncens)) {
      sp.Alist[[i]] <- abundance(cnsdata[[i]], type = "abund", mindbh = min_dbh, 
                                 split1 = cnsdata[[i]]$sp, dbhunit = "cm")
    }
    print("Calculating species abundance") 
    
  }
  return(sp.Alist)
}



# Calculate species basal area
# =======================================================================================
sp_ba<-function(cnsdata, large_only, ncens){
  sp.BAlist <- list()
  if(large_only){
    for (i in seq_len(ncens)) {
      sp.BAlist[[i]] <- abundance(cn[[i]], type = "ba", mindbh = min_dbh, 
                                  split1 = cn[[i]]$sp, dbhunit = "cm")
    }
  }
  if(!large_only){
    for (i in seq_len(ncens)) {
      sp.BAlist[[i]] <- abundance(cnsdata[[i]], type = "ba", mindbh = min_dbh, 
                                  split1 = cnsdata[[i]]$sp, dbhunit = "cm")
    }
    print("Calculating species basal area") 
    
  }
  return(sp.BAlist)
}
