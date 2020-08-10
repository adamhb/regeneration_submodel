#==============================================================================
# Utility functions for forestgeo census benchmarks (fgcb)
#==============================================================================

# CheckUsage error reporting wrapper
checkUsageErr <- function(process,err_log){
  #  err_log[length(err_log)+1] <- capture.output(checkUsage(process, all = TRUE , name = ""))
  return(err_log)
}


print_scope_log <- function(err_log){
  print(paste("======= SCOPING ERROR LOG ========="))
  print(paste(length(err_log)," total errors"))
  for (i in seq_along(err_log)) {
    print(paste("[",i,"]",err_log[i]))
  }
  print(paste("==================================="))
}