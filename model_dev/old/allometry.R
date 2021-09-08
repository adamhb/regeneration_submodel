
Chave.AGB=function(dbh,density=0.62,htparam=c(41.7,.057,.748),heightmodel=predht.asym,forest='moist'
) { 
  if(is.null(htparam)) 
  { 
    if(forest=="moist") param=c(-1.499,2.148,0.207,-0.0281) 
    else if(forest=="dry") param=c(-.667,1.784,0.207,-.0281) 
    else if(forest=="wet") param=c(-1.239,1.98,0.207,-0.0281) 
    
    AGB=agb.dbhmodel(dbh,density,param) 
  } 
  else 
  { 
    if(forest=="moist") param=c(.0501,1) 
    else if(forest=="dry") param=c(.112,.91) 
    else if(forest=="wet") param=c(.077,.94) 
    
    ht=heightmodel(dbh,htparam) 
    AGB=agb.model(dbh,density,ht,param) 
  } 
  
  return(AGB) 
} 


predht.asym=function(dbh,param) 
{ 
  if(is.null(dim(param))) 
  { 
    ymax=param[1] 
    a=param[2] 
    b=param[3] 
  } 
  else 
  { 
    ymax=param[,1] 
    a=param[,2] 
    b=param[,3] 
  } 
  
  return(ymax*(1-exp(-a*dbh^b))) 
} 


agb.model=function(dbh,density,height,param) 
  return(param[1]*(density*dbh^2*height)^param[2]) 

dbh2LAI <- function(dbh = 10){
  max_canopy_area <- 20 * 20
  slope <- (max_canopy_area - 1) / (2000 - 10) 
  LAI <- slope * dbh 
  return(LAI)
}  #converting the dbh to LAI in (m2)


########CONVERTING TOTAL BIOMASS TO DBH###########
#Using Chave function to creat the function that converts total biomass (g of C) to dbh (mm)

y <- seq(from = 1, to = 300, by = 0.25)
x <- Chave.AGB(dbh = y) * 1.2 * 0.4

plot(x,y)
plot(log(x),log(y))

log_growth_model <- lm(log(y)~log(x))


C2dbh <- function(carbon){
  carbon_kg <- carbon / 1000
  log_dbh <- log(carbon_kg)*coefficients(log_growth_model)[2] + coefficients(log_growth_model)[1]
  dbh <- exp(log_dbh) * 10
  return(dbh)
}