##============================================================
##  for_apply_mcapply.R
##============================================================


##  Get data ready

if( Sys.info()[['sysname']] == 'Windows' ){
  setwd("X:/project2017/evalParal/")
}else if (Sys.info()[['sysname']] == 'Linux' ){
  setwd("/ddn/gs1/home/li11//project2017/evalParal/")
}else if( Sys.info()[['sysname']] == 'Darwin' ){
  setwd("/Users/li11/myGit/helloworld/IB_group_meeting/evalParal")
}


load ("data/affy_probes.Rda")
load ("data/affy_probe_sets.Rda")

  
  


##===============================================
##  choose a small data
##===============================================
probe2tests <- probeSets[1:100]


##===============================================
##  Using for loop
##===============================================


ptm <- proc.time()  
n <- NULL
for (probe2test in probe2tests)
{
      prob_at.idx   <- grep (probe2test, substr( probes, 1,nchar(probe2test)) ,fixed=TRUE)
      probes_at   <- as.vector(as.character(probes[prob_at.idx]))
      temp <-  cbind (probe2test, length(probes_at),  length(unique(probes_at)))
      n <- rbind(n, temp)
}
proc.time() - ptm 
  


##  Windows time
# user  system elapsed 
# 18.95    0.40   19.36 

##  Latimer time
# user  system elapsed 
# 30.915   0.150  38.128 

##  Mac time
# user  system elapsed 
# 12.604   0.169  12.786 
##===============================================
##  Using for apply
##===============================================

##  Windows
library(doParallel)
number.of.workers <- detectCores() 
cl <- makeCluster(number.of.workers)
registerDoParallel(cl)

##  Linux
library(doMC)
registerDoMC()


trial <- as.data.frame(probe2tests)
ptm <- proc.time()
parse.apply <- apply (trial, 1, function(x) {
    idx        <- grep (x, substr(probes , 1, nchar(x)) ,fixed=TRUE)
    probes_at  <-probes[idx]
    cbind (x, length(probes_at), length(unique(probes_at)) )
})
n <- foreach (i = 1: dim(parse.apply)[2] ,
        .combine = 'rbind') %dopar%{
         as.vector(parse.apply [,i])
      }
proc.time() - ptm


##  Windows need to stop parallel
stopCluster(cl)


##  Windows time
# user  system elapsed 
#  12.94    0.35   13.91 

##  Latimer time
# user  system elapsed 
#  23.811   1.048  28.385 

##===============================================
##  Using for mcapply  
##  ONLY on liunx
##===============================================

library(doMC)
registerDoMC()

trial <- probe2tests

myParse <- function(singleProbeSet, pools = probes) {
    idx        <- grep (singleProbeSet, substr(pools , 1, nchar(singleProbeSet)) ,fixed=TRUE)
    probes_at  <- pools[idx]
    cbind (singleProbeSet, length(probes_at), length(unique(probes_at)))
}


ptm <- proc.time()

parse.result <- mclapply(trial, myParse, pools = probes, mc.cores = 80)
                        
n <- foreach (i = 1: length(parse.result),
            .combine = 'rbind') %dopar%{
            as.vector(parse.result[[i]])
        }
proc.time() - ptm



## Latimer time
#user  system elapsed 
#29.294   5.902   3.202

## Mac time
# user  system elapsed 
# 28.541  12.935   5.636 
