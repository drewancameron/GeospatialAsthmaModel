### Model for the count of asthma per SA1 unit over all four largest cities in Australia

setwd("~/VIRTUAL_WA/ASTHMA/")

library(rgdal)
library(MASS)
library(Matrix)
library(truncnorm)
library(TMB)
library(sparseMVN)
library(VGAM)
library(rjson)
library(readxl)

### Read in and prepare data

sa1_2021 <- readOGR("asthma_nonInd.shp")

# CENSUS DATA

filename_SA1 <- "SA1_by_HASTP_FourCities_59only.csv"
filename_SA2 <- "SA2_by_HASTP_FourCities_59only.csv"

# SA1 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)

# SA2 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)

sa1sa2codes <- match(sa1_2021$SA2_NAME21,unique(sa1_2021$SA2_NAME21))

M <- dim(SA1_num_dwellings_by_DATUM)[2]-2

SA1_num_dwellings_by_DATUM_obs <- SA1_num_dwellings_by_DATUM[,-c(1,M+2)]
SA1_num_dwellings_obs <- SA1_num_dwellings_by_DATUM[,M+2]
SA2_num_dwellings_by_DATUM_obs <- SA2_num_dwellings_by_DATUM[,-c(1,M+2)]
SA2_num_dwellings_obs <- SA2_num_dwellings_by_DATUM[,M+2]

# Load 10-14 EXTRA

filename_SA1 <- "SA1_by_HASTP_FourCities_1014only.csv"
filename_SA2 <- "SA2_by_HASTP_FourCities_1014only.csv"

# SA1 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=11,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)

# SA2 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=11,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=9,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA2",ydata[-1][1:(length(SA2_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA2_num_dwellings_by_DATUM) <- xcolnames
SA2_num_dwellings_by_DATUM <- as.data.frame(SA2_num_dwellings_by_DATUM)
SA2_num_dwellings_by_DATUM <- SA2_num_dwellings_by_DATUM[match((unique(sa1_2021$SA2_NAME21)),SA2_num_dwellings_by_DATUM$SA2),]
SA2_num_dwellings_by_DATUM[,1] <- 1:length(SA2_num_dwellings_by_DATUM[,1])
SA2_num_dwellings_by_DATUM[,2] <- as.numeric(SA2_num_dwellings_by_DATUM[,2])
SA2_num_dwellings_by_DATUM[,3] <- as.numeric(SA2_num_dwellings_by_DATUM[,3])
SA2_num_dwellings_by_DATUM[,4] <- as.numeric(SA2_num_dwellings_by_DATUM[,4])
SA2_num_dwellings_by_DATUM[,5] <- as.numeric(SA2_num_dwellings_by_DATUM[,5])
SA2_num_dwellings_by_DATUM <- as.matrix(SA2_num_dwellings_by_DATUM)

SA1_num_dwellings_by_DATUM_obsX <- SA1_num_dwellings_by_DATUM[,-c(1,M+2)]
SA1_num_dwellings_obsX <- SA1_num_dwellings_by_DATUM[,M+2]
SA2_num_dwellings_by_DATUM_obsX <- SA2_num_dwellings_by_DATUM[,-c(1,M+2)]
SA2_num_dwellings_obsX <- SA2_num_dwellings_by_DATUM[,M+2]

# Checks

true_SA1_num_dwellings_by_DATUM_current <- SA1_num_dwellings_by_DATUM_obs

check_SA1s <- which(true_SA1_num_dwellings_by_DATUM_current==0 & SA1_num_dwellings_by_DATUM_obs>0)
if (length(check_SA1s)>0) {
  true_SA1_num_dwellings_by_DATUM_current[check_SA1s] <- 1
}
check_SA1rowsums <- which(rowSums(true_SA1_num_dwellings_by_DATUM_current)==0 & SA1_num_dwellings_obs >0)
if (length(check_SA1rowsums)>0) {
  for (i in 1:length(check_SA1rowsums)) {
    true_SA1_num_dwellings_by_DATUM_current[check_SA1rowsums[i],sample(1:M,1)] <- 1
  }
}
check_SA2s <- which(aggregate(true_SA1_num_dwellings_by_DATUM_current,list(sa1sa2codes),sum)[,-1]==0 & SA2_num_dwellings_by_DATUM_obs>0,arr.ind = TRUE)
if (length(check_SA2s)>0) {
  for (i in 1:length(check_SA2s[,1])) {
    true_SA1_num_dwellings_by_DATUM_current[sample(which(sa1sa2codes==check_SA2s[i,1]),1),check_SA2s[i,2]] <- 1
  }
}
check_SA2rowsums <- which(aggregate(rowSums(true_SA1_num_dwellings_by_DATUM_current),list(sa1sa2codes),sum)[,-1]==0 & SA2_num_dwellings_obs >0)
if (length(check_SA2rowsums)>0) {
  for (i in 1:length(check_SA2rowsums)) {
    true_SA1_num_dwellings_by_DATUM_current[sample(which(sa1sa2codes==check_SA2rowsums[i,1]),1),sample(1:M,1)] <- 1
  }
}

true_SA1_num_dwellings_current <- rowSums(true_SA1_num_dwellings_by_DATUM_current)
true_SA2_num_dwellings_by_DATUM_current <- as.matrix(aggregate(true_SA1_num_dwellings_by_DATUM_current,list(sa1sa2codes),sum)[,-1])
true_SA2_num_dwellings_current <- rowSums(true_SA2_num_dwellings_by_DATUM_current)

true_SA1_num_dwellings_by_DATUM_currentX <- SA1_num_dwellings_by_DATUM_obsX

check_SA1s <- which(true_SA1_num_dwellings_by_DATUM_currentX==0 & SA1_num_dwellings_by_DATUM_obsX>0)
if (length(check_SA1s)>0) {
  true_SA1_num_dwellings_by_DATUM_currentX[check_SA1s] <- 1
}
check_SA1rowsums <- which(rowSums(true_SA1_num_dwellings_by_DATUM_currentX)==0 & SA1_num_dwellings_obsX >0)
if (length(check_SA1rowsums)>0) {
  for (i in 1:length(check_SA1rowsums)) {
    true_SA1_num_dwellings_by_DATUM_currentX[check_SA1rowsums[i],sample(1:M,1)] <- 1
  }
}
check_SA2s <- which(aggregate(true_SA1_num_dwellings_by_DATUM_currentX,list(sa1sa2codes),sum)[,-1]==0 & SA2_num_dwellings_by_DATUM_obsX>0,arr.ind = TRUE)
if (length(check_SA2s)>0) {
  for (i in 1:length(check_SA2s[,1])) {
    true_SA1_num_dwellings_by_DATUM_currentX[sample(which(sa1sa2codes==check_SA2s[i,1]),1),check_SA2s[i,2]] <- 1
  }
}
check_SA2rowsums <- which(aggregate(rowSums(true_SA1_num_dwellings_by_DATUM_currentX),list(sa1sa2codes),sum)[,-1]==0 & SA2_num_dwellings_obsX >0)
if (length(check_SA2rowsums)>0) {
  for (i in 1:length(check_SA2rowsums)) {
    true_SA1_num_dwellings_by_DATUM_currentX[sample(which(sa1sa2codes==check_SA2rowsums[i,1]),1),sample(1:M,1)] <- 1
  }
}

true_SA1_num_dwellings_currentX <- rowSums(true_SA1_num_dwellings_by_DATUM_currentX)
true_SA2_num_dwellings_by_DATUM_currentX <- as.matrix(aggregate(true_SA1_num_dwellings_by_DATUM_currentX,list(sa1sa2codes),sum)[,-1])
true_SA2_num_dwellings_currentX <- rowSums(true_SA2_num_dwellings_by_DATUM_currentX)

library(extraDistr)

error_sd <- 2.0

logdiffexp <- function(y,x) {
  # x > y
  x+log(1-exp(y-x))
}
logsumexp <- function(y,x) {
  if (length(x)==1) {
    max(c(x,y))+log(1+exp(min(c(x,y))-max(c(x,y))))} else {
      mmax <- apply(cbind(x,y),1,max)
      mmin <- apply(cbind(x,y),1,min)
      mmax+log(1+exp(mmin-mmax))
    }
}

library(INLA)

coords_SA1 <- coordinates(sa1_2021)

sydney.mesh <-  inla.mesh.2d(coords_SA1[sa1_2021$GCC_NAME21=="Greater Sydney",],cutoff = 0.01,max.n=300)
melbourne.mesh <-  inla.mesh.2d(coords_SA1[sa1_2021$GCC_NAME21=="Greater Melbourne",],cutoff = 0.01,max.n=300)
brisbane.mesh <-  inla.mesh.2d(coords_SA1[sa1_2021$GCC_NAME21=="Greater Brisbane",],cutoff = 0.01,max.n=300)
perth.mesh <-  inla.mesh.2d(coords_SA1[sa1_2021$GCC_NAME21=="Greater Perth",],cutoff = 0.01,max.n=300)

perth_spde_fine <- (inla.spde2.matern(perth.mesh,alpha=2)$param.inla)[c("M0","M1","M2")]
perth_A_fine <- inla.mesh.project(perth.mesh,coords_SA1[sa1_2021$GCC_NAME21=="Greater Perth",])$A
sydney_spde_fine <- (inla.spde2.matern(sydney.mesh,alpha=2)$param.inla)[c("M0","M1","M2")]
sydney_A_fine <- inla.mesh.project(sydney.mesh,coords_SA1[sa1_2021$GCC_NAME21=="Greater Sydney",])$A
melbourne_spde_fine <- (inla.spde2.matern(melbourne.mesh,alpha=2)$param.inla)[c("M0","M1","M2")]
melbourne_A_fine <- inla.mesh.project(melbourne.mesh,coords_SA1[sa1_2021$GCC_NAME21=="Greater Melbourne",])$A
brisbane_spde_fine <- (inla.spde2.matern(brisbane.mesh,alpha=2)$param.inla)[c("M0","M1","M2")]
brisbane_A_fine <- inla.mesh.project(brisbane.mesh,coords_SA1[sa1_2021$GCC_NAME21=="Greater Brisbane",])$A

N_SA1 <- length(true_SA1_num_dwellings_by_DATUM_current[,1])
N_SA2 <- length(true_SA2_num_dwellings_by_DATUM_current[,1])

## RW MCMC Sampler

log_likelihood_diff_fn <- function(proposed_value,current_value,observed_value) {
  
  if (proposed_value==0 & observed_value>0) {
    proposed_likelihood <- NA
  } else if (proposed_value==0 & observed_value==0) {
    proposed_likelihood <- 0
  } else if (proposed_value>0 & observed_value==0) {
    proposed_likelihood <- -logdiffexp(pnorm(0.5,proposed_value,error_sd,log.p=TRUE),0)+logdiffexp(pnorm(0.5,proposed_value,error_sd,log.p=TRUE),pnorm(2.5,proposed_value,error_sd,log.p=TRUE))
  } else if (abs(proposed_value-observed_value) < 5 & abs(current_value-observed_value) < 5) {
     proposed_likelihood <- -logdiffexp(pnorm(0.5,proposed_value,error_sd,log.p=TRUE),0)+logdiffexp(pnorm(observed_value-0.5,proposed_value,error_sd,log.p = TRUE),pnorm(observed_value+0.5,proposed_value,error_sd,log.p = TRUE))
  } else {
    proposed_likelihood <- -logdiffexp(pnorm(0.5,proposed_value,error_sd,log.p=TRUE),0)+dnorm(observed_value,proposed_value,error_sd,log = TRUE)
  }
  
  if (current_value==0 & observed_value==0) {
    current_likelihood <- 0
  } else if (current_value>0 & observed_value==0) {
    current_likelihood <- -logdiffexp(pnorm(0.5,current_value,error_sd,log.p=TRUE),0)+logdiffexp(pnorm(0.5,current_value,error_sd,log.p=TRUE),pnorm(2.5,current_value,error_sd,log.p=TRUE))
  } else if (abs(proposed_value-observed_value) < 5 & abs(current_value-observed_value) < 5) {
    current_likelihood <- -logdiffexp(pnorm(0.5,current_value,error_sd,log.p=TRUE),0)+logdiffexp(pnorm(observed_value-0.5,current_value,error_sd,log.p = TRUE),pnorm(observed_value+0.5,current_value,error_sd,log.p = TRUE))
  } else {
    current_likelihood <- -logdiffexp(pnorm(0.5,current_value,error_sd,log.p=TRUE),0)+dnorm(observed_value,current_value,error_sd,log = TRUE)
  }
  
  return(proposed_likelihood - current_likelihood)
}

xacc_tot <- true_SA1_num_dwellings_by_DATUM_current*0
xacc_SA1 <- xacc_SA1X <- matrix(0,nrow=N_SA1,ncol=M)

#save.image("prefitx.dat")

posterior_draw_asthma <- rep(sum(SA2_num_dwellings_by_DATUM_obs[,1]+SA2_num_dwellings_by_DATUM_obsX[,1])/sum(SA2_num_dwellings_by_DATUM_obs[,1:2]+SA2_num_dwellings_by_DATUM_obsX[,1:2]),N_SA1)

compile("asthma_inla_perth_nocov.cpp")
dyn.load(dynlib("asthma_inla_perth_nocov"))
compile("asthma_inla_sydney_nocov.cpp")
dyn.load(dynlib("asthma_inla_sydney_nocov"))
compile("asthma_inla_melbourne_nocov.cpp")
dyn.load(dynlib("asthma_inla_melbourne_nocov"))
compile("asthma_inla_brisbane_nocov.cpp")
dyn.load(dynlib("asthma_inla_brisbane_nocov"))

logit_pred <- sa1_2021$FIE+sa1_2021$POL+sa1_2021$SES+sa1_2021$CLI

ilogit <- function(x) {1/(1+exp(-x))}
xtally <- 0
xcount <- xxcount <- xxxcount <- ycount <- 0
for (z in 1:30000000) {

  ### Regression Model
  
  if ((z %% 500000)==1 & z > 1000000) {

    xtally <- 0
    xacc_SA1 <- xacc_SA1*0
    xacc_SA1X <- xacc_SA1X*0

    cat("regression happening ...")
    ycount <- ycount + 1

    asthma_fraction_pos_stated <- true_SA1_num_dwellings_by_DATUM_current[,1]
    asthma_fraction_neg_stated <- true_SA1_num_dwellings_by_DATUM_current[,2]
    asthma_fraction_neg_stated[which((asthma_fraction_pos_stated+asthma_fraction_neg_stated)==0)] <- 1

    asthma_fraction_pos_statedX <- true_SA1_num_dwellings_by_DATUM_currentX[,1]
    asthma_fraction_neg_statedX <- true_SA1_num_dwellings_by_DATUM_currentX[,2]
    asthma_fraction_neg_statedX[which((asthma_fraction_pos_statedX+asthma_fraction_neg_statedX)==0)] <- 1

    # Perth
    input.data <- list('N_SA1'=N_SA1,
                        'N_SA1_perth'=sum(sa1_2021$GCC_NAME21=="Greater Perth"),
                       'spde_perth'=perth_spde_fine,
                       'A_perth'=perth_A_fine,
                       'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Perth"]
    )

    if (!exists('parameters_perth')) {parameters_perth <- list(
      'log_range'=0,
      'log_sd'=0,
      'intercept_perth'=-2,
      'field_perth'=rep(0,perth.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth_nocov",
                     random = c('intercept_perth','field_perth'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_perth_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_perth <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'intercept_perth'=xsample[names(xsample)=="intercept_perth"],
      'field_perth'=xsample[names(xsample)=="field_perth"]
    )
    
    obj <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth_nocov")
    xreport <- obj$report()

    posterior_draw_asthma_perth <- xreport$predicted_surface_asthma

    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xreport_perth_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_perth,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_posterior_draw_asthma_perth_",sprintf("%03i",ycount),".dat"))
    
    # Sydney
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_sydney'=sum(sa1_2021$GCC_NAME21=="Greater Sydney"),
                       'spde_sydney'=sydney_spde_fine,
                       'A_sydney'=sydney_A_fine,
                       'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Sydney"]
    )
    
    if (!exists('parameters_sydney')) {parameters_sydney <- list(
      'log_range'=0,
      'log_sd'=0,
      'intercept_sydney'=-2,
      'field_sydney'=rep(0,sydney.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney_nocov",
                     random = c('intercept_sydney','field_sydney'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_sydney_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_sydney <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'intercept_sydney'=xsample[names(xsample)=="intercept_sydney"],
      'field_sydney'=xsample[names(xsample)=="field_sydney"]
    )
    
    obj <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney_nocov")
    xreport <- obj$report()
    
    posterior_draw_asthma_sydney <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xreport_sydney_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_sydney,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_posterior_draw_asthma_sydney_",sprintf("%03i",ycount),".dat"))
    
    # Brisbane
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_brisbane'=sum(sa1_2021$GCC_NAME21=="Greater Brisbane"),
                       'spde_brisbane'=brisbane_spde_fine,
                       'A_brisbane'=brisbane_A_fine,
                       'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Brisbane"]
    )
    
    if (!exists('parameters_brisbane')) {parameters_brisbane <- list(
      'log_range'=0,
      'log_sd'=0,
      'intercept_brisbane'=-2,
      'field_brisbane'=rep(0,brisbane.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane_nocov",
                     random = c('intercept_brisbane','field_brisbane'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_brisbane_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_brisbane <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'intercept_brisbane'=xsample[names(xsample)=="intercept_brisbane"],
      'field_brisbane'=xsample[names(xsample)=="field_brisbane"]
    )
    
    obj <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane_nocov")
    xreport <- obj$report()
    
    posterior_draw_asthma_brisbane <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xreport_brisbane_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_brisbane,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_posterior_draw_asthma_brisbane_",sprintf("%03i",ycount),".dat"))
    
    # Melbourne
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_melbourne'=sum(sa1_2021$GCC_NAME21=="Greater Melbourne"),
                       'spde_melbourne'=melbourne_spde_fine,
                       'A_melbourne'=melbourne_A_fine,
                       'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Melbourne"]
    )
    
    if (!exists('parameters_melbourne')) {parameters_melbourne <- list(
      'log_range'=0,
      'log_sd'=0,
      'intercept_melbourne'=-2,
      'field_melbourne'=rep(0,melbourne.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne_nocov",
                     random = c('intercept_melbourne','field_melbourne'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_melbourne_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_melbourne <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'intercept_melbourne'=xsample[names(xsample)=="intercept_melbourne"],
      'field_melbourne'=xsample[names(xsample)=="field_melbourne"]
    )
    
    obj <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne_nocov")
    xreport <- obj$report()
    
    posterior_draw_asthma_melbourne <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xreport_melbourne_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_melbourne,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_posterior_draw_asthma_melbourne_",sprintf("%03i",ycount),".dat"))

    posterior_draw_asthma <- numeric(N_SA1)
    posterior_draw_asthma[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draw_asthma_perth
    posterior_draw_asthma[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draw_asthma_brisbane
    posterior_draw_asthma[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draw_asthma_sydney
    posterior_draw_asthma[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draw_asthma_melbourne
    
  }
  
  ### MCMC ACCEPT_REJECT
  
  proposed_location <- cbind(sample(1:N_SA1,1),sample(1:M,1,prob=c(10,3,1)))

  current_likelihood_diff <- 0
  
  proposed_agebin <- sample(1:2,1)
  
  # 5-9 data
  if (proposed_agebin==1) {
    
    proposed_move <- sample(c(-1,1),1)

    current_value <- true_SA1_num_dwellings_by_DATUM_current[proposed_location]
    
    if (current_value==0 & proposed_move==-1) {
    } else {
        
        # SA1 DATUM likelihood
        proposed_sa1_location <- proposed_location[1]
        current_value <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]]
        proposed_value <- current_value+proposed_move
        observed_value <- SA1_num_dwellings_by_DATUM_obs[proposed_sa1_location,proposed_location[2]]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)
        
        # SA1 ROW TOT likelihood
        current_value <- true_SA1_num_dwellings_current[proposed_sa1_location]
        proposed_value <- current_value+proposed_move
        observed_value <- SA1_num_dwellings_obs[proposed_sa1_location]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)
        
        # Regression priors

          current_pos <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,1]+true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,1]
          current_neg <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,2]+true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,2]

          proposed_pos <- current_pos
          proposed_neg <- current_neg
          if (proposed_location[2]==1) {proposed_pos <- proposed_pos+proposed_move}
          if (proposed_location[2]==2) {proposed_neg <- proposed_neg+proposed_move}

          proposed_priors <- current_priors <- 0

          if ((proposed_location[2] %in% c(1,2)) & ((current_pos+current_neg)>0 & (proposed_pos+proposed_neg)>0)) {
            current_priors <- current_priors + dbinom(current_pos,(current_pos+current_neg),posterior_draw_asthma[proposed_sa1_location],log=TRUE)
            proposed_priors <- proposed_priors + dbinom(proposed_pos,(proposed_pos+proposed_neg),posterior_draw_asthma[proposed_sa1_location],log=TRUE)
          }

          current_likelihood_diff <- current_likelihood_diff + proposed_priors - current_priors

        # SA2 DATUM likelihood
        proposed_sa2_location <- sa1sa2codes[proposed_location[1]]
        current_value <- true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]]
        proposed_value <- current_value+proposed_move
        observed_value <- SA2_num_dwellings_by_DATUM_obs[proposed_sa2_location,proposed_location[2]]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)
        
        # SA2 ROW TOT likelihood
        current_value <- true_SA2_num_dwellings_current[proposed_sa2_location]
        proposed_value <- current_value+proposed_move
        observed_value <- SA2_num_dwellings_obs[proposed_sa2_location]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)

        if (is.na(current_likelihood_diff) || current_likelihood_diff < log(runif(1))) {
          proposed_likelihood <- NA
        } else {
          
          xtally <- xtally+proposed_move
          
          true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]] <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,proposed_location[2]]+proposed_move
          true_SA1_num_dwellings_current[proposed_sa1_location] <- true_SA1_num_dwellings_current[proposed_sa1_location]+proposed_move
          
          true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]] <- true_SA2_num_dwellings_by_DATUM_current[proposed_sa2_location,proposed_location[2]]+proposed_move
          true_SA2_num_dwellings_current[proposed_sa2_location] <- true_SA2_num_dwellings_current[proposed_sa2_location]+proposed_move
          
          xacc_SA1[proposed_sa1_location,proposed_location[2]] <- xacc_SA1[proposed_sa1_location,proposed_location[2]] + 1
        }
      }
      
    } else if (proposed_agebin==2) {
    
    # 10-14
    proposed_move <- sample(c(-1,1),1)

    current_value <- true_SA1_num_dwellings_by_DATUM_currentX[proposed_location]
    
      if (current_value==0 & proposed_move==-1) {
      } else {
        
        # SA1 DATUM likelihood
        proposed_sa1_location <- proposed_location[1]
        current_value <- true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,proposed_location[2]]
        proposed_value <- current_value+proposed_move
        observed_value <- SA1_num_dwellings_by_DATUM_obsX[proposed_sa1_location,proposed_location[2]]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)

        # SA1 ROW TOT likelihood
        current_value <- true_SA1_num_dwellings_currentX[proposed_sa1_location]
        proposed_value <- current_value+proposed_move
        observed_value <- SA1_num_dwellings_obsX[proposed_sa1_location]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)

        # Regression priors

        current_pos <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,1]+true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,1]
        current_neg <- true_SA1_num_dwellings_by_DATUM_current[proposed_sa1_location,2]+true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,2]
        
        proposed_pos <- current_pos
        proposed_neg <- current_neg
        if (proposed_location[2]==1) {proposed_pos <- proposed_pos+proposed_move}
        if (proposed_location[2]==2) {proposed_neg <- proposed_neg+proposed_move}
        
        proposed_priors <- current_priors <- 0
        
        if ((proposed_location[2] %in% c(1,2)) & ((current_pos+current_neg)>0 & (proposed_pos+proposed_neg)>0)) {
          current_priors <- current_priors + dbinom(current_pos,(current_pos+current_neg),posterior_draw_asthma[proposed_sa1_location],log=TRUE)
          proposed_priors <- proposed_priors + dbinom(proposed_pos,(proposed_pos+proposed_neg),posterior_draw_asthma[proposed_sa1_location],log=TRUE)
        }
        
        current_likelihood_diff <- current_likelihood_diff + proposed_priors - current_priors
        
        # SA2 DATUM likelihood
        proposed_sa2_location <- sa1sa2codes[proposed_location[1]]
        current_value <- true_SA2_num_dwellings_by_DATUM_currentX[proposed_sa2_location,proposed_location[2]]
        proposed_value <- current_value+proposed_move
        observed_value <- SA2_num_dwellings_by_DATUM_obsX[proposed_sa2_location,proposed_location[2]]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)

        # SA2 ROW TOT likelihood
        current_value <- true_SA2_num_dwellings_currentX[proposed_sa2_location]
        proposed_value <- current_value+proposed_move
        observed_value <- SA2_num_dwellings_obsX[proposed_sa2_location]
        
        current_likelihood_diff <- current_likelihood_diff +log_likelihood_diff_fn(proposed_value,current_value,observed_value)
        
        if (is.na(current_likelihood_diff) || current_likelihood_diff < log(runif(1))) {
          proposed_likelihood <- NA
        } else {
          
          xtally <- xtally+proposed_move
          
          true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,proposed_location[2]] <- true_SA1_num_dwellings_by_DATUM_currentX[proposed_sa1_location,proposed_location[2]]+proposed_move
          true_SA1_num_dwellings_currentX[proposed_sa1_location] <- true_SA1_num_dwellings_currentX[proposed_sa1_location]+proposed_move
          
          true_SA2_num_dwellings_by_DATUM_currentX[proposed_sa2_location,proposed_location[2]] <- true_SA2_num_dwellings_by_DATUM_currentX[proposed_sa2_location,proposed_location[2]]+proposed_move
          true_SA2_num_dwellings_currentX[proposed_sa2_location] <- true_SA2_num_dwellings_currentX[proposed_sa2_location]+proposed_move
          
          xacc_SA1X[proposed_sa1_location,proposed_location[2]] <- xacc_SA1X[proposed_sa1_location,proposed_location[2]] + 1
        }
      }

  }
      
  if ( (z %% 100000)==1 ) {
    cat("c: ",xtally," x:",mean(xacc_SA1[,1]>1)," ",mean(xacc_SA1X[,1]>1)," ")
  }

  if ((z %% 500000)==0 & z > 1000000) {
    xcount <- xcount + 1
    save(true_SA1_num_dwellings_by_DATUM_current,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xhist_both_SA1_",sprintf("%03i",xcount),".dat"))
    xxcount <- xxcount + 1
    save(true_SA1_num_dwellings_by_DATUM_currentX,file=paste0("/mnt/Z/ewan/XASTHMA/yasthma_xhist_bothX_SA1_",sprintf("%03i",xxcount),".dat"))
  }
}

save.image("yasthma_postfit_asthma.dat")

### Visualise and check results

quantilehigh <- function(x) {quantile(x,0.95)}

sa1_2021_outputs <- sa1_2021

logit <- function(x) {log(x)-log(1-x)}

### Perth

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_perth'=sum(sa1_2021$GCC_NAME21=="Greater Perth"),
                   'spde_perth'=perth_spde_fine,
                   'A_perth'=perth_A_fine,
                   'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Perth"]
)

obj_perth <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth_nocov")

posterior_draws <- list()
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_perth_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_perth$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
    }
}
posterior_draws_perth <- do.call(rbind,posterior_draws)
posterior_draws_perth_median <- apply((posterior_draws_perth),2,median)

posterior_draws_perth_rankconf <- apply((posterior_draws_perth>median(posterior_draws_perth_median)),2,mean)
posterior_draws_perth_rankconf[posterior_draws_perth_rankconf < 0.05 | posterior_draws_perth_rankconf > 0.95] <- 1
posterior_draws_perth_rankconf[posterior_draws_perth_rankconf >= 0.05 & posterior_draws_perth_rankconf <= 0.95] <- 0

### Brisbane

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_brisbane'=sum(sa1_2021$GCC_NAME21=="Greater Brisbane"),
                   'spde_brisbane'=brisbane_spde_fine,
                   'A_brisbane'=brisbane_A_fine,
                   'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Brisbane"]
)

obj_brisbane <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane_nocov")

posterior_draws <- list()
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_brisbane_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_brisbane$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
  }
}
posterior_draws_brisbane <- do.call(rbind,posterior_draws)
posterior_draws_brisbane_median <- apply((posterior_draws_brisbane),2,median)

posterior_draws_brisbane_rankconf <- apply((posterior_draws_brisbane>median(posterior_draws_brisbane_median)),2,mean)
posterior_draws_brisbane_rankconf[posterior_draws_brisbane_rankconf < 0.05 | posterior_draws_brisbane_rankconf > 0.95] <- 1
posterior_draws_brisbane_rankconf[posterior_draws_brisbane_rankconf >= 0.05 & posterior_draws_brisbane_rankconf <= 0.95] <- 0

### Sydney

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_sydney'=sum(sa1_2021$GCC_NAME21=="Greater Sydney"),
                   'spde_sydney'=sydney_spde_fine,
                   'A_sydney'=sydney_A_fine,
                   'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Sydney"]
)

obj_sydney <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney_nocov")

posterior_draws <- list()
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_sydney_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_sydney$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
  }
}
posterior_draws_sydney <- do.call(rbind,posterior_draws)
posterior_draws_sydney_median <- apply((posterior_draws_sydney),2,median)

posterior_draws_sydney_rankconf <- apply((posterior_draws_sydney>median(posterior_draws_sydney_median)),2,mean)
posterior_draws_sydney_rankconf[posterior_draws_sydney_rankconf < 0.05 | posterior_draws_sydney_rankconf > 0.95] <- 1
posterior_draws_sydney_rankconf[posterior_draws_sydney_rankconf >= 0.05 & posterior_draws_sydney_rankconf <= 0.95] <- 0

### Melbourne

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_melbourne'=sum(sa1_2021$GCC_NAME21=="Greater Melbourne"),
                   'spde_melbourne'=melbourne_spde_fine,
                   'A_melbourne'=melbourne_A_fine,
                   'logit_pred'=logit_pred[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Melbourne"]
)

obj_melbourne <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne_nocov")

posterior_draws <- list()
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/yasthma_regression_melbourne_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_melbourne$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
  }
}
posterior_draws_melbourne <- do.call(rbind,posterior_draws)
posterior_draws_melbourne_median <- apply((posterior_draws_melbourne),2,median)

posterior_draws_melbourne_rankconf <- apply((posterior_draws_melbourne>median(posterior_draws_melbourne_median)),2,mean)
posterior_draws_melbourne_rankconf[posterior_draws_melbourne_rankconf < 0.05 | posterior_draws_melbourne_rankconf > 0.95] <- 1
posterior_draws_melbourne_rankconf[posterior_draws_melbourne_rankconf >= 0.05 & posterior_draws_melbourne_rankconf <= 0.95] <- 0

all_median <- numeric(N_SA1)
all_rank <- numeric(N_SA1)

all_median[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_perth_median
all_median[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_brisbane_median
all_median[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_melbourne_median
all_median[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_sydney_median

all_rank[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_perth_rankconf
all_rank[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_brisbane_rankconf
all_rank[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_melbourne_rankconf
all_rank[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_sydney_rankconf

sa1_2021_outputs <- readOGR("asthma_nonInd.shp")

tot_children <- SA1_num_dwellings_obs+SA1_num_dwellings_obsX

sa1_2021_outputs$INDV <- all_median
sa1_2021_outputs$INDR <- all_rank
sa1_2021_outputs$INDC <- tot_children




