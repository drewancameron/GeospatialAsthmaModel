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

sa1_2021 <- readOGR("sa1_2021_covs.shp")

# CENSUS DATA

filename_SA1 <- "SA1_by_HASTP_FourCities_59onlyAUS.csv"
filename_SA2 <- "SA2_by_HASTP_FourCities_59onlyAUS.csv"

# SA1 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=13,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=11,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)

# SA2 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=13,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=11,header = FALSE,nrows = 1)
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

filename_SA1 <- "SA1_by_HASTP_FourCities_1014onlyAUS.csv"
filename_SA2 <- "SA2_by_HASTP_FourCities_1014onlyAUS.csv"

# SA1 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=13,header = FALSE)
SA1_num_dwellings_by_DATUM <- as.numeric(xdata$V1)
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA1_num_dwellings_by_DATUM <- cbind(SA1_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA1),skip=11,header = FALSE,nrows = 1)
xcolnames <- unlist(c("SA1",ydata[-1][1:(length(SA1_num_dwellings_by_DATUM[1,])-1)]))
colnames(SA1_num_dwellings_by_DATUM) <- xcolnames
SA1_num_dwellings_by_DATUM <- as.data.frame(SA1_num_dwellings_by_DATUM)
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[-which(is.na(SA1_num_dwellings_by_DATUM$SA1)),]
SA1_num_dwellings_by_DATUM <- SA1_num_dwellings_by_DATUM[match(as.numeric(unique(sa1_2021$SA1_CODE21)),SA1_num_dwellings_by_DATUM$SA1),]
SA1_num_dwellings_by_DATUM <- as.matrix(SA1_num_dwellings_by_DATUM)

# SA2 level Tablebuilder Outputs
xdata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=13,header = FALSE)
SA2_num_dwellings_by_DATUM <- xdata$V1
for (i in 2:(dim(xdata)[2]-1)) {
  eval(parse(text=paste0("SA2_num_dwellings_by_DATUM <- cbind(SA2_num_dwellings_by_DATUM,as.numeric(xdata$V",i,"))")))
}
ydata <- read.csv(paste0("/mnt/Z/ewan/ABS_Tablebuilder/",filename_SA2),skip=11,header = FALSE,nrows = 1)
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

standardise <- function(x) {
  qq <- list()
  qq[[1]] <- (x[sa1_2021$GCC_NAME21=="Greater Perth"]-mean(x[sa1_2021$GCC_NAME21=="Greater Perth"]))/sd(x[sa1_2021$GCC_NAME21=="Greater Perth"])
  qq[[2]] <- (x[sa1_2021$GCC_NAME21=="Greater Sydney"]-mean(x[sa1_2021$GCC_NAME21=="Greater Sydney"]))/sd(x[sa1_2021$GCC_NAME21=="Greater Sydney"])
  qq[[3]] <- (x[sa1_2021$GCC_NAME21=="Greater Brisbane"]-mean(x[sa1_2021$GCC_NAME21=="Greater Brisbane"]))/sd(x[sa1_2021$GCC_NAME21=="Greater Brisbane"])
  qq[[4]] <- (x[sa1_2021$GCC_NAME21=="Greater Melbourne"]-mean(x[sa1_2021$GCC_NAME21=="Greater Melbourne"]))/sd(x[sa1_2021$GCC_NAME21=="Greater Melbourne"])
  pp <- numeric(N_SA1)
  pp[sa1_2021$GCC_NAME21=="Greater Perth"] <- qq[[1]]
  pp[sa1_2021$GCC_NAME21=="Greater Sydney"] <- qq[[2]]
  pp[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- qq[[3]]
  pp[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- qq[[4]]
  pp
}

qnt <- function(x) {qtruncnorm(pnorm(x),a=0)}
pollution_covariates <- cbind(
  standardise(sa1_2021$COM),
  standardise(sa1_2021$COX),
  standardise(sa1_2021$NOM),
  standardise(sa1_2021$NOX),
  standardise(sa1_2021$SOM),
  standardise(sa1_2021$SOX),
  standardise(sa1_2021$O3M),
  standardise(sa1_2021$O3X),
  standardise(sa1_2021$PM25),
  standardise(sa1_2021$RHD)
)
pollution_covariates[pollution_covariates>3] <- 3
pollution_covariates[pollution_covariates< -3] <- -3
pollution_covariates <- apply(pollution_covariates,2,qnt)

humid_hot_covariates <- cbind(
  standardise(sa1_2021$HUM),
  standardise(sa1_2021$HUA),
  standardise(sa1_2021$RAI),
  standardise(sa1_2021$LSTX),
  standardise(sa1_2021$LST)
)
humid_hot_covariates[humid_hot_covariates>3] <- 3
humid_hot_covariates[humid_hot_covariates< -3] <- -3
humid_hot_covariates <- apply(humid_hot_covariates,2,qnt)

dry_cold_covariates <- cbind(
  -standardise(sa1_2021$HUM),
  -standardise(sa1_2021$HUA),
  -standardise(sa1_2021$RAI),
  -standardise(sa1_2021$LSTM),
  -standardise(sa1_2021$LST)
)
dry_cold_covariates[dry_cold_covariates>3] <- 3
dry_cold_covariates[dry_cold_covariates< -3] <- -3
dry_cold_covariates <- apply(dry_cold_covariates,2,qnt)

vegetation <- cbind(
  standardise(sa1_2021$EVI),
  -standardise(sa1_2021$TCB),
  standardise(sa1_2021$WIN)
)
vegetation[vegetation>3] <- 3
vegetation[vegetation< -3] <- -3
vegetation <- apply(vegetation,2,qnt)

ses <- cbind(
  sa1_2021$IRS,
  -standardise(sa1_2021$OSK),
  -standardise(sa1_2021$HEA)
)
ses[ses>3] <- 3
ses[ses< -3] <- -3

lstdiff <- standardise(sa1_2021$LSTD)
lstdiff[lstdiff>3] <- 3
lstdiff[lstdiff< -3] <- -3

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

#save.image("prefit.dat")

posterior_draw_asthma <- rep(sum(SA2_num_dwellings_by_DATUM_obs[,1]+SA2_num_dwellings_by_DATUM_obsX[,1])/sum(SA2_num_dwellings_by_DATUM_obs[,1:2]+SA2_num_dwellings_by_DATUM_obsX[,1:2]),N_SA1)

compile("asthma_inla_perth.cpp")
dyn.load(dynlib("asthma_inla_perth"))
compile("asthma_inla_sydney.cpp")
dyn.load(dynlib("asthma_inla_sydney"))
compile("asthma_inla_melbourne.cpp")
dyn.load(dynlib("asthma_inla_melbourne"))
compile("asthma_inla_brisbane.cpp")
dyn.load(dynlib("asthma_inla_brisbane"))

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
                       'N_pollution'=length(pollution_covariates[1,]),
                       'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                       'N_dry_cold'=length(dry_cold_covariates[1,]),
                       'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                       'N_humid_hot'=length(humid_hot_covariates[1,]),
                       'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                       'N_vegetation'=length(vegetation[1,]),
                       'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Perth",],
                       'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Perth"]-1),
                       'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Perth"]
    )

    if (!exists('parameters_perth')) {parameters_perth <- list(
      'log_range'=0,
      'log_sd'=0,
      'logit_ses_par'=2,
      'log_ses_sd'=0,
      'log_pollution_norm'=rep(0,3),
      'log_interaction_pollution_cold'=-2,
      'log_interaction_pollution_hot'=-2,
      'log_interaction_vegetation_cold'=-2,
      'log_interaction_vegetation_hot'=-2,
      'intercept_perth'=-2,
      'log_pollution_slopes'=rep(-2,length(pollution_covariates[1,])),
      'log_dry_cold_slopes'=rep(-2,length(dry_cold_covariates[1,])),
      'log_humid_hot_slopes'=rep(-2,length(humid_hot_covariates[1,])),
      'log_vegetation_slopes'=rep(-2,length(vegetation[1,])),
      'log_lstdiff_slopes'=-2,
      'ses_slopes'=rep(0,10),
      'field_perth'=rep(0,perth.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth",
                     random = c('log_interaction_pollution_cold','log_interaction_pollution_hot','log_interaction_vegetation_cold','log_interaction_vegetation_hot','intercept_perth','log_pollution_slopes','log_dry_cold_slopes','log_humid_hot_slopes','log_vegetation_slopes','log_lstdiff_slopes','ses_slopes','field_perth'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_perth_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_perth <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'logit_ses_par'=xsample[names(xsample)=="logit_ses_par"],
      'log_ses_sd'=xsample[names(xsample)=="log_ses_sd"],
      'log_pollution_norm'=xsample[names(xsample)=="log_pollution_norm"],
      'log_interaction_pollution_cold'=xsample[names(xsample)=="log_interaction_pollution_cold"],
      'log_interaction_pollution_hot'=xsample[names(xsample)=="log_interaction_pollution_hot"],
      'log_interaction_vegetation_cold'=xsample[names(xsample)=="log_interaction_vegetation_cold"],
      'log_interaction_vegetation_hot'=xsample[names(xsample)=="log_interaction_vegetation_hot"],
      'intercept_perth'=xsample[names(xsample)=="intercept_perth"],
      'log_pollution_slopes'=xsample[names(xsample)=="log_pollution_slopes"],
      'log_dry_cold_slopes'=xsample[names(xsample)=="log_dry_cold_slopes"],
      'log_humid_hot_slopes'=xsample[names(xsample)=="log_humid_hot_slopes"],
      'log_vegetation_slopes'=xsample[names(xsample)=="log_vegetation_slopes"],
      'log_lstdiff_slopes'=xsample[names(xsample)=="log_lstdiff_slopes"],
      'ses_slopes'=xsample[names(xsample)=="ses_slopes"],
      'field_perth'=xsample[names(xsample)=="field_perth"]
    )
    
    obj <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth")
    xreport <- obj$report()

    posterior_draw_asthma_perth <- xreport$predicted_surface_asthma

    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xreport_perth_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_perth,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_posterior_draw_asthma_perth_",sprintf("%03i",ycount),".dat"))
    
    png(paste0("/mnt/Z/ewan/XASTHMA/xasthma_map_perth_",sprintf("%03i",ycount),".png"),width=5,height=5,units='in',res=300)
    layout(matrix(1:4,nrow=2,ncol=2))
    par(mai=c(0.05,0.05,0.05,0.05))
    xcol <- xreport$pollution
    xcol <- xcol-quantile(xreport$pollution,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$dry_cold+xreport$humid_hot+xreport$lstdiff_effects
    xcol <- xcol-quantile(xreport$dry_cold+xreport$humid_hot,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$ses_effects
    xcol <- xcol-quantile(xreport$ses_effects,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$vegetation
    xcol <- xcol-quantile(xreport$vegetation,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",],col=hsv(xcol),border=NA)
    
    dev.off()
    
    # Sydney
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_sydney'=sum(sa1_2021$GCC_NAME21=="Greater Sydney"),
                       'spde_sydney'=sydney_spde_fine,
                       'A_sydney'=sydney_A_fine,
                       'N_pollution'=length(pollution_covariates[1,]),
                       'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                       'N_dry_cold'=length(dry_cold_covariates[1,]),
                       'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                       'N_humid_hot'=length(humid_hot_covariates[1,]),
                       'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                       'N_vegetation'=length(vegetation[1,]),
                       'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Sydney",],
                       'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Sydney"]-1),
                       'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Sydney"]
    )
    
    if (!exists('parameters_sydney')) {parameters_sydney <- list(
      'log_range'=0,
      'log_sd'=0,
      'logit_ses_par'=2,
      'log_ses_sd'=0,
      'log_pollution_norm'=rep(0,3),
      'log_interaction_pollution_cold'=-2,
      'log_interaction_pollution_hot'=-2,
      'log_interaction_vegetation_cold'=-2,
      'log_interaction_vegetation_hot'=-2,
      'intercept_sydney'=-2,
      'log_pollution_slopes'=rep(-2,length(pollution_covariates[1,])),
      'log_dry_cold_slopes'=rep(-2,length(dry_cold_covariates[1,])),
      'log_humid_hot_slopes'=rep(-2,length(humid_hot_covariates[1,])),
      'log_vegetation_slopes'=rep(-2,length(vegetation[1,])),
      'log_lstdiff_slopes'=-2,
      'ses_slopes'=rep(0,10),
      'field_sydney'=rep(0,sydney.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney",
                     random = c('log_interaction_pollution_cold','log_interaction_pollution_hot','log_interaction_vegetation_cold','log_interaction_vegetation_hot','intercept_sydney','log_pollution_slopes','log_dry_cold_slopes','log_humid_hot_slopes','log_vegetation_slopes','log_lstdiff_slopes','ses_slopes','field_sydney'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_sydney_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_sydney <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'logit_ses_par'=xsample[names(xsample)=="logit_ses_par"],
      'log_ses_sd'=xsample[names(xsample)=="log_ses_sd"],
      'log_pollution_norm'=xsample[names(xsample)=="log_pollution_norm"],
      'log_interaction_pollution_cold'=xsample[names(xsample)=="log_interaction_pollution_cold"],
      'log_interaction_pollution_hot'=xsample[names(xsample)=="log_interaction_pollution_hot"],
      'log_interaction_vegetation_cold'=xsample[names(xsample)=="log_interaction_vegetation_cold"],
      'log_interaction_vegetation_hot'=xsample[names(xsample)=="log_interaction_vegetation_hot"],
      'intercept_sydney'=xsample[names(xsample)=="intercept_sydney"],
      'log_pollution_slopes'=xsample[names(xsample)=="log_pollution_slopes"],
      'log_dry_cold_slopes'=xsample[names(xsample)=="log_dry_cold_slopes"],
      'log_humid_hot_slopes'=xsample[names(xsample)=="log_humid_hot_slopes"],
      'log_vegetation_slopes'=xsample[names(xsample)=="log_vegetation_slopes"],
      'log_lstdiff_slopes'=xsample[names(xsample)=="log_lstdiff_slopes"],
      'ses_slopes'=xsample[names(xsample)=="ses_slopes"],
      'field_sydney'=xsample[names(xsample)=="field_sydney"]
    )
    
    obj <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney")
    xreport <- obj$report()
    
    posterior_draw_asthma_sydney <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xreport_sydney_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_sydney,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_posterior_draw_asthma_sydney_",sprintf("%03i",ycount),".dat"))
    
    png(paste0("/mnt/Z/ewan/XASTHMA/xasthma_map_sydney_",sprintf("%03i",ycount),".png"),width=5,height=5,units='in',res=300)
    layout(matrix(1:4,nrow=2,ncol=2))
    par(mai=c(0.05,0.05,0.05,0.05))
    xcol <- xreport$pollution
    xcol <- xcol-quantile(xreport$pollution,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$dry_cold+xreport$humid_hot+xreport$lstdiff_effects
    xcol <- xcol-quantile(xreport$dry_cold+xreport$humid_hot,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$ses_effects
    xcol <- xcol-quantile(xreport$ses_effects,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$vegetation
    xcol <- xcol-quantile(xreport$vegetation,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",],col=hsv(xcol),border=NA)
    
    dev.off()
    
    # Brisbane
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_brisbane'=sum(sa1_2021$GCC_NAME21=="Greater Brisbane"),
                       'spde_brisbane'=brisbane_spde_fine,
                       'A_brisbane'=brisbane_A_fine,
                       'N_pollution'=length(pollution_covariates[1,]),
                       'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                       'N_dry_cold'=length(dry_cold_covariates[1,]),
                       'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                       'N_humid_hot'=length(humid_hot_covariates[1,]),
                       'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                       'N_vegetation'=length(vegetation[1,]),
                       'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                       'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Brisbane"]-1),
                       'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Brisbane"]
    )
    
    if (!exists('parameters_brisbane')) {parameters_brisbane <- list(
      'log_range'=0,
      'log_sd'=0,
      'logit_ses_par'=2,
      'log_ses_sd'=0,
      'log_pollution_norm'=rep(0,3),
      'log_interaction_pollution_cold'=-2,
      'log_interaction_pollution_hot'=-2,
      'log_interaction_vegetation_cold'=-2,
      'log_interaction_vegetation_hot'=-2,
      'intercept_brisbane'=-2,
      'log_pollution_slopes'=rep(-2,length(pollution_covariates[1,])),
      'log_dry_cold_slopes'=rep(-2,length(dry_cold_covariates[1,])),
      'log_humid_hot_slopes'=rep(-2,length(humid_hot_covariates[1,])),
      'log_vegetation_slopes'=rep(-2,length(vegetation[1,])),
      'log_lstdiff_slopes'=-2,
      'ses_slopes'=rep(0,10),
      'field_brisbane'=rep(0,brisbane.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane",
                     random = c('log_interaction_pollution_cold','log_interaction_pollution_hot','log_interaction_vegetation_cold','log_interaction_vegetation_hot','intercept_brisbane','log_pollution_slopes','log_dry_cold_slopes','log_humid_hot_slopes','log_vegetation_slopes','log_lstdiff_slopes','ses_slopes','field_brisbane'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_brisbane_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_brisbane <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'logit_ses_par'=xsample[names(xsample)=="logit_ses_par"],
      'log_ses_sd'=xsample[names(xsample)=="log_ses_sd"],
      'log_pollution_norm'=xsample[names(xsample)=="log_pollution_norm"],
      'log_interaction_pollution_cold'=xsample[names(xsample)=="log_interaction_pollution_cold"],
      'log_interaction_pollution_hot'=xsample[names(xsample)=="log_interaction_pollution_hot"],
      'log_interaction_vegetation_cold'=xsample[names(xsample)=="log_interaction_vegetation_cold"],
      'log_interaction_vegetation_hot'=xsample[names(xsample)=="log_interaction_vegetation_hot"],
      'intercept_brisbane'=xsample[names(xsample)=="intercept_brisbane"],
      'log_pollution_slopes'=xsample[names(xsample)=="log_pollution_slopes"],
      'log_dry_cold_slopes'=xsample[names(xsample)=="log_dry_cold_slopes"],
      'log_humid_hot_slopes'=xsample[names(xsample)=="log_humid_hot_slopes"],
      'log_vegetation_slopes'=xsample[names(xsample)=="log_vegetation_slopes"],
      'log_lstdiff_slopes'=xsample[names(xsample)=="log_lstdiff_slopes"],
      'ses_slopes'=xsample[names(xsample)=="ses_slopes"],
      'field_brisbane'=xsample[names(xsample)=="field_brisbane"]
    )
    
    obj <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane")
    xreport <- obj$report()
    
    posterior_draw_asthma_brisbane <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xreport_brisbane_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_brisbane,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_posterior_draw_asthma_brisbane_",sprintf("%03i",ycount),".dat"))
    
    png(paste0("/mnt/Z/ewan/XASTHMA/xasthma_map_brisbane_",sprintf("%03i",ycount),".png"),width=5,height=5,units='in',res=300)
    layout(matrix(1:4,nrow=2,ncol=2))
    par(mai=c(0.05,0.05,0.05,0.05))
    xcol <- xreport$pollution
    xcol <- xcol-quantile(xreport$pollution,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$dry_cold+xreport$humid_hot+xreport$lstdiff_effects
    xcol <- xcol-quantile(xreport$dry_cold+xreport$humid_hot,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$ses_effects
    xcol <- xcol-quantile(xreport$ses_effects,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$vegetation
    xcol <- xcol-quantile(xreport$vegetation,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",],col=hsv(xcol),border=NA)
    
    dev.off()
    
    # Melbourne
    
    input.data <- list('N_SA1'=N_SA1,
                       'N_SA1_melbourne'=sum(sa1_2021$GCC_NAME21=="Greater Melbourne"),
                       'spde_melbourne'=melbourne_spde_fine,
                       'A_melbourne'=melbourne_A_fine,
                       'N_pollution'=length(pollution_covariates[1,]),
                       'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                       'N_dry_cold'=length(dry_cold_covariates[1,]),
                       'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                       'N_humid_hot'=length(humid_hot_covariates[1,]),
                       'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                       'N_vegetation'=length(vegetation[1,]),
                       'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                       'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Melbourne"]-1),
                       'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                       'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Melbourne"]
    )
    
    if (!exists('parameters_melbourne')) {parameters_melbourne <- list(
      'log_range'=0,
      'log_sd'=0,
      'logit_ses_par'=2,
      'log_ses_sd'=0,
      'log_pollution_norm'=rep(0,3),
      'log_interaction_pollution_cold'=-2,
      'log_interaction_pollution_hot'=-2,
      'log_interaction_vegetation_cold'=-2,
      'log_interaction_vegetation_hot'=-2,
      'intercept_melbourne'=-2,
      'log_pollution_slopes'=rep(-2,length(pollution_covariates[1,])),
      'log_dry_cold_slopes'=rep(-2,length(dry_cold_covariates[1,])),
      'log_humid_hot_slopes'=rep(-2,length(humid_hot_covariates[1,])),
      'log_vegetation_slopes'=rep(-2,length(vegetation[1,])),
      'log_lstdiff_slopes'=-2,
      'ses_slopes'=rep(0,10),
      'field_melbourne'=rep(0,melbourne.mesh$n)
    )}
    
    obj <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne",
                     random = c('log_interaction_pollution_cold','log_interaction_pollution_hot','log_interaction_vegetation_cold','log_interaction_vegetation_hot','intercept_melbourne','log_pollution_slopes','log_dry_cold_slopes','log_humid_hot_slopes','log_vegetation_slopes','log_lstdiff_slopes','ses_slopes','field_melbourne'))
    gc()
    obj$fn()
    gc()
    opt <- nlminb(obj$par, obj$fn, obj$gr)
    rep <- sdreport(obj, getJointPrecision = TRUE)
    
    xsample <- rmvn.sparse(10, c(rep$par.fixed, rep$par.random), Cholesky(rep$jointPrecision), prec = TRUE)
    colnames(xsample) <- names(c(rep$par.fixed,rep$par.random))
    save(xsample,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_melbourne_",sprintf("%03i",ycount),".dat"))
    xsample <- xsample[1,]
    
    parameters_melbourne <- list(
      'log_range'=xsample[names(xsample)=="log_range"],
      'log_sd'=xsample[names(xsample)=="log_sd"],
      'logit_ses_par'=xsample[names(xsample)=="logit_ses_par"],
      'log_ses_sd'=xsample[names(xsample)=="log_ses_sd"],
      'log_pollution_norm'=xsample[names(xsample)=="log_pollution_norm"],
      'log_interaction_pollution_cold'=xsample[names(xsample)=="log_interaction_pollution_cold"],
      'log_interaction_pollution_hot'=xsample[names(xsample)=="log_interaction_pollution_hot"],
      'log_interaction_vegetation_cold'=xsample[names(xsample)=="log_interaction_vegetation_cold"],
      'log_interaction_vegetation_hot'=xsample[names(xsample)=="log_interaction_vegetation_hot"],
      'intercept_melbourne'=xsample[names(xsample)=="intercept_melbourne"],
      'log_pollution_slopes'=xsample[names(xsample)=="log_pollution_slopes"],
      'log_dry_cold_slopes'=xsample[names(xsample)=="log_dry_cold_slopes"],
      'log_humid_hot_slopes'=xsample[names(xsample)=="log_humid_hot_slopes"],
      'log_vegetation_slopes'=xsample[names(xsample)=="log_vegetation_slopes"],
      'log_lstdiff_slopes'=xsample[names(xsample)=="log_lstdiff_slopes"],
      'ses_slopes'=xsample[names(xsample)=="ses_slopes"],
      'field_melbourne'=xsample[names(xsample)=="field_melbourne"]
    )
    
    obj <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne")
    xreport <- obj$report()
    
    posterior_draw_asthma_melbourne <- xreport$predicted_surface_asthma
    
    save(xreport,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xreport_melbourne_",sprintf("%03i",ycount),".dat"))
    save(posterior_draw_asthma_melbourne,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_posterior_draw_asthma_melbourne_",sprintf("%03i",ycount),".dat"))
    
    png(paste0("/mnt/Z/ewan/XASTHMA/xasthma_map_melbourne_",sprintf("%03i",ycount),".png"),width=5,height=5,units='in',res=300)
    layout(matrix(1:4,nrow=2,ncol=2))
    par(mai=c(0.05,0.05,0.05,0.05))
    xcol <- xreport$pollution
    xcol <- xcol-quantile(xreport$pollution,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$dry_cold+xreport$humid_hot+xreport$lstdiff_effects
    xcol <- xcol-quantile(xreport$dry_cold+xreport$humid_hot,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$ses_effects
    xcol <- xcol-quantile(xreport$ses_effects,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",],col=hsv(xcol),border=NA)
    
    xcol <- xreport$vegetation
    xcol <- xcol-quantile(xreport$vegetation,0.05)
    xcol[xcol < 0.05] <- 0.05
    xcol[xcol > 0.5] <- 0.5
    xcol <- xcol/(0.5)
    xcol <- (1-xcol)*0.666
    xcol[sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",]$MUA==0] <- 0.85
    plot(sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",],col=hsv(xcol),border=NA)
    
    dev.off()

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
    save(true_SA1_num_dwellings_by_DATUM_current,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xhist_both_SA1_",sprintf("%03i",xcount),".dat"))
    xxcount <- xxcount + 1
    save(true_SA1_num_dwellings_by_DATUM_currentX,file=paste0("/mnt/Z/ewan/XASTHMA/xasthma_xhist_bothX_SA1_",sprintf("%03i",xxcount),".dat"))
  }
}

save.image("xasthma_postfit_asthma.dat")

### Visualise and check results

quantilehigh <- function(x) {quantile(x,0.95)}

sa1_2021_outputs <- sa1_2021

logit <- function(x) {log(x)-log(1-x)}

### Perth

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_perth'=sum(sa1_2021$GCC_NAME21=="Greater Perth"),
                   'spde_perth'=perth_spde_fine,
                   'A_perth'=perth_A_fine,
                   'N_pollution'=length(pollution_covariates[1,]),
                   'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                   'N_dry_cold'=length(dry_cold_covariates[1,]),
                   'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                   'N_humid_hot'=length(humid_hot_covariates[1,]),
                   'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Perth",],
                   'N_vegetation'=length(vegetation[1,]),
                   'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Perth",],
                   'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Perth"]-1),
                   'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Perth"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Perth"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Perth"]
)

obj_perth <- MakeADFun(input.data,parameters_perth,DLL = "asthma_inla_perth")

posterior_draws <- list()
posterior_draws_pollution <- list()
posterior_draws_climenv <- list()
posterior_draws_ses <- list()
posterior_draws_field <- list()
posterior_draws_var_explained <- list()
posterior_draws_var_explained_raw <- list()
in.mua <- which(sa1_2021[sa1_2021$GCC_NAME21=="Greater Perth",]$MUA==1)
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_perth_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_perth$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
    posterior_draws_pollution[[length(posterior_draws_pollution)+1]] <- pp$pollution
    posterior_draws_climenv[[length(posterior_draws_climenv)+1]] <- pp$dry_cold+pp$humid_hot+pp$vegetation+pp$lstdiff_effects
    posterior_draws_ses[[length(posterior_draws_ses)+1]] <- pp$ses_effects-pp$ses_effects[which(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Perth"]==1)[1]]
    posterior_draws_field[[length(posterior_draws_field)+1]] <- logit(pp$predicted_surface_asthma)-posterior_draws_pollution[[length(posterior_draws_pollution)]]-posterior_draws_climenv[[length(posterior_draws_climenv)]]-posterior_draws_ses[[length(posterior_draws_ses)]] 
    posterior_draws_field[[length(posterior_draws_field)]] <- posterior_draws_field[[length(posterior_draws_field)]]-mean(posterior_draws_field[[length(posterior_draws_field)]])
    posterior_draws_var_explained[[length(posterior_draws_var_explained)+1]] <- c(
      sd(posterior_draws_pollution[[length(posterior_draws_pollution)]][in.mua])^2,
      sd(posterior_draws_climenv[[length(posterior_draws_climenv)]][in.mua])^2,
      sd(posterior_draws_ses[[length(posterior_draws_ses)]][in.mua])^2
    )
    posterior_draws_var_explained_raw[[length(posterior_draws_var_explained_raw)+1]] <- sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])/(sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])+sd(posterior_draws_field[[length(posterior_draws_field)]][in.mua])^2)
    posterior_draws_var_explained[[length(posterior_draws_var_explained)]] <-  posterior_draws_var_explained[[length(posterior_draws_var_explained)]] /sum( posterior_draws_var_explained[[length(posterior_draws_var_explained)]] )
    }
}
posterior_draws_perth <- do.call(rbind,posterior_draws)
posterior_draws_pollution_perth <- do.call(rbind,posterior_draws_pollution)
posterior_draws_climenv_perth <- do.call(rbind,posterior_draws_climenv)
posterior_draws_ses_perth <- do.call(rbind,posterior_draws_ses)
posterior_draws_field_perth <- do.call(rbind,posterior_draws_field)
posterior_draws_var_explained_perth_raw <- unlist(posterior_draws_var_explained_raw)
posterior_draws_var_explained_perth <- do.call(rbind,posterior_draws_var_explained)

posterior_draws_pollution_perth_median <- apply((posterior_draws_pollution_perth),2,median)
posterior_draws_climenv_perth_median <- apply((posterior_draws_climenv_perth),2,median)
posterior_draws_ses_perth_median <- apply((posterior_draws_ses_perth),2,median)
posterior_draws_field_perth_median <- apply((posterior_draws_field_perth),2,median)
posterior_draws_pollution_perth_median <- posterior_draws_pollution_perth_median-mean(posterior_draws_pollution_perth_median)
posterior_draws_climenv_perth_median <- posterior_draws_climenv_perth_median-mean(posterior_draws_climenv_perth_median)
posterior_draws_ses_perth_median <- posterior_draws_ses_perth_median-mean(posterior_draws_ses_perth_median)
posterior_draws_field_perth_median <- posterior_draws_field_perth_median-mean(posterior_draws_field_perth_median)
posterior_draws_var_explained_perth_median <- apply((posterior_draws_var_explained_perth),2,median)
posterior_draws_var_explained_perth_high <- apply((posterior_draws_var_explained_perth),2,quantilehigh)
posterior_draws_var_explained_perth_raw_median <- median(posterior_draws_var_explained_perth_raw)
posterior_draws_var_explained_perth_raw_high <- quantilehigh(posterior_draws_var_explained_perth_raw)

### Brisbane

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_brisbane'=sum(sa1_2021$GCC_NAME21=="Greater Brisbane"),
                   'spde_brisbane'=brisbane_spde_fine,
                   'A_brisbane'=brisbane_A_fine,
                   'N_pollution'=length(pollution_covariates[1,]),
                   'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                   'N_dry_cold'=length(dry_cold_covariates[1,]),
                   'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                   'N_humid_hot'=length(humid_hot_covariates[1,]),
                   'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                   'N_vegetation'=length(vegetation[1,]),
                   'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Brisbane",],
                   'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Brisbane"]-1),
                   'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Brisbane"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Brisbane"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Brisbane"]
)

obj_brisbane <- MakeADFun(input.data,parameters_brisbane,DLL = "asthma_inla_brisbane")

posterior_draws <- list()
posterior_draws_pollution <- list()
posterior_draws_climenv <- list()
posterior_draws_ses <- list()
posterior_draws_field <- list()
posterior_draws_var_explained <- list()
in.mua <- which(sa1_2021[sa1_2021$GCC_NAME21=="Greater Brisbane",]$MUA==1)
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_brisbane_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_brisbane$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
    posterior_draws_pollution[[length(posterior_draws_pollution)+1]] <- pp$pollution
    posterior_draws_climenv[[length(posterior_draws_climenv)+1]] <- pp$dry_cold+pp$humid_hot+pp$vegetation+pp$lstdiff_effects
    posterior_draws_ses[[length(posterior_draws_ses)+1]] <- pp$ses_effects-pp$ses_effects[which(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Brisbane"]==1)[1]]
    posterior_draws_field[[length(posterior_draws_field)+1]] <- logit(pp$predicted_surface_asthma)-posterior_draws_pollution[[length(posterior_draws_pollution)]]-posterior_draws_climenv[[length(posterior_draws_climenv)]]-posterior_draws_ses[[length(posterior_draws_ses)]] 
    posterior_draws_field[[length(posterior_draws_field)]] <- posterior_draws_field[[length(posterior_draws_field)]]-mean(posterior_draws_field[[length(posterior_draws_field)]])
    posterior_draws_var_explained[[length(posterior_draws_var_explained)+1]] <- c(
      sd(posterior_draws_pollution[[length(posterior_draws_pollution)]][in.mua])^2,
      sd(posterior_draws_climenv[[length(posterior_draws_climenv)]][in.mua])^2,
      sd(posterior_draws_ses[[length(posterior_draws_ses)]][in.mua])^2
    )
    posterior_draws_var_explained_raw[[length(posterior_draws_var_explained_raw)+1]] <- sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])/(sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])+sd(posterior_draws_field[[length(posterior_draws_field)]][in.mua])^2)
    posterior_draws_var_explained[[length(posterior_draws_var_explained)]] <-  posterior_draws_var_explained[[length(posterior_draws_var_explained)]] /sum( posterior_draws_var_explained[[length(posterior_draws_var_explained)]] )
  }
}
posterior_draws_brisbane <- do.call(rbind,posterior_draws)
posterior_draws_pollution_brisbane <- do.call(rbind,posterior_draws_pollution)
posterior_draws_climenv_brisbane <- do.call(rbind,posterior_draws_climenv)
posterior_draws_ses_brisbane <- do.call(rbind,posterior_draws_ses)
posterior_draws_field_brisbane <- do.call(rbind,posterior_draws_field)
posterior_draws_var_explained_brisbane <- do.call(rbind,posterior_draws_var_explained)
posterior_draws_var_explained_brisbane_raw <- unlist(posterior_draws_var_explained_raw)

posterior_draws_pollution_brisbane_median <- apply((posterior_draws_pollution_brisbane),2,median)
posterior_draws_climenv_brisbane_median <- apply((posterior_draws_climenv_brisbane),2,median)
posterior_draws_ses_brisbane_median <- apply((posterior_draws_ses_brisbane),2,median)
posterior_draws_field_brisbane_median <- apply((posterior_draws_field_brisbane),2,median)
posterior_draws_pollution_brisbane_median <- posterior_draws_pollution_brisbane_median-mean(posterior_draws_pollution_brisbane_median)
posterior_draws_climenv_brisbane_median <- posterior_draws_climenv_brisbane_median-mean(posterior_draws_climenv_brisbane_median)
posterior_draws_ses_brisbane_median <- posterior_draws_ses_brisbane_median-mean(posterior_draws_ses_brisbane_median)
posterior_draws_field_brisbane_median <- posterior_draws_field_brisbane_median-mean(posterior_draws_field_brisbane_median)
posterior_draws_var_explained_brisbane_median <- apply((posterior_draws_var_explained_brisbane),2,median)
posterior_draws_var_explained_brisbane_high <- apply((posterior_draws_var_explained_brisbane),2,quantilehigh)
posterior_draws_var_explained_brisbane_raw_median <- median(posterior_draws_var_explained_brisbane_raw)
posterior_draws_var_explained_brisbane_raw_high <- quantilehigh(posterior_draws_var_explained_brisbane_raw)

### Melbourne

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_melbourne'=sum(sa1_2021$GCC_NAME21=="Greater Melbourne"),
                   'spde_melbourne'=melbourne_spde_fine,
                   'A_melbourne'=melbourne_A_fine,
                   'N_pollution'=length(pollution_covariates[1,]),
                   'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                   'N_dry_cold'=length(dry_cold_covariates[1,]),
                   'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                   'N_humid_hot'=length(humid_hot_covariates[1,]),
                   'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                   'N_vegetation'=length(vegetation[1,]),
                   'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Melbourne",],
                   'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Melbourne"]-1),
                   'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Melbourne"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Melbourne"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Melbourne"]
)

obj_melbourne <- MakeADFun(input.data,parameters_melbourne,DLL = "asthma_inla_melbourne")

posterior_draws <- list()
posterior_draws_pollution <- list()
posterior_draws_climenv <- list()
posterior_draws_ses <- list()
posterior_draws_field <- list()
posterior_draws_var_explained <- list()
in.mua <- which(sa1_2021[sa1_2021$GCC_NAME21=="Greater Melbourne",]$MUA==1)
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_melbourne_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_melbourne$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
    posterior_draws_pollution[[length(posterior_draws_pollution)+1]] <- pp$pollution
    posterior_draws_climenv[[length(posterior_draws_climenv)+1]] <- pp$dry_cold+pp$humid_hot+pp$vegetation+pp$lstdiff_effects
    posterior_draws_ses[[length(posterior_draws_ses)+1]] <- pp$ses_effects-pp$ses_effects[which(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Melbourne"]==1)[1]]
    posterior_draws_field[[length(posterior_draws_field)+1]] <- logit(pp$predicted_surface_asthma)-posterior_draws_pollution[[length(posterior_draws_pollution)]]-posterior_draws_climenv[[length(posterior_draws_climenv)]]-posterior_draws_ses[[length(posterior_draws_ses)]] 
    posterior_draws_field[[length(posterior_draws_field)]] <- posterior_draws_field[[length(posterior_draws_field)]]-mean(posterior_draws_field[[length(posterior_draws_field)]])
    posterior_draws_var_explained[[length(posterior_draws_var_explained)+1]] <- c(
      sd(posterior_draws_pollution[[length(posterior_draws_pollution)]][in.mua])^2,
      sd(posterior_draws_climenv[[length(posterior_draws_climenv)]][in.mua])^2,
      sd(posterior_draws_ses[[length(posterior_draws_ses)]][in.mua])^2
    )
    posterior_draws_var_explained_raw[[length(posterior_draws_var_explained_raw)+1]] <- sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])/(sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])+sd(posterior_draws_field[[length(posterior_draws_field)]][in.mua])^2)
    
    posterior_draws_var_explained[[length(posterior_draws_var_explained)]] <-  posterior_draws_var_explained[[length(posterior_draws_var_explained)]] /sum( posterior_draws_var_explained[[length(posterior_draws_var_explained)]] )
  }
}
posterior_draws_melbourne <- do.call(rbind,posterior_draws)
posterior_draws_pollution_melbourne <- do.call(rbind,posterior_draws_pollution)
posterior_draws_climenv_melbourne <- do.call(rbind,posterior_draws_climenv)
posterior_draws_ses_melbourne <- do.call(rbind,posterior_draws_ses)
posterior_draws_field_melbourne <- do.call(rbind,posterior_draws_field)
posterior_draws_var_explained_melbourne <- do.call(rbind,posterior_draws_var_explained)
posterior_draws_var_explained_melbourne_raw <- unlist(posterior_draws_var_explained_raw)

posterior_draws_pollution_melbourne_median <- apply((posterior_draws_pollution_melbourne),2,median)
posterior_draws_climenv_melbourne_median <- apply((posterior_draws_climenv_melbourne),2,median)
posterior_draws_ses_melbourne_median <- apply((posterior_draws_ses_melbourne),2,median)
posterior_draws_field_melbourne_median <- apply((posterior_draws_field_melbourne),2,median)
posterior_draws_pollution_melbourne_median <- posterior_draws_pollution_melbourne_median-mean(posterior_draws_pollution_melbourne_median)
posterior_draws_climenv_melbourne_median <- posterior_draws_climenv_melbourne_median-mean(posterior_draws_climenv_melbourne_median)
posterior_draws_ses_melbourne_median <- posterior_draws_ses_melbourne_median-mean(posterior_draws_ses_melbourne_median)
posterior_draws_field_melbourne_median <- posterior_draws_field_melbourne_median-mean(posterior_draws_field_melbourne_median)
posterior_draws_var_explained_melbourne_median <- apply((posterior_draws_var_explained_melbourne),2,median)
posterior_draws_var_explained_melbourne_high <- apply((posterior_draws_var_explained_melbourne),2,quantilehigh)
posterior_draws_var_explained_melbourne_raw_median <- median(posterior_draws_var_explained_melbourne_raw)
posterior_draws_var_explained_melbourne_raw_high <- quantilehigh(posterior_draws_var_explained_melbourne_raw)

### Sydney

input.data <- list('N_SA1'=N_SA1,
                   'N_SA1_sydney'=sum(sa1_2021$GCC_NAME21=="Greater Sydney"),
                   'spde_sydney'=sydney_spde_fine,
                   'A_sydney'=sydney_A_fine,
                   'N_pollution'=length(pollution_covariates[1,]),
                   'pollution_covariates'=pollution_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                   'N_dry_cold'=length(dry_cold_covariates[1,]),
                   'dry_cold_covariates'=dry_cold_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                   'N_humid_hot'=length(humid_hot_covariates[1,]),
                   'humid_hot_covariates'=humid_hot_covariates[sa1_2021$GCC_NAME21=="Greater Sydney",],
                   'N_vegetation'=length(vegetation[1,]),
                   'vegetation_covariates'=vegetation[sa1_2021$GCC_NAME21=="Greater Sydney",],
                   'ses'=as.integer(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Sydney"]-1),
                   'lstdiff'=lstdiff[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'npos' = asthma_fraction_pos_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_pos_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'nneg' = asthma_fraction_neg_stated[sa1_2021$GCC_NAME21=="Greater Sydney"]+asthma_fraction_neg_statedX[sa1_2021$GCC_NAME21=="Greater Sydney"],
                   'MUA'=0.1+0.9*sa1_2021$MUA[sa1_2021$GCC_NAME21=="Greater Sydney"]
)

obj_sydney <- MakeADFun(input.data,parameters_sydney,DLL = "asthma_inla_sydney")

posterior_draws <- list()
posterior_draws_pollution <- list()
posterior_draws_climenv <- list()
posterior_draws_ses <- list()
posterior_draws_field <- list()
posterior_draws_var_explained <- list()
in.mua <- which(sa1_2021[sa1_2021$GCC_NAME21=="Greater Sydney",]$MUA==1)
for (i in 7:58) {
  cat(i," ")
  load(paste0("/mnt/Z/ewan/XASTHMA/xasthma_regression_sydney_",sprintf("%03i",i),".dat"))
  for (j in 1:10) {
    pp <- obj_sydney$report(xsample[j,])
    posterior_draws[[length(posterior_draws)+1]] <- pp$predicted_surface_asthma
    posterior_draws_pollution[[length(posterior_draws_pollution)+1]] <- pp$pollution
    posterior_draws_climenv[[length(posterior_draws_climenv)+1]] <- pp$dry_cold+pp$humid_hot+pp$vegetation+pp$lstdiff_effects
    posterior_draws_ses[[length(posterior_draws_ses)+1]] <- pp$ses_effects-pp$ses_effects[which(sa1_2021$IRS[sa1_2021$GCC_NAME21=="Greater Sydney"]==1)[1]]
    posterior_draws_field[[length(posterior_draws_field)+1]] <- logit(pp$predicted_surface_asthma)-posterior_draws_pollution[[length(posterior_draws_pollution)]]-posterior_draws_climenv[[length(posterior_draws_climenv)]]-posterior_draws_ses[[length(posterior_draws_ses)]] 
    posterior_draws_field[[length(posterior_draws_field)]] <- posterior_draws_field[[length(posterior_draws_field)]]-mean(posterior_draws_field[[length(posterior_draws_field)]])
    posterior_draws_var_explained[[length(posterior_draws_var_explained)+1]] <- c(
      sd(posterior_draws_pollution[[length(posterior_draws_pollution)]][in.mua])^2,
      sd(posterior_draws_climenv[[length(posterior_draws_climenv)]][in.mua])^2,
      sd(posterior_draws_ses[[length(posterior_draws_ses)]][in.mua])^2
    )
        
    posterior_draws_var_explained_raw[[length(posterior_draws_var_explained_raw)+1]] <- sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])/(sum(posterior_draws_var_explained[[length(posterior_draws_var_explained)]])+sd(posterior_draws_field[[length(posterior_draws_field)]][in.mua])^2)

    posterior_draws_var_explained[[length(posterior_draws_var_explained)]] <-  posterior_draws_var_explained[[length(posterior_draws_var_explained)]] /sum( posterior_draws_var_explained[[length(posterior_draws_var_explained)]] )
  }
}
posterior_draws_sydney <- do.call(rbind,posterior_draws)
posterior_draws_pollution_sydney <- do.call(rbind,posterior_draws_pollution)
posterior_draws_climenv_sydney <- do.call(rbind,posterior_draws_climenv)
posterior_draws_ses_sydney <- do.call(rbind,posterior_draws_ses)
posterior_draws_field_sydney <- do.call(rbind,posterior_draws_field)
posterior_draws_var_explained_sydney <- do.call(rbind,posterior_draws_var_explained)
posterior_draws_var_explained_sydney_raw <- unlist(posterior_draws_var_explained_raw)

posterior_draws_pollution_sydney_median <- apply((posterior_draws_pollution_sydney),2,median)
posterior_draws_climenv_sydney_median <- apply((posterior_draws_climenv_sydney),2,median)
posterior_draws_ses_sydney_median <- apply((posterior_draws_ses_sydney),2,median)
posterior_draws_field_sydney_median <- apply((posterior_draws_field_sydney),2,median)
posterior_draws_pollution_sydney_median <- posterior_draws_pollution_sydney_median-mean(posterior_draws_pollution_sydney_median)
posterior_draws_climenv_sydney_median <- posterior_draws_climenv_sydney_median-mean(posterior_draws_climenv_sydney_median)
posterior_draws_ses_sydney_median <- posterior_draws_ses_sydney_median-mean(posterior_draws_ses_sydney_median)
posterior_draws_field_sydney_median <- posterior_draws_field_sydney_median-mean(posterior_draws_field_sydney_median)
posterior_draws_var_explained_sydney_median <- apply((posterior_draws_var_explained_sydney),2,median)
posterior_draws_var_explained_sydney_high <- apply((posterior_draws_var_explained_sydney),2,quantilehigh)
posterior_draws_var_explained_sydney_raw_median <- median(posterior_draws_var_explained_sydney_raw)
posterior_draws_var_explained_sydney_raw_high <- quantilehigh(posterior_draws_var_explained_sydney_raw)

all_pollution <- numeric(N_SA1)
all_climenv <- numeric(N_SA1)
all_ses <- numeric(N_SA1)
all_field <- numeric(N_SA1)

all_pollution[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_pollution_perth_median
all_pollution[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_pollution_brisbane_median
all_pollution[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_pollution_melbourne_median
all_pollution[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_pollution_sydney_median

all_climenv[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_climenv_perth_median
all_climenv[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_climenv_brisbane_median
all_climenv[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_climenv_melbourne_median
all_climenv[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_climenv_sydney_median

all_ses[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_ses_perth_median
all_ses[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_ses_brisbane_median
all_ses[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_ses_melbourne_median
all_ses[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_ses_sydney_median

all_field[sa1_2021$GCC_NAME21=="Greater Perth"] <- posterior_draws_field_perth_median
all_field[sa1_2021$GCC_NAME21=="Greater Brisbane"] <- posterior_draws_field_brisbane_median
all_field[sa1_2021$GCC_NAME21=="Greater Melbourne"] <- posterior_draws_field_melbourne_median
all_field[sa1_2021$GCC_NAME21=="Greater Sydney"] <- posterior_draws_field_sydney_median

tot_children <- SA1_num_dwellings_obs+SA1_num_dwellings_obsX

sa1_2021_outputs$POL <- all_pollution
sa1_2021_outputs$CLI <- all_climenv
sa1_2021_outputs$SES <- all_ses
sa1_2021_outputs$FIE <- all_field
sa1_2021_outputs$NCH <- tot_children
