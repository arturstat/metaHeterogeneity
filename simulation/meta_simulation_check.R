# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# October 2020

# define working directory
while ( !"meta_simulation_check.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"varPool" %in% ls() ) { # if function does not exist
  source(file="./functions/varPool.R"); # source file containing function
}

if ( !"metaRatioSerial" %in% ls() ) { # if function does not exist
  source(file="./functions/metaRatioSerial.R"); # source file containing function
}

if ( !"metaRatioParallel" %in% ls() ) { # if function does not exist
  source(file="./functions/metaRatioParallel.R"); # source file containing function
}

iseed <- 3141593; # seed for RNG

argsA <- list(n=32, mean=0, sd=1); # list of parameters for arm A
argsB <- argsA; # list of parameters for arm B
k <- 10; # number of trials
nsim <- 10000; # number of total simulations
nsub <- 100; # number of subset elements for variance estimation

# the RNG used by parallel package
RNGkind(kind="L'Ecuyer-CMRG"); # define RNG kind

# seed for RNG running in serial
set.seed(seed=iseed); # set seed for RNG

## use serial computation
serial.data <- metaRatioSerial(
  argsA=argsA,
  argsB=argsB,
  k=k,
  nsim=nsim,
  nsub=nsub
);

## use parallel computation
cl <- parallel::makeCluster(spec=1, type="PSOCK"); # create parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed); # set seed on parallel cluster

parallel.data <- metaRatioParallel(
  argsA=argsA,
  argsB=argsB,
  k=k,
  nsim=nsim,
  nsub=nsub,
  cl=cl
);

# check equality between
# serial and parallel objects
all.equal(target=parallel.data, current=serial.data);

## stop parallel cluster
parallel::stopCluster(cl=cl);

RNGkind(kind="default"); # set default RNG kind
