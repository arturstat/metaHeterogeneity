# Written by Artur Araujo

# artur.stat@gmail.com

# October 2020

# define working directory
while ( !"meta_simulation_serial.R" %in% list.files() ) {
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

###########################
##### Data simulation #####
###########################

ncpus <- parallel::detectCores(); # get number of threads on local machine

iseed <- 3141593; # seed for RNG

set.seed(seed=iseed); # set seed for RNG

nsim <- 10000*ncpus; # number of simulations per parallel cluster
nsub <- 0.01*nsim; # number of subset elements for variance estimation
rm(ncpus);

n <- c(4, 6, 8, 10, 14, 18); # number of subjects per arm
nu <- 2*n-2; # degrees of freedom
k <- c(2, 4, 8, 16, 32, 64); # number of trials

s.res <- vector( mode="list", length=length(n) );
for ( i in 1:length(n) ) {
  s.res[[i]] <- vector( mode="list", length=length(k) );
}

serial.time <- system.time(
  expr={
    for ( i in 1:length(n) ) { # loop through the subject number vector
      for ( j in 1:length(k) ) { # loop through the trial number vector
        s.res[[i]][[j]] <- metaRatioSerial(
          argsA=list(n=n[i], mean=0, sd=1),
          argsB=list(n=n[i], mean=0, sd=1),
          k=k[j],
          nsim=nsim,
          nsub=nsub
        );
      }
    }
  }
);
rm(i, j);

print(serial.time);

s.ratio.data <- matrix(
  data=NA,
  nrow=length(k),
  ncol=length(n)+1
);

s.var.data <- matrix(
  data=NA,
  nrow=length(k),
  ncol=length(n)+1
);

for ( i in 1:length(n) ) {
  for ( j in 1:length(k) ) {
    s.ratio.data[j, i+1] <- s.res[[i]][[j]]$mean;
    s.var.data[j, i+1] <- s.res[[i]][[j]]$var;
  }
}
rm(i, j);

s.ratio.data <- as.data.frame(s.ratio.data);
rownames(s.ratio.data) <- k;
colnames(s.ratio.data) <- c("trials", nu);
s.ratio.data$trials <- k;

s.var.data <- as.data.frame(s.var.data);
rownames(s.var.data) <- k;
colnames(s.var.data) <- c("trials", nu);
s.var.data$trials <- k;

save(
  s.res,
  serial.time,
  s.ratio.data,
  s.var.data,
  file="./rdata/metaDataSerial.RData",
  envir=environment()
); # save data

rm(iseed);
rm(nsim, nsub);
rm(n, nu, k);
