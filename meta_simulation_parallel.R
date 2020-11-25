# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# October 2020

# define working directory
while ( !"meta_simulation_parallel.R" %in% list.files() ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

if ( !"varPool" %in% ls() ) { # if function does not exist
  source(file="./functions/varPool.R"); # source file containing function
}

if ( !"metaRatioParallel" %in% ls() ) { # if function does not exist
  source(file="./functions/metaRatioParallel.R"); # source file containing function
}

###########################
##### Data simulation #####
###########################

iseed <- 3141593; # seed for RNG

ncpus <- parallel::detectCores(); # get number of threads on local machine
cl <- parallel::makeCluster(spec=ncpus, type="PSOCK"); # create parallel cluster
parallel::clusterSetRNGStream(cl=cl, iseed=iseed); # set seed on parallel cluster
rm(ncpus);

nsim <- 10000; # number of simulations per parallel cluster
nsub <- 0.01*nsim*length(cl); # number of subset elements for variance estimation

n <- c(4, 6, 8, 10, 14, 18); # number of subjects per arm
nu <- 2*n-2; # degrees of freedom
k <- c(2, 4, 8, 16, 32, 64); # number of trials

p.res <- vector( mode="list", length=length(n) );
for ( i in 1:length(n) ) {
  p.res[[i]] <- vector( mode="list", length=length(k) );
}

parallel.time <- system.time(
  expr={
    for ( i in 1:length(n) ) { # loop through the subject number vector
      for ( j in 1:length(k) ) { # loop through the trial number vector
        p.res[[i]][[j]] <- metaRatioParallel(
          argsA=list(n=n[i], mean=0, sd=1),
          argsB=list(n=n[i], mean=0, sd=1),
          k=k[j],
          nsim=nsim,
          nsub=nsub,
          cl=cl
        );
      }
    }
  }
);
rm(i, j);

print(parallel.time);

## stop parallel cluster
parallel::stopCluster(cl=cl);

p.ratio.data <- matrix(
  data=NA,
  nrow=length(k),
  ncol=length(n)+1
);

p.var.data <- matrix(
  data=NA,
  nrow=length(k),
  ncol=length(n)+1
);

for ( i in 1:length(n) ) {
  for ( j in 1:length(k) ) {
    p.ratio.data[j, i+1] <- p.res[[i]][[j]]$mean;
    p.var.data[j, i+1] <- p.res[[i]][[j]]$var;
  }
}
rm(i, j);

p.ratio.data <- as.data.frame(p.ratio.data);
rownames(p.ratio.data) <- k;
colnames(p.ratio.data) <- c("trials", nu);
p.ratio.data$trials <- k;

p.var.data <- as.data.frame(p.var.data);
rownames(p.var.data) <- k;
colnames(p.var.data) <- c("trials", nu);
p.var.data$trials <- k;

save(
  p.res,
  parallel.time,
  p.ratio.data,
  p.var.data,
  file="./rdata/metaDataParallel.RData",
  envir=environment()
); # save data

rm(iseed, cl);
rm(nsim, nsub);
rm(n, nu, k);
