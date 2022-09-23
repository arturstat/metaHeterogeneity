# Written by Artur Araujo

# artur.stat@gmail.com

# October 2020

## computes empirical ratio
## using parallel computation
metaRatioParallel <- function(
  argsA=list(n=32, mean=0, sd=1), # list of parameters for arm A
  argsB=argsA, # list of parameters for arm B
  k=2, # number of trials
  nsim=10000, # number of simulations per parallel cluster
  nsub=100, # number of subset elements for variance estimation
  cl # parallel cluster
) {
  ## export objects to parallel cluster
  parallel::clusterExport(
    cl=cl,
    varlist=c(
      "argsA",
      "argsB",
      "k",
      "nsim",
      "varPool"
    ),
    envir=environment()
  );
  
  ## evaluate expression on parallel cluster
  clret <- parallel::clusterEvalQ(
    cl=cl,
    expr={ # begin expression
      ret <- data.frame(
        "theta"=vector(mode="numeric", length=nsim), # pooled mean
        "gamma"=vector(mode="numeric", length=nsim) # variance of pooled mean
      );
      
      trial.data <- data.frame(
        "meanD"=vector(mode="numeric", length=k), # difference between means
        "wi"=vector(mode="numeric", length=k) # weight
      );
      
      for (i in 1:nsim) { # loop through the simulations
        for (j in 1:k) { # loop through the trials
          armA <- rnorm(n=argsA$n, mean=argsA$mean, sd=argsA$sd); # simulate data for arm A
          armB <- rnorm(n=argsB$n, mean=argsB$mean, sd=argsB$sd); # simulate data for arm B
          trial.data$meanD[j] <- mean(armB)-mean(armA); # difference between means
          trial.data$wi[j] <- varPool(armA, armB)*(1/argsA$n + 1/argsB$n); # variance of the above
        } # end loop
        
        # compute variance of pooled mean
        ret$gamma[i] <- 1/sum(1/trial.data$wi);
        
        # compute weights
        trial.data$wi <- ret$gamma[i]/trial.data$wi;
        
        # compute pooled mean
        ret$theta[i] <- with(
          data=trial.data,
          expr={sum(wi*meanD);}
        );
      } # end loop
      
      rm(i, j);
      rm(trial.data);
      rm(armA, armB);
      
      ret; # return object
    } # end expression
  );
  
  ## data.frame to merge data
  sim.data <- data.frame(
    "theta"=vector(
      mode="numeric",
      length=length(clret)*nsim
    ),
    "gamma"=vector(
      mode="numeric",
      length=length(clret)*nsim
    )
  );
  
  ## merge data from parallel cluster
  ## avoiding the use of 'cbind' or 'rbind'
  for ( i in 1:length(clret) ) {
    s <- nsim*(i-1)+1; # start index
    e <- nsim*i; # end index
    sim.data[s:e,] <- clret[[i]];
  }
  
  rm(i, s, e);
  rm(clret);
  
  # the number of sets equals the integer part of the quotient
  nset <- nrow(sim.data)%/%nsub; # number of sets
  
  # vector of empirical ratios
  vec <- vector(mode="numeric", length=nset);
  
  # compute empirical ratio for all sets except last one
  for ( i in 1:(nset-1) ) { # loop through the sets
    s <- nsub*(i-1)+1; # start index
    e <- nsub*i; # end index
    vec[i] <- var(sim.data$theta[s:e])/mean(sim.data$gamma[s:e]);
  } # end loop
  
  # compute empirical ratio for last set
  s <- nsub*(nset-1)+1; # start index
  # the remainder of the quotient is added to the end index
  e <- nsub*nset+nrow(sim.data)%%nsub; # end index
  vec[nset] <- var(sim.data$theta[s:e])/mean(sim.data$gamma[s:e]);
  
  rm(i, s, e);
  
  ret <- list(
    argsA=argsA, # list of parameters for arm A
    argsB=argsB, # list of parameters for arm B
    k=k, # number of trials
    nsim=nrow(sim.data), # total number of simulations
    nsub=nsub, # number of subset elements for variance estimation
    df=argsA$n+argsB$n-2, # degrees of freedom
    ratio=var(sim.data$theta)/mean(sim.data$gamma), # empirical ratio
    mean=mean(vec), # mean of empirical ratio
    var=var(vec)/nset # variance of the above
  );
  
  rm(nset, vec);
  rm(sim.data);
  
  return(ret);
} # metaRatioParallel
