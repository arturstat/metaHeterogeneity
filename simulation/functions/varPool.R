# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# October 2020

## computes the pooled variance between two samples
varPool <- function(vecA, vecB) {
  ret <- (length(vecA)-1)*var(vecA);
  ret <- ret + (length(vecB)-1)*var(vecB);
  ret <- ret / (length(vecA)+length(vecB)-2);
  return(ret);
} # varPool
