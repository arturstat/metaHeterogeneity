# Written by Artur Araujo

# artur.stat@gmail.com

# October 2020

varRatio <- function(df, k, f1=FALSE) {
  ret <- if (f1) {df/(df-4*(k-1)/k);}
  else {df/( df-( 1+3*(k-1)/(k+1) ) );}
  return(ret);
} # varRatio
