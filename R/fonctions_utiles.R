######################################
# générateur de données suivant
# une loi de distribution empirique
# et avec copule
######################################


## Function: fpx
## Description : Obtain the ranks of a vector (A factor of
##               0.5 is used to avoid extremes values of ranks).
fpx <- function(x) (rank(x, ties.method = "max") - 0.5) / length(x)

## Function: icdf
## Description : Generalized inverse of the empirical cumulative
##               function.
icdf <- function(u, x, n) {
  freq <- fpx(x)
  Fn   <- splinefun(x, freq, method = "monoH.FC")
  xstar <- numeric(n)
  for(i in seq_along(xstar)){
    xstar[i] <- uniroot(function(x) Fn(x) - u[i],
                        range(x), extendInt = "upX",
                        f.lower = - u[i], f.upper = 1 - u[i])$root
  }
  return(xstar)
}

######################################
#formule distance spatiale
######################################

distance<-function(votant, candidats){apply(candidats, 1, function(x) sqrt(sum((votant-x)^2)))}

######################################
#transformation des scores en distance
######################################

ScoresToDist<-function(x, dim=2, method="linear")
{
  if (method=="linear")
  {T<-dim*(1-x)
  }else{#transformation sigmoïde
    lambda<-5
    x_min<-1/(1+exp(lambda*(2*sqrt(dim)-1)))
    x_max<-1/(1+exp(-lambda))
    x[x<x_min]<-x_min
    x[x>x_max]<-x_max
    T<-((log(1/x-1))/lambda+1)/2
    T[T<0]<-0
  }

  return(as.data.frame(T))
}
######################################
#transformation des distances en scores
######################################

DistToScores <- function(dist, dim=2, method="linear", lambda=5)
{
  if (method=="linear"){
    x <- 1-2*dist
    x[x<0]<-0
  }else{#transformation sigmoide
    x <- 1/(1+exp(lambda*(4*dist-1)))
  }

  return(as.data.frame(x))
}


######################################
#Metric Unfolding Using the MLSMU6 Procedure
######################################
doubleCenterRect <- function(T){ # version score d'appétence
  n <- nrow(T)
  q <- ncol(T)
  (T-matrix(apply(T,1,mean), nrow=n, ncol=q) - t(matrix(apply(T,2,mean), nrow=q, ncol=n)) + mean(T))/2
}


MLSMU6 <- function(df, ndim=2){

  T <- as.matrix(df)
  n <- nrow(T)
  T <- (1-T)*2
  TTSQ <- T*T
  TTSQ[is.na(T)] <- (mean(T,na.rm=TRUE))^2
  TTSQDC <- doubleCenterRect(TTSQ)
  xsvd <- svd(TTSQDC)
  zz <- xsvd$v[,1:ndim]
  xx <- matrix(0, nrow=n, ncol=ndim)
  for (i in 1:ndim){
    zz[,i] <- zz[,i] * sqrt(xsvd$d[i])
    xx[,i] <- xsvd$u[,i] * sqrt(xsvd$d[i])
  }
  MLSMU6<-list(X=xx, Z=zz)
}