# tay 20180307
# sieve.R - Eratosthenes again
#
# source("sieve.R")
#


sieve <- function(MAXN=10000){

  numbers <- rep(0,MAXN) ; nseq <- seq(1:MAXN) ;
  maxSmallFactor <- round(sqrt(MAXN)) ;

  currPrime <- 2 ; nPrimes <- 0 ; numbers[1] <- 1 ;
  while(currPrime <= maxSmallFactor) {
      nPrimes <- nPrimes + 1 ;
      checkNum <- 2*currPrime ;
      while(checkNum<=MAXN){
          numbers[checkNum] <- 1 ;
          checkNum <- checkNum + currPrime ;
      }
      currPrime <- currPrime + 1 ;
      while (numbers[currPrime]==1)  currPrime <- currPrime + 1 ;
      
  }

  nseq[numbers==0]
  

}



















