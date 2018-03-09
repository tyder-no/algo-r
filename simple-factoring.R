#
# tay 20180307
#  simple-factoring
#
# source("simple-factoring.R")
                                        #
# Providing arrays of primes
source("sieve.R")







simpleFactoring <- function(nToFactor,primes){


    restToFactor <- nToFactor ;
    primeFactors <- rep(0,length(primes)) ;
    
    i <- 1 ; primesI <- primes[i] ;
    while (primesI<=round(sqrt(restToFactor))) {
        while (restToFactor %% primesI == 0) {
            primeFactors[i] <- primeFactors[i] + 1;
            restToFactor <- restToFactor %/% primesI
        }
        i <- i + 1;
        primesI <- primes[i] ;
    }

    primeFactors[primes==restToFactor] <- 1
    
    rbind(primes,primeFactors)

}




digits10 <- function(nToDigitize) {

    ifelse (nToDigitize<10, nToDigitize,c(digits10(nToDigitize %/%10),nToDigitize %%10))
    

}
