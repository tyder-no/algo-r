#
# source("prime_testing.R")
#
#
#

source("sieve.R")



#

timeSieve <- function(MAXN=10000) {

  t1 <- system.time(p1 <- sieveVector(MAXN))
  list(time=t1,primes=p1)
}





timeAndCountPrimes <- function(firstN=100,nSteps=10,stepSize=2){

    currSize <- firstN ;
    resM <- matrix(0,nrow=nSteps,ncol=3,byrow=3)
    for (i in 1:nSteps) {
        t1 <- system.time(p1 <- sieveVector(currSize)) ;
        resM[i,] <- c(currSize,t1[3],length(p1))    
        currSize <- currSize * stepSize ;
        if (currSize>500000) print(paste("CurrSize: ",currSize)) ;
    }

    resM
}

primeProperties <- function() {

   tc1 <- timeAndCountPrimes(nSteps=20)

    
# 1 Prime number theorem illustrated x/pi(x) = log(x)-1
   X11() 
   plot(log(tc1[,1]),tc1[,1]/tc1[,3],col=2,ylim=c(4,18))
   points(log(tc1[,1]),log(tc1[,1])-1,type="l")

# 2 Timing in R vector implementation
    lf <- lm(log(tc1[6:20,2]) ~ log(tc1[6:20,1]))
   # line y = -17.1 + 1.15x    
   # ie in this case approx time = 1/exp(17.1)*N^1.15
    
    X11()
    plot(log(tc1[,1]),log(tc1[,2]),col=2)  
    abline(coef(lf)[1],coef(lf)[2]) 
 
    
}


#> tc1
#          [,1]   [,2]    [,3]
# [1,]      100  0.000      26
# [2,]      200  0.000      47
# [3,]      400  0.001      79
# [4,]      800  0.000     140
# [5,]     1600  0.000     252
# [6,]     3200  0.001     453
# [7,]     6400  0.001     835
# [8,]    12800  0.002    1527
# [9,]    25600  0.004    2819
#[10,]    51200  0.007    5240
#[11,]   102400  0.017    9806
#[12,]   204800  0.034   18368
#[13,]   409600  0.080   34580
#[14,]   819200  0.231   65349
#[15,]  1638400  0.540  123842
#[16,]  3276800  1.109  235371
#[17,]  6553600  2.638  448183
#[18,] 13107200  6.628  855733
#[19,] 26214400 16.144 1637055
#[20,] 52428800 41.951 3137913
