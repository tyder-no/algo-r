####################
#
# POKER
#
# source("poker_play.R")
#
#

source("basic-game-structures.R")


pokerPoints <- function(handResult) {
    pP <- handResult[1]
    for (i in 2:5) pP <- 13*pP + handResult[i] 
    pP
}


                                        # Hands functions

                                        # 1 Royal Flush

isRoyalFlush <- function(hand,suits,values) {
    if ((suits[1] != suits[5]) || (values[5] != 10))  retVal <- 0 
    else retVal <- 10 ; # Must be royal flush...
    list(eval=c(retVal,values),type="Royal flush")
  
}


                                        # 2 Straight Flush

isStraightFlush <- function(hand,suits,values) {
    if ((suits[1] != suits[5]) || (values[1] != (values[5] + 4)))  retVal <- 0 
    else retVal <- 9 ; # Must be straight flush...
    list(eval=c(retVal,values),type="Straight flush")
}

                                        # 3 Four of a kind

isFourOfAKind <- function(hand,suits,values) {
    if ((suits[5] != suits[1]-3) || (values[2] != values[3]) || (values[4] != values[3]))  retVal <- 0 
    else { # At least 3 
        if  ((values[2] == values[1])|| (values[4] == values[5]))   retVal <- 8
        else retVal <- 0 ;                                  
    }
    if (values[2] == values[1]) ev <- c(retVal,values)
    else ev <- c(retVal,values[2:4],values[1])
    list(eval=ev,type="Four of a kind")
}

                                        # 4 Full house

isFullHouse <- function(hand,suits,values) {
    if ((suits[5] > suits[1]-2) || (values[2] != values[1]) || (values[4] != values[5]))  retVal <- 0 
    else { # At least two pairs
        if  ((values[2] == values[3])|| (values[4] == values[3]))   retVal <- 7
        else retVal <- 0 ;                                  
    }
    if (values[2] == values[3]) ev <- c(retVal,values)
    else ev <- c(retVal,values[3:5],values[1:2])
    list(eval=ev,type="Full house")
}


                                        # 5 Flush

isFlush <- function(hand,suits,values) {
    if (suits[1] != suits[5])  retVal <- 0 
    else  retVal <- 6 ;                                  
    list(eval=c(retVal,values),type="Flush")
}


                                        # 6 Straight

isStraight <- function(hand,suits,values) {
    # print(diff(values))
    dv <- diff(values) 
    if ((dv[1]!=-1)|| (dv[2]!=-1)||(dv[3]!=-1)||(dv[4]!=-1))  retVal <- 0 
    else retVal <- 5;
    list(eval=c(retVal,values),type="Straight")
}

                                        # 7 Three of a kind

isThreeOfAKind <- function(hand,suits,values) {
    if  ((values[3] == values[1]) || (values[4] == values[2]) ||(values[3] == values[5]) )   retVal <- 4
    else retVal <- 0 ;                                  
    if  (values[3] == values[1]) ev <- c(retVal,values)
    else if (values[4] == values[2]) ev <- c(retVal,values[2:4],values[1],values[5])
    else ev <- c(retVal,values[3:5],values[1:2])
    list(eval=ev,type="Three of a kind")
}



                                        # 8 Two pairs

isTwoPairs <- function(hand,suits,values) {
    if  ((values[2] == values[1] && values[3] == values[4]) || (values[2] == values[1] && values[4] == values[5]) ||(values[4] == values[5] && values[2] == values[3]) )   retVal <- 3
    else retVal <- 0 ;                                  
    if (values[2] == values[1] && values[3] == values[4]) ev <- c(retVal,values)
    else if (values[2] == values[1] && values[5] == values[4]) ev <- c(retVal,values[1:2],values[4:5],values[3])
    else ev <- c(retVal,values[2:5],values[1])  
    list(eval=ev,type="Two pairs")
}


                                        # 9 One pair

isOnePair <- function(hand,suits,values) {
    if  ((values[2] == values[1]) ||  (values[3] == values[2]) || (values[3] == values[4]) || (values[4] == values[5])) retVal <- 2  
    else retVal <- 0 ;                                  
    if  (values[2] == values[1]) ev <- c(retVal,values)
    else  if  (values[2] == values[3]) ev <- c(retVal,values[2:3],values[1],values[4:5])
    else  if  (values[4] == values[3]) ev <- c(retVal,values[3:4],values[1:2],values[5])
    else  ev <- c(retVal,values[4:5],values[1:3])
    list(eval=ev,type="One pair")
    
}

                                        # 10 High card

isHighCard <- function(hand,suits,values) {
    retVal <- 1
    list(eval=c(retVal,values),type="High card")
}



#
#  Main evaluation
#

evaluateHand <- function(hand) {

    vASS <- valuesAndSuitsSorted(hand) ; values <-  vASS$values ; suits <- vASS$suits ;

                                        # Eventually, handResult[1]!=0
    handResult <- isRoyalFlush(hand,suits,values)    
    if (handResult$eval[1]==0)  handResult <- isStraightFlush(hand,suits,values)    
    if (handResult$eval[1]==0)  handResult <-isFourOfAKind(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isFullHouse(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isFlush(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isStraight(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isThreeOfAKind(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isTwoPairs(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isOnePair(hand,suits,values)   
    if (handResult$eval[1]==0)  handResult <-isHighCard(hand,suits,values)  # Will always match 

    list(eval=c(handResult$eval,pokerPoints(handResult$eval)),type=handResult$type)

    
}

reportEval <- function(player,hand) {

    evRes <- evaluateHand(hand) ;
    print(paste("Player: ",player," type: ",evRes$type," points: ",evRes$eval[7]))

    evRes$eval[7]
}

getType <- function(hand) {

    evRes <- evaluateHand(hand) ;
    evRes$eval[1]
}



runAndRank <- function(nPlayers) {

   if(nPlayers<2) nPlayers <- 2
   if(nPlayers>6) nPlayers <- 6

   game <- shuffleAndDeal(5,nPlayers)

}


#######

#
# Testing code
#

assertEquals <- function(a,b) {

    if (a != b) {
        print(paste("a:",a," and b:",b," differ"))
        res <- 0
    }
    else res <- 1
    res
}

testEvaluation <- function() {

    sumOK <- 0 ;
    ev10 <- evaluateHand(c("Q_C","10_C","J_C","A_C","K_C"))
    sumOK <- sumOK + assertEquals(ev10$eval[1],10)

    ev9 <- evaluateHand(c("Q_C","10_C","J_C","9_C","K_C"))
    sumOK <- sumOK + assertEquals(ev9$eval[1],9)

    ev8 <- evaluateHand(c("Q_C","Q_D","J_C","Q_H","Q_S"))
    sumOK <- sumOK + assertEquals(ev8$eval[1],8)

    ev7 <- evaluateHand(c("Q_C","Q_D","J_C","J_H","Q_S"))
    sumOK <- sumOK + assertEquals(ev7$eval[1],7)
   
    ev6 <- evaluateHand(c("Q_C","A_C","J_C","2_C","3_C"))
    sumOK <- sumOK + assertEquals(ev6$eval[1],6)
   
    ev5 <- evaluateHand(c("Q_C","10_D","J_S","9_H","K_C"))
    sumOK <- sumOK + assertEquals(ev5$eval[1],5)

    ev4 <- evaluateHand(c("Q_C","Q_D","J_C","8_H","Q_S"))
    sumOK <- sumOK + assertEquals(ev4$eval[1],4)

    ev3 <- evaluateHand(c("Q_C","Q_D","8_C","8_H","2_S"))
    sumOK <- sumOK + assertEquals(ev3$eval[1],3)
 
    ev2 <- evaluateHand(c("K_C","Q_D","8_C","2_H","2_S"))
    sumOK <- sumOK + assertEquals(ev2$eval[1],2)

    ev1 <- evaluateHand(c("K_C","Q_D","8_C","2_H","9_S"))
    sumOK <- sumOK + assertEquals(ev1$eval[1],1)

    sumOK

}


testRunAndRank <- function(nPlayers) {
    printRes <- 0 ;
    
   if(nPlayers<2) nPlayers <- 2
   if(nPlayers>6) nPlayers <- 6

    game <- shuffleAndDeal(5,nPlayers)
    gPoints <- numeric(nPlayers)
    for (i in 1:nPlayers) {
        gPoints[i] <- reportEval(i,game$deal[i,]) ;
        
        }
    winner <- which.max(gPoints)
    print(" ") ;
    print(paste("Player ",winner, "wins")) ;
    print(" ") ;
    
}

getWinnerType <- function(nPlayers) {

    if(nPlayers<2) nPlayers <- 2
    if(nPlayers>6) nPlayers <- 6
    
    game <- shuffleAndDeal(5,nPlayers)
    gTypes <- numeric(nPlayers)
    for (i in 1:nPlayers) gTypes[i] <- reportEval(i,game$deal[i,]) ;
}


testSeriesRunAndRank <- function(nrep=10) {

   for (i in 1:nrep) testRunAndRank(6) ;


}


repeatUntilType <- function(resType) {
    
    if (resType<1 || resType>10) resType <- 8 ;
    nrep <- 1 ; lastType <- 0 ; nPlayers <- 6 ;
    game <- shuffleAndDeal(5,nPlayers)
    gTypes <- numeric(nPlayers)
    
    while (lastType != resType) {
        for (i in 1:nPlayers) gTypes[i] <- getType(game$deal[i,])
        if (length(gTypes[gTypes==resType])>0 ) lastType <- resType
        else   game <- shuffleAndDeal(5,nPlayers)
        nrep <- nrep + 1 ;
        if (nrep %% 5000 == 0) print(paste("....",nrep)) ;
    }

    list(nrep=nrep,game=game)

}


countTypes <- function(totReps=10000) {
    nPlayers <- 6 ;     nReps <- 1 ;
    winTypes <- numeric(10)  ;
    gTypes <- numeric(nPlayers) ;
    
    while (nReps < totReps) {
        game <- shuffleAndDeal(5,nPlayers)
        for (i in 1:nPlayers) gTypes[i] <- getType(game$deal[i,])
        winnerType <- max(gTypes)
        nReps <- nReps + 1 ;
        if (nReps %% 5000 == 0) print(paste("....",nReps)) ;
        winTypes[winnerType] <-   winTypes[winnerType] + 1 ;       
    }
    
    winTypes

}

