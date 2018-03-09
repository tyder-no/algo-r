#  tay 20180308
#
# source("basic-game-structures.R")
#
#
#


cards <-
  as.character(outer(
    c(2:10, "J", "Q", "K", "A")
    , c("S", "H", "D", "C")
    , paste
    , sep = "_"
  ))

nums <- seq(1:52)


#
# deckOrder - from card 'value' to order in unshuffled deck
#

deckOrder <- function(card) { nums[cards==card] }

#
# deckOrderHand - from hand 'values' to orders in unshuffled deck
#

deckOrderHand <- function(hand) { sapply(hand,deckOrder) }


#
# cardValue - from card to list with (value,suit) pair
#

cardValue <- function(x) { ifelse(x%%13>0,x%%13,13) }

#
# valueAndSuit - from card to list with (value,suit) pair
#

valueAndSuit <- function(card) { list(value=cardValue(deckOrder(card)),suit=deckOrder(card)%/%13) }


#
#  valuesAndSuits - from hand to values and suits lists
#

valuesAndSuits <- function(hand) { list(values=cardValue(deckOrderHand(hand)),suits=deckOrderHand(hand)%/%13) }

#
#  valuesAndSuitsSorted - from hand to values and suits lists, sorted
#

valuesAndSuitsSorted <- function(hand) { list(values=sort(cardValue(hand),decreasing=TRUE),suits=sort(deckOrderHand(hand)%/%13,decreasing=TRUE)) }


#
# shuffle - returns a shuffled card deck
#

shuffle <- function(){ sample(cards) }


#
# shuffleAndDeal - given #cards and #players, returns a deal,deck and pos of next card
#

shuffleAndDeal <- function(nCards,nPlayers){

 # Adjust input if necessary, simplest case 1 player, 52 cards
    if (nCards>52) nCards <- 52 ;
    if ((nPlayers*nCards) > 52) nPlayers <- 52 %/% nCards ;
    
    deck <- sample(cards) ;

 # Do standard round dealing (players by row, dealing by column)
    deal <-  matrix(deck[1:(nCards*nPlayers)],nrow=nPlayers,ncol=nCards)

 # Return list with deal, deck and position of next card to be drawn   
    list(deal=deal,deck=deck,nextCard=nCards*nPlayers+1)

}

####################
#
# POKER
#
#

pokerPoints <- function(handResult) {
    pP <- handResult[1]
    for (i in 2:5] pP <- 13*pP + handResult[i] 
    pP
}


                                        # Hands functions

                                        # 1 Royal Flush

isRoyalFlush <- function(hand,suits,values) {
    if ((suits[1] != suits[5]) || (values[5] != 10))  retVal <- 0 
    else retVal <- 10 ; # Must be royal flush...
    c(retVal,values)
}


                                        # 2 Straight Flush

isStraightFlush <- function(hand,suits,values) {
    if ((suits[1] != suits[5]) || (values[1] != (values[5] + 4)))  retVal <- 0 
    else retVal <- 9 ; # Must be straight flush...
    c(retVal,values)
}

                                        # 3 Four of a kind

isFourOfAKind <- function(hand,suits,values) {
    if ((suits[5] != suits[1]-3) || (values[2] != values[3]) || (values[4] != values[3]))  retVal <- 0 
    else { # At least 3 
        if  ((values[2] == values[1])|| (values[4] == values[5]))   retVal <- 8
        else retVal <- 0 ;                                  
    }
    if (values[2] == values[1]) c(retVal,values)
    else c(retVal,values[2:4],values[1])   
}

                                        # 4 Full house

isFullHouse <- function(hand,suits,values) {
    if ((suits[5] > suits[1]-2) || (values[2] != values[1]) || (values[4] != values[5]))  retVal <- 0 
    else { # At least two pairs
        if  ((values[2] == values[3])|| (values[4] == values[3]))   retVal <- 7
        else retVal <- 0 ;                                  
    }
    if (values[2] == values[3]) c(retVal,values)
    else c(retVal,values[3:5],values[1:2])
}


                                        # 5 Flush

isFlush <- function(hand,suits,values) {
    if (suits[1] != suits[5])  retVal <- 0 
    else  retVal <- 6 ;                                  
    c(retVal,values)
}


                                        # 6 Straight

isStraight <- function(hand,suits,values) {
    if (values[5] != (values[1] + 4))  retVal <- 0 
    else retVal <- 5;
    c(retVal,values)
}

                                        # 7 Three of a kind

isThreeOfAKind <- function(hand,suits,values) {
    if  ((values[3] == values[1]) || (values[4] == values[2]) ||(values[3] == values[5]) )   retVal <- 4
    else retVal <- 0 ;                                  
    if  (values[3] == values[1]) c(retVal,values)
    else if (values[4] == values[2]) c(retVal,values[2:5],values[1],values[5])
    else c(retVal,values[3:5],values[1:2])
}



                                        # 8 Two pairs

isTwoPairs <- function(hand,suits,values) {
    if  ((values[2] == values[1] && values[3] == values[4]) || (values[2] == values[1] && values[4] == values[5]) ||(values[4] == values[5] && values[2] == values[3]) )   retVal <- 3
    else retVal <- 0 ;                                  
    if (values[2] == values[1] && values[3] == values[4]) c(retVal,values)
    else if (values[2] == values[1] && values[5] == values[4]) c(retVal,values[1:2],values[4:5],values[3])
    else c(retVal,values[2:5],values[1])  
  
}


                                        # 9 One pair

isOnePair <- function(hand,suits,values) {
    if  ((values[2] == values[1]) ||  (values[3] == values[2]) || (values[3] == values[4]) || (values[4] == values[5])) retVal <- 2  
    else retVal <- 0 ;                                  
    if  (values[2] == values[1]) c(retVal,values)
    else  if  (values[2] == values[3]) c(retVal,values[2:3],values[1],values[4:5])
    else  if  (values[4] == values[3]) c(retVal,values[3:4],values[1:2],values[5])
    else  c(retVal,values[4:5],values[1:3]) 
    
}

                                        # 10 High card

isHighCard <- function(hand,suits,values) {
    retVal <- 1
    c(retVal,values)
}



#
#  Main evaluation
#

evaluateHand <- function(hand) {

    vASS <- valuesAndSuitsSorted(hand) ; values <-  vASS$values ; suits <- vASS$suits ;

                                        # Eventually, handResult[1]!=0
    handResult <- isRoyalFlush(hand,values,suits)    
    if (handResult[1]==0)  handResult <- isStraightFlush(hand,values,suits)    
    if (handResult[1]==0)  handResult <-isFourOfAKind(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isFullHouse(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isFlush(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isStraight(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isThreeOfAKind(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isTwoPairs(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isOnePair(hand,values,suits)   
    if (handResult[1]==0)  handResult <-isHighCard(hand,values,suits)  # Will always match 

    pokerPoints(handResult)

    
}


runAndRank <- function(nPlayers) {

   if(nPlayers<2) nPlayers <- 2
   if(nPlayers>6) nPlayers <- 6

   game <- shuffleAndDeal(5,nPlayers)


}


#######





