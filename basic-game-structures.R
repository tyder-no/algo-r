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

cardValue <- function(x) { ifelse(x%%13>0,x%%13+1,14) }

#
# valueAndSuit - from card to list with (value,suit) pair
#

valueAndSuit <- function(card) { list(value=cardValue(deckOrder(card)),suit=deckOrder(card)%/%13) }


#
#  valuesAndSuits - from hand to values and suits lists
#

valuesAndSuits <- function(hand) { list(values=cardValue(deckOrderHand(hand)),suits=(deckOrderHand(hand)-1)%/%13) }

#
#  valuesAndSuitsSorted - from hand to values and suits lists, sorted
#

valuesAndSuitsSorted <- function(hand) {
    valuesUS <-cardValue(deckOrderHand(hand)) ; values <- sort(valuesUS,decreasing=TRUE) ;
  #  list(values=sort(cardValue(deckOrder(hand)),decreasing=TRUE),suits=sort(deckOrderHand(hand)%/%13,decreasing=TRUE))
     list(values=values,suits=sort((deckOrderHand(hand)-1)%/%13,decreasing=TRUE))

}


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

