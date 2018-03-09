# In this example we simulate a plain vanilla Blackjack game 
# and we calculate the probabilites of win and tie for both the player and the dealer

# A deck of 52 cards (Jokers are excluded), A is an ace.
# Please note: as in BlackJack figures worth 10 each, we will deal with each picture as if it were a ten
deck = c("A","A","A","A",10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,9,9,9,9,8,8,8,8,7,7,7,7,6,6,6,6,5,5,5,5,4,4,4,4,3,3,3,3,2,2,2,2)

# Usually, in casinos, Blackjack games are played 
# with 4 to 6 decks. The following function will take in 
# account this matter and return the appropriate deck 
# according to the number the casino uses (n)
deck_in_use <- function(n)
{
    deck_to_return = rep(deck,n)
    return(deck_to_return)
}


# The function hand takes a deck and returns a hand of Blackjack
hand <- function(deck_to_use, player_hand)
{
    # We fix player's hand
    p_hand = player_hand
    # We fix the deck
    deck = deck_to_use
    
    # Player's hand gets removed from the deck, 
    # since it is no longer available to be dealt
    for(i in p_hand)
    {
        if(is.element(i,deck))
        {            
            deck<-deck[-match(i,deck)]     
        }
    }
    
    # In case player's hand contains aces, the following 
    # code trasforms aces in 11 or 1 according to BJ rules
    if((p_hand[1]) == "A" &amp;&amp; (p_hand[2] == "A"))
    {
        p_hand[1] <- 1
        p_hand[2] <- 11
    }else if((p_hand[1] == "A") && (p_hand[2] != "A"))
    {
        p_hand[1] <- 11
    }else if((p_hand[2] == "A") &amp;&amp; (p_hand[1] != "A"))
    {
        p_hand[2] <- 11
    }
    p_hand <- as.numeric(p_hand)
    
    
    # Dealer's drawing function: if dealer's hand is 
    # lower than 16, then another card is dealt
    d_draw <- function(hand)
    {
        
        if(sum(hand) > 16)
        {
            return(hand)
        }else
        {
            new_card = sample(deck,1,F)
            hand = append(hand,new_card)
            
            if(is.element(new_card, deck))
            {         
                deck<-deck[-match(new_card, deck)]    
            }
            
            # In case the newly dealt card is an ace, 
            # the following code deals with it
            if(new_card == "A")
            {
                place_holder_ace_d2 <- match(new_card, hand)
                vect_partial_sum_hand = hand[-place_holder_ace_d2]
                vect_partial_sum_hand <- as.numeric(vect_partial_sum_hand)
                if(sum(vect_partial_sum_hand) <= 10)
                {
                    hand[place_holder_ace_d2] <- 11
                }else
                {
                    hand[place_holder_ace_d2] <- 1
                }
            }
            hand <- as.numeric(hand)
            
            # In case something went missing from before, 
            # the following code checks if every ace has
            # the appropriate value
            hand_minus_new_card &lt;- hand[-match(new_card, deck)]
            if((sum(hand) <= 21) && (is.element(11,hand_minus_new_card)))
            {
                hand[match(11, hand)] <- 1
            }
            hand <- as.numeric(hand)
            
            # Redraw is needed in case dealer's hand is lower than 16
            return(d_draw(hand))
        }
    }
    
    # Dealer's hand is eventually sampled and implemented with the 
    # d_draw function to simulate a real draw.
    dealer_hand <- function()
    {
        hand = sample(deck,2,F)
        
        for(i in hand)
        {
            if(is.element(i,deck))
            {         
                deck<-deck[-match(i,deck)]   
            }
        }
        
        # If the dealer has two Aces they are converted into a 11 and a 1 
        if((hand[1] == "A") && (hand[2] == "A"))
        {
            hand[1] <- 11
            hand[2] <- 1
            hand <- as.numeric(hand)
        }else
        {
            # If an A is in the dealer hand, it is converted into an 11
            for(i in hand)
            {
                place_holder_ace_d <- match(i, hand)
                if(i == "A")
                {
                    hand[place_holder_ace_d] <- 11
                }
            }
            hand <- as.numeric(hand)
        }
        
        hand <- d_draw(hand)
        
        return(hand)
    }
    
    # Results are eventually collected into a list and returned by the function
    results = list(p_hand, dealer_hand())
    
    return(results)
}


# The following function simulates n Blackjack hands and store the
# results in a matrix. It returns some probabilities (please check below)
# n is the number of hands to be played in the simulation
simulate_games<;- function(deck_to_use, player_hand, n)
{
    
    names_ <- c("Dealer","Player", "Test_win", "Test_draw")
    result_matrix <- matrix(ncol = 4)
    colnames(result_matrix) <- names_
    
    for(i in 1:n)
    {
        game <- hand(deck_to_use,player_hand)
        vec_result <- c(sum(game[[2]]), sum(game[[1]]))
        
        draw <- (vec_result[2] == vec_result[1])
        
        # Tie
        if(draw)
        {
            vec_result[3] <- 0
            vec_result[4] <- 1
            result_matrix <- rbind(result_matrix, vec_result)
            # Dealer hits Blackjack    
        }else if((vec_result[1] == 21) && (length(game[[1]]) == 2) &&; (vec_result[2] != 21) )
        {
            vec_result[3] <- 0
            vec_result[4] <- 0
            result_matrix <- rbind(result_matrix, vec_result)
            # Player wins on points
        }else if((vec_result[2] < vec_result[1]) | (vec_result[1] < 21))
        {
            vec_result[3] <- 1
            vec_result[4] <- 0
            result_matrix <- rbind(result_matrix, vec_result)
            # Dealer wins
        }else
        {
            vec_result[3] <- 0
            vec_result[4] <- 0
            result_matrix <- rbind(result_matrix, vec_result)
        }
    }
    
    # Delete first row of the results matrix
    result_matrix <- result_matrix[-1,]
    
    # Show results matrix
    #View(result_matrix)
    
    probab_win = sum(result_matrix[,3])/n
    probab_tie = sum(result_matrix[,4])/n
    probab_dealer_win = 1 - probab_win - probab_tie
    
    #name_data <- c("Prob of win", "Prob of a tie", "Prob of dealer win")
    
    vect_data <- c(probab_win, probab_tie, probab_dealer_win)
    
    #names(vect_data) <- name_data
    
    return(vect_data)
}



# Simulation with 3 decks a hand equal to 19 and 100 games
# Sidenote: this game is as if the dealer was shuffling the 
# cards at each game
simulate_games(deck_in_use(3), c(10,9), 100)


# Simulation of n_games*n_hand_game with n_deck decks and a hand equal to hand
simulate_n_games &lt;- function(n_deck, hand, n_hand_game, n_games)
{
    res_matrix <- matrix(ncol = 3)
    names = c("prob of win", "prob of tie", "prob of dealer win")
    colnames(res_matrix) &lt;- names
    
    for(i in 1:n_games)
    {
        data_vec <- simulate_games(deck_in_use(n_deck), hand, n_hand_game)
        res_matrix <- rbind(res_matrix, data_vec)
    }
    
    res_matrix <- res_matrix[-1,]
    
    View(res_matrix)
    
    probab_win <- sum(res_matrix[,1])/nrow(res_matrix)
    probab_tie <- sum(res_matrix[,2])/nrow(res_matrix)
    probab_dealer_win &lt;- 1 - probab_win - probab_tie
    
    probabilities <- c(probab_win, probab_tie, probab_dealer_win)
    name_data <- c("Prob of win", "Prob of a tie", "Prob of dealer win")
    names(probabilities) <- name_data
    
    return(probabilities)
}

# Simulate 2000*100 Blackjack games with 3 decks and a hand equal to 18
simulate_n_games(3,c(10,8),2000,100)

# You may want to run the following lines one a time since they 
#are a little "heavy" to compute.

# The simulation is run for hands from 16 to 21
first_row_16 <- simulate_n_games(3,c(10,6),2000,100)
second_row_17 <- simulate_n_games(3,c(10,7),2000,100)
third_row_18 <- simulate_n_games(3,c(10,8),2000,100)
fourth_row_19 <- simulate_n_games(3,c(10,9),2000,100)
fifth_row_20 <- simulate_n_games(3,c(10,10),2000,100)
sixth_row_21 <- simulate_n_games(3,c(10,11),2000,100)


BJ_probabilities <- matrix(ncol = 3)
BJ_probabilities <- rbind(BJ_probabilities,first_row_16)
BJ_probabilities <- rbind(BJ_probabilities,second_row_17)
BJ_probabilities <- rbind(BJ_probabilities,third_row_18)
BJ_probabilities <- rbind(BJ_probabilities,fourth_row_19)
BJ_probabilities <- rbind(BJ_probabilities,fifth_row_20)
BJ_probabilities <- rbind(BJ_probabilities,sixth_row_21)
BJ_probabilities <- BJ_probabilities[-1,]
View(BJ_probabilities)


hands_points <- c(16,17,18,19,20,21)
plot(hands_points, BJ_probabilities[,1], xlab = "Hands full points", main = "Probability of player win", type = "b", col="blue")
plot(hands_points, BJ_probabilities[,2], xlab = "Hands full points", main = "Probability of tie", type = "b", col="blue")
plot(hands_points, BJ_probabilities[,3], xlab = "Hands full points", main = "Probability of dealer win", type = "b", col="blue")
