order_counts <- function(nTop, nDice, nFaces) {
    ## calculate the last nTop order statistics of the rolls of nDice
    ## each with nFaces
    
    totals <- array(0, rep(nFaces, nTop))

    rolls <- matrix(NA, nFaces ^ nDice, nDice)

    for(i in 1:nDice){
        rolls[,i] <- rep(1:nFaces, each = nFaces ^ (i - 1))
    }
    
    for(i in 1:(nFaces ^ nDice)){
        roll <- rolls[i,]
        tmp <- sort(roll, decreasing = TRUE)[1:nTop]
        totals[matrix(tmp, 1)] = totals[matrix(tmp, 1)] + 1
    }

    return(totals)
}

loss_prob <- function(attLoss, attDice, defDice) {
    ## calculate the probability of the attacker losing attLoss
    ## armies when attacking with attDice when the defender has
    ## defDice
    
    nFaces <- 6
    nTop <- min(attDice, defDice)

    attTop <- order_counts(nTop, attDice, nFaces)
    defTop <- order_counts(nTop, defDice, nFaces)

    suc <- 0

    attRolls <- matrix(NA, nFaces ^ nTop, nTop)

    for(i in 1:nTop){
        attRolls[,i] <- rep(1:nFaces, each = nFaces ^ (i - 1))
    }

    defRolls <- matrix(NA, nFaces ^ nTop, nTop)

    for(i in 1:nTop){
        defRolls[,i] <- rep(1:nFaces, each = nFaces ^ (i - 1))
    }
    
    for(i in 1:nrow(attRolls)) {
        for(j in 1:nrow(defRolls)) {

            aRoll <- attRolls[i,]
            dRoll <- defRolls[j,]

            loss <- sum(aRoll <= dRoll)

            if(loss == attLoss){
                suc <- suc + attTop[matrix(aRoll, 1)] * defTop[matrix(dRoll, 1)]
            }
        }
    }
    
    tot <- nFaces ^ (attDice + defDice)
    
    prob <- suc / tot
    
    return(prob)
}

att_win_prob <- function(attArmies, defArmies, prob_table) {
    ## Calculate the recursion from the bottom up
    value_table = array(0.0, c(attArmies + 1, defArmies + 1))
    
    value_table[, 0 + 1] <- 1.0
    
    for(a in 1:attArmies) {
        for(d in 1:defArmies) {
            attDice <- min(a, 3)
            defDice <- min(d, 2)
            
            atRisk <- min(attDice, defDice)
            
            for(attLoss in 0:atRisk) {
                defLoss <- atRisk - attLoss
                
                prob <- prob_table[attLoss + 1, attDice, defDice]
                value <- value_table[a - attLoss + 1, d - defLoss + 1]
                
                value_table[a + 1, d + 1] <- value_table[a + 1, d + 1] + prob * value
            }
        }
    }
    
    return(value_table)
}

## Precalculate the transition probabilitities
prob_table = array(0, c(4, 3, 2))

for(a in 1:3) {
    for(d in 1:2) {
        for(l in 0:min(a, d)) {
            prob_table[l + 1, a, d] <- loss_prob(l, a, d)
        }
    }
}

## Examples
require(microbenchmark)
microbenchmark(att_win_prob(1000, 1000, prob_table), times = 1)
