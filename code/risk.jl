using Iterators

function order_counts(nTop::Int, nDice::Int, nFaces::Int)
    ## calculate the last nTop order statistics of the rolls of nDice
    ## each with nFaces

    ## ... is the splat operator f(1, [2,3]..., 4) = f(1,2,3,4).
    totals = zeros(Int, fill(nFaces, nTop)...)
    
    for roll in product(fill(1:nFaces, nDice)...)
        tmp = sort(collect(roll), rev = true)[1:nTop]
        ## In-place updating
        totals[tmp...] += 1
    end

    return totals
end

function loss_prob(attLoss::Int, attDice::Int, defDice::Int, nFaces::Int)
    ## calculate the probability of the attacker losing attLoss
    ## armies when attacking with attDice when the defender has
    ## defDice
    
    nTop = min(attDice, defDice)

    attTop = order_counts(nTop, attDice, nFaces)
    defTop = order_counts(nTop, defDice, nFaces)

    suc = 0


    ## Iterates through all possible rolls, even though we only need
    ## sorted rolls. The extra rolls don't get counted
    for aRoll in product(fill(1:nFaces, nTop)...), dRoll in product(fill(1:nFaces, nTop)...)
        
        loss = sum(collect(aRoll) .<= collect(dRoll))
        if loss == attLoss
            suc += attTop[aRoll...] * defTop[dRoll...]
        end
    end
    
    tot = nFaces ^ (attDice + defDice)
    
    prob = suc / tot
    
    return prob
end

function att_win_prob(attArmies::Int, defArmies::Int, prob_table::Array{Float64, 3})
    ## Calculate the recursion from the bottom up
    value_table = zeros(attArmies + 1, defArmies + 1)
    
    for a = 1:attArmies
        value_table[a + 1, 0 + 1] = 1.0
    end
    
    for d = 1:defArmies, a = 1:attArmies #cache-friendly
        attDice = min(a, 3)
        defDice = min(d, 2)
        
        atRisk = min(attDice, defDice)
        
        for attLoss = 0:atRisk
            defLoss = atRisk - attLoss
            
            prob = prob_table[attLoss + 1, attDice, defDice]
            value = value_table[a - attLoss + 1, d - defLoss + 1]
           
            value_table[a + 1, d + 1] += prob * value
        end
    end

    return value_table
end

## Precalculate the transition probabilitities
nFaces = 6

prob_table = zeros(4, 3, 2)

for a = 1:3, d = 1:2, l = 0:min(a, d)
    prob_table[l + 1, a, d] = loss_prob(l, a, d, nFaces)
end

## Examples
@time att_win_prob(1000, 1000, prob_table);
