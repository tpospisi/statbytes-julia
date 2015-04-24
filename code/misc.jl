## User-defined types
immutable IRM_Data
    V::Array{Int, 2}
    T::Array{Int, 2}
end

type IRM_Model
    Zp::Array{Int, 1} ## Partition of pols
    Zb::Array{Int, 1} ## Partition of bills
    βr::Float64 ## Beta prior on votes
    βt::Float64 ## Beta prior on tags
    γp::Float64 ## Politician CRP
    γb::Float64 ## Bill CRP
end

type IRM_Suff
    yeas::Array{Int, 2} ## Suff. stats for yea votes
    nays::Array{Int, 2} ## Suff. stats for nay votes
    ins::Array{Int, 2} ## Suff. stats for included tags
    outs::Array{Int, 2} ## Suff. stats for excluded tags
    Np::Array{Int, 1}  ## Np[i] = number of pols in cluster i
    Nb::Array{Int, 1} ## Nb[i] = number of bills in cluster i
end

## Multiple Dispatch
function lPosterior(M::IRM_Model, D::IRM_Data)
    ## Code here
end

function lPosterior(M::IRM_Model, S::IRM_Suff)
    ## Code here
end


## Help system
help(push!)
methods(push!)

## Package management
Pkg.add("Distributions")
Pkg.update()

## Distributions
using Distributions

α = 3, β = 4 #Unicode variable names

dist = Beta(α, β)
draws = rand(dist, 100)

rand(Gamma(3, 5), 1000) #rgamma
quantile(Gamma(3,5), 0.1) #qgamma
pdf(Gamma(3, 5), 2) #dgamma
cdf(Gamma(3, 5), 2) #pgamma


## Comprehensions
N = 10
add = [i + j for i = 1:N, j = 1:N]
mult = [i * j for i = 1:N, j = 1:N]

## Big Int Support

# Project Euler #20
## Find the sum of the digits in 100!
sum(digits(factorial(BigInt(100))))
