#### Vectorization ####
function vectorized(x)
    sumsq = sum((x - mean(x)) .^ 2)

    return sumsq
end

function forloop(x)
    sumsq = 0.0

    N = length(x)
    m = mean(x)

    for i = 1:N
        sumsq += (x[i] - m)^2
    end

    return sumsq
end

x = rand(10000)

@time for i = 1:1000
    vectorized(x)
end

@time for i = 1:1000
    forloop(x)
end


#### Type Stability ####
function unstableSeries(n)
    s = 0
    for i = 1:n
        s += 1 / (i*i)
    end

    return s
end

function stableSeries(n)
    s = 0.0
    for i = 1:n
        s += 1 / (i*i)
    end

    return s
end


@time for i = 1:1000
    unstableSeries(10000)
end

@time for i = 1:1000
    stableSeries(10000)
end

@code_typed unstableSeries(10)
@code_typed stableSeries(10)


#### Profiling ####
using Distributions

function bt_sampler(D::Matrix{Int})
    const K = size(D,1)                 # number of teams
    const w = sum(D,2)                  # w[i] is the number of games
                                        # team i has won
    const n = D + D'                    # n[i,j] is the number of
                                        # times team i and j have
                                        # played (n[i,j] = n[j,i])

    ## Prior on λ 
    a = 1
    b = 1
    
    λ = ones(Float64, K)

    ## we actually only care about the sums of Z, so we're storing
    ## that in Zs here
    Zs = zeros(Float64, K)

    while true
        ## update Z
        for j = 2:K, i = 1:(j-1)
            if n[i,j] > 0
                val = rand(Gamma(n[i,j], 1.0/(λ[i] + λ[j])))
                Zs[i] += val
                Zs[j] += val
            end
        end
        
        ## update λ
        for i = 1:K
            λ[i] = rand(Gamma(a + w[i], 1.0/(b + Zs[i])))
            Zs[i] = 0.0 # clear our Zs while we're at it
        end
        produce(λ)
    end
end

N = 100
D = rand(0:100, N, N)
for i = 1:N
    D[i,i] = 0
end

sampler = @task bt_sampler(D)

using ProfileView

@profile for i = 1:10000
    consume(sampler)
end

ProfileView.view()
