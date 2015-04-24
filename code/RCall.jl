using RCall

y = rand(100)

rcopy("x = seq(1:100)")
rcopy("y = x + rnorm(100)")

## If you don't want to use GLM.jl
rcopy("z = lm(y ~ x)") ## doesn't work
rprint("z = lm(y ~ x)")

rcopy("z\$coef") #must escape $
