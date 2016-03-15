
##############################################################################
#
# EXERCISE 1

p_01 =  function(alpha, beta, t) {
    term1 = alpha / (alpha + beta)
    term2 = 1 - exp(-1 * (alpha + beta) * t)
    return(term1 * term2)
}


p_10 =  function(alpha, beta, t) {
    term1 = beta / (alpha + beta)
    term2 = 1 - exp(-1 * (alpha + beta) * t)
    return(term1 * term2)
}

#
##############################################################################

# Here is a simple 3 taxa tree plotted with node labels and branch lengths:
library(ape)
t = read.tree(text="((A:0.39,B:0.39):0.93,C:1.32);")
plot(t, show.tip.label=FALSE)
nodelabels()
tiplabels()
edgelabels(t$edge.length)

##############################################################################
#
# EXERCISE 2
#
# Using the functions you wrote in exercise 1, and assuming alpha = 2 and beta = 3,
# calculate the probability that the tree has the following states:
#
# Node 1 = 0
# Node 2 = 0
# Node 3 = 1
# Node 4 = 1
# Node 5 = 0

alpha = 2
beta = 3
p_node5 = (1 - p_01(alpha, beta, 0.39))^2
p_tree = p_node5 * p_10(alpha, beta, 0.93) * (1 - p_10(alpha, beta, 1.32))
# p_tree = 0.1027863


##############################################################################
#
# EXERCISE 3
#
# 2 * (fitSYM$loglik - fitSR$loglik)
# 12.35872
# 0.5 < p < 0.75 so the ER model is better fitting

