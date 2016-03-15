#
# Integrative Biology 200
# Principles of Phylogenetics
# University of California, Berkeley
#
# Lab 08: Intro to R continued, ancestral state reconstruction, simulating under 
# Brownian motion and Ornstein-Uhlenbeck, and phylogenetically independent contrasts
# by Will Freyman
#



# In R you can define your own functions. Here is an example of a totally trivial 
# function that takes as input two numbers x and y and outputs their sum:

addition = function(x, y) {
    return(x + y)
}

# We can now use our function:

addition(4, 5)

# But let's use functions to do something more useful, like calculate
# the probability of a character evolving over a tree!

##############################################################################
#
# EXERCISE 1
#
# Page 2 of lecture 17 describes a continuous-time Markov model for a binary 
# character that has two rates of change: alpha is the rate of transitioning 
# from state 0 to 1, and beta is the rate of transitioning from state 1 to 0.
# See: http://ib.berkeley.edu/courses/ib200/lect/lect17.pdf
#
# Write two functions to calculate the probability of each possible transition
# over a branch of length t. Each function should take as input alpha, beta, and t.
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
#
##############################################################################

# Now that we know how to calculate probabilities of continuous-time Markov 
# chain models of discrete character evolution over a phylogeny, let's look at 
# an example of how such models can be used to estimate ancestral states.
# We'll use an example from Liam J. Revell's phytools package:
library(phytools)
data(anoletree)
x = getStates(anoletree, "tips")
tree = anoletree

# Take a look at the character data:
x

# We can plot the data onto the tree:
plotTree(tree, type = "fan", fsize = 0.9, ftype = "i")
cols = setNames(palette()[1:length(unique(x))], sort(unique(x)))
tiplabels(pie = to.matrix(x, sort(unique(x))), piecol = cols, cex = 0.2)
add.simmap.legend(colors = cols, prompt = FALSE, x = 0.9 * par()$usr[1], y = -max(nodeHeights(tree)))

# Instead of using a two-rate model like we did above, here we'll fit 
# a fit a single-rate continuous-time Markov chain model and 
# reconstruct the marginal ancestral states at internal nodes in the tree.
# We have 6 character states, so for a single-rate model, the transition rate 
# matrix looks like:
#       -   a   a   a   a   a 
#       a   -   a   a   a   a
#       a   a   -   a   a   a
#       a   a   a   -   a   a
#       a   a   a   a   -   a
#       a   a   a   a   a   -
# where the diagonal elements are -5a.
fitSR = rerootingMethod(tree, x, model = "ER")

# What is the log-likelihood of this model?
fitSR$loglik

# Let's plot the tree again, but this time adding the probabilities
# of each state at the internal nodes of the tree:
plotTree(tree, type = "fan", fsize = 0.9, ftype = "i")
nodelabels(node = as.numeric(rownames(fitSR$marginal.anc)), pie = fitSR$marginal.anc, piecol = cols, cex = 0.5)
tiplabels(pie = to.matrix(x, sort(unique(x))), piecol = cols, cex = 0.2)
add.simmap.legend(colors = cols, prompt = FALSE, x = 0.9 * par()$usr[1], y = -max(nodeHeights(tree)))

# Take a screen shot of the ancestral states under the single-rate model.
# Now let's fit a symmetrical-rates model, that allows different (but symmetrical)
# rates for each character state transition:
# For a symmetrical-rate 6 state model, the transition rate matrix looks like:
#       -   a   b   c   d   e
#       a   -   f   g   h   i
#       b   f   -   j   k   l
#       c   g   j   -   m   n
#       d   h   k   m   -   o
#       e   i   l   n   o   -
# where the diagonal elements are defined as -1 times the sum of the other row elements,
# for example, row 1's diagonal element is -(a + b + c + d + e).
fitSYM = rerootingMethod(tree, x, model = "SYM")

# What is the log-likelihood of this model?
fitSYM$loglik

##############################################################################
#
# EXERCISE 3
#
# Plot the tree again, but show the ancestral states inferred using the
# symmetrical-rates model. Send me screen shots of both reconstructed ancestral 
# states. Even though the single-rate model is simply a special case of the symmetrical-rates
# model, are the ancestral state reconstructions the same?
#
# Which model fits the data better? Calculate the likelihood ratio test:
# D = 2 * ( loglike of alternative model - loglike of null model )
# There are 15 parameters in the alternative (SYM) model and 1 parameter in the SR model,
# so we have 15 - 1 = 14 degrees of freedom. Look up D in a chi-squared distribution table
# and report the p-value. Is the SYM model supported over the SR model?
#
##############################################################################




# OK, now lets move on to continuous characters. We'll simulate a character 
# evolving under Brownian motion over a phylogeny.

# First, let's simulate a tree
tree = rcoal(n = 30)
plotTree(tree, ftype = "off")

# Now we'll simulate the character evolving with variance = 1.0
x = fastBM(tree, a=0, sig2=1.0, internal = TRUE)

# We can plot a 'traitgram' (Ackerly, 2009):
phenogram(tree, x, spread.labels = TRUE)

# Try simulating characters with a few other values for the variance.

# Let's now simulate a character evolving under the Ornstein-Uhlenbeck process.
# The OU model is Brownian motion but with two extra parameters: the optimum (theta)
# and the strength of selection (alpha).

# When alpha is close to 0, the OU model collapses down to Brownian motion
x = fastBM(tree, a=0, sig2=1.0, alpha=0.01, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)

# As alpha increase, we see more of a trend towards the optimum:
x = fastBM(tree, a=0, sig2=1.0, alpha=0.5, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)

# Alpha acts as a 'rubber band', pulling the trait to the optimum:
x = fastBM(tree, a=0, sig2=1.0, alpha=2.0, theta=4.0, internal = TRUE)
phenogram(tree, x, spread.labels = TRUE)

# The above examples of Brownian motion and Ornstein-Uhlenbeck assume that
# the same model applies to the entire tree. It is also possible to have
# time or branch heterogenous models, in which the parameter values or model
# changes over the tree. If you are interested in these models, check out 
# the R packages OUwie, ouch, and bayou.


# How do we test if two traits are evolving in a correlated manner? Let's 
# simulate two traits independently evolving on a tree:
tree = rcoal(n = 100)
x = fastBM(tree)
y = fastBM(tree)

# Are the traits correlated if we ignore the phylogeny?
plot(x, y)
abline(lm(y ~ x))
cor.test(x, y)

# I just got p-value = 3.153e-13, but we know these two traits
# evolved independently! You can see how easy it is to get a type I 
# error (false positive). As an alternative, let's use phylogenetically independent 
# contrasts (PIC; Felsenstein 1985) to estimate the evolutionary correlation 
# between characters:
x_c = pic(x, tree)
y_c = pic(y, tree)
cor.test(x_c, y_c)

# Now I got a p-value = 0.7092, so we correctly reject the hypothesis
# that the two characters were correlated.

# What happens when we simulate two correlated characters?
x = fastBM(tree)
y = 0.4 * x + fastBM(tree, sig2 = 0.2)

##############################################################################
#
# EXERCISE 4
#
# Without "correcting" for phylogeny, are the two characters correlated?
# What is the p-value and correlation coefficient?
# Using PIC, are x and y correlated? Now what is the p-value and correlation 
# coefficient?
#
##############################################################################

##############################################################################
#
# WHEN YOU ARE FINISHED:
#
# Send me your working R code to exercises 1 and 2, screenshots to exercise 3,
# and answers to exercise 4.
#
##############################################################################

