##############################################################
# =======================================================
# Lab 18: Diversification and Lineage Through Time Plots
# ======================================================
# You'll need to load the following libraries

library(geiger)
library(ape)
library(TreeSim)
library(laser)
library(diversitree)

#############################################################################
#Part I: Simulate phylogenies under birth-death process & plot those trees
#############################################################################
#Expectations under stochastic branching process model (SBP) models are diffuse to a degree that defies intuition, which makes it extremely difficult to detect departures from expectations.

# It is instructive to simulate data under these models to get a sense of the variance that occurs stochastically.

# We will simulate some trees under a constant birth-death SBP model using the TreeSim R package.  For the first run, we will generate 100 trees under a speciation rate of 0.9 and an extinction rate of 0.5, conditioning on the time of the process, 10 units. We'e going to generate sample of simulated trees with a relatively high extinction rate
# condition on T = 10
# replicates = 100
# b = .9
# d = .6


#Simulate your trees
simTrees <- sim.bd.age(10, 100, 0.9, 0.5)


# Now we'd like to determine what fraction of our trees went extinct.  To do this, we're going to use the sapply function.  The way we're going to do this is by asking if the simulation ended up producing a tree (when the entire clade goes extinct, the output of the simulation is the number 0 rather than a tree).  To ask if a simulation produced a tree, we're going to use the "phylo" function.

survivors <- sapply(1:length(simTrees), function(i) is(simTrees[[i]], "phylo"))

# We're now going to estimate what fraction of our trees survived to the present day

sum(survivors)/length(simTrees)

# Now let's restrict our attention to those trees that survived to the present day. To do this, we're going to make a new list that consists only of those trees that survived:

simTrees_survivors <- simTrees[survivors]

# To confirm that this worked, we can plot a sample of the surviving trees.  Let's just plot the first 10 trees. If you're using RStudio, you may want to Zoom in on the window to actually see what these trees look like.

par(mfcol=c(2,5)) #generate 2-row, 5-column plotting window
sapply(1:10, function(i) plot(simTrees_survivors[[i]], show.tip.label=FALSE))

# Now we want to eliminate extinct taxa from the trees we just generated.  To do this, we're going to loop through all of our trees and trim the extinct taxa using the "prune.extinct.taxa" command.  We'll save the resulting trees to a new list called "simTrees_extantOnly." This will take a few seconds.

simTrees_extantOnly <- list()
for(i in 1:length(simTrees_survivors))
{
  prune.extinct.taxa(simTrees_survivors[[i]]) -> simTrees_extantOnly[[i]]
}

# Let's take a look at the subsample of 10 trees after deleting these extinct species. Again, you may want to zoom in on your plots to actually see your trees

sapply(1:10, function(i) plot(simTrees_extantOnly[[i]], show.tip.label=FALSE))

# To get a sense of the inherent variance of the constant-rate birth-death SBP, we can plot the species diversity of the realized trees. To do this, we’ll generate a histogram of the number of species in each of the simulated trees.

tips <- vector()
for (i in 1:length(simTrees_extantOnly))
{
  Ntip(simTrees_extantOnly[[i]]) -> tips[[i]]
}
par(mfcol=c(1,1))
hist(tips)

# Now we're ready to start investigating patterns of diversification from a sample of extant trees.  Let's first take a look at some lineage-through-time plots. Note that we're going to use the "log="y"" command to plot lineage accumulation on a log axis (recall that we expect lineages to accumulate exponentially)

par(mfcol=c(2,5)) #generate 2-row, 5-column plotting window
for(i in 1:5)
{
  plot(simTrees_extantOnly[[i]], show.tip.label=FALSE)
  ltt.plot(simTrees_extantOnly[[i]], log="y")
}

# Now let's estimate birth-death parameters for our trees.  We're only going to try to do this for trees that have at least 10 taxa. This cut-off is somewhat arbitrary, but we’d like to avoid doing something ridiculous like trying to estimate rate parameters from trees with only two or three tips. Thus, the next thing we need to do is to get the set of simulated trees that have more than 10 extant taxa.

count <- 1
list() -> simTrees_extantOnly10plus
for(i in 1:length(simTrees_extantOnly))
{
  if(length(simTrees_extantOnly[[i]]$tip.label)>= 10)
  {
    simTrees_extantOnly[[i]] -> simTrees_extantOnly10plus[[count]]
    count <- count + 1
  }
}

# Now we’re ready to estimate diversification rate parameters from our simulated data. The point of doing this is to ask whether we can obtain the diversification parameters under the best case scenario (i.e., when we know the simple processes that were used to generate the trees we’re investigating). To do this, we’re going to use the bd function in the R package laser. This function will estimate two parameters from our trees using maximum likelihood: (1) the net diversification rate (b-d) and (2) the relative extinction rate (b/d). We’re going to use a loop to obtain parameter estimates for each of our simulated datasets.

BDresults <- list() #Generates empty list to store our results
BDparameters <- matrix(NA, length(simTrees_extantOnly10plus), 5) #Generates empty matrix to store parameter values
for(i in 1:length(simTrees_extantOnly10plus))
{
  branching.times(simTrees_extantOnly10plus[[i]]) -> btimes
  bd(btimes, ai=seq(0.05, 0.99, length.out=20)) -> BDresults[[i]]
  #birthdeath(simTrees_extantOnly[[i]]) -> results[[i]]
  BDresults[[i]]$r1 -> BDparameters[i,1]
  BDresults[[i]]$a -> BDparameters[i,2]
}

#Now, compare these estimates to the true values of the parameters
# Parameter 1 (b-d) = 0.9-0.5 = 0.4
# Parameter 2 (b/d) = 0.9/0.5 = 1.8

BDparameters

# We can also generate a box plot from these parameter estimates to get some idea of the uncertainty expected when estimating speciation and extinction parameters from simulated datasets.

par(mfcol=c(1,1))
boxplot(BDparameters[,1], BDparameters[,2])

# Let’s just do this one more time to make sure that our ML estimates didn’t settle on the wrong area of the likelihood surface.

list() -> BDresults #Generates empty list to store our results
for(i in 1:length(simTrees_extantOnly10plus))
{
  branching.times(simTrees_extantOnly10plus[[i]]) -> btimes
  bd(btimes, ai=seq(0.05, 0.99, length.out=20)) -> BDresults[[i]]
  BDresults[[i]]$r1 -> BDparameters[i,3]
  BDresults[[i]]$a -> BDparameters[i,4]
}

# Take a look at your table (BDparameters) and create a boxplot of all estimates to see if you got similar estimates from each run of this analysis.

par(mfcol=c(1,1))
boxplot(BDparameters[,1], BDparameters[,2], BDparameters[,3], BDparameters[,4])

#############################################################################
#Part II: Is Diversification Constant Over Time?
#############################################################################

# All of the simulations we've done have assumed diversification rate constancy. That is, rates of both speciation and extinction have been constant over time. This is obviously not a a very good assumption.  It has been postulated that species accumulation tends to plateau over time as niches are filled, either due to reduced in speciation rates, increased extinction rates, or some combination of the two.  Pybus and Harvey introduced the gamma statistic to address this question.  We're going to start by estimating the gamma parameter from our simulated datasets. Our expectation is that we will recover the positive gamma values typical of clades that did not experience a temporal shift in diversification rate over time.

#Fill in the table with estimates of the gamma parameter
for(i in 1:length(simTrees_extantOnly10plus))
{
  gammaStat(simTrees_extantOnly10plus[[i]]) -> BDparameters[i,5]
}

hist(BDparameters[,5])

BDparameters


#Question 1:  Do any of your simulated datasets support the hypothesis of a decline in diversification rate over time?  If so, which ones?  Take a screenshot or send me a jpg of your gamma statistic histogram.


#############################################################################
#Part III: Empirical Examples
#############################################################################

# Now let's try running the gamma parameter on some real trees.  Let's start with the example of Anolis lizards, a clade that we suspect has slowed over time as island niches have been filled.

# You will have to first download the "anolis_GA_BEAST.nex" file from the website and set your working directory so that R can find this file.

anoleTrees <- read.nexus(file="anolis_GA_BEAST.nex")

# Let's just take a look at one tree and its associated LTT plot. NOTE: When I ran this, I had some problems displaying the phylogeny along with the LTT plot

par(mfcol=c(2,1))
plot(anoleTrees[[1]])
ltt.plot(anoleTrees[[1]], log="y")

# Now let's estimate the gamma parameter for each of the trees in our set of trees from the posterior distribution of our BEAST analysis

anoleGamma <- matrix(NA, 1, length(anoleTrees))

for(i in 1:length(anoleTrees))
{
  gammaStat(anoleTrees[[i]]) -> anoleGamma[1,i]
}

# Becuase we expect incomplete sampling will simulate a slow-down, we want to use a MCCR technique to assess whether the slow-down indicated by the gamma parameter is significant given the amount of missing taxa in our dataset.  To run this test, we're going to use the following lines:

# Let's first get the maximum gamma value from the trees in our posterior distribution.

max(anoleGamma[1,])
mccrTest(91, 85, 100, max(anoleGamma[1,]))

# Next we'll run some analyses on a phylogeny for the plant group, Valerianaceae. This phylogeny includes 103 of ~300 species, was estimated from ~2800 bp from 4 gene regions, and was dated using the UCL relaxed clock model implemented in BEAST.

# First let's load the tree into R and generate a LTT plot for the consensus tree. NOTE: This time I had no problems with these plots.

ValTree <- read.tree(file="Val_traits.phy")
par(mfcol=c(2,1))
plot(ValTree, show.tip.label=FALSE)
ltt.plot(ValTree, log="y")

# From the LTT plot, it appears that there was a non-zero extinction rate. Let's calculate gamma using the following command:

ValGamma <- gammaStat(ValTree)

# Then we can estimate the critical value for gamma given that we've sampled 103 of the 300 species in the Valerianaceae. 

mccrTest(300, 197, 100, ValGamma)

