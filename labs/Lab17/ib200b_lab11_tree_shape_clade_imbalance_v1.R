##############################################################
# =============================================
# Lab 11: Tree simulation and tree shape in R
# =============================================
# by Nick Matzke (and whoever else adds to this PhyloWiki page)
# Copyright 2011-infinity
# matzkeATberkeley.edu
# March 2011
#
# Please link/cite if you use this, email me if you have 
#   thoughts/improvements/corrections.
#
##############################################################
#
# Free to use/redistribute under:
# Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA 3.0) 
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the above license, linked here:
# 
# http://creativecommons.org/licenses/by-nc-sa/3.0/
# 
# Summary:
#
# You are free:
#
#   * to Share -- to copy, distribute and transmit the work
#   * to Remix -- to adapt the work
#
# Under the following conditions:
#
#   * Attribution -- You must attribute the work in the manner 
#     specified by the author or licensor (but not in any way that 
#     suggests that they endorse you or your use of the work).
#   * Noncommercial -- You may not use this work for commercial purposes. 
#
#   * Share Alike -- If you alter, transform, or build upon this work,
#     you may distribute the resulting work only under the same or
#     similar license to this one. 
#
# http://creativecommons.org/licenses/by-nc-sa/3.0/
# 
###################################################################
#
# Assignment for Thursday, March 10
#
# Run the code below, and answer the questions at the end in email.
#
# Please make the subject of your email:
# "IB200B lab11 by [your name]"
# Thanks!
# Nick
# 
###################################################################


# setup
library(ape)

# Rabosky's laser package
# does Yule trees, but in table form
#library(laser)

# Hmm, doesn't install correctly
# library(diversitree)

# for birthdeath.tree
library(geiger)



# Remote sourcing:
source("http://ib.berkeley.edu/courses/ib200b/scripts/_genericR_v1.R")
source("http://ib.berkeley.edu/courses/ib200b/scripts/_conus_analyses_v1.R")
source("http://ib.berkeley.edu/courses/ib200b/scripts/_R_tree_functions_v1.R")


# Let's generate some random trees
numtips = 20

# branching times determined by coalescent process (nodes pushed towards tips)
coalescent_tree = rcoal(numtips, br="coalescent")
plot(coalescent_tree)
title("Random coalescent tree")


# branching times under a uniform distribution (non-ultrametric)
runif_tree = rtree(numtips)
plot(runif_tree)
title("Random uniform tree")


# make branch lengths Grafen
rgraf_tree = compute.brlen(runif_tree, method="Grafen")
plot(rgraf_tree)
title("Random tree with Grafen branch lengths")


# Simulate trees under the Yule (pure-birth) process
birthrate = 1
deathrate = 0
yuletree = birthdeath.tree(b=birthrate, d=deathrate, taxa.stop=numtips)
plot(yuletree)
title("Tree simulated under the pure-birth process")


# Simulate trees under the birth-death process
# NOTE: Birth-death trees sometimes die out by accident.
# So, re-run the code several times until you get a tree
# that actually gets up to 20 taxa.
birthrate = 1
deathrate = 0.5
bdtree = birthdeath.tree(b=birthrate, d=deathrate, taxa.stop=numtips)
plot(bdtree)
title("Tree simulated under the birth-death process")

# the bdtree contains extinct ("fossil") lineages, which means it's not 
# ultrametric.  So we'll do Grafen branch lengths again.  NOTE: DON'T 
# DO THIS AT HOME, IT IS PRETTY MEANINGLESS EVEN AS A SIMULATION,
# THIS IS JUST A "FOR INSTANCE" TO SHOW HOW THE CODE WORKS
bdtree2 = compute.brlen(bdtree, method="Grafen")
plot(bdtree2)
title("Tree simulated under the birth-death process, and arbitrarily changing\nthe branch lengths using Grafen method to make ultrametric")




# Calculate tree balance for each tree;
# we are going to calculate the number of 
# branches on each side of the basal split
bal_ctree = balance(coalescent_tree)
(ctree_RminusL = bal_ctree[1,2] - bal_ctree[1,1])

bal_rtree = balance(runif_tree)
(rtree_RminusL = bal_rtree[1,2] - bal_rtree[1,1])

bal_rgtree = balance(rgraf_tree)
(rgtree_RminusL = bal_rgtree[1,2] - bal_rgtree[1,1])

bal_ytree = balance(yuletree)
(ytree_RminusL = bal_ytree[1,2] - bal_ytree[1,1])

bal_bdtree = balance(bdtree)
(bdtree_RminusL = bal_bdtree[1,2] - bal_bdtree[1,1])

bal_bdtree2 = balance(bdtree2)
(bdtree2_RminusL = as.numeric(bal_bdtree2[1,2] - bal_bdtree2[1,1]) )


# QUESTION: Do the branch lengths matter for this statistic?




# What's the distribution of:
#
# (number of tips on left side of the tree 
# ...minus...
# number of tips on the right side of tree)
#
# ...if you do a bunch of simulations?


# set number of simulations
numsims = 100
numtips = 20
# make an empty array
balance_matrix = matrix(NA, nrow=numsims, ncol=4)
for (i in 1:numsims)
	{
	cat("Simulation #", i, "\n", sep="")

	# simulate the trees:
	coalescent_tree = rcoal(numtips, br="coalescent")
	runif_tree = rtree(numtips)

	# Yule tree
	birthrate = 1
	deathrate = 0
	yuletree = birthdeath.tree(b=birthrate, d=deathrate, taxa.stop=numtips)

	# simulate a birth-death tree until you get 20 tips
	birthrate = 1
	deathrate = 0.5
	for (j in 1:100)
		{
		#cat("Trying bdtree #", j, "\n", sep="")
		bdtree = birthdeath.tree(b=birthrate, d=deathrate, taxa.stop=numtips)
		bdtree_table = prt(bdtree, printflag=FALSE)
		
		#print(sum(round(bdtree_table$time_bp, digits=5) == 0))
		numtips_observed = sum(round(bdtree_table$time_bp, digits=5) == 0)
		if (numtips_observed >= numtips)
			{
			rows_to_drop = bdtree_table[bdtree_table$node.type=="tip", ]
			labels_to_drop = rows_to_drop[(round(rows_to_drop$time_bp, digits=5) > 0), ]$label
			bdtree = drop.tip(bdtree, labels_to_drop)
			#bdtree = read.tree(text=fixtree(bdtree))
			
			# Sometime drop.tip messes up the tree, control for this
			if (length(bdtree$tip.label) < numtips)
				{
				foo = 2
				} else {
				# yay, you have a good tree, break out and continue
				break
				}
			}
		}


	# Calculate tree balance for each tree;
	# we are going to calculate the number of 
	# branches on each side of the basal split
	bal_ctree = balance(coalescent_tree)
	(ctree_RminusL = bal_ctree[1,2] - bal_ctree[1,1])
	
	bal_rtree = balance(runif_tree)
	(rtree_RminusL = bal_rtree[1,2] - bal_rtree[1,1])
		
	bal_ytree = balance(yuletree)
	(ytree_RminusL = bal_ytree[1,2] - bal_ytree[1,1])
	
	bal_bdtree = balance(bdtree)
	(bdtree_RminusL = bal_bdtree[1,2] - bal_bdtree[1,1])
	
	
	# fill in a temporary row
	tmprow = c(ctree_RminusL, rtree_RminusL, ytree_RminusL, bdtree_RminusL)
	
	balance_matrix[i, ] = tmprow
	}

# convert to a data frame (adf)
balance_matrix = adf(balance_matrix)

# add column names
names(balance_matrix) = c("ctree", "rtree", "ytree", "bdtree")

# plot histograms

# 4 subplots
par(mfrow=c(2,2))
for (i in 1:ncol(balance_matrix))
	{
	hist(balance_matrix[,i], breaks=numsims/10, main=names(balance_matrix)[i], xlab="#right tips - # left tips", ylab="count")
	}

# It will take a few seconds to run 100 simulations for each of the 4 tree processes.
# QUESTION: Do you see any differences in the distributions?
#
# Now, change numsims to 1000 and re-run (this will take a few minutes, smoke 'em 
# if you got 'em).  
#
# QUESTION: What do the distributions look like now?  If your observed data 
# gave you a tree with 1 tip on the left, and 19 on the right, would you say
# that this observation has a low p-value and thus was unlikely to be produced
# by your null model? 


