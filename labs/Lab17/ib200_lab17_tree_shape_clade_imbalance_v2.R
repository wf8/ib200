##############################################################
# =============================================
# Lab 17 (continued): Tree simulation in R
# =============================================
# by Nick Matzke, updated by Traci Grzymala
# Copyright 2011-infinity
# matzkeATberkeley.edu, malaATberkeley.edu
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
# Initial set-up.  Load the ape phylogenetics package.
library(ape)

# Install and load Dan Rabosky's laser package, which does Yule trees, but in table form.  'laser' requires 'geiger', so this will also automatically load
install.packages ("laser")
library(laser)

# Hmm, doesn't install correctly
library(diversitree)


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


# make branch lengths Grafen (NOTE: Grafen's (1989) computation of branch lengths: each node is given a `height', namely the number of leaves of the subtree minus one, 0 for leaves. Each height is scaled so that root height is 1, and then raised at power 'rho' (> 0). Branch lengths are then computed as the difference between height of lower node and height of upper node.)
rgraf_tree = compute.brlen(runif_tree, method="Grafen")
plot(rgraf_tree)
title("Random tree with Grafen branch lengths")


# Simulate trees under the Yule (pure-birth) process
birthrate = 1
deathrate = 0
yuletree = birthdeath.tree(b=birthrate, d=deathrate, taxa.stop=numtips)
plot(yuletree)
title("Tree simulated under the pure-birth process")


# Calculate tree balance for each tree; we are going to calculate the number of branches on each side of the basal split

bal_ctree = balance(coalescent_tree)
(ctree_RminusL = bal_ctree[1,2] - bal_ctree[1,1])

bal_rtree = balance(runif_tree)
(rtree_RminusL = bal_rtree[1,2] - bal_rtree[1,1])

bal_rgtree = balance(rgraf_tree)
(rgtree_RminusL = bal_rgtree[1,2] - bal_rgtree[1,1])

bal_ytree = balance(yuletree)
(ytree_RminusL = bal_ytree[1,2] - bal_ytree[1,1])


# QUESTION 2: Do the branch lengths matter for this statistic?

