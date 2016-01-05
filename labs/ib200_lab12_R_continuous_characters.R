##############################################################
# =============================================
# Lab 12: Continuous characters using APE in R
# =============================================
# by Nick Matzke and Nat Hallinan, updated by Traci Grzymala
# Copyright 2011-infinity
# matzkeATberkeley.edu
# March 2014
#
# Please link/cite if you use this, email me if you have 
#   thoughts/improvements/corrections.
#
# Much of the text of this lab was derived from Nat
# Hallinan's 2009 lab for IB200b on continuous characters
# in R.
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
# Assignment for Wednesday, March 5.
#
# Do the three questions and PRINT OUT the answers.
#
# Purpose: Learn more about tree-plotting and ancestral character
#   reconstruction
# 
###################################################################




# Stuff that is handy at the beginning of a script
######################################################
# R, close all devices/windows/ quartz windows
graphics.off()

# Turn off all BS stringsAsFactors silliness
# R tends to read 
options(stringsAsFactors = FALSE)
######################################################


# Working directories:
# One of the first things you want to do, usually, is 
# decide on your working directory

# On my Mac, this is the directory
setwd ("/Users/mala/Desktop/Lab12")

# On a PC, you might have to specify paths like this:
#setwd ("c:\\Users\\nick\\Desktop\\_ib200a\\ib200b_sp2011\\lab03")

#desc: get working directory
getwd()

#desc: list the files in the directory
list.files()


# Sourcing scripts
# Local sourcing:
# source a script (windows machines require \\ instead of /)
# sourcedir = '/_njm/'
#source3 = '_genericR_v1.R'
#source(paste(sourcedir, source3, sep=""))

# You need the APE package to source genericR_v1.R scripts
# install.packages("ape")
library(ape)

# Remote sourcing:
source("http://ib.berkeley.edu/courses/ib200b/scripts/_genericR_v1.R")
source("http://ib.berkeley.edu/courses/ib200b/scripts/_R_tree_functions_v1.R")



############################################
# Introduction
############################################
#
# Today we're just going to do some basic analysis of 
# continuous characters using R.  Most of it should be based on 
# things that we learned today in class.  We'll start with reading
# Nexus files and manipulating trees in R and then move 
# on to character reconstruction.
#
############################################
# Class phylo
############################################
#
# The first thing that we're going to do today  is learn about
# the class phylo.  This is the object type used by APE to
# store and manipulate trees.  First let's simulate a small
# tree of 4 taxa:

# QUESTION 1: Look up the function rcoal.  What does it do?  Explain
# in your own words how this kind of simulator works. (You may have
# to google some key words from the help page.)
simulated_tree = rcoal(4)

# To see what class the object stored in "simulated_tree" is:
class(simulated_tree)

# To see some details of the tree type:
summary(simulated_tree)

# To see more details, use some of Nick Matzke's functions:
summ(simulated_tree)
prt(simulated_tree)


# "summary" functions like the "plot" function in that
# it produces a different output depending on the 
# information that is passed to it.  In this case it
# provides you with some basic info about the tree.  
# As you can see this is a tree with n=4 tips, and since 
# it is fully bifurcating it has 
# 
# (n-1 = 3) internal nodes 
# 
# and 
# (2n-2 = 6) branches.
# 
# To see a slightly different summary, just type "simulated_tree"
# and hit Enter.  To see what the tree actually looks like type:

tree = simulated_tree
plot(tree)

# R actually codes for phylogenies as a list with several
# elements.  The elements can be referenced using the "$".
# Type:

tree$tip.label


# This will return a character vector of 4 elements showing the
# names of the taxa in the tree.  The order of the taxa is important,
# as the order of a vector of character states will correspond to
# the order of taxa.  To see the structure of the tree itself type:

tree$edge


# This is a matrix with 2 columns and 6 rows.  Each number in
# the matrix represents a node.  Numbers 1 to n are the tips
# with each number referring to the equivalent element of the
# $tip.label vector; the number n+1 (in this case 5) refers 
# to the root node; and  the numbers above that are the other 
# internal nodes.   Each row represents a branch of the tree 
# stretching from the node in column 1 to the node in column 
# 2.  This matrix is set up such that the branches that make 
# up any clade are all grouped next to each other.  Take a 
# minute to compare the $edge matrix to your plotted tree.  
# Make sure that you can see how this matrix codes for this 
# tree.
# 
# Finally, the branch lengths are stored in $edge.length:

tree$edge.length

# As you can see this is a numerical vector with six elements 
# each corresponding to the branch lengths of one of the 
# branches.  The branches are in the same order as the 
# branches in the $edge matrix.  Does that make sense for 
# the branch lengths you see in your plot?
# 
# Other elements of the class phylo inclue $root.edge which 
# gives the branch length of the root, if there is one, and 
# node.label, a vector of the node names.



############################################ 
# Uploading Trees into R
############################################
#
# Continuous Character Format
# 
# Open the Anoles Continuous file in a text editor.  I stole 
# this file from the Mesquite examples and according to them 
# it comes from "the work of Jonathan Losos and colleagues 
# (Losos et al., Science, 1998 and Losos & de Queiroz, Biol. 
# J. Linn. Soc., 1997) on Anolis lizards of the caribbean 
# that looks at convergence in ecomorphs." This file will be 
# pretty much the same as the previous Nexus files that you 
# looked at with the exception of the first character block.  
# (There are two other character blocks in this file.)  First 
# thing to note is that the Datatype is set to "Continuous".  
# Also, when you look at the actual matrix, you will see that 
# it is filled with numeric values separated by spaces rather 
# than the normal format.
# 
# 
# Reading Trees into R
# Open R and load the ape package.
library(ape)

# Read in the Nexus file. 

Anole.tree = read.nexus("anolis_continuous.nex")

###########################################
# Plotting Trees in R
###########################################
# 
# Basic Tree Plotting
# 
# OK, can you visualize the tree by reading the 
# tip.label matrix? Don't worry; you don't have to.
# Type:
plot(Anole.tree) 

# Isn't the plot function cool?  You can make different shaped
# trees too. Try:
plot(Anole.tree,type="f")

# and
plot(Anole.tree,type="r")

# You can also make the tree face in different directions:
plot(Anole.tree,direction="d")

# and
plot(Anole.tree,direction="u")

# You can manipulate many other visual factors of the tree,
# including the position, size, and font of the text.  Type
# help("plot.phylo") for more info.  In the next two sections
# we will explore how to modify specific branches of the tree.

# 
# Identifying Branches
# You can now use this plotted tree to identify the number
# of a given node.  To do this we will use the "identify" function.
# identify is another function that gives a different result
# for different arguments.  It is an interactive function,
# which means that its output depends on where you click on a 
# plot.  When fed an object of class phylo, identify will return 
# the number corresponding to a node that you click on.  Type:

identify(Anole.tree)

# ...and click on a node in the tree.

# The number it returns is the number of that node.  You can 
# find which branch ends with this node by using the which 
# function, which returns the index of a vector for which a 
# given statement is true.  Type:
which(Anole.tree$edge[,2]==node number)

# or just:
which(Anole.tree$edge[,2]==identify(Anole.tree))

# ...and then click a node


# 
# Modifying Branch Color
# 
# You can use that information to change the color of the
# branch you've identified.  To do this we have to create a
# vector of numbers that correspond to a color for each branch
# of the tree.  Since we want most of the tree to stay black, we will
# first create a vector of length 58 full of 1s:

br.col = rep(1,58)

# Then we will change the number corresponding to the branch we
# want to a 2 for red (or maybe give another number a try):

# picking an arbitrary branch number to color
branch_number_to_color = 31

# 4 for blue
br.col[branch_number_to_color] = 4

# Now we'll draw the tree again but add information about the
# colors of the branches.  Type:

plot(Anole.tree,edge.color=br.col) 


# Identifying Nodes
# 
# You may also want to identify an entire clade.  This is a
# little trickier; it relies on the assumption that the branches
# that make up a clade are all next to each other in our $edge 
# matrix.  The first thing that we will do is identify a bunch 
# of info about a node:

clade.info = identify(Anole.tree,tips=TRUE)

# and click on the root node of a clade
# 
# clade.info is now a list with two elements: node which is the
# node for the root of the tree, and tips which is a vector of
# the tip numbers.  

# Print "clade.info" to screen:
(clade.info)


# So the first branch of our clade will be 
# the one that ends at the node we picked:

start = which(Anole.tree$edge[,2] == clade.info$node)

# We will find the last listed branch in our clade by taking
# advantage of the fact that it has to end in one of the taxa 
# from our clade; so we need to find out which branches end 
# in each of the tips that are in this clade:

tip.branches = match(clade.info$tips,Anole.tree$edge[,2])


# "match" returns a vector of the order that the elements of 
# its first argument are found in its second argument.  So 
# that data.order is a vector ordered in the same way as 
# the taxa in the $tips vector, but with the numbers 
# representing the index of the branches that those 
# taxa appear in the $edge matrix.  Therefore the 
# last branch in our clade is:

end = max(tip.branches)


# Now start is the first branch or our node and end is the last.
# 
# 
# 
# Modifying Branch Widths for a Node
# 
# We can now use this info to make those branches four 
# times as wide as the others.  Once again we need to 
# create a vector of numbers corresponding to the branches.

br.wid = rep(1,58)
br.wid[start:end] = 4
plot(Anole.tree, edge.color=br.col, edge.width=br.wid)

# Let's make those branches blue, instead of 
# thick (4=blue).

plot(Anole.tree, edge.color=br.wid, edge.width=4)

# That worked.  Maybe if you want to light up a whole clade, 
# you should stick with colors.  Thickness can be saved for 
# the whole tree or individual branches.




#######################################################
# Likelihood Calculations, and Branch Lengths
#######################################################
#
# Graffen Branch Lengths
#
# To make any type of likelihood calculation we need branch lengths, but this
# tree does not have any.  We've talked some in class, and we will talk more
# later about where your branch lengths should come from.  However, sometimes
# you can not find branch lengths.  In that case there are several ways of 
# generating branch lengths.  In practice, if you have to use branch lengths 
# that are not justified, you should generate many different sets of such 
# branch lengths in order to make sure that your conclusions are robust to 
# your assumptions.  However, in this case it doesn't really matter, so 
# we'll just generate some and use them.
# 
# The ape function compute.brlen has one internal method for assigning 
# branch lengths; that method is based on 
# Graffen, Phil. Trans. R. Soc. B (1989) 326, 119-157.  
# I will not discuss the details because I do not understand them.  
# 
# To add Graffen branch lengths to our tree type:
# 

Anole.Graf = compute.brlen(Anole.tree)



# 
# Plotting Branch Lengths
# 
# Now let's plot that tree with the branch lengths listed:

plot(Anole.Graf)


# That shows the branch lengths graphically, to add actual numbers 
# to the plot we will use the edgelabels function, which adds text 
# to the branches.  The nodelabels and tiplabels functions do the 
# same thing for other parts of the plot. Type:

edgelabels(Anole.Graf$edge.length)


# OK, that's just ugly.  Try this instead:

plot(Anole.Graf)
edgelabels(round(Anole.Graf$edge.length, digits=3), frame="n", adj=c(0.5,-0.1), cex=0.6)


# That should look better.  If the numbers are still too cramped, 
# try just blowing up the graphics window.  So what did all those 
# commands do?  $edge.length is a vector of the branch lengths 
# for the tree; the round(vector,digits=3) return a vector with 
# all the values in that vector rounded to 3 decimal places;  
# frame="n" removed that ugly green box; adj determines the 
# position of the text with the first number being the horizontal 
# position and the second the vertical; and cex=0.6 shrunk the 
# text to 60% of normal.
# 
# Alternatively you could add an axis to the plot that 
# corresponds to the branchlengths.
# 

axisPhylo()


# That's nice, and that is usually the way branch lengths are 
# indicated anyways.

# 
# Random Branch Lengths
# 
# Graffen branch lengths are cool, and have at least some biological 
# justification, but they will always give you the same results.  It 
# would be nice to generate random branch lengths, so that we can 
# look at multiple sets of branch lengths for comparisons.  Any function 
# that returns a numeric value can be used to generate branch lengths 
# with compute.brlen.  To generate a random set of branch lengths type:

Anole.rand = compute.brlen(Anole.tree, runif, min=0, max=10)


# runif is a function that generates draws from a uniform random 
# distributions.  min and max are arguments passed to the runif 
# function setting the minimum and maximum value of that 
# distribution.  So that for every branch this command will 
# run the function runif(min=0,max=10) to generate a branch 
# length.
# 
# Let's see what this tree looks like:

plot(Anole.rand)
axisPhylo()


# That's cool.  The only real problem that I see, is that this tree 
# is not ultrametric.  To make it ultrametric let's use the 
# chronopl function:

Anole.ultra = chronopl(Anole.rand, lambda=0)

# lambda sets the depth of the root for this tree.  Why don't you 
# plot it with an axis to see what it looks like.  This function uses 
# the NPRS(non-parametric rate smoothing) algorithm from 
# Sanderson, Mol. Biol. and Evo. (1997) 14, 1218-1231.

plot(Anole.ultra)
axisPhylo()

################################################### 
# Continuous Characters
###################################################
# 
# 
# Reading a Data Matrix
# 
# The first step is to get our data out of the nexus file.  
# read.nexus only reads the tree.  Unfortunately R does not 
# have a function for reading continuous data directly from 
# a nexus file.  Instead we will use the regular read.table function.
# 
# Before we do that we need to create a vector of character 
# names so that we can keep track of what the different 
# characters are.  First open the anole_continuous.nex file in 
# a text editor and scroll down to the data matrix with the 
# continuous characters.  Under CHARSTATELABELS you will 
# find the names of all the character states.  We want to 
# create a vector starting with taxon followed by all the 
# state names in that same order, e.g. in R, type:
#
data.names = c("taxon", "snout-vent length", "mass","foreleg", "hindleg", "tail", "lamellae")

# Now, I've editted the nexus file to remove everything except the 
# data matrix itself.  That means just leave the taxon names, 
# the numbers and the spaces around them.  This was saved as 
# anoles_continuous_data_table.txt.  Open it in a text editor
# and look at it also.
#
# Now we can read the data:
#
(Anole.data = read.table("anolis_continuous_data_table.txt", row.names=1, col.names=data.names))

# row.names=1 made the first column be the names of the rows.  
# col.names set the column names as the the elements of our vector.  
# 
# We had to start our vector with a "taxon", because the first column 
# of our input file was the taxon names, and the actual data did not 
# start until the second column.  Since the first column has no name, 
# it does not matter what the first element of the vector is.  You 
# could have set up this table in any program; the advantage of 
# using Mesquite is that you know the taxon names for the data 
# will exactly match the taxon names for the tree.
# 
# The problem now is that the order of the taxa differs between our 
# tree and our data matrix, and we need to fix that.  The first step 
# is to identify the order that the names in the tree appear in 
# the data matrix.  To do this we will use the match function:

data.order = match(Anole.ultra$tip.label, rownames(Anole.data))


# rownames returns a vector containing the names of the rows, so 
# that data.order is a vector ordered in the same way as the taxa 
# in the tree, but with the numbers representing the order that 
# those taxa appear in the data matrix.
# 
# Now we can easily use that vector to rearrange our data matrix 
# to match the tree:

Anole.ordered = Anole.data[data.order, ]


# Do you see why that worked?  If not, you should review the sections 
# on indexing from the previous lab.



# 
# Plotting data on the Tree
# So now let's just look at the data on our tree, to see how it is 
# distributed.  First plot the tree again, but this time without 
# names:

plot(Anole.ultra, show.tip.label=FALSE, x.lim=c(0,110))


# show.tip.label=FALSE removes the tip labels, and x.lim=c(0,110) 
# sets the x-axis with a little extra space on the right side beyond 
# the 100 units of tree depth.  Now add the data to the tips:

tiplabels(Anole.ordered[,1], frame="n", adj=c(0,0.5), cex=0.8)


# Just for looking that's OK, but it really doesn't tell you anything.  
# You can repeat those two commands to look at the distribution of the 
# other characters.  Maybe it would look better if we did it with 
# colors (see the last section for how to do this).  

# 
# Ancestral State Reconstruction
# 
# Let's do an actual analysis with the data.  Let's reconstruct the 
# ancestral states of these data.  To do this we will use the ace 
# function.  We probably want to do these analyses on the logs of 
# our data not the actual data values four a couple of reasons: 
# these values run from zero to infinity and can not be negative 
# as their logs can; and it is probably more reasonable to assume 
# that multiplying a measurement by a given factor will be 
# evolutionarily equivalent, not adding a given amount.  For example 
# it makes sense that going from 2 inches to 3 inches is equivalent 
# to going from 2 feet to 3 feet not from 2 feet to 2 feet and 1 inch. 
#  
# To log transform our data:

Anole.log = log(Anole.ordered) 


# If you run ace without any additional commands, it will do a Maximum 
# Likelihood analysis using Brownian motion.

Anole.ASR = ace(Anole.log[,1],Anole.ultra)
 
# This will produce a list with several elements.  To see them all 
# type "Anole.ASR".  $loglik is the natural log of the maximum 
# likelihood;  $ace contains the reconstructions at the nodes; 
# $sigma2 shows the value of the parameters for the Brownian motion model; 
# and $CI95 shows the 95% confidence intervals for those reconstructions.
# 
# You can also use ace to reconstruct the ancestral nodes using other methods.  
# To reconstruct the nodes using independent contrasts:

ace(Anole.log[,1], Anole.ultra, method="pic")


# For independent contrasts not scaled by branch lengths:

ace(Anole.log[,1],Anole.ultra,method="pic",scaled=FALSE)


# For generalized least squares you first have to define a correlation 
# structure.  There are several different functions to do this, we will 
# use the simplest:

Anole.cor = corBrownian(1, Anole.ultra)


# You can find other correlation structures in the ape manual.  
# They all start with "cor".  Now we will use that to reconstruct the 
# ancestral nodes by Generalized Least Squares:

ace(Anole.log[,1],Anole.ultra,method="GLS",corStruct=Anole.cor)

# How do the results of these different methods differ?  What about
# the confidence intervals?
# 
# ace can also be used to reconstruct ancestral states for discrete 
# characters, and is in many ways more flexible than Mesquite.  See 
# the ape manual for instructions.



# 
# Plotting data with error bars
# 
# So, we have all these reconstructions with errors, but often it is difficult 
# to visualize what they mean from numbers alone.  First let's transform our 
# results back to a non-log scale:

Anole.ace = exp(Anole.ASR$ace)
Anole.CI95 = exp(Anole.ASR$CI95)


# Then, let's do a simple plot of are data against our nodes:

plot(31:59,Anole.ace, ylim=c(min(Anole.CI95), max(Anole.CI95)))


# This plots the reconstructions against the node numbers.  We reset 
# the range of the y axis to provide enough space for us to add 
# the error bars.
# 
# Now to add the error bars:

segments(31:59, Anole.CI95[,1], 31:59, Anole.CI95[,2])


# This function will draw a series of lines between the first 
# set of coordinates and the second.  To figure out which nodes 
# these results correspond to, you can plot the tree and add 
# the node labels:

Anole.ultra$node.label = 31:59
plot(Anole.ultra, edge.width=2, show.node.label=TRUE)


# Or you could plot the tree and use identify to click on the nodes.
# 
# QUESTION 2.  What is the ancestral state for the most recent common 
# ancestor of angusticeps and strahmi?  95% CI?

# 

# Plotting Reconstructions on the tree as colors
# 
# There is not a straight forward way to do this. The problem is that the 
# basic colors don't tell us that much.  Instead let's create a new set 
# of colors over a more easy to interpret range and assign them values.  
# Then assign those colors to our branches based on their values.
# 
# The first step is to organize our data in the same order as the branches:

data.branches = rep(0,58)
data.branches[match(1:30, Anole.ultra$edge[,2])] = Anole.ordered[,1]
data.branches[match(32:59, Anole.ultra$edge[,2])] = Anole.ace[2:29]


# Now we create a vector of 100 new colors that range from red as the 
# lowest color through white as the highest:

color.range = heat.colors(100)


# heat.colors is one particular set of colors to see others go to 
# help("heat.colors").  To see what the colors we just made look like:

plot(1:100,cex=1.5,col=color.range)


# Then we need to assign every value from our data as an integer between 
# 0 and 100 that is proportional to the data for that branch.  First let's
# find the range of our data in order to decide what colors should be
# assigned to what numbers:

min(data.branches)
max(data.branches)


# I got 35.3 and 183, so let's run our colors from 30 to 200.  

data.rounded = data.branches-30
data.rounded = round(data.rounded*99/170)+1


# We will use this to create a new vector of colors, where the order of 
# the colors corresponds to the order of the branches, and the colors 
# correspond to the value of our data on that branch:

color.data = color.range[data.rounded]


# Now we just use that to plot our results:

plot(Anole.ultra,edge.width=2,edge.color=color.data)


# OK, so the problem with that is almost all the data is in the 
# bottom of the range, so you don't see most of the differences.  
# We can fix that by taking the log of the data:

data.rounded = log(data.branches - 30)
data.rounded = round(data.rounded*99 / log(170)) + 1
color.data = color.range[data.rounded]
plot(Anole.ultra, edge.width=2, edge.color=color.data)


# You may want to add a scale bar to this plot:

value.range = rep(NA,11)
value.range[ 1: (6*2-1) ] = round(exp((c( 5:1*20, 1)-1) * log(170)/99) + 30, digits=1)



# Next, type:
legend(locator(), legend=value.range, fill=color.range[c(10:1*10, 1)], y.intersp=0.5, cex=0.8)

# Then left click on the figure where you want the legend to go, 
# right click and select stop.  (Ummm, on a Mac sometimes you 
# can't right click: Try <ctrl> click, or <enter>, your guess is 
# probably better than mine.)
# 
# I'll leave it up to you to figure out what all those commands mean.


# QUESTION 3. Print out this picture for me.

# Save Your Work
#
# We're going to use this same data set next time:  File>Save workspace...




















