##############################################################
# ============================================================
# Lab 14: Community Ecology & Phylogenetics in R
# ============================================================
# by David Ackerly, Nat Hallinan, Traci Grzymala (updated by Will Freyman)
# much of this was also taken from Community phylogenetic analysis with picante by Steven Kembel (skembel@uoregon.edu)
# http://picante.r-forge.r-project.org/picante-intro.pdf
#
##############################################################

# Today's lab will cover spatial and community phylogenetics. These two approaches to phylogenetics
# are highly related but applied differently. Community phylogenetics incorporates phylogenetic
# structure into community ecology, and tries to understand how specific ecological communities 
# are assembled. Spatial phylogenetics operates at the landscape level, incorporating spatial data 
# to view patterns of phylogenetic diversity and endemism across large landscapes.

# For spatial phylogenetics, Andrew Thornhill will walk us through a demo of the software Biodiverse. 
# Then we'll use the R package picante to calculate community phylogenetic statistics and visualize
# the results on the phylogeny.


# Download the two files from the website for today 'oakphylo.txt' and 'oaksample.txt':
# http://ib.berkeley.edu/courses/ib200/labs/14/oakphylo.txt
# http://ib.berkeley.edu/courses/ib200/labs/14/oaksample.txt

# list the files in the directory. Make sure 'oakphylo.txt' and 'oaksample.txt' are in your working directory.  
list.files()

# You need the PICANTE R package
# install.packages("picante")
library(picante)

# The package picante has many of the operations found in the program phylocom available from: www.phylodiversity.net/phylocom.  The two statistics we will focus on are NRI and NTI, defined in lecture.  There are many other functions in the picante package for phylogenetic analysis of community ecology.  If you are interested in these types of analyses then I would suggest that you read over the picante manual.

#We will use some of the data that is already found in the picante package. 
data(phylocom)

#This is the made up phylogenetic tree for our example. Let's rename it 'phy'
phy <- phylocom$phylo
plot(phy)


#This is a matrix of abundance data for the taxa in this tree from 6 different locations (communities).  The names of species should match names in the phylogeny.  Let's rename this 'samp'
samp <- phylocom$sample
samp

#This is another matrix with discrete character data. Let's rename this 'traits'
traits <- phylocom$traits
traits

########################################################## 
# Plotting Community Data
##########################################################
#
# Let's look at how the taxa found in those communities are distributed on our tree.  First, let's prune any taxa from our tree that are not also represented in our sample matrix (ie. also represented in our community).
prunedphy <- prune.sample(samp, phy)
prunedphy

# We also need to make sure the species are arranged in the some order in the community data and the phylogeny. This is an important step - several functions in picante assume that the community or trait data and phylogeny data have species arranged in the same order, so it's good to always make sure we've done so before running any analysis. The following command sorts the columns of samp to be in the same order as the tip labels of the phylogeny:

samporder <- samp[, prunedphy$tip.label]
samporder

# Let's visualize our data. Now let's see how taxa from the six communities in the Phylocom example data set are arranged on the tree. The following commands set up the layout of the plot to have 2 rows and 3 columns, and then plot a black dot for the species present in each of the six communities:

par(mfrow = c(2, 3))
for (i in row.names(samporder)) {
  plot(prunedphy, show.tip.label = FALSE, main = i)
  tiplabels(tip = which(samporder[i, ] > 0), pch = 19, cex = 2)
}

# You may want to expand your window to visualize this better.
#
#QUESTION 1:  Look at the names of the rows of the phylocom sample matrix. Do the names of the rows seem to make sense based on what you see in these trees?  Explain why or why not.
#
#
# Let's also visualize the trait data that we have. We'll plot the traits with a different color for each trait value:

par(mfrow = c(2, 2))
for (i in names(traits)) {
  plot(phy, show.tip.label = FALSE, main = i)
  tiplabels(pch = 22, col = traits[, i] + 1, bg = traits[,i] + 1, cex = 1.5)
}

# Again, you should zoom the plotting window to get a better sense of how these data are distributed across your tree.
#
# QUESTION 2: Which of the traits appear to have the greatest phylogenetic signal?
#
#
#################################################################################
#  Calculating NRI and NTI
#################################################################################
#
#Now we will calculate NRI (Net Relatedness Index) and NTI (Nearest Taxon Index) for our different communities. Rembember, negative NRI and NTI values indicate a high level of phylogenetic overdispersion, and positive NRI and NTI values indicate phylogenetic clustering.
#First we need to make a phylogenetic distance matrix.

phydist <- cophenetic(phy)

# Take a look at phydist.  This is a matrix where the rows and columns are the taxa and the elements of the matrix are the phylogenetic distance between those pairs of taxa.  

phydist

# To calculate the NRI:

ses.mpd(samporder, phydist,null.model="taxa.labels")

# The rows are the communities.  The first four columns should be pretty straight forward given the definition of the NRI from lecture.  To review:
# ntaxa Number of taxa in community
# mpd.obs Observed mean pairwise distance (MPD) in community
# mpd.rand.mean Mean MPD in null communities
# mpd.rand.sd Standard deviation of MPD in null communities
# mpd.obs.rank Rank of observed MPD vs. null communities
# mpd.obs.z Standardized effect size of MPD vs. null communities (equivalent to -NRI)

# The fifth column is the rank of the score against randomized communities; the sixth column is the negative NRI; and the seventh column is the one tailed p-value for significantly high NRI.  So that communities 1, 2 and 3 are significantly clustered and community 5 is significantly spread out.

# You can also calculate NTI:

ses.mntd(samporder, phydist,null.model="taxa.labels")

# The explanations for the columns are as above, except substitute NTI for NRI.  
#
# QUESTION 3.  Compare the results to the figures we made for each community – do the NRI and NTI values make sense?  Explain how.
#
#################################################################################
#  Null Distributions
#################################################################################
#
#You can also use different null models to calculate these values.  “taxa.labels” shuffles the distance matrix across the taxa in the community.  Let’s try “phylogeny.pool”, which randomly draws species from the phylogeny for its null distribution.

ses.mpd(samporder, phydist,null.model="phylogeny.pool")

# Did that have much effect on our estimate of NTI and our p-values? There are several other null distributions available.  Look at the documentation for these two functions to see your options.
#
#
# We can also calculate measures of phylogenetic distance
traits <- traits[phy$tip.label, ]
multiPhylosignal(traits, phy)
#
#
# QUESTION 4:  Look at the column labeled 'K'. This is Bloomberg's K that we have previously discussed in lecture.  K measures the phylogenetic signal of a trait -- when K is greater than 1.0 there is more phylogenetic signal than expected under a Brownian motion model -- closely related species resemble each other more than expected. When K is less than 1.0 it means closely related species differ more than would be expected under Brownian motion. 
# Look at the results. What do these numbers mean?  Which traits have strong phylogenetic signal?  Do these values agree with your previous predictions?
#
#################################################################################
#  Reading Oak Data
#################################################################################
#
# Open the file named 'oakphylo' in a text editor. This is the default name for the phylogeny read in by phylocom, and it is a simple newick string (with or without branch lengths).  To open it in R drag the file onto the terminal and edit around the file name to use the ape function read.tree.

tree.oak <- read.tree('oakphylo.txt')
plot(tree.oak)

# Why does that look so small and sad?  Oh right...

par(mfrow = c(1, 1))
plot(tree.oak)

# Open the 'oaksample' file in a texteditor as well to examine the format of the file. This is the format to input species lists for individual communities.  As you can see the first column is the community the second column is the abundance and the third column is the taxon.  Let's read this file into R and display the sample matrix:

sample_oak <- read.table('oaksample.txt')
sample_oak <- sample2matrix(sample_oak)
sample_oak

# QUESTION 5:  Plot each community on the tree.  Calculate NRI and NTI for each community.  Compare results to Fig. 4 from Cavender-Bares et al. 2004 (see lecture handout). Do they tell the same story? Explain. **HINT** You can just use the R script from above and substitute the oak files to get at these questions, which is good practice for what you might be doing in your final projects.  As always, please ask if you run into any difficulties.
#
#
#
