## Quick R script to conduct monte carlo null model test for the number of steps on a phylogenetic tree

library(ape)
library(phangorn)

# Here's a 32 taxon tree with a basal split between 8 and 24 taxon clades:
t32=read.tree(text="((((t30:1,t23:1):1,(t32:1,(t19:1,(t11:1,t24:1):1):1):1):1,((((t26:1,t14:1):1,(((t21:1,t20:1):1,t5:1):1,(t22:1,t13:1):1):1):1,(t10:1,(t3:1,t18:1):1):1):1,((t15:1,t7:1):1,(((t6:1,(t17:1,t9:1):1):1,t29:1):1,(t25:1,t31:1):1):1):1):1):1,(((t12:1,t27:1):1,(t4:1,t1:1):1):1,(t8:1,(t16:1,(t28:1,t2:1):1):1):1):1);")
plot(t32,'cladogram',use.edge.length=F)

#create a color pallette for plotting trait values
pall <- c('yellow','lightblue')

# create a trait with a high degree of phylosignal, convert to class phyDat to work with the phangorn library
x <- c(rep(1,24),rep(0,8))
names(x) <- t32$tip.label
xx <- phyDat(x,type='USER',levels=c(0,1))

# plot tree with trait values
plot(t32,'cladogram',use.edge.length=F,show.tip.label = F,direction='upwards',edge.width=2)
tiplabels(xx,bg=pall[x+1])

# calculate minimum number of steps under parsimony for evolution of this trait
parsimony(t32,xx)

# now create a trait with 8 0's scattered across tips, each one with a sister taxon with state 1
y <- c(1,0,1,1,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,0)
names(y) <- t32$tip.label
yy <- phyDat(y,type='USER',levels=c(0,1))
plot(t2,'cladogram',use.edge.length=F,show.tip.label = F,direction='upwards',edge.width=2)
tiplabels(y,bg=pall[y+1])

# calculate minimum number of steps
parsimony(t32,yy)

# create a function that takes a tree and a trait vector, and randomly resamples the trait with or without replacement. Without replacement simply permutes the states across the tips. With replacement will create a distribution of trait vectors where the number of taxa with each state follows a binomial distribution around the observed number. Function returns the number of taxa with state '1' and the number of steps in the evolution of the trait. As written this will only make sense if states are (0,1), because I use a simple sum to calculate number of taxa with state 1
rpars <- function(t,x,rep=F) {
    xx <- sample(x,replace = rep)
    names(xx) <- t$tip.label
    xp <- phyDat(xx,type='USER',levels=c(0,1))
    ns <- parsimony(t,xp)
    return(c(sum(xx),ns))
}

# Now obtain the null distribution without replacement. The replicate command runs a function N times and returns the results as a matrix with N columns and as many rows as there are output values in the function
x8 <- replicate(9999,rpars(t32,x,F))
dim(x8)

# Add the observed result for trait x above to the vector of null results, and look at the distribution. The table is useful because the bars in the tail are so small you can't see them in the histogram. Calculate the p value for trait x as the number of cases in which the null values are equal to or less than the observed
xMP <- parsimony(t32,xx)
x8n <- c(xMP,x8[2,])
hist(x8n,breaks=seq(0.5,8.5,by=1),main='sample without replacement',xlab='number of steps')
table(x8n)
p1 <- length(which(x8n<=xMP))/10000
p1

# Now repeat sampling with replacement
x8r <- replicate(9999,rpars(t32,x,T))
x8n <- c(xMP,x8[2,])
hist(x8r[2,],xlab='number of steps',main='resample with replacement')
plot(t(x8r),xlab="number of 1's",ylab='number of steps')
table(x8r)
p2 <- length(which(x8r<=xMP))/10000
p2
