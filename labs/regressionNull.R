## example of using R to simulate data sets to obtain null distributions of critical statistics and conduct power analyses of 'true' relationships

# This example will use linear regression, as the F-statistic is especially clearly developed (compared to some ambiguity about the statistical basis of testing correlations)

# Step 1 - generating null data
# R has a number of functions for standard statistical distributions, and each function can be prepended with 'r' for random values, 'q' for quantiles, 'd' for densities, and a couple others.

# For example, the density of the normal distributions
x <- seq(-3,3,0.1) # what does this do?
plot(x)
y <- dnorm(x,mean=0,sd=1)
plot(x,y)

#simple numerical integration - each value represents an interval of 0.1, so:
sum(y*0.1) #close to 1, just a little left in the extreme tails

# Now generate random values from the null distribution
z <- rnorm(1000,mean=0,sd=1)
hist(z)

# the mean and sd have default values of 0,1 so this is the same as:
z <- rnorm(1000)
hist(z)

# Now generate a regression data set under the null hypothesis. Remember the x-axis is independent and does not need to follow a normal distribution. The key assumption is that the residuals of the y-variable around the true regression line are independent and identically distributed, following a normal distribution. If there is no regression line, that simply means that the y-values are drawn independently from the normal

x <- 1:20
y <- rnorm(20)
plot(x,y)

# fit a regression
fit <- lm(y~x)
abline(fit)

# now select the four lines above starting with the random y values and run them repeatedly, looking at the random regression relationships. Just occassionally they generate pretty steep slopes, but most of the time nothing at all.

# Now let's run this simulation 1000 times and look at the distribution of slopes under the null

slopes <- c() #make an empty variable to hold the slopes
for (i in 1:10) {# short loop to make sure it's working
    y <- rnorm(20)
    fit <- lm(y~x)
    slopes <- c(slopes,summary(fit)$coeff[2,1])
    # you need to look at the summary(fit) object to see that this extracts the slope of the regression
}
hist(slopes)

# looks like it's working, so now let's run it 1000 times. Change the 10 above to 1000 in the 'for' loop, and look at the histogram again. Be sure to run from line 39 resetting the 'slopes' vector to start empty

# Success? Now run it 10000 times to get a nice distributions

# Success? Okay, the next step is to check the critical values of the slopes that fall in the two-tailed significance region - lower 2.5% and upper 2.5%. Sort the slopes and examine the 250th and 9750th values
sort(slopes)[250]
sort(slopes)[9750]

# or use the quantile function to get two tailed and one tailed cutoffs
quantile(slopes,probs=c(0.025,0.05,0.95,0.975))

# So, under the null hypothesis slopes < -0.076 or > 0.076 would be significant, at a 5% type I error rate. In other words, we would judge them significant even though we would be wrong because we know the true answer is these data were generated under the null hypothesis!

# However, these slopes are only relevant to this exact case of N=20 and y values drawn from a (0,1) normal distribution. If you want to check that, go back and change the sd of the rnorm statement, and recheck the distribution of slopes - the quantiles for the critical values will change. So we need a statistic that adjusts to the data and provides a more general solution. People smarter than we are solved this problem and invented the F-statistic - a ratio of variances among vs. within groups - which is sensitive to sample size but not to the absolute values of the data, as long as they are normally distributed.

# If you do a regression with 20 observations, you look up the significance of the result by comparing an F-statistic to the critical values, which have been solved analytically. For example, with N=20 there is 1 degree of freedom lost to the grand mean of the data, 1 degree of freedom for the slope, and 18 'error' df remaining. So the slope will be significant if the F-value exceeds the 95th percentile (F-values are always positive, but it is too tailed because it will be large for negative or positive slopes). So the critical value is:
qf(0.95,1,18) 

y <- rnorm(20)
fit <- lm(y~x)
plot(y~x)
summary(fit)
# look at the F-statistic on the bottom line, did it exceed the critical value?

# Now let's simulate a null distribution, and we can show that the critical value of F matches the theoretical value

Fvals <- c() #make an empty variable to hold the slopes
for (i in 1:1000) {# short loop to make sure it's working
    y <- rnorm(20)
    fit <- lm(y~x)
    Fvals <- c(Fvals,summary(fit)$fstatistic[1])
    # you need to look at the summary(fit) object to see that this extracts the slope of the regression
}
hist(Fvals)

# Is the 95th percentile of the simulated data close to the tabled critical value:
quantile(Fvals,probs=0.95)
qf(0.95,1,18) 

# If they don't seem very close, try the loop again but do 10,000 or 100,000 reps instead of 1000. (BUT: don't do this until you are ready for a coffee break. R is notorious for being slow running loops so 100,000 could take an hour or more depending on your CPU speed.) Does that bring it closer? For some purposes you have to do large numbers of simulations to obtain precise critical values manually.

# Now, why would you do all this if you can just look up the critical value of F in a table? Because, that value is based on the assumption of normality of the data. If your data violates the assumption, you can use simulation to obtain your own, custom critical values for significance. Now you never have to rely on the standard assumptions of parametric statistics.

# For example, let's assume your dependent data are binary (0,1). You can use logistic regression. Or you can use normal regression and just figure out your own custom, critical values:

Fvals <- c() #make an empty variable to hold the slopes
for (i in 1:10000) {# short loop to make sure it's working
    y <- rbinom(20,1,0.5)
    fit <- lm(y~x)
    Fvals <- c(Fvals,summary(fit)$fstatistic[1])
    # you need to look at the summary(fit) object to see that this extracts the slope of the regression
}
hist(Fvals)
quantile(Fvals,probs=0.95)

# how does that compare to the standard critical value under the normal distribution?
qf(0.95,1,18) 

# You can see that slightly larger F values are needed to obtain a significant result, but maybe not so different given this violation of the normality assumption. Now, you might say, that's not allowed. You can't use an F-statistic unless your data meets the assumptions of parametric statistics. No - not true. You can't use the standard CRITICAL VALUE to determine significance. But you can use the F-value, or any other statistic you want to make up, if you determine your critical values by simulation where your simulated data matches the patterns in your observed data. 

# The key word there is 'matches': how do you know your simulation is drawing on the same distribution as your data? A standard approach is to use resampling with replacement to bootstrap the null distributions from the distribution represented by your actual data set, or resampling without replacement to get a distribution of the possible orderings of your observed data to see if your observed ordering departs from null. This is how you would do it.

y <- c(1,1,1,1,1,1,1,1,1,5,1,4,1,10,1,3,1,2,1,16)
plot(y~x) #looks like a relationships, but not a good fit to a standard regression model
fit.obs <- lm(y~x)
abline(fit.obs)
summary(fit.obs)
summary(fit.obs)$f 
#Yes, significant at p = 0.04469 - so just barely and the distribution is not very pretty, so maybe normality of residuals isn't valid and we shouldn't trust this borderline result. Let's resample without replacement to get a null distribution of F-values:

Fvals <- c()
Nreps <- 10000 # I'm going to take this out of the for loop because we need to use the value twice in the next bit of code, so we only want to have to change it in one location
for (i in 1:Nreps) {
    yrand <- sample(y,20,replace=FALSE)
    fit.null <- lm(yrand~x)
    Fvals <- c(Fvals,summary(fit.null)$fstatistic[1])
}
hist(Fvals)
sum(Fvals>=summary(fit.obs)$f[1])/Nreps # WHY DOES THIS WORK TO GET A SIGNIFICANCE VALUE?
#Yes, under this simulated distribution, observed data depart from null expectation at a more significant level than we obtained using (incorrectly) standard statistical test

### Power analysis
# This is all interesting, but the real value of simulations may lie in power analyses, where you intentionally want to simulate data that DOES have pattern in it to determine whether your statistical methods have enough power to detect this real result. For this, we need a slightly more complex formula for our randomized data, so that it departs from the null. For example, here is y data with a true slope of 2 and a normal residual error around the slope
y <- x*0.15 + rnorm(20)
plot(y~x)
fit <- lm(y~x);abline(fit)
summary(fit) # is your example significant?

# Now, if the true model for your data is a slope of 0.15 and residual error with a mean=0 and sd=1, what is the likelihood that you would detect a significant result over multiple possible data sets
Fvals <- c()
Nreps <- 1000
for (i in 1:Nreps) {
    y <- x*0.15 + rnorm(20)
    fit <- lm(y~x)
    Fvals <- c(Fvals,summary(fit)$fstatistic[1])
}
# Now, let's fall back on the table statistical value
Fcrit <- qf(0.95,1,18)
Fcrit
# How many of your simulated results had an f-statistic greater than the critical value
sum(Fvals>=Fcrit)/Nreps #why does this work?

# So, this suggests that a pattern of that magnitude would be detected more than 90% of the time in your research. Seems like regression will be pretty effective as a statistical tool to address your hypothesis if the true effect is of this magnitude. Now try changing the slope value above to see how power shifts with the strength of the true effect.

# Challenge assignment
# Set up a script that will test different slopes and record the power value, and then make a plot of power vs. true slope to see how power increases with the effect size of a relationship.