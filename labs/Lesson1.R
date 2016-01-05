# R script - UCT Short course, Day 1

# This file is an R script. You should open it in R to run these exercises.

# Any text preceeded by the hash '#' is a comment and is ignored by R. If the comment is more than one line, it's all commented out by the hash, as long as there are no carriage returns. 
# Once you enter a carriage return, you need a new hash

# When you open R, you will get a welcome screen with version numbers, etc., and then a prompt: >

# The commands below can be cut and paste, or typed in by hand, at the prompt. (Don't type the prompt itself - that's on the R console window.) After typing a command, hit return, and see R's magic happen!

# R's simplest function is as a calculator. Try typing the following at the prompt:
2 + 2

# and R will answer: 
[1] 4

# Clearly 4 is the answer to the problem. So what does that [1] refer to? This indicates that the answer to this problem is a vector, in this case with only one value. More on that later.

# As you would expect, R will follow standard order of operations, and you can write complex expressions. Try typing this:
(13*2)+(15^2)/3
[1] 101


# If you see these brackets or section numbers in these tutorials, it refers to corresponding sections of the R-intro.pdf manual: {sec. 1.9} Now, let's say that you meant to divide the second expression by 2 instead of 3. Do you have to retype or cut and paste the entire expression? No! In the console, simply press the 'up' key and R will fill in the last command. Keep pressing 'up' and you can go back through your entire history, while 'down' will return you to the more recent expressions. So you can just press 'up', backspace once to delete the 3 and replace it with a 2, getting this result:
(13*2)+(15^2)/2
[1] 138.5

# The next trick is to enter commands in a script file, like this one, and have R run them without having to type or cut and paste. Move your cursor to the following line, and place it anywhere on the line. Now, type the 'paste and execute keystroke': on a Mac, it's 'command-Return'. On a PC, type 'control-r'. 
(13*2)+(15^2)/3
[1] 101

# See what happened? The entire text of the line is pasted into the console and executed, and you didn't have to leave this file. Now that's smooth! But what if you wanted to just check the second half of the expression. Then on the following line, select just the text you wish, e.g. '(15^2)/3', and use the same keystroke. Now you just evaluate that part.
(13*2)+(15^2)/3
[1] 75

#You can also select more than one line of commands. E.g. select over both of the next two lines and press the execute keystroke.
2+2
(13*2)+(15^2)/3

# You'll see that R executes each line, shows the answer, and then executes the next. Later on we'll select and run long sections of script using this approach.

# {1.7}
# Try out the help commands:
help(solve)

#equivalent to:
?solve

#or start the local (i.e. not online) interactive help engine
help.start()

# Chapter 2 - P7
# {2.1}
# Now, for the most basic operation - assigning values to variables
# The basic data structure is a vector, which can have one variable or a string of variables of the same type (more on that later)
# To assign a single value to a variable you can use the '=' or the left arrow:
x = 2
x <- 2

#You'll notice that nothing happens when you do this. The value 2 has been assigned to the variable x. Now to see the result, you can type:
x

#On the Mac, option- (option-dash) is a shortcut for the left arrow
# Originally R required use of the left arrow, so hard core users still insist on it. Most newer users just use the equal sign. It is a good practice to adopt one and use it consistently. Unfortunately, as someone who started a while ago with R, I'm not consistent, so you'll see both in my scripts.

# Now to assign a series of values as a vector, you use the 'c' command, which stands for 'concatenate'
x <- c(10,4,5.6,3.1)
x

# You can also assign the other direction:
c(10,4,5.6,3.1) -> x
x

#Vectors can be strung together in assignments to other variables:
y <- c(x,0,x)
y

# Vectors of successive integers can be created without using the c command, instead using the colon ':'. You've seen above how to look at the result of your assignments by typing the variables name. I'm going to leave that out now, and after each assignment you can check the result yourself. You can also use a semi-colon to place a second command on the same line, and check the result there:
x <- 1:10; x
x <- 23:36
x <- 15:5 #Check that out! Descending order of integers!

# {2.2} Vector arithmetic
# Arithmetic is conducted on vector values element by element
x <- c(1,2,3)
x
2*x
x^2

# CRITICALLY IMPORTANT: When two vectors are combined in an operation the shorter one will be recycled until it reaches the length of the longer one. This provides you with great flexibility and power but with power comes danger!! Be very careful that the recycling accomplished what you intended! In the simple case where one vector is a single value, it's a simple result:
x <- c(1,2,3)
y <- 3
x*y

#When both have more than one value it is trickier:
x <- c(1,2,3,4)
y <- c(5,10)
x*y

#in other words, y is recycled to become c(5,10,5,10)
# But what about this:
x <- c(1,2,3)
y <- c(5,10)
x*y

#Now y is recycled to become c(5,10,5), matching the length of x. But R is suspicious that maybe you didn't mean to do this and gives a warning because the longer object's length is not a multiplier of the shorter object's length

# R provides the full range of normal mathematical expressions, which can be used with any vector:
# log, exp, sign, cos, tan, sqrt, ^, etc.

# When you see the asterisks, that means you're being given an assignment - put your answer into this script in the space provided:

# ***** EXERCISE 1.A: Generate the sequence of squares from 1, 4, 9, 16 ... 100 in two commands *****


# **** Success?

# Now the real fun begins. R has all the basic statistical functions (and many more you'll learn as you go along) that can be applied to vectors
x <- 1:10
max(x)
min(x)
length(x) #to get sample size
mean(x)
sd(x) # standard deviation

# and look how easily you can write the expression for the mean:
sum(x)/length(x)

# and the sample variance as a command or an explicit calculation
var(x)
sum((x-mean(x))^2)/(length(x)-1)

# note how in the expression above, the mean and the length are nested inside the brackets. So the command is evaluating mean, sample size and sum all at once, without storing any intermediate calculations along the way.

# *** EXERCISE 1.B: write an expression for the standard error of the mean (standard deviation divided by square root of sample size) ****



# ********

#sort a vector
x <- c(3,1,5,7,2,10)
sort(x)

#and get the pairwise max or min of two vectors
x <- 1:10
y <- 10:1
pmax(x,y)
pmin(x,y)

{2.3} Generating regular sequences
# We've seen the ':' for integer sequences. A more general command for generating regular sequences is 'seq'
# IMPORTANT: This is our first example of a command with alternative arguments, and demonstrates the flexibility of R functions. Check the help document

help(seq)

# As you see, seq has 5 arguments: from, to, by, length.out, and along.with. These give you many different ways of generating sequences. The values of the arguments shown below are the defaults. In other words, if you don't enter anything, the function will use those values. On the other hand, if you enter a value for an argument, it will use your value rather than the default. Try the examples below to make sense of this
#seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),length.out = NULL, along.with = NULL, ...)

seq() #why did it give the value 1? The default values were from=1, to=1 giving a vector with the value 1

seq(from=3,to=10)
#IMPORTANT: If you enter an argument in its correct position, you can omit the argument's name. Or you can use the name and put the arguments in any order. So the following are equivalent

seq(from=3,to=10)
seq(3,10)
seq(to=10,from=3)

#now try some of the other arguments
seq(3,11,by=2) #sequence of odd numbers
seq(0,by=2,length.out=10) #the first 10 even numbers

#what if the arguments conflict?
seq(2,10,by=2,length.out=10)

#you get an error (which of course is preferable to getting a nonsense result without a warning about the error of your ways!)

#and finally the nifty along.with argument which matches the length of another vector
x <- 1:10
seq(10,by=5,along.with=x)

#TIP One place that I like to use the right arrow assignment is to first evaluate an expression and see if it's doing what I expect, and then recall it and add the right arrow to assign the result

seq(10,by=5,along.with=x)

#now if I like the result, go to the console, type 'up-arrow' to recall it, and then add the right arrow and y to assign the result, so you quickly get this
seq(10,by=5,along.with=x) -> y

# Another powerful command for sequences is 'rep' for 'repeat'. Check the help to see the arguments: times, length.out, each (no defaults). It takes some practice using these and allows you to generate all sorts of reqular, repeating sequences

rep(1,times=10)
rep(c(1,2,3),times=2)
rep(c(1,2,3),each=2)

# {2.4} Logical vectors
# R has a special class of vectors with the logical values TRUE and FALSE (also known as boolean operators). These values can be assigned:
x <- TRUE

#but more often they arise as the result of R evaluating an expression of equality or inequality
x <- 1:10
x > 5 # gives a vector of length 10 with values TRUE and FALSE depending on whether x is greater than or less than 5. This vector can be assigned to another variables
y <- x > 5

# There are the regular 4 inequality expressions: <, >, <=, >=. 
# IMPORTANT: Equal and not equal use special expressions you may not have seen before: '==' and '!='. '==' is not an assignment operator, but rather a question: x==y means 'is the value of x equal to y?'. '!=' (pronounced 'bang-equals') means 'is x not equal to y':

x <- 1:5
x==5
x!=5

# The 'and' and 'or' commands for logical vectors check to see if both values are TRUE (&) or if either value is TRUE (|)
x <- c(FALSE,FALSE,TRUE)
y <- c(FALSE,TRUE,TRUE)
x & y
x | y

# TIP The bang operator '!' can be used to reverse the values of a boolean vector:
x
!x

#{2.5} Missing values
# Handling missing values is critical (especially for field ecologists!) and R has a special value 'NA'. The function is.na(x) then checks for the presence of NAs, returning a boolean

x <- c(1,3,3,NA)
is.na(x)

# Note that the values FALSE and TRUE are coerced to 0 and 1, respectively, for arithmetic expressions. Thus you can calculate the number of missing values as:
y <- is.na(x)
sum(y)

# Note that doing this in two steps leaves you with an extra variable 'y' floating around. This can be deleted from your workspace:
rm(y)
y

# More generally, you can nest expressions to avoid creating these unwanted intermediate variables:
sum(is.na(x) 

#What happened? Do you see the + on the next line of your console. This argument was not complete as it's missing the final closed parentheses. You can go to the console and type ')' to finish it. But you need to get those parentheses right, and these nested expressions can be hard to keep track of:
sum(is.na(x)) 

#IMPORTANT: Note that any NA in an arithmetic expression leads to an NA result:
2 + NA

#Thus, all standard statistical operators also return NA
x <- c(1,2,3,NA)
sum(x)
mean(x)

# All of these functions have an optional argument 'na.rm' which stands for 'remove NAs'. The default value is FALSE, and you need to switch it to TRUE to handle data with NAs:
sum(x,na.rm=T)
mean(x,na.rm=T)

#Please read {2.5} for a discussion of 'NaN' ('not a number')

# {2.6} Character vectors
# So far we've dealt with two types of variables: numbers (numeric) and logical (boolean). A third important type is character strings. Any values placed inside quotes are handles as characters:
x <- c('a','b','xyz','cda')
x

#Some operations can be performed on character vectors:
length(x)

#but some can't:
sum(x)

#The 'paste' command combines character strings or numbers into characters, with a specified separator character:
paste('a','b',sep='/')

#paste combined with R's vector repetition rules can give interesting results. Do you understand why this gives the result it does?
paste(c('X','Y'),1:10,sep="")

#We'll have lots of use for character vectors as we move on

# {2.7} Index variables - A VERY IMPORTANT TOPIC
# Subsets of any vector can be identified by index vectors which are placed inside square brackets after the variable name. There are four types of index variables (read section 2.7 for a bit more detail on each one)

#1. Logical - values in the positions with TRUE are returned
x <- c(1,2,3)
y <- c(FALSE,FALSE,TRUE)
x[y]

#Logical index vectors are particularly useful for identifying NAs , non NA values, or other values that meet certain conditions:
y <- x<2
x[y]

x <- c(1,2,NA,4)
x[!is.na(x)]

#2. Positive integers
# Any vector of positive integers will pull out the corresponding items of a vector
x <- 10:20
y <- c(1,4,5)
x[y]

# IMPORTANT: index vectors do not need to be in order and values can be repeated - this can become very valuable:
x <- 10:20
y <- c(2,5,3,2,2,2)
x[y]

#3. Negative integers
# Negative integers exclude the values in those positions
x <- 10:20
y <- -2
x[y]
y <- c(-2,-4,-5)

x[y]
#which is the same as:
y <- -c(2,4,5)
x[y]

#4. Character strings with the names of the variables or values
# Here's an example - we'll learn more about names later
Nobserved <- c(5,10,1,20)
names(Nobserved) <- c('eland','kudu','elephants','warthogs')
Nlarge.animals <- sum(Nobserved[c('eland','kudu','elephants')])
Nlarge.animals

# IMPORTANT: The 'which' function (not introduced in the tutorial) is a very useful way to construct index variables. which returns the positions in a vector that satisfy the stated condition:
x <- 1:10
y <- which(x>5)
y
x[y]

# You could also use a logical vector to extract these values. The difference is that the logical vector has the same length as x, whereas the which command has a length matching the number of values that meet the condition. You can also use the index value to directly extract records. So for example, in a large data set you might want to see all the records for a certain species. Try this out (using some commands we haven't learned yet, but you'll see what they do):

library(ape) # load the ape library
data(carnivora) #load the data set on carnivore body size
head(carnivora) #have a quick look
felis <- which(carnivora$Genus=='Felis')
felis # row numbers in the data set for Felis
length(felis) # the number of records for the genus
carnivora[felis,] #extract those record

# which() is very powerful and frequently used (in my experience)

## ***** EXERCISE 1.C ******
# The variables sex (0 = male, 1 = female) and height below are measurements on a population of elephants (a couple individuals could not be measured). Write an efficient set of R commands using the commands you've learned so far to calculate:
# 1) Number of males and females
# 2) Sample size, mean and st. dev. of height for males and females (excluding the NA values)
sex <- c(0,0,1,1,0,1,0,0,1,1,0,1)
ht <- c(254,211,323,213,NA,289,300,NA,112,350,400,315)



# **********

# CONGRATULATIONS - THIS COMPLETES LESSON 1