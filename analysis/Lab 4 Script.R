###LAB 4 Scripts - For Loops, Functions, and Bootstrapping in R

library(knitr)
opts_knit$set(root.dir = 'C:/Users/sarah/OneDrive/Desktop/R Stats Course/R_Class_2026')

###Model data with a normal distribution, we want 1000 observations

x_1000 <- rnorm(1000)

#make a histogram of the data, define the axis limits, add title

hist(x_1000, freq = FALSE, ylim = c(0,2), xlim = c(-4,4), main = "1000 observations")

#calculate the mean of the distribution 1000 times
mean.x_1000 <- replicate (1000, mean(rnorm(1000)))
hist(mean.x_1000, add = TRUE, col = "blue", freq = FALSE)

#Get 95% intervals of the data
(mean(x_1000))+(1.96*(sd(x_1000)))
(mean(x_1000))-(1.96*(sd(x_1000)))


#Get 95% CI on the mean
(mean(x_1000))+(1.96*(sd(x_1000)/sqrt(1000)))
(mean(x_1000))-(1.96*(sd(x_1000)/sqrt(1000)))


#same thing with 25 observations 

x <- rnorm(25)
hist(x, freq = FALSE, ylim = c(0,2), xlim = c(-4,4), main = "25 observations")

#calculate the mean of that distribution 1000 times
mean.x <- replicate(1000, mean(rnorm(25)))
hist(mean.x, add = TRUE, col = "blue", freq = F)

#get 95% intervals of the data
sd(x)
(mean(x))+(1.96*(sd(x)))
(mean(x))-(1.96*(sd(x)))


#Get 95% CI interval on the mean
(mean(x))+(1.96*(sd(x)/sqrt(25)))
(mean(x))-(1.96*(sd(x)/sqrt(25)))



###Creating a function for a 95% CI to be stored in r session. will output standard deviation, standard error, 95% CI of the data, and 95% CI on the mean

#Create the function
My.CI <- function(x.vect){
  #x.vect should be normally distributed otherwise won't work
  sd.x <- sd(x.vect)
  n <- length(x.vect)
  se.x <- sd.x/sqrt(n)
  data.95 <- mean(x.vect)+1.96*c(-1,1)*sd.x
  
  mean.95 <- mean(x.vect)+1.96*c(-1,1)*se.x
  
  return(list(sd=sd.x, se=se.x, mean=mean(x.vect),
              data.95=data.95,
              mean.95=mean.95
              )
         )
}
My.CI(x_1000)


#FOR LOOPS - use for bootstrapping
#this loop counts from 1-100, then print the current counting number (i)

for(i in 1:100){
  print(i)
}


#create an empty vector to store output
my.output <- c()
my.input <- seq(53, 128, by=5)

#loop through each element of my input, do a calculation, and save it in my.output

for(i in 1:length(my.input)){
  my.output[i] <- my.input[i]^2 + my.input[i]
}
my.output


##Use sample() to resample the vector my.input with replacement

my.input
sample(my.input, replace = TRUE)

#different than my.input, which is just a sequence counting by fives. 

#now resample the names of two groups 100 times, with different probabilities

s1 <- sample(c("Group 1", "Group 2"), size = 100, replace = TRUE, prob = c(0.3, 0.7))
table(s1)



##Bootstrap example: test the null hypothesis that the slope between two variables does not equal zero

#upload fish growth temp dataset. Fisgh growth rate per year was measured as a function of temp degrees C

fish.df <- read.csv("data/FishGrowthTemp.csv", header=TRUE)

#plot the data
par(mfrow=c(1,1))
plot(fish.df$temp, fish.df$growth.rate, 
     xlab = 'Temperature (C)', ylab = 'Growth Rate (mm/y)')
#add fit line for a linear moedel of growth rate to temp
abline(lm(fish.df$growth.rate~fish.df$temp))


#the model assumes residuals are normally distributed - look at them and see if they are

hist(lm(fish.df$growth.rate~fish.df$temp)$residuals,
     xlab = 'Growth Rate (mm/y)')

##use qq plot to see if data follows an expected distribution (ie normal)

qqnorm(fish.df$growth.rate)
qqline(fish.df$growth.rate)


#this is the slope we observe from a linear model (y = ax + b)


lm(fish.df$growth.rate~fish.df$temp)$coef

#slope is positive, so test if it is significant based on a stat test that assumes the normal distribution

summary(lm(fish.df$growth.rate~fish.df$temp))



#OK gonna bootstrap this data, resample it 1000 times and calculate if the resampled data is different from zero. 

#create a sample size

n.boot <- 1000

#fill it in 

slope <- rep(NA, n.boot)
n.rows <- nrow(fish.df)

#create the slopes

for (i in 1:n.boot){
  new.rows <- sample(1:n.rows, replace = TRUE)
  new.data <- fish.df[new.rows,]
  slope[i] <- lm(new.data$growth.rate~new.data$temp)$coef[2]
}

#look at the distribution of the slope: 
hist(slope, breaks = 100)


#find the lower value for the 95% CI on the distribution
sort(slope)[n.boot*0.025]
#find the upper value for the 95% CI on the distribution
sort(slope)[n.boot*0.975]


#Use bootstrapping to show that the standard error works for the mean (you can trust the 95% CI of the mean even in non-normal distributions of data)

n = 100000
set.seed(1)
x1 <- rpois(n/2, 2)
x2 <- rpois(n/2, 19)
data <- c(x1, -x2)
hist(data, xlab = 'x', main = 'Really non-normal data')
mean(data)

#sample with n=30 individuals
set.seed(1)
my.sample <- sample(data, 30, replace = TRUE)
mean(my.sample)

#mean wasn't too far off. now get 95% CI

se <- sd(my.sample)/sqrt(length(my.sample))
mean.95 <- mean(my.sample)+1.96*c(-1,1)*se
print(mean.95)

#means that 95% chance the true mean is between those two values, which it was. Now check sampling distribution for the mean

n.boot <- 10000
mean.est <- rep(NA, n.boot)

for(i in 1:n.boot){
  my.sample <- sample(data, 30, replace = TRUE)
  mean.est[i] <- mean(my.sample)
}

#the actual 95% of sample means are in the range...
hist(mean.est, xlab = 'Sample means')

#get lower 95% CI on distribution of means

sort(mean.est)[n.boot*0.025]

#get upper 95% CI on the distribution of means

sort(mean.est)[n.boot*0.975]

#put lines where the 95% CI is in the distribution of the means

abline(v = c(sort(mean.est)[n.boot*0.025], sort(mean.est)[n.boot*0.975]),
       lty = 2, col = 'red')





####In-class examples
#specify numbers

n <- 85
mu_C <- 5
mu_T <- 5

#what is a sigma
sigma <- 8

#Replicate the process, store the treatment effect for all of them
replicates <- 1000
treatment_effect <- rep(NA, replicates)
p.values <- rep(NA, replicates)

for(i in 1:replicates){

#specify that both means would be the same
X_C <- rnorm(n, mean = mu_C, sd = sigma)
X_T <- rnorm(n, mean = mu_T, sd = sigma)

test.outcome <- t.test(X_T, X_C)

treatment_effect[i] <- mean(X_T) - mean(X_C)
p.values[i] <- test.outcome$p.value
}

treatment_effect

t.test(X_T, X_C)

hist(treatment_effect, main = "Dist of Treatment Effect", 
     xlab = "Treatment Effect", col = "lightblue", border = "black")
hist(p.values)


