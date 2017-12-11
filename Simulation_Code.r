##Survival simulation tutorial -- FAITH LEE ##
##It is really simple and straightforward## 

#For reproducibility 
set.seed(100) 

#Define the following parameters outlined in the step: 
n = 1000 
beta_0 = 0.5
beta_1 = -1
beta_2 = 1 

b = 1.6 #This will be changed later as mentioned in Step 5 of documentation 

#Step 1
x_1<-rbinom(n, 1, 0.25)
x_2<-rbinom(n, 1, 0.7)

#Step 2 
U<-runif(n, 0,1)
T<-(-log(U)*exp(-(beta_0+beta_1*x_1+beta_2*x_2))) #Eqn (5) 

#Step 3 
C<-runif(n, 0, b)

#Step 4 
delta<-ifelse(T<=C, 1, 0)

#Step 5 
prop<-sum(delta==0)/n
#0.235 which is in our desired range. Otherwise, go back and look at the distribution of T and 
# set a reasonable b accordingly. b shold be somewhere around 1 to 2 based on T and our covariates. 

#Step 6 
surv_time<-ifelse(T<=C, T, C)

#Step7 
library(survival) #load the required package to do the plot 
simulated_data<-data.frame(x_1, x_2, delta, surv_time)

plot_1<-survfit(Surv(surv_time, delta)~1, data = simulated_data)
plot_2<-survfit(Surv(surv_time, delta)~x_1, data= simulated_data)
png("plot_1.png")
plot(plot_2, lty = 2:3, xlim = c(0,2), col= c("red", "blue"), xlab = "Time", ylab = "Survival probability") 
legend(1.2, .9, c("KM curve", "No x_1", "x_1"), lty = 1:3, col = c("black", "red", "blue")) 
title("Kaplan-Meier Curves\n for simulation study, stratified by x_1") 
lines(plot_1, lty = 1, col = c("black"), conf.int = FALSE)
dev.off()
png("plot_2.png")
plot_3<-survfit(Surv(surv_time, delta)~x_2, data= simulated_data)

plot(plot_3, lty = 2:3, xlim = c(0,2), col= c("red", "blue"), xlab = "Time", ylab = "Survival probability") 
legend(1.2, .9, c("KM curve", "No x_2", "Has x_2"), lty = 1:3, col = c("black", "red", "blue")) 
title("Kaplan-Meier Curves\n for simulation study, stratified by x_2") 
lines(plot_1, lty = 1, col = c("black"), conf.int = FALSE)
dev.off()





