#Simulating fake random normal data
x_1 <- rnorm(1000,5,7) #1000 Random numbers mean 5 and standard deviation of 7

#Simulate gamma distrubation
x_2 <- rgamma (n = 1000, shape = 3, scale = 2)

#Plot data
hist(x_1, col = "gray") #plot p(x)
hist(x_2, col = "blue") #plot p(x)

true_error <- rnorm(1000,0,2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2
true_beta_2 <- 6.5

y_1 <- true_beta_0 + true_beta_1*x_1 + true_error
hist(y_1) #plot p(y_1)
plot(x_1,y_1, pch=20, col= "red", xlab = "1000 Random Numbers with rnorm",
     ylab = "Y_1", abline(lm(y_1 ~ x_1))) #plot p(x,y)

#using gamma distrubation
y_2 <- true_beta_0 + true_beta_1*x_2 + true_error
hist(y_2) #plot p(y_2)
plot(x_2,y_2, pch=20, col= "red", xlab = "1000 Random Numbers with rgamma",
     ylab = "Y_2", abline(lm(y_2 ~ x_2))) #plot p(x,y)

#using both distrubations
y_3 <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_error
hist(y_3) #plot p(y_2)
plot(x_2,y_3, pch=20, col= "red", xlab = "Random Numbers with both",
     ylab = "Y_3", abline(lm(y_3 ~ x_2))) #plot p(x,y)

#checking the original data
true_beta_0
true_beta_1
true_beta_2

# Test the model
model_1 <- lm(formula = y_1~x_1)
model_2 <- lm(formula = y_2~x_2)
model_3 <- lm(formula = y_3~x_1+x_2)

lm(formula = y_1 ~ x_1)
coefs_1 <- coef(model_1)

lm(formula = y_2 ~ x_2)
coefs_2 <- coef(model_2)

lm(formula = y_3~x_1+x_2)
coefs_3 <- coef(model_3)
