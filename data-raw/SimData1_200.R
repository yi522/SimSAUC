## code to prepare `SimData1_200` dataset goes here
set.seed(2021)
num.cov<-4
beta_true <- c(1, -1, 1, -1)
logit.inv <- function(x){exp(x)/(1+exp(x))}
samplesize<-200
# model 1_200
z1 <- matrix(rnorm(samplesize*2, mean=0, sd=1), nrow=2)
z2 <- matrix(rbinom(samplesize*2, 1, 0.5), nrow=2)
z <- rbind(z1, z2)
z <- round(z, digits=3)
p <- logit.inv(t(as.matrix(beta_true))%*%as.matrix(z))
y <- rbinom(samplesize, 1, p)
sum(y)
SimData <- list(t(z), y)
names(SimData) <- c("X", "Y")
SimData1_200 <- SimData
usethis::use_data(SimData1_200, overwrite = TRUE)
