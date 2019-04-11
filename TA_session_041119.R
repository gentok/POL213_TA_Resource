#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "April 11, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required packages
library(readr) # Reading csv file
library(ggplot2) # Plotting
library(faraway) # for ilogit function

#'
#' # Coarse Grid Search
#' 
#' Think about the voter turnout of counties within a state, follows a normal distribution 
#' with mean 53.2 and standard deviation 8

set.seed(780)
y <- rnorm(100, mean = 53.2, sd = 8)
hist(y)

#' Assuming that standard deviation is 10, conduct a coarse grid search of theta parameter.

z1 <- seq(50,60, by = 2.5)
loglike1 <- sapply(z1, function(z) log(prod((1/sqrt(2*100*pi))*exp(-((y-z)^2/(2*100))))))

z2 <- seq(50,60, by = .1)
loglike2 <- sapply(z2, function(z) log(prod((1/sqrt(2*100*pi))*exp(-((y-z)^2/(2*100))))))

# Make it a data.frame
searchdata <- data.frame(z = c(z1,z2),
                         loglike = c(loglike1, loglike2),
                         Level = c(rep("By 2.5",length(z1)),
                                 rep("By .1", length(z2))))

# Export plot
ggplot(searchdata, aes(x=z, y=loglike, color=Level)) + 
  geom_line(size=1) + theme_bw()

# Find Max
z1[which.max(loglike1)] 
z2[which.max(loglike2)] # More fine grained 

#'
#' # Fitting Logit
#'

#' The following data contains county level presidential election results 2000-2016.
#' (Check codebook at https://github.com/gentok/POL213_TA_Resource/blob/master/data/County%2BPresidential%2BReturns%2B2000-2016.md)

d <- read_csv("https://raw.githubusercontent.com/gentok/POL213_TA_Resource/master/data/countypres_2000-2016.csv")
d <- na.omit(d)

#' Let's subset the data and extract county-level votes for Gore (2000), Bush (2000), 
#' Obama (2008), and McCain (2008) in Texas.

# Gore Vote Share in TX
TX_gore <- d[d$year==2000 & d$party == "democrat" & d$state_po == "TX",]
# Bush Vote Share in TX
TX_bush <- d[d$year==2000 & d$party == "republican" & d$state_po == "TX",]
# Obama Vote Share in TX
TX_obama <- d[d$year==2008 & d$party == "democrat" & d$state_po == "TX",]
# McCain Vote Share in TX
TX_mccain <- d[d$year==2008 & d$party == "republican" & d$state_po == "TX",]
# Check if county rows match
all(TX_obama$FIPS == TX_mccain$FIPS)
all(TX_obama$FIPS == TX_bush$FIPS)
all(TX_obama$FIPS == TX_gore$FIPS)

#' Calculate 2008 Obama win-lose and 2000 Gore Vote Share

# Create Data
TX_data <- data.frame(FIPS = TX_bush$FIPS)

# Gore Vote Share
TX_data$goreshare <- TX_gore$candidatevotes/(TX_gore$candidatevotes + 
                                        TX_bush$candidatevotes) * 100
# Obama Win-Lose in County
TX_data$obamawin <- (TX_obama$candidatevotes >= TX_mccain$candidatevotes) * 1

#'
#' Estimate Logistic Regression predicting Obama win-lose by Gore vote share.
#'

# Plot Obama win-lose by Gore Vote Share
p <- ggplot(TX_data, aes(x=goreshare,y=obamawin)) + 
  geom_jitter(height=0.1) + # Jittered points
  geom_hline(aes(yintercept=1), linetype=2) + # Horizontal dashed line @ 1
  geom_hline(aes(yintercept=0), linetype=2) + # Horizontal dashed line @ 0
  xlab("Gore Vote Share (2000)") + 
  ylab("Obama Victory (2008)") + 
  ggtitle("TX Counties 2000 Gore Vote Share and 2008 Obama Victory") +
  theme_bw()
p

# Estimate Logistic regression
logit.TX_obamawin <- glm(obamawin ~ goreshare, TX_data, family = binomial)
summary(logit.TX_obamawin)

# Log-likelihood of the estimates
logLik(logit.TX_obamawin)

# Calculate Logit prediction
prediction <- ilogit(-17.8919 + 0.36081*TX_data$goreshare)
# OR
prediction <- predict(logit.TX_obamawin, type="response")

# Add prediction to the plot
p + geom_line(aes(y=prediction), size=1)


#' 
#' # Manually Fitting Logit
#' 
#' ## Prepare Variables & functions

# DV  
y <- cbind(TX_data$obamawin)
# IV
x <- cbind(TX_data$goreshare)
# Constant
cons <- rep(1, length(x[,1]))
# Matrix of Constant and IV(s)
xmat<-cbind(cons, x)

# Function to calculate Log Likelihood
llk.logit <- function(param,y,x) {
  # prepare constant
  cons <- rep(1, length(x[,1]))
  # matrix of constant and IV(s)
  x <- cbind(cons, x)
  # assigned beta parameters
  b <- param[1 : ncol(x)]
  # calculate fitted values 
  xb<-x%*%b
  # calculate log-likelihood
  sum(y*log(1 + exp(-xb)) + (1-y)*log(1 + exp(xb)))
}

# Set starting values taken from OLS.
ols.result <- lm(y~x); ols.result
stval <- ols.result$coeff

#' ## First iteration

# Optimize by log-likelihood
logit.result <- optim(stval, llk.logit, method="BFGS", 
                      control=list(maxit=0, trace=1), hessian=TRUE, y=y, x=x)

# Printing Optimization results #
# beta paramter estimates
parm_est <- logit.result$par; parm_est 
# variance covariance matrix
var_cov <- solve(logit.result$hessian); var_cov
# Standard error of beta estimates
std_err <- sqrt(diag(var_cov)); std_err
# Log-likelihood
log_like <- -logit.result$value; log_like
# Deviance
dev <- -2*(log_like - 0); dev

# Find new starting value
beta <- cbind(parm_est) # Store paramete estiamtes
plgtb <- 1/(1 + exp(-xmat%*%beta)) 
# score vector
score.vector <- t(xmat)%*%(y - plgtb); score.vector
# direction vector
direction.vector <- var_cov%*%score.vector; direction.vector
# updated starting value
update <- cbind(stval) + direction.vector; update

#' ## Second iteration

# Optimize by log-likelihood
logit.result <- optim(update, llk.logit, method="BFGS", 
                      control=list(maxit=0, trace=1), hessian=TRUE, y=y, x=x)

# Printing Optimization results #
# beta paramter estimates
parm_est <- logit.result$par; parm_est 
# variance covariance matrix
var_cov <- solve(logit.result$hessian); var_cov
# Standard error of beta estimates
std_err <- sqrt(diag(var_cov)); std_err
# Log-likelihood
log_like <- -logit.result$value; log_like
# Deviance
dev <- -2*(log_like - 0); dev

# Find new starting value
beta <- cbind(parm_est) # Store paramete estiamtes
plgtb <- 1/(1 + exp(-xmat%*%beta)) 
# score vector
score.vector <- t(xmat)%*%(y - plgtb); score.vector
# direction vector
direction.vector <- var_cov%*%score.vector; direction.vector
# updated starting value
update <- cbind(update) + direction.vector; update

#' ## Third iteration

# Optimize by log-likelihood
logit.result <- optim(update, llk.logit, method="BFGS", 
                      control=list(maxit=0, trace=1), hessian=TRUE, y=y, x=x)

# Printing Optimization results #
# beta paramter estimates
parm_est <- logit.result$par; parm_est 
# variance covariance matrix
var_cov <- solve(logit.result$hessian); var_cov
# Standard error of beta estimates
std_err <- sqrt(diag(var_cov)); std_err
# Log-likelihood
log_like <- -logit.result$value; log_like
# Deviance
dev <- -2*(log_like - 0); dev

# Find new starting value
beta <- cbind(parm_est) # Store paramete estiamtes
plgtb <- 1/(1 + exp(-xmat%*%beta)) 
# score vector
score.vector <- t(xmat)%*%(y - plgtb); score.vector
# direction vector
direction.vector <- var_cov%*%score.vector; direction.vector
# updated starting value
update <- cbind(update) + direction.vector; update

#' ## Fourth iteration

# Optimize by log-likelihood
logit.result <- optim(update, llk.logit, method="BFGS", 
                      control=list(maxit=0, trace=1), hessian=TRUE, y=y, x=x)

# Printing Optimization results #
# beta paramter estimates
parm_est <- logit.result$par; parm_est 
# variance covariance matrix
var_cov <- solve(logit.result$hessian); var_cov
# Standard error of beta estimates
std_err <- sqrt(diag(var_cov)); std_err
# Log-likelihood
log_like <- -logit.result$value; log_like
# Deviance
dev <- -2*(log_like - 0); dev

# Find new starting value
beta <- cbind(parm_est) # Store paramete estiamtes
plgtb <- 1/(1 + exp(-xmat%*%beta)) 
# score vector
score.vector <- t(xmat)%*%(y - plgtb); score.vector
# direction vector
direction.vector <- var_cov%*%score.vector; direction.vector
# updated starting value
update <- cbind(update) + direction.vector; update

#' ## Fifth iteration

# Optimize by log-likelihood
logit.result <- optim(update, llk.logit, method="BFGS", 
                      control=list(maxit=0, trace=1), hessian=TRUE, y=y, x=x)

# Printing Optimization results #
# beta paramter estimates
parm_est <- logit.result$par; parm_est 
# variance covariance matrix
var_cov <- solve(logit.result$hessian); var_cov
# Standard error of beta estimates
std_err <- sqrt(diag(var_cov)); std_err
# Log-likelihood
log_like <- -logit.result$value; log_like
# Deviance
dev <- -2*(log_like - 0); dev

# Find new starting value
beta <- cbind(parm_est) # Store paramete estiamtes
plgtb <- 1/(1 + exp(-xmat%*%beta)) 
# score vector
score.vector <- t(xmat)%*%(y - plgtb); score.vector
# direction vector
direction.vector <- var_cov%*%score.vector; direction.vector
# updated starting value
update <- cbind(update) + direction.vector; update

#' ## Compare manual and automatic results

# Fit Prediction
prediction_manual <- ilogit(parm_est[1] + parm_est[2]*x)

# Compare predictions
p + geom_line(aes(y=prediction, color="Auto"), size=1) + 
  geom_line(aes(y=prediction_manual, color="Manual"), size=1) + 
  scale_color_discrete(name="Prediction")
  
#' # Workshop question
#' 
#' Fit logistic regression that predicts Trump victory by 
#' 2008 McCain vote share and 2012 Romney vote share (i.e., two IVs) in California.
#' Optimize by both automatic and manual methods and compare results.


#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
rmarkdown::render('TA_session_041119.R', 'pdf_document')
rmarkdown::render('TA_session_041119.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_041119.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_041119.R', 'pdf_document')"
