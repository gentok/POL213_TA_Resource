#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "April 25, 2019"
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
library(pscl) # For pseudo R squared (pR2)
library(DAMisc) # For pre function
library(readstata13) # For importing data
library(MASS) # For mvrnorm
library(Zelig) # zelig function

#'
#' # Study of Religious Message and Participation in Kenya
#' 
#' Check the paper [HERE](https://www.journals.uchicago.edu/doi/full/10.1086/682717).
#' 
#' Their Replication Data are [HERE](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/7KSNCE).
#'

#+ eval=FALSE
# install.packages("dataverse") # Only Once
library(dataverse)
serverset <- "dataverse.harvard.edu"

(meta <- get_dataset("doi:10.7910/DVN/7KSNCE", server=serverset))

# Get Stata Do File
writeBin(get_file("McClendonRiedl_religionasstimulant.do","doi:10.7910/DVN/7KSNCE",
                  server=serverset), "McClendonRiedl_religionasstimulant.do")
# Get Data
writeBin(get_file("religionasstimulant.tab","doi:10.7910/DVN/7KSNCE",
                  server=serverset), "religionasstimulant.dta")

#+
# Import Data
d <- read.dta13("religionasstimulant.dta", convert.factors = FALSE)
# Variables
summary(d)

#+
#'
#' # Recoding of Variables
#' 

#*VARIABLE CODING FOR PARTICIPATION PAPER

# Religious, Non-Selfaffirming Treatment
d$rsi = 0
d$rsi[d$treatment==1] = 1

# Secular, Non-Selfaffirming Treatment 
d$ssi = 0
d$ssi[d$treatment==2] = 1

# Religious, Selfaffirming (Prosperity) Treatment
d$rpm = 0
d$rpm[d$treatment==3] = 1

# Secular, Selfaffirming (Prosperity) Treatment
d$spm = 0
d$spm[d$treatment==4] = 1

# Religious Treatment Summary (Religious=1, Secular=0)
d$religioustreatment = 0
d$religioustreatment[d$rsi==1 | d$rpm==1] = 1

# Self-affirming Treatment Summary (Self-affirming=1, Not=0)
d$prosperitytreatment = 0
d$prosperitytreatment[d$rpm==1 | d$spm==1] = 1

# Age
d$age = 2014 - d$birthyear

# Christian =1 or Not =0 
d$christian = 1
d$christian[d$religion %in% c(5,6,7,8)] = 0
summary(d$christian)

# Obtained Seconday Education = 1, Not = 0
d$secondaryed = 0
d$secondaryed[d$education > 10] = 1

# Owning Car, Television, or Motorcycle (Have = 1, Not = 0)
d$car = ifelse(d$ownscar==1,1,0)
d$television = ifelse(d$ownstv==1,1,0)
d$motorcycle = ifelse(d$ownsmotorcycle==1,1,0)

# Female = 1, Male = 0
d$female = ifelse(d$gender==2,1,0)

# Denomination (Belong to the denomination = 1, Not = 0)
d$catholic <- ifelse(d$religion==2,1,0)
d$pentecostal <- ifelse(d$religion==1,1,0)
d$tradchristian <- ifelse(d$religion %in% c(2,3),1,0)
#**above includes mainline protestant and catholic
d$pentecostal2 <- ifelse(d$religion %in% c(1,4),1,0)
#** above includes pentecostal and the category "other" (likely charismatic or renewalist)

# Marital Status (Currently Married)
d$married <- ifelse(d$marital==2,1,0)
# Marital Status (Ever Married)
d$evermarried <- ifelse(d$marital %in% c(2,3),1,0)

# Native Language (Speak that language=1, Not=0)
d$kalenjin = ifelse(d$nativelanguage==12,1,0)
d$luhya = ifelse(d$nativelanguage==3,1,0)
d$kamba = ifelse(d$nativelanguage==5,1,0)
d$kisii = ifelse(d$nativelanguage==4,1,0)
d$kikuyu = ifelse(d$nativelanguage==1,1,0)
d$luo = ifelse(d$nativelanguage==2,1,0)
d$other = ifelse(d$nativelanguage%in%c(22,6,7,14,8),1,0)

# Kept Amount % in Dictator Game
table(d$dictatorkept)

# DV1: Sending SMS Message (1=Yes, 0=No)
table(d$sentsms)

# DV2: Took Pamphlet (1=Yes, 0=No)
table(d$tookpamphlet)

# DV3: Intention to Joing Youth Group or Community Forum
d$joingroup=0
d$joingroup[d$youthagendajoin==1 & d$communityforumjoin==1] = 1
table(d$joingroup)

# Number of Previous Experiment Participation
d$beforeattend[d$beforeattend<0] <- NA
table(d$beforeattend)

# Drop Those Who Attended Too Many Times Previously
d <- d[d$beforeattend %in% c(0,1,2,3),]

#'
#' # Run Logistic Regression
#'

#' ## 1. Run Logit Model using glm function
#' 
#' DV is Sending SMS, Treatments are religious message and self-affirming message
#' Think about how you can capture all treatments
#' 
#' * Cecular, Non-Self-Affirming as Reference Category
m1 <- glm(sentsms ~ spm + rsi + rpm, data = d, family=binomial("logit"))
summary(m1)

#' * Another way of doing the same thing (You see that coefficients are identical)
m1x <- glm(sentsms ~ prosperitytreatment*religioustreatment, 
          data = d, family=binomial("logit"))
summary(m1x)

#' ## 2. Calculate Odds Ratio for Treatment Variables.
#' 
#' ### 2.1 Generate Odds Ratio
#' 
#' * Following codes produce identical results

exp(m1$coefficients) # OR
exp(summary(m1)$coefficients[,1]) # OR
exp(coef(summary(m1))[,1])

#' ### 2.2. Generate Odds Ratio with Confidence Intervals
#' 
#' * From Scott's Code
logit.or <- function(model) {
  logit.coeffs <- coef(summary(model))
  odds.ratio <- exp(logit.coeffs[ ,1])
  lci <- exp(logit.coeffs[ ,1] - 1.96 * logit.coeffs[ ,2])
  uci <- exp(logit.coeffs[ ,1] + 1.96 * logit.coeffs[ ,2])
  logit.or <- cbind(odds.ratio, lci, uci)
  logit.or
}

logit.or(m1)

#' * Alternatively...

m1cf <- as.data.frame(summary(m1)$coefficients)
names(m1cf) <-c("est","se","z","p")

data.frame(odds.ratio=exp(m1cf$est),
           lci = exp(m1cf$est - pnorm(0.975) * m1cf$se),
           uci = exp(m1cf$est + pnorm(0.975) * m1cf$se),
           row.names = row.names(m1cf))

#' ### 2.3. Interpret the meaning!
#'
#' * Make Numbers Easier to interpret 

(exp(coef(m1))-1) * 100

#' Those who heard secular & self-affirming message are 35% (1.35 times) more likely;
#' those who heard religious & non-self-affirming message are 5% (1.05 times) more likely;
#' those who heard religious & self-affirming message are 50% (1.5 times) more likely than 
#' those who heard secular & non-self-affirming message to send SMS.   

#' ## 3. Export Pseudo R Square 
#' 
#' ### 3.1. Use pR2 Function

round(pR2(m1),4) # all very low

#'
#' ### 3.2. McKelvey-Zavonia pseudo-R2 (manually)

yhat.m1 <- predict(m1, type="response")
round(mckR2.m1 <- var(yhat.m1) / (var(yhat.m1) + (pi^2/3)),5)

#'
#' ## 4. Obtain Classification Table
#' 
#' * Check value...
summary(yhat.m1) # No Value Over .39
pred.m1 <- (yhat.m1 > .35)*1 # Let's Make .35 a split point

#' * Deal with missing value
dx <- na.omit(d[,all.vars(m1$formula)])
classtab <- data.frame(response = dx$sentsms, predicted = pred.m1)

#' * Alternatively... (Not using new data) 
classtab <- data.frame(response = m1$model$sentsms, predicted = pred.m1)

#' * Result
xtabs(~ predicted + response, data = classtab)

#'
#' ## 5. Proportional Reduction in Error (PRE)

pre(m1, sim=TRUE, R=1000)

#' The reduction in error is effectively zero.

#'
#' ## 6. Generate ROC Curve
#' 
#' * Generalize Scott's Code

roc.curve=function(s,m,print=FALSE){
  # Predicted Probabilities
  Ps=(predict(m, type="response")>s)*1
  # False Positive
  FP=sum((Ps==1)*(m$model[,1]==0))/sum(m$model[,1]==0)
  # True Positive
  TP=sum((Ps==1)*(m$model[,1]==1))/sum(m$model[,1]==1)
  # Print Table
  if(print==TRUE){
    print(table(Observed=m$model[,1],Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

#' * Test Function
threshold = 0.35
roc.curve(threshold,m1,print=TRUE)

#' * Make roc.curve function applicable to vector of threshold
ROC.curve=Vectorize(roc.curve, "s")

#' * Plot ROC curve
M.ROC=ROC.curve(seq(0,1,by=.01),m1)
plot(M.ROC[1,], M.ROC[2,], col="grey", lwd=2, type="l", xlab="1 - Specificity" ,ylab="Sensitivity")
# Add 45 degrees line
abline(0, 1, col="black", lty=2)

#' * Use ggplot2

# Create Data
rocdt <- data.frame(probs = seq(0, 1, by=.01))
roc <- ROC.curve(rocdt$probs, m1)
rocdt$fpr <- roc["FPR",]
rocdt$tpr <- roc["TPR",]

# Plot
ggplot(rocdt, aes(x=fpr,y=tpr)) + 
  geom_line(aes(y=fpr),linetype=2) + 
  geom_line() + 
  xlab("1 - Specificity") + ylab("Sensitivity") +
  theme_bw()

#'
#' # Assess Moderation of Treatment
#'
#' ## 1. Choose One of Demographic/Attitudinal Variables and Interact with Treatment

m2 <- glm(sentsms ~ spm*pentecostal2 + rsi*pentecostal2 + rpm*pentecostal2, 
          data = d, family=binomial("logit"))
summary(m2)

#'
#' ## 2. Calculate Adjusted McFadden R2 and compare it with the first model
#' 
#' * Check that dimention of m1 and m2 are the same.
dim(m1$model)==dim(m2$model)
#' * Run NULL Model
m0 <- glm(sentsms ~ 1, data= m1$model, family = binomial(link="logit"))
summary(m0)
#' * Calculate Adjusted Statistics

L.m1 <- logLik(m1)
L.m2 <- logLik(m2)
L.m0 <- logLik(m0)
P <- attr(L.m1, "df")

McFadden.R2.m1 <- 1 - (L.m1 / L.m0); McFadden.R2.m1
McFadden.Adj.R2.m1 <- 1 - ((L.m1 - P) / L.m0); McFadden.Adj.R2.m1

McFadden.R2.m2 <- 1 - (L.m2 / L.m0); McFadden.R2.m2
McFadden.Adj.R2.m2 <- 1 - ((L.m2 - P) / L.m0); McFadden.Adj.R2.m2

#'
#' Both have negative value, implying that those models are 
#' probably not good. Model 2 has slightly less negative value, implying that 
#' adding moderators might worth.
#' 
#' ## 3. Generate Several Profiles of Interest
#' 

# Check what coefficients exist.
names(m2$coefficients)

# Secular & Non-Self-Affirming Treatment & Non-Pentecostal
profile1 <- c(1, 0, 0, 0, 0, 0, 0, 0)
# Religious & Self-Affirming Treatment & Non-Pentecostal
profile2 <- c(1, 0, 0, 0, 1, 0, 0, 0)
# Secular & Non-Self-Affirming Treatment & Pentecostal
profile3 <- c(1, 0, 1, 0, 0, 0, 0, 0)
# Religious & Self-Affirming Treatment & Pentecostal
profile4 <- c(1, 0, 1, 0, 1, 0, 0, 1)

#' ## 4. Assess First Differences in Treatment Effect, Conditional on Moderator Values
#' 
#' ### Manually...

# Draw coefficients randomly from Multivariate Normal Distribution
ndraws <- 1000
betadraw <- mvrnorm(ndraws, coef(m2), vcov(m2))

# Generalize Scott's Code into function

logisprob <- function(profile,betadraw) {
  
  profile_beta <- betadraw %*% profile
  profile_prob <- exp(profile_beta) / (1 + exp(profile_beta))
  
  meanprob <- mean(profile_prob)
  sdprob <- apply(profile_prob, 2, sd)
  qtprob <- apply(profile_prob, 2, quantile, probs=c(0.025,0.5,0.975))
  
  res <- c(meanprob,sdprob,qtprob)
  names(res) <- c("mean","se","per025","per50","per975")
  return(res)
  
}

# Generate Predictions
pred <- sapply(list(profile1,profile2,profile3,profile4),
               logisprob, betadraw=betadraw)
preddt <- as.data.frame(t(pred))
preddt$tr <- rep(c("Secular & \nNon-Self-Affirming",
                   "Religious & \nSelf-Affirming"),2)
preddt$tr <- factor(preddt$tr,levels=unique(preddt$tr))
preddt$mod <- rep(c("Not Pentecostal","Pentecostal"), each=2)
preddt$mod <- factor(preddt$mod,levels=unique(preddt$mod))

ggplot(preddt, aes(x=tr,y=mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=per025,ymax=per975),width=0.3) +
  xlab("Treatment") + ylab("Predicted Probability") +
  facet_grid(.~mod) + theme_bw()

#' Being Pentecostal (or other "prosperity" faction of the Christianity) 
#' does moderate the treatment effect. Those who are Pentecostal are much more 
#' likely to respond to Religious & Self-Affirming Treatment.
#' 
#' 
#' ### Use zelig...

# Model
z.out <- zelig(m2$formula, model="logit", data=m2$model)

# Profiles
x.prof1 <- setx(z.out, rsi=0, spm=0, rpm=0, pentecostal2=0)
x.prof2 <- setx(z.out, rsi=0, spm=0, rpm=1, pentecostal2=0)
x.prof3 <- setx(z.out, rsi=0, spm=0, rpm=0, pentecostal2=1)
x.prof4 <- setx(z.out, rsi=0, spm=0, rpm=1, pentecostal2=1)

# Simulated Results
s.out1 <- sim(z.out, x = x.prof1)
s.out2 <- sim(z.out, x = x.prof2)
s.out3 <- sim(z.out, x = x.prof3)
s.out4 <- sim(z.out, x = x.prof4)

# Plot
plot(s.out1)
plot(s.out2)
plot(s.out3)
plot(s.out4)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_042519_ws.R', 'pdf_document')
# rmarkdown::render('TA_session_042519_ws.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_042519_ws.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_042519_ws.R', 'pdf_document')"
