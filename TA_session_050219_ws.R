#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "May 2, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required packages
library(readstata13) # For importing data
library(ggplot2) # Plotting
library(faraway) # for ilogit function
library(pscl) # For pseudo R squared (pR2)
library(DAMisc) # For pre function
library(MASS) # For mvrnorm

#'
#' # Study of Social Protest and Immigration Attitudes
#' 
#' Check the paper [HERE](https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12159).
#' 
#' Their Replication Data are [HERE](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/27113).
#'

#+ eval=FALSE
# install.packages("dataverse") # Only Once
library(dataverse)
serverset <- "dataverse.harvard.edu"

(meta <- get_dataset("doi:10.7910/DVN/27113", server=serverset))

# Get Stata Do File
writeBin(get_file("ajps_replication.do","doi:10.7910/DVN/27113",
                  server=serverset), "ajps_replication.do")
# Get Data
writeBin(get_file("ajps2_replication.tab","doi:10.7910/DVN/27113",
                  server=serverset), "ajps2_replication.dta")

#+
# Import Data
d <- read.dta13("ajps2_replication.dta", convert.factors = FALSE)
# Variables
summary(d)
# Description of Variables
paste(names(d), attr(d, "var.labels"), sep=": ")

#' # Descriptives

# DV
table(d$immpolinew)
# (1) immediate legalization of current undocumented immigrants, 
# (2) a guest worker program leading to legalization eventually, 
# (3) a guest worker program permitting immigrants to be in the country, but only temporarily, 
# (4) an effort to seal or close off the border to stop illegal immigration, and 
# (5) none of the above

# IV 
table(d$protest_period)
# coded 1 if the respondent is surveyed after the protests began and 0 otherwise

# Controls

summary(d$pprhispx)
#Pct. Hispanic population by GEOFIPS

summary(d$ppehhscx)
#Pct. of 25+ year olds with high school diploma (or equivalency) by GEOFIP

d$latcomm <- as.numeric(as.factor(d$latcomm))
table(d$latcomm)
# Perceived Commonality with Latino
# 1 Nothing, 2 Little, 3 Some, 4 Lot

table(d$generation)
# ranges from 0 to 4, where 0 reflects noncitizen, 
# 1 reflects foreign‐born citizen, 
# 2 reflects second generation, 
# 3 reflects third generation, and 
# 4 reflects fourth‐plus generation.

table(d$american, d$national_origin)
# Identify as American / National Origin
# Both 0 implies that identifying as Latino

table(d$language_skills)
# Higher score indicates higher skill

table(d$knowledge)
# Higher score indicates higher knowledge

table(d$catholic)
# Catholic

table(d$attend_church)
# Church Attendance (Not Clear How It's Coded)

table(d$community_participate)
# a respondent is involved in a civic organization# 

table(d$cuba)
# Cuba Origin

table(d$pr)
# Puerto Rico Origin

table(d$dr)
# Dominican Republic Origin

table(d$south)
# South America Origin

table(d$central)
# Central America Origin

table(d$age)
# Age

table(d$female)
# Female

d$edu <- as.numeric(as.factor(d$edu))
table(d$edu)
# Education 
# 1= None to 8=graduate or professional degree

# Income Variables
table(d$incomeq_dummy1) # No Report
table(d$incomeq_dummy3) # Second Quartile
table(d$incomeq_dummy4) # Third Quartile
table(d$incomeq_dummy5) # Fourth Quartile
# Firt Quartile is the Reference Category

table(d$perfin)
# Financial Situation
# (1) gotten worse, (2) stays about the same, and (3) gotten better.

table(d$samplestate1) # AR
table(d$samplestate2) # AZ
table(d$samplestate3) # CA
table(d$samplestate4) # CO
table(d$samplestate5) # FL
table(d$samplestate6) # GA
table(d$samplestate7) # IA 
table(d$samplestate8) # IL
table(d$samplestate9) # MD
table(d$samplestate10) # NC
table(d$samplestate11) # NJ
table(d$samplestate12) # NM
table(d$samplestate13) # NV
table(d$samplestate14) # NY
table(d$samplestate15) # TX
table(d$samplestate16) # VA
# Residing States

# Other Variables

# Weight Variable
summary(d$wt_nation_rev)

# County Code
summary(d$metro_county_code)

#'
#' # Use multinom function
#'
library(nnet)
#'
#' ## 1.1. Use multinom function (in nnet package) to estimate multinomial logit model
#' with immigration preference as DV and protest exposure as IV. 
#' 

# Make DV a factor (to make pre function work)
d$immpolinew <- as.factor(d$immpolinew)

m1 <- multinom(immpolinew ~ protest_period, data=d, Hess=TRUE)
summary(m1)

#' 
#' ## 1.1.1. Find Odds Ratio for the exposure variables estimated in 1.1. 
#' Make interpretation
#'    

exp(coef(m1))

#' The odds ratio explanation of the first coefficient implies that 
#' those who are exposed to protest makes the choiceof 2 (lenient guest worker program) 
#' over 1 (full legalization) 1.2 times more likely than those 
#' who are not exposed. 
#' 
#' ## 1.1.2. Calculate Wald Statistics (z-score) for the coefficients in 1.1. and
#' generate p-values. Are the coefficients significantly different from zero?

z <- summary(m1)$coefficients / summary(m1)$standard.errors
round(z, 3)

p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, 3)

#'
#' Only the first and fourth coefficients for exposure variable are 
#' significantly different from zero.
#' 
#'
#' ## 1.2. Obtain Fitted Values (Predicted Probabilities) from the model estimated in 1.1
#'

yhat.m1 <- predict(m1, typ = "prob")
summary(yhat.m1)

#'
#' ## 1.3. Manually calculate predicted probabilities of each preference for 
#' those who are exposed to protest and those who are not.
#'

# Not Exposed (all + coef(m1)[?,?]*0 parts can be omitted)
(p1_0 <- 1 / (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*0))))
(p2_0 <- exp(coef(m1)[1,1] + coef(m1)[1,2]*0) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*0))))
(p3_0 <- exp(coef(m1)[2,1] + coef(m1)[2,2]*0) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*0))))
(p4_0 <- exp(coef(m1)[3,1] + coef(m1)[3,2]*0) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*0))))
(p5_0 <- exp(coef(m1)[4,1] + coef(m1)[4,2]*0) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*0))))
# Check that probability sums to 1
p1_0 + p2_0 + p3_0 + p4_0 + p5_0

# Exposed
(p1_1 <- 1 / (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*1))))
(p2_1 <- exp(coef(m1)[1,1] + coef(m1)[1,2]*1) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*1))))
(p3_1 <- exp(coef(m1)[2,1] + coef(m1)[2,2]*1) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*1))))
(p4_1 <- exp(coef(m1)[3,1] + coef(m1)[3,2]*1) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*1))))
(p5_1 <- exp(coef(m1)[4,1] + coef(m1)[4,2]*1) / 
    (1 + sum(exp(coef(m1)[,1] + coef(m1)[,2]*1))))
# Check that probability sums to 1
p1_1 + p2_1 + p3_1 + p4_1 + p5_1

#'
#' ## 1.4. Add as many control variables as you want to the model in 1.1 and 
#' estimate a new model. Does the addition of control variables change results?
#'

m2 <- multinom(immpolinew ~ protest_period + pprhispx + ppehhscx + 
                 latcomm + generation + american + national_origin + 
                 language_skills + knowledge + catholic + community_participate + 
                 attend_church + cuba + pr + dr + south + central + 
                 age + female + edu + incomeq_dummy1 + incomeq_dummy3 + 
                 incomeq_dummy4 + incomeq_dummy5 + perfin,
               data=d, Hess=TRUE)
summary(m2)

#'
#' ## 1.5. Find Pseudo R Squares, Proportional Reduction in Error (PRE) and 
#' infomration measures (i.e., AIC, BIC) for BOTH models estimated in 1.1 and 1.4. 
#' Compare results. Which model explains DV better?
#'

# Pseudo-R2
m1_pR2 <- pR2(m1)
m2_pR2 <- pR2(m2)
rbind(m1=round(m1_pR2, 5),
      m2=round(m2_pR2, 5))

# PRE (Getting Errors) 
# Taking Too Much Time, so reducing iteration here
pre(m1, sim=TRUE, R=100)
pre(m2, sim=TRUE, R=100)

# Information Measures
AIC(m1)
AIC(m2)
BIC(m1)
BIC(m2)

#'
#' Model 2 obviously performs better than the model 1.
#'
#' ## 1.6. Using model estimated in 1.4, create several profiles of interests and 
#' simulate the Predicted Probability with Confidence Interval. Plot Results.
#'

# Create Profiles
# 1. Treated
profile1 <- c(1, 1, median(d$pprhispx,na.rm=TRUE), 
              median(d$ppehhscx,na.rm=TRUE), 
              median(d$latcomm,na.rm=TRUE),
              median(d$generation,na.rm=TRUE), 0, 0, 
              median(d$language_skills,na.rm=TRUE), 
              median(d$knowledge,na.rm=TRUE), 
              median(d$catholic,na.rm=TRUE), 
              median(d$community_participate,na.rm=TRUE),
              median(d$attend_church,na.rm=TRUE), 0, 0, 0, 0, 0, 
              median(d$age,na.rm=TRUE), median(d$female,na.rm=TRUE),
              median(d$edu,na.rm=TRUE), 0, 0, 0, 0, median(d$perfin,na.rm=TRUE))
# Not Treated
profile0 <- c(1, 0, median(d$pprhispx,na.rm=TRUE), 
              median(d$ppehhscx,na.rm=TRUE), 
              median(d$latcomm,na.rm=TRUE),
              median(d$generation,na.rm=TRUE), 0, 0, 
              median(d$language_skills,na.rm=TRUE), 
              median(d$knowledge,na.rm=TRUE), 
              median(d$catholic,na.rm=TRUE), median(d$community_participate,na.rm=TRUE),
              median(d$attend_church,na.rm=TRUE), 0, 0, 0, 0, 0, 
              median(d$age,na.rm=TRUE), median(d$female,na.rm=TRUE),
              median(d$edu,na.rm=TRUE), 0, 0, 0, 0, median(d$perfin,na.rm=TRUE))

# Function for Prediction
predictmlogit <- function(m2, profile) {
  
  coeffs1 <- summary(m2)$coefficients
  coeffs <- cbind(t(coeffs1[1, ]), t(coeffs1[2, ]), 
                  t(coeffs1[3, ]), t(coeffs1[4, ]))
  covmat <- solve(m2$Hessian)
  
  ndraws <- 1000
  betadraw <- mvrnorm(ndraws, coeffs, covmat)
  
  nvars <- ncol(coeffs1)
  
  xb2 <- betadraw[ ,1:nvars]%*%profile
  xb3 <- betadraw[ ,(nvars+1):(2*nvars)]%*%profile
  xb4 <- betadraw[ ,(2*nvars+1):(3*nvars)]%*%profile
  xb5 <- betadraw[ ,(3*nvars+1):ncol(betadraw)]%*%profile
  
  prob1 <- exp(0) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4) + exp(xb5))
  prob2 <- exp(xb2) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4) + exp(xb5))
  prob3 <- exp(xb3) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4) + exp(xb5))
  prob4 <- exp(xb4) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4) + exp(xb5))
  prob5 <- exp(xb5) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4) + exp(xb5))
  
  means <- cbind(mean(prob1), mean(prob2), mean(prob3), 
                 mean(prob4), mean(prob5))
  sds <- cbind(apply(prob1, 2, sd), apply(prob2, 2, sd), apply(prob3, 2, sd), 
               apply(prob4, 2, sd), apply(prob5, 2, sd))
  lci <- cbind(quantile(prob1, probs=0.025),quantile(prob2, probs=0.025),
               quantile(prob3, probs=0.025),quantile(prob4, probs=0.025),
               quantile(prob5, probs=0.025))
  uci <- cbind(quantile(prob1, probs=0.975),quantile(prob2, probs=0.975),
               quantile(prob3, probs=0.975),quantile(prob4, probs=0.975),
               quantile(prob5, probs=0.975))
  zs <- means / sds
  ps <- 2 * (1 - pnorm(abs(zs)))
  presults <- t(rbind(means, sds, lci, uci, zs, ps))
  presults <- as.data.frame(presults)
  colnames(presults) <- c("mean","sd","lci","uci","z","p")
  presults$choice <- c("Full \nLegalization",
                       "Lenient Guest \nWorker Program",
                       "Temporal Guest \nWorker Program",
                       "Not Allow", "None of \nthe Above")
  presults$choice <- factor(presults$choice, levels=presults$choice)
  return(presults)
}  

# Make Prediction
(pred1 <- predictmlogit(m2, profile1))
(pred0 <- predictmlogit(m2, profile0))

# Plot Prediction
preddt <- rbind(pred1,pred0)
preddt$treatment <- rep(c("After Protest","Before Protest"), each=5)
preddt$treatment <- factor(preddt$treatment, levels=c("Before Protest",
                                                      "After Protest"))
# Plot
ggplot(preddt, aes(x=choice, y=mean)) + 
  geom_point(aes(shape=treatment), position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=lci,ymax=uci, linetype=treatment), 
                position=position_dodge(width=0.5), width=0.3) + 
  theme_bw() + xlab(NULL) + ylab("Predicted Probability") + 
  labs(caption="Other variables are fixed at median.") + 
  theme(legend.position="bottom")

#' 
#' # 2. Use mlogit function
#' 
library(mlogit)
#'
#' ## 2.1. Use mlogit function from mlogit package to estimate the same model 
#' as in 1.4. Does it yield the same results?
#'

# Create Long Data
d.mlogit <- mlogit.data(d, shape = "wide", choice = "immpolinew")
head(d.mlogit[,1:5],10)


# Replicate Mlogit Model of Immigration Policy Preference
m3 <- mlogit(immpolinew ~ 0 | protest_period + pprhispx + ppehhscx + 
               latcomm + generation + american + national_origin + 
               language_skills + knowledge + catholic + community_participate + 
               attend_church + cuba + pr + dr + south + central + 
               age + female + edu + incomeq_dummy1 + incomeq_dummy3 + 
               incomeq_dummy4 + incomeq_dummy5 + perfin, data=d.mlogit)
summary(m3)

# Coefficients Look Mostly the Same, but slightly different
comp <- cbind(as.numeric(coef(m2)), coef(m3), 
              abs(as.numeric(coef(m2))-coef(m3)))
colnames(comp) <- c("multinom","mlogit","difference")
round(comp,3)


#'
#' ## 2.2. Create some choice level variable in the dataset (i.e., some 
#' combination of individual level characteristics and choice characteristics). 
#' Run the new model with choice level variable.
#' 

# commonality high (3,4) * Immigration Yes (1,2,3) as 1 
# commonality low  (1,2) * Immigration No (4) as 1
# 0 Otherwise
commandimm.1 <- ifelse(d$latcomm%in%c(3,4),1,0)  
commandimm.2 <- ifelse(d$latcomm%in%c(3,4),1,0)
commandimm.3 <- ifelse(d$latcomm%in%c(3,4),1,0)
commandimm.4 <- ifelse(d$latcomm%in%c(1,2),1,0)
commandimm.5 <- rep(0, nrow(d))

d2 <- cbind(commandimm.1,commandimm.2,
            commandimm.3,commandimm.4,
            commandimm.5, d)

# Create Long Data
d2.mlogit <- mlogit.data(d2, shape = "wide", varying=1:5, choice = "immpolinew")
head(d2.mlogit$commandimm,10)

# Replicate Mlogit Model of Immigration Policy Preference
m4 <- mlogit(immpolinew ~ commandimm | protest_period + pprhispx + ppehhscx + 
               latcomm + generation + american + national_origin + 
               language_skills + knowledge + catholic + community_participate + 
               attend_church + cuba + pr + dr + south + central + 
               age + female + edu + incomeq_dummy1 + incomeq_dummy3 + 
               incomeq_dummy4 + incomeq_dummy5 + perfin, data=d2.mlogit)
summary(m4)


#'
#' # 3. Use Zelig function
#' 
library(Zelig) # zelig function
library(ZeligChoice) # zelig function to do multinomial logit
#'
#' ## 3.1. Use Zelig function to estimate the same model as in 1.4. 
#'

m5 <- zelig(as.factor(immpolinew) ~ protest_period + pprhispx + ppehhscx + 
               latcomm + generation + american + national_origin + 
               language_skills + knowledge + catholic + community_participate + 
               attend_church + cuba + pr + dr + south + central + 
               age + female + edu + incomeq_dummy1 + incomeq_dummy3 + 
               incomeq_dummy4 + incomeq_dummy5 + perfin, 
               model="mlogit", data=d)
# Reference Group is Level 5 (Not Level 1)
summary(m5)

#' 
#' ## 3.2. Export predicted probabilities for several profiles.
#'

# Other Variables are unspecified (Set to Default)

# Treated
x1 <- setx(m5, protest_period=1)
s1.out <- sim(m5, x = x1)
summary(s1.out)
plot(s1.out)

# Untreated
x2 <- setx(m5, protest_period=0)
s2.out <- sim(m5, x = x2)
summary(s2.out)
plot(s2.out)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_050219_ws.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('TA_session_050219_ws.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_050219_ws.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_050219_ws.R', 'pdf_document')"
