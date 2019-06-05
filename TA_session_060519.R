#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "JUne 5, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required packages
library(ggplot2) # Plotting
library(faraway) # for ilogit function
library(pscl) # For pseudo R squared (pR2)
library(DAMisc) # For pre function
library(MASS) # For mvrnorm & glm.nb

#'
#' ## Data Preparation (Bill Data Described in Session 7)
#' 

## Read in data
d <- read.table("bills_93_110.txt", header=TRUE, sep="\t")
# Subset Columns
d <- d[,c("all_bills","all_aic","seniority","majority","chair","subchr",
          "female","latino","meddist","votepct")]
# Rename Columns
colnames(d) <- c("bills", "action", "seniority", "majority", "chair", "subchair", 
                 "female", "latino", "distance", "margin")

## Histogram of DV
ggplot(d, aes(action)) + geom_histogram(color="white") + 
  theme_bw() + ylab(NULL) + 
  xlab("# Bills Receiving Action in Committee") 

## Set Model Formula
fm <- formula(action ~ seniority + majority + chair + subchair + 
                female + latino + margin + distance)

## Store Independent Variables Meaningful Labels (with Intercept)
vn <- c("(Intercept)", "Seniority", "Majority Party", 
        "Committee Chair", "Committee Sub-Chair",
        "Female","Latino","Ideological Distance","Vote Margin")

#'
#' ## Run Poisson & Negative Binomial Model
#'

# Poisson
m1 <- glm(fm, data=d, family=poisson)
summary(m1)

# Negative Binomial
m2 <- glm.nb(fm, data = d)
summary(m2)

#'
#' ## Coefficient Table
#'

require(texreg)

# Exporting to R Console
screenreg(list(m1,m2), 
          digits = 3, single.row = TRUE,
          custom.coef.names = vn,
          custom.model.names = c("Poisson","Negative Binomial"))

# Write as Word Document
htmlreg(list(m1,m2), 
          digits = 3, single.row = TRUE,
          custom.coef.names = vn,
          custom.model.names = c("Poisson","Negative Binomial"),
        file = "countrestab.doc")

#'
#' ## Model Fit
#'
#' * Log-likelihood Ratio Test
#' 

# Null Models
m0p <- glm(action ~ 1, data = d, family = poisson)
m0nb <- glm.nb(action ~ 1, data = d)

require(lmtest)
lrtest(m1, m0p) # Poisson
lrtest(m2, m0nb) # Negative Binomial

#'
#' * Pseudo R^2
#'

pR2(m1)
pR2(m2)

# Adjusted McFadden
1 - ((logLik(m1) - attr(logLik(m1),"df")) / logLik(m0p)) # Poisson
1 - ((logLik(m2) - attr(logLik(m2),"df")) / logLik(m0nb)) # Negative Binomial

#'
#' * Information criteria
#' 

AIC(m1)
AIC(m2)

BIC(m1)
BIC(m2)

#'
#' ## Assession Over-dispersion
#' 
#' ### Poisson
#' 

# Goodness of Fit Based Test 
# (Significant Result Indicates Over-dispersion)
round(1 - pchisq(deviance(m1), m1$df.residual),4)

# Pearson chi-square and dispersion (Dispersion > 1 indicates over-dispersion)
(valchisq <- sum(residuals(m1, typ = "pearson")^2)) # Chi-squared
valchisq/m1$df.residual # Dispersion
# Use Hilbe's canned routine
require(COUNT)
P__disp(m1)

# Score test (Significant coefficient implies Over-dispersion)
mu <- predict(m1, type = "response")
z <- ((d$action - mu)^2 - d$action) / (mu * sqrt(2))
summary(zscore <- lm(z ~ 1))


# Score Test

#' 
#' ### Negative Binomial
#' 
#' Less over-dispersion, but still significantly over-dispersed. 
#' 

# Goodness of Fit Based Test 
# (Significant Result Indicates Over-dispersion)
round(1 - pchisq(deviance(m2), m2$df.residual),4)

# Pearson chi-square and dispersion (Dispersion > 1 indicates over-dispersion)
P__disp(m2)

# Score test (Significant coefficient implies Over-dispersion)
mu <- predict(m2, type = "response")
z <- ((d$action - mu)^2 - d$action) / (mu * sqrt(2))
summary(zscore <- lm(z ~ 1))

#'
#' ## Coefficient Plot
#'
#' Create Data Frames with Coefficient Values

(coef1 <- coef(m1)) # coefficient 
(ci1 <- confint(m1, level=0.95)) # 95% confidence interval
cdt1 <- as.data.frame(cbind(coef1, ci1)) # make it a data
colnames(cdt1) <- c("cf","lci","uci") # new names of data
cdt1$name <- "Poisson" # model name
cdt1$vn <- vn # variable names (defined above)
cdt1$vn <- factor(cdt1$vn, levels = rev(vn)) # Assign factor order

(coef2 <- coef(m2)) # coefficient 
(ci2 <- confint(m2, level=0.95)) # 95% confidence interval
cdt2 <- as.data.frame(cbind(coef2, ci2)) # make it a data
colnames(cdt2) <- c("cf","lci","uci") # new names of data
cdt2$name <- "Negative Binomial" # model name
cdt2$vn <- vn # variable names (defined above)
cdt2$vn <- factor(cdt2$vn, levels = rev(vn)) # assign factor order

#'
#' ### Draw Plot (Single Models)
#' 

# Poisson
ggplot(cdt1, aes(x=vn)) + 
   # data is cdt1, y axis is variable name = vn (flip later)
  geom_point(aes(y=cf),size=2) + 
   # plot point estimate = cf
   # size to control point size
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.3, size = 0.5) + 
   # plot confidence interval (lower bound is lci, upper bound is uci)
   # size to control line width
   # width to control th height of vertical lines at the edges
  geom_hline(aes(yintercept=0), linetype=2, size=0.5) + 
   # horizontal line at 0
   # linetype to control form of line (2 is dashed)
   # size to control line width
  xlab(NULL) + 
   # no grand label for variables
  ylab("Coefficient with 95% Confidence Interval") + 
   # Label for x axis (for coefficient value)
  ggtitle("Explaining Number of Bills Receiving Action \n(Poisson Regression)") + 
   # Title (if not needed, use NULL)
  coord_flip() + 
   # Flip Plot 
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5)
        # x axis labels (coefficient values)
  )

# Negative Binomial
ggplot(cdt2, aes(x=vn)) + 
  # data is cdt1, y axis is variable name = vn (flip later)
  geom_point(aes(y=cf),size=2) + 
  # plot point estimate = cf
  # size to control point size
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.3, size = 0.5) + 
  # plot confidence interval (lower bound is lci, upper bound is uci)
  # size to control line width
  # width to control th height of vertical lines at the edges
  geom_hline(aes(yintercept=0), linetype=2, size=0.5) + 
  # horizontal line at 0
  # linetype to control form of line (2 is dashed)
  # size to control line width
  xlab(NULL) + 
  # no grand label for variables
  ylab("Coefficient with 95% Confidence Interval") + 
  # Label for x axis (for coefficient value)
  ggtitle("Explaining Number of Bills Receiving Action \n(Negative Binomial Regression)") + 
  # Title (if not needed, use NULL)
  coord_flip() + 
  # Flip Plot 
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5)
        # x axis labels (coefficient values)
  )

#'
#' ### Draw Plot (Two Models Side by Side)
#' 
#' Optimized for Paper purposes

# Combine data of two models
cdt <- rbind(cdt1, cdt2)

ggplot(cdt, aes(x=vn)) + 
  # data is cdtx, y axis is variable name = vn (flip later)
  geom_point(aes(y=cf),size=2) + 
  # plot point estimate = cf
  # size to control point size
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.3, size = 0.5) + 
  # plot confidence interval (lower bound is lci, upper bound is uci)
  # size to control line width
  # width to control th height of vertical lines at the edges
  geom_hline(aes(yintercept=0), linetype=2, size=0.5) + 
  # horizontal line at 0
  # linetype to control form of line (2 is dashed)
  # size to control line width
  facet_grid(. ~ name) +
  # facetting by the model name (name is the model variable created in the data)
  xlab(NULL) + 
  # no grand label for variables
  ylab("Coefficient with 95% Confidence Interval") + 
  # Label for x axis (for coefficient value)
  ggtitle("Explaining Number of Bills Receiving Action") + 
  # Title (if not needed, use NULL)
  coord_flip() + 
  # Flip Plot 
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5),
        # x axis labels (coefficient values)
        strip.text = element_text(size=11, face="bold", color="black", hjust=0.5)
        # facet strip texts
  )

#'
#' ## Incident Rate Ratio Plot 
#' 

# Create a data with exponentiated coefficients & cis 
cdt_irr <- cdt
cdt_irr$irr <- exp(cdt_irr$cf) # Incident Rate Ratio
cdt_irr$irrlci <- exp(cdt_irr$lci) # lower CI
cdt_irr$irruci <- exp(cdt_irr$uci) # upper CI

# Use the same data cdt.
ggplot(cdt_irr, aes(x=vn)) + 
  # data is cdtx, y axis is variable name = vn (flip later)
  geom_point(aes(y=irr),size=2) + 
  # plot point estimate = irr
  # size to control point size
  geom_errorbar(aes(ymin=irrlci,ymax=irruci),width=0.3, size = 0.5) + 
  # plot confidence interval (lower bound is lci, upper bound is uci)
  # size to control line width
  # width to control th height of vertical lines at the edges
  geom_hline(aes(yintercept=1), linetype=2, size=0.5) + 
  # horizontal line at 0
  # linetype to control form of line (2 is dashed)
  # size to control line width
  facet_grid(. ~ name) +
  # facetting by the model name (name is the model variable created in the data)
  xlab(NULL) + 
  # no grand label for variables
  ylab("Incident Rate Ratio with 95% Confidence Interval") + 
  # Label for x axis (for coefficient value)
  ggtitle("Explaining Number of Bills Receiving Action") + 
  # Title (if not needed, use NULL)
  coord_flip() + 
  # Flip Plot 
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5),
        # x axis labels (coefficient values)
        strip.text = element_text(size=11, face="bold", color="black", hjust=0.5)
        # facet strip texts
  )

#'
#' ## Plotting First Differences of Predicted Probabilities
#' 
#' CompareCommittee Chair Effect by Methods 
#'
#' ### Creating Data
#' 
#' Using custom function (for logit)
#' 

# function
predcount <- function(model,profile,ndraws=1000,cilevel=0.95) {
  # Draw Beta Coefficients
  betadraw <- mvrnorm(ndraws, coef(model), vcov(model))
  # Matrix multiply profile and coefficients
  profile_beta <- as.matrix(profile) %*% t(betadraw)
  # Calculate probability
  profile_prob <- exp(profile_beta)
  # Summarize
  meanprob <- rowMeans(profile_prob)
  sdprob <- apply(profile_prob, 1, sd)
  qtprob <- t(apply(profile_prob, 1, quantile, probs=c(0.5,(1-cilevel)/2,1 - (1-cilevel)/2)))
  res <- as.data.frame(cbind(meanprob,sdprob,qtprob))
  colnames(res) <- c("mean","se","median","lci","uci")
  # Return summary
  return(res)
}

# profiles
coef(m1) # check the list of coefficients
# Base profile (All Medians)
baseprof <- c(1,median(d$seniority),median(d$majority),median(d$chair),
              median(d$subchair),median(d$female),median(d$latino),
              median(d$margin),median(d$distance))
# Not Chair
profile1 <- baseprof; profile1[4] <- 0
# Chair 
profile2 <- baseprof; profile2[4] <- 1
# combine two profiles
(profile1to2 <- rbind(profile1,profile2))

# simulate
set.seed(34)
(predres1 <- predcount(m1, profile1to2)) # Poisson
(predres2 <- predcount(m2, profile1to2)) # Negative Binomial  
predres <- rbind(predres1, predres2) # Combine Both

# Add Profile Identifiers
predres$chair <- factor(rep(c("Non-Chair","Chair"),2),
                        levels=c("Non-Chair","Chair"))
predres$method <- factor(rep(c("Poisson","Negative Binomial"),each=2),
                         levels=c("Poisson","Negative Binomial"))
predres

#'
#' Using zelig
#'

require(Zelig)
m1z <- zelig(fm, data=d, model="poisson")
summary(m1z)
m2z <- zelig(fm, data=d, model="negbin")
summary(m2z)

# Create Profiles
profile1z1 <- setx(m1z, seniority=4, majority=1, chair=0, 
                   subchair=0, female=0, latino=0,
                   margin=66, distance=0.327)
profile1z2<- setx(m1z, seniority=4, majority=1, chair=1, 
                   subchair=0, female=0, latino=0,
                   margin=66, distance=0.327)
profile2z1 <- setx(m2z, seniority=4, majority=1, chair=0, 
                   subchair=0, female=0, latino=0,
                   margin=66, distance=0.327)
profile2z2 <- setx(m2z, seniority=4, majority=1, chair=1, 
                   subchair=0, female=0, latino=0,
                   margin=66, distance=0.327)

# Prediction
set.seed(34)
pred1z <- sim(m1z, x = profile1z1)
set.seed(34)
pred2z <- sim(m1z, x = profile1z2)
set.seed(34)
pred3z <- sim(m2z, x = profile2z1)
set.seed(34)
pred4z <- sim(m2z, x = profile2z2)

# Extract Simulation Ouput
profile_count <- rbind(as.numeric(pred1z$sim.out$x$ev[[1]]),
                      as.numeric(pred2z$sim.out$x$ev[[1]]),
                      as.numeric(pred3z$sim.out$x$ev[[1]]),
                      as.numeric(pred4z$sim.out$x$ev[[1]]))
# Summarize
meancount <- rowMeans(profile_count)
sdcount <- apply(profile_count, 1, sd)
qtcount <- t(apply(profile_count, 1, quantile, probs=c(0.5,0.025,0.975)))
predresz <- as.data.frame(cbind(meancount,sdcount,qtcount))
colnames(predresz) <- c("mean","se","median","lci","uci")
# Add Profile Identifiers
predresz$chair <- factor(rep(c("Non-Chair","Chair"),2),
                        levels=c("Non-Chair","Chair"))
predresz$method <- factor(rep(c("Poisson","Negative Binomial"),each=2),
                         levels=c("Poisson","Negative Binomial"))
predresz

# Zelig results look similar as custom results
round(predresz[,1:5]-predres[,1:5],5)

#'
#' ### Bar Plot
#' 
#' Use predres data
#' 
# plot 
ggplot(predres, aes(y=mean, x=chair)) + 
  # data is predres, 
  # y axis is mean predicted probability = mean
  # x axis is Committee chair or not
  geom_bar(stat="identity", fill="gray40") + 
  # stat allows you to plot value as it is (not aggregating)
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.3, size = 0.5) + 
  # plot confidence interval (lower bound is lci, upper bound is uci)
  # size to control line width
  # width to control th height of vertical lines at the edges
  facet_grid(. ~ method) +
  # facetting by method
  xlab(NULL) + 
  # Label for x axis # Null if not needed
  ylab("Predicted Count of Bills Receiving Action \nwith 95% Confidence Interval") + 
  # Label for y axis
  ggtitle("Predicted Count of Bills Receiving Action \nby Committee Chair Status and Method") + 
  # Title (if not needed, use NULL)
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5),
        # x axis labels (coefficient values)
        strip.text = element_text(size=11, face="bold", color="black", hjust=0.5)
        # facet strip texts
  ) 

#' 
#' ### Point Plot (Notice that scale changes in the y axis)
#' 
#' Use predresz (zelig prediction) data
#' 

# plot 
ggplot(predresz, aes(y=mean, x=chair)) + 
  # data is predres, 
  # y axis is mean predicted probability = mean
  # x axis is committee chair status
  geom_point(size=2) + 
  # size to control point size
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.3, size = 0.5) + 
  # plot confidence interval (lower bound is lci, upper bound is uci)
  # size to control line width
  # width to control th height of vertical lines at the edges
  facet_grid(. ~ method) +
  # facetting by method
  xlab(NULL) + 
  # Label for x axis # Null if not needed
  ylab("Predicted Count of Bills Receiving Action \nwith 95% Confidence Interval") + 
  # Label for y axis
  ggtitle("Predicted Count of Bills Receiving Action \nby Committee Chair Status and Method") + 
  # Title (if not needed, use NULL)
  theme_bw() + 
  theme(plot.title = element_text(size=13, face="bold", hjust=0.5),
        # plot title setting (ggtitle argument)
        axis.title.x = element_text(size=11, face="plain", hjust=0.5),
        # x axis title setting 
        axis.text.y = element_text(size=11, face="plain", color="black", hjust=1),
        # y axis labels (variables)
        axis.text.x = element_text(size=11, face="plain", color="black",hjust=0.5),
        # x axis labels (coefficient values)
        strip.text = element_text(size=11, face="bold", color="black", hjust=0.5)
        # facet strip texts
  ) 

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_060519.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('TA_session_060519.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_060519.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_060519.R', 'pdf_document')"
