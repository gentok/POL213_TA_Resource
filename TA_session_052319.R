#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "May 23, 2019"
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
library(MASS) # For mvrnorm
library(readstata13)

#'
#' ## Data Preparation (Experiment Data on Name Recognition)
#' 
#' For original article, See [HERE](https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12034)
#' 

# Import Data
d <- read.dta13("https://github.com/gentok/POL213_TA_Resource/raw/master/KamZechmeister_Study1.dta", 
                convert.factors = FALSE)
names(d)

# FT Griffin Advantage
d$FTgrifadv <- d$FTgriffin - d$FTwilliams

#'
#' ## Run Logit Model
#'

# Baseline Model
m1 <- glm(votegriffin ~ namecond + female + democrat + age1,
          data=d, family=binomial("logit"))
summary(m1)

# Interaction
m2 <- glm(votegriffin ~ namecond*female + democrat + age1,
          data=d, family=binomial("logit"))
summary(m2)

# Some significant continous variable
m3 <- glm(votegriffin ~ FTgrifadv + democrat + age1, 
          data=d, family=binomial("logit")) 
summary(m3)

#'
#' ## Coefficient Plot
#'

#'
#' ## Odds Ratio Plot 
#'

#'
#' ## Plotting First Differences of Predicted Probabilities
#'

#'
#' ### Creating Data

#'
#' ### Bar Plot

#' 
#' ### Point Plot

#'
#' ## Plotting Predicted Probabilities by Continuous Variable
#'
#' ### Creating Data
#' 
#' ### Line Plot

#' Examples for Poisson Regression and Negative Binomial Regression 
#' will be discussed next week.

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_052319.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('TA_session_052319.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_052319.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_052319.R', 'pdf_document')"
