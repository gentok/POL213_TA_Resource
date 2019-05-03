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
# First Quartile is the Reference Category

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

#' 
#' ## 1.1.1. Find Odds Ratio for the exposure variables estimated in 1.1. 
#' Make interpretation
#'    

#'
#' ## 1.1.2. Calculate Wald Statistics (z-score) for the coefficients in 1.1. and
#' generate p-values. Are the coefficients significantly different from zero?
#' 


#'
#' ## 1.2. Obtain Fitted Values (Predicted Probabilities) from the model estimated in 1.1
#'

#'
#' ## 1.3. Manually calculate predicted probabilities of each preference for 
#' those who are exposed to protest and those who are not.
#'

#'
#' ## 1.4. Add as many control variables as you want to the model in 1.1 and 
#' estimate a new model. Does the addition of control variables change results?
#'

#'
#' ## 1.5. Find Pseudo R Squares, Proportional Reduction in Error (PRE) and 
#' infomration measures (i.e., AIC, BIC) for BOTH models estimated in 1.1 and 1.4. 
#' Compare results. Which model explains DV better?
#'

# Pseudo-R2

# PRE

# Information Measures

#'
#' ## 1.6. Using model estimated in 1.4, create several profiles of interests and 
#' simulate the Predicted Probability with Confidence Interval. Plot Results.
#'

#' 
#' # Use mlogit function
#' 
library(mlogit)
#'
#' ## 2.1. Use mlogit function from mlogit package to estimate the same model 
#' as in 1.4. Does it yield the same results?
#' 

#'
#' ## 2.2. Create some choice level variable in the dataset (i.e., some 
#' combination of individual level characteristics and choice characteristics). 
#' Run the new model with choice level variable.
#' 


#'
#' # Use Zelig function
#' 
library(Zelig) # zelig function
library(ZeligChoice) # zelig function to do multinomial logit
#'
#' ## 3.1. Use Zelig function to estimate the same model as in 1.4. 
#'

#'  
#' ## 3.2. Export predicted probabilities for several profiles.
#'

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_042519_ws.R', 'pdf_document')
# rmarkdown::render('TA_session_042519_ws.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_042519_ws.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_042519_ws.R', 'pdf_document')"
