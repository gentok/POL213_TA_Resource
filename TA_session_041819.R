#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "April 18, 2019"
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
#' # Let's Replicate Boudreau and MacKenzie 2014!
#' 
#' Check their paper [HERE](https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12054).
#' 
#' Their Replication Data are [HERE](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CNNXPB).
#'

#+ eval=FALSE
# install.packages("dataverse") # Only Once
library(dataverse)
serverset <- "dataverse.harvard.edu"

(meta <- get_dataset("doi:10.7910/DVN/CNNXPB", server=serverset))

# Get Codebook
writeBin(get_file("boudreau_mackenzie_codebook_ajps.pdf","doi:10.7910/DVN/CNNXPB",
                  server=serverset), "boudreau_mackenzie_codebook_ajps.pdf")
# Get Data
writeBin(get_file("table2_fig1_fig2.tab","doi:10.7910/DVN/CNNXPB",
                  server=serverset), "table2_fig1_fig2.dta")

#+
# Import Data
d <- read.dta13("table2_fig1_fig2.dta", convert.factors = FALSE)
# Variables
summary(d)

#'
#' # Run Logistic Regression
#'

# For Figure 1
logit.cueexp <- glm(sup_init ~ 0 + rei_party + rei_policy + rei_party_policy + rei_control +
                    con_party + con_policy + con_party_policy + con_control +
                    bal_party + bal_policy + bal_party_policy + bal_control, data=d, 
                    family=binomial("logit"))
summary(logit.cueexp)

# Focus on the difference in "conflicting environment"

# Predicted Probability

# (Control in Conflicting Environment / Preferred by Party)
(pi_ctl <- exp(0.47523) / (1 + exp(0.47523)))
# (Party Cue Received in Conflicting Envionrment / Preferred by Party)
(pi_cue <- exp(0.80722) / (1 + exp(0.80722)))
# (Party Cue & Opposint Info Received in Conflicting Envionrment / Preferred by Party)
(pi_both <- exp(0.28344) / (1 + exp(0.28344)))

# Comparing Odds Ratio

# Calculate Odds Ratio
(odds_ctl <- pi_ctl/(1-pi_ctl))
(odds_cue <- pi_ctl/(1-pi_cue))
(odds_both <- pi_ctl/(1-pi_both))

# Control vs. Cue Reception
odds_ctl / odds_cue
# Cue + Info vs. Cue Reception
odds_both / odds_cue
# COntrol vs. Cue + Info
odds_ctl / odds_both

# Wald statistic and confidence intervals

# Coefficient Table
(cftab <- summary(logit.cueexp)$coefficients)

# Z Score
(z_ctl <- (cftab[8,1] - 0) / cftab[8,2])
(z_cue <- (cftab[5,1] - 0) / cftab[5,2])
(z_both <- (cftab[7,1] - 0) / cftab[7,2])

# Confidence Interval
(ci_ctl <- c(cftab[8,1]-1.96*cftab[8,2],cftab[8,1]+1.96*cftab[8,2]))
(ci_cue <- c(cftab[5,1]-1.96*cftab[5,2],cftab[5,1]+1.96*cftab[5,2]))
(ci_both <- c(cftab[7,1]-1.96*cftab[7,2],cftab[7,1]+1.96*cftab[7,2]))
# or Just
(citab <- confint(logit.cueexp))

# Replicate Figure 1 in the paper
# Assuming that all other conditions are 0, 
# predicted probabilities are just the inverse logit of estimates

(cfcitab <- as.data.frame(cbind(as.numeric(cftab[,1]),citab)))
colnames(cfcitab) <- c("est","lb","ub")
cfcitab$est <- ilogit(cfcitab$est)
cfcitab$lb <- ilogit(cfcitab$lb)
cfcitab$ub <- ilogit(cfcitab$ub)
# Add Environment Identifier
cfcitab$env <- factor(rep(c("Reinforcing","Conflicting","Balanced"),each=4),
                      levels=c("Reinforcing","Balanced","Conflicting"))
# Add Treatment Identifier
cfcitab$trt <- factor(rep(c("Party","Policy \nInfo","Party \n+ Policy","Control"),3),
                      levels=c("Control","Party","Policy \nInfo","Party \n+ Policy")) 

ggplot(cfcitab, aes(x=trt,y=est)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lb,ymax=ub), width=0.3) + 
  facet_grid(.~env) + xlab("Treatment") + 
  ylab("Predicted Proabability of Policy Support") +
  theme_bw()

# Likelihood ratio test

logit.null <- glm(sup_init ~ 1, d, family=binomial("logit"))
summary(logit.null)

(ll1 <- logLik(logit.cueexp))
(ll0 <- logLik(logit.null))
(g_statusquo <- 2*(ll1[[1]] - ll0[[1]]))
# Or, use the lrtest function to conduct this test
library(lmtest)
lrtest(logit.cueexp, logit.null)

#'
#' # Workshop (Choose Either One of Them)
#' 
#' * In the same dataset, <code>know_high</code> is the indicator 
#' for knowledge level (1=high, 0=low) and <code>pty_strong</code> is 
#' the indicator for partisanship strength (1=high, 0=low). Construct 
#' the logistic regression model with interaction and replicate figure 2 in Boudreau and MacKenzie 2014.
#' 
#' * Run probit with the same model as above. Any difference?
#'


#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_041819.R', 'pdf_document')
# rmarkdown::render('TA_session_041819.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_041819.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_041819.R', 'pdf_document')"
