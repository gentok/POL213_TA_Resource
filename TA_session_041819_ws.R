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
library(ggplot2) # Plotting
library(faraway) # for ilogit function
library(ggrepel) # For Convenient Text Label
library(readstata13) # Read stata type data

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


# Statistical Significance from Control
library(multcomp)

names(coef(logit.cueexp))

# Linear Combination (Compare With Control Group)
compare <- c("rei_party - rei_control = 0",
             "rei_policy - rei_control = 0",
             "rei_party_policy - rei_control = 0",
             "con_party - con_control = 0",
             "con_policy - con_control = 0",
             "con_party_policy - con_control = 0",
             "bal_party - bal_control = 0",
             "bal_policy - bal_control = 0",
             "bal_party_policy - bal_control = 0")
# Function to test linear combination hypotheses and store p-value
complh <- function(k) as.numeric(summary(glht(logit.cueexp, linfct = k))$test$pvalues)[1]
pvals <- sapply(compare, complh)
(pvals <- c(pvals[1:3],NA,pvals[4:6],NA,pvals[7:9],NA))

# Difference from Party Cue Group 
compare2 <- c("rei_policy - rei_party = 0",
             "rei_party_policy - rei_party = 0",
             "rei_control - rei_party = 0",
             "con_policy - con_party = 0",
             "con_party_policy - con_party = 0",
             "con_control - con_party = 0",
             "bal_policy - bal_party = 0",
             "bal_party_policy - bal_party = 0",
             "bal_control - bal_party = 0"
             )
# Function to test linear combination hypotheses and store p-value
pvals2 <- sapply(compare2, complh)
(pvals2 <- c(NA,pvals2[1:3],NA,pvals2[4:6],NA,pvals2[7:9]))


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
# Statistical Significance (compared to control)
cfcitab$pval <- pvals
cfcitab$p5 <- ifelse(pvals<0.05,"*",NA)
# Compared to party cue
cfcitab$pvalx <- pvals2
cfcitab$p5x <- ifelse(pvals2<0.05,"#",NA)

# Plot
ggplot(cfcitab, aes(x=trt,y=est)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lb,ymax=ub), width=0.3) + 
  geom_text(aes(label=p5), vjust=-1.75) + 
  geom_text(aes(label=p5x), vjust=-2) + 
  facet_grid(.~env) + xlab("Treatment") + 
  ylab("Predicted Proabability of Policy Support") + 
  labs(caption="* indicates p <.05 compared to control condition.\n # indicates p <.05 compared to party cue condition under conflicting environment.") +
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
#' ## Replicate Figure 2
#'

# Shortcutting the process (the custom package under development by me...)
#install.packages("devtools")
# Here is how you install somebodyelse's custom R package (not in CRAN).
# devtools::install_github("gentok/estvis") 
library(estvis)

# Knowledge Moderation Model
logit.cueexp.kn <- glm(sup_init ~ 0 + rei_party*know_high + rei_policy*know_high + rei_party_policy*know_high + rei_control*know_high +
                      con_party*know_high + con_policy*know_high + con_party_policy*know_high + con_control*know_high +
                      bal_party*know_high + bal_policy*know_high + bal_party_policy*know_high + bal_control, 
                    data=d, 
                    family=binomial("logit"))
# Partisanship Moderation Model
logit.cueexp.ps <- glm(sup_init ~ 0 + rei_party*pty_strong + rei_policy*pty_strong + rei_party_policy*pty_strong + rei_control*pty_strong +
                         con_party*pty_strong + con_policy*pty_strong + con_party_policy*pty_strong + con_control*pty_strong +
                         bal_party*pty_strong + bal_policy*pty_strong + bal_party_policy*pty_strong + bal_control, 
                       data=d, 
                       family=binomial("logit"))

# Prediction Profile
newprof <- data.frame(rei_party = median(d$rei_party),
                      rei_policy = median(d$rei_policy), 
                      rei_party_policy = median(d$rei_party_policy),
                      rei_control = median(d$rei_control),
                      bal_party = median(d$bal_party),
                      bal_policy = median(d$bal_policy), 
                      bal_party_policy = median(d$bal_party_policy),
                      bal_control = median(d$bal_control),
                      con_party = median(d$con_party),
                      con_policy = median(d$con_policy), 
                      con_party_policy = median(d$con_party_policy),
                      con_control = median(d$con_control),
                      know_high = rep(c(1,0), each=12),
                      pty_strong = rep(c(1,0), each=12))
# Add conditions
for(i in 1:12) newprof[,i][c(i,i+12)] <- 1

# Make Prediction
pred.kn <- simu_pred(logit.cueexp.kn, newprof)$predsum
pred.ps <- simu_pred(logit.cueexp.ps, newprof)$predsum
pred.int <- rbind(pred.kn, pred.ps)
pred.int$cat <- rep(c("High Knowledge","Low Knowledge",
                      "Strong Partisan","Weak Partisan"),each=12)
pred.int$cat <- factor(pred.int$cat, levels=c("High Knowledge","Strong Partisan",
                                              "Low Knowledge","Weak Partisan"))
# Add Environment Identifier
pred.int$env <- factor(rep(rep(c("Reinforcing","Conflicting","Balanced"),each=4),2),
                      levels=c("Reinforcing","Balanced","Conflicting"))
# Add Treatment Identifier
pred.int$trt <- factor(rep(c("Party","Policy \nInfo","Party \n+ Policy","Control"),6),
                      levels=c("Control","Party","Policy \nInfo","Party \n+ Policy")) 

ggplot(pred.int, aes(x=env, y=Mean, fill=trt)) + 
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.75)) +
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI), width=0.3, 
                position=position_dodge(width=0.75)) + 
  facet_wrap(.~cat) + xlab("Context") + 
  scale_fill_brewer(name="Treatment", type="qual", palette=6) +
  ylab("Predicted Proabability of Policy Support") + 
  theme_bw()

#' * Run probit

probit.cueexp <- glm(sup_init ~ 0 + rei_party + rei_policy + rei_party_policy + rei_control +
                       con_party + con_policy + con_party_policy + con_control +
                       bal_party + bal_policy + bal_party_policy + bal_control, data=d, 
                     family=binomial("probit"))
summary(probit.cueexp)

pred.probit <- simu_pred(probit.cueexp, newprof[1:12,])$predsum
# Add Environment Identifier
pred.probit$env <- factor(rep(rep(c("Reinforcing","Conflicting","Balanced"),each=4),1),
                       levels=c("Reinforcing","Balanced","Conflicting"))
# Add Treatment Identifier
pred.probit$trt <- factor(rep(c("Party","Policy \nInfo","Party \n+ Policy","Control"),3),
                       levels=c("Control","Party","Policy \nInfo","Party \n+ Policy")) 

# Plot
ggplot(pred.probit, aes(x=trt, y=Mean)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI), width=0.3) + 
  facet_grid(. ~ env) + xlab("Treatment") + 
  ylab("Predicted Proabability of Policy Support") + 
  theme_bw()

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_041819_ws.R', 'pdf_document')
# rmarkdown::render('TA_session_041819_ws.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_041819_ws.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_041819_ws.R', 'pdf_document')"
