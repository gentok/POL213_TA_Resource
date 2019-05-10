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
#' # Annotated (and Slightly Modified) Class Codes
#' 
#' ## Data Preparation
#' 

# Import Data
d <- read.table("ca_taxes_soda.txt", header=TRUE, sep="\t")
dim(d) # 9 variables, 795 cases

# Some Additional Variables
d$soda_tax <- d$tax_soda
d$soda_tax2 <- as.factor(d$tax_soda) # Ordinal Variable
d$education <- d$educate

# Initiate Data for Analysis
## Keeping Following Variables
keepvars <- c("soda_tax", "soda_tax2", "ideology", "income", 
              "education", "cue", "ind", "gop", "gop_cue")
# Subset Variables and Create A New Dataset
ca_soda <- d[,keepvars]

#'
#' ## Ordinal Logit
#'

library(MASS) # For polr function

# Ordinal Logit
ologit.soda <- polr(soda_tax2 ~ ideology + income + education + 
                      ind + gop + cue + gop_cue, data = ca_soda, Hess = TRUE)
# Summary
(sum.ologit.soda <- summary(ologit.soda))

# Significance Test
table.ologit.soda <- coef(sum.ologit.soda)
p <- pnorm(abs(table.ologit.soda[, "t value"]), lower.tail = FALSE) * 2
(table.ologit.soda2 <- cbind(table.ologit.soda, "p value" = p))

# The Easier Way
library(lmtest)
(cft <- coeftest(ologit.soda))

# Confidence Interval
(ci <- confint(ologit.soda))

# Plot 
## Data Frame with Coefficient Values
cdt <- as.data.frame(cbind(cft[,1],ci))
colnames(cdt) <- c("cf","lci","uci")
## Variable Names
cdt$vn <- factor(row.names(cdt), 
                 levels=rev(row.names(cdt)))
## Draw Plot
ggplot(cdt, aes(x=vn,y=cf,ymin=lci,ymax=uci)) + 
  geom_point() + geom_errorbar(width=0.3) +
  geom_hline(aes(yintercept=0), linetype=2) +
  xlab(NULL) + 
  ylab("Coefficient with 95% Confidence Interval") + 
  coord_flip() + # Flip Plot
  theme_bw()

# Odds Ratio
## Conversion
cdt$or <- exp(cdt$cf)
cdt$orlci <- exp(cdt$lci)
cdt$oruci <- exp(cdt$uci)
## Draw Plot
ggplot(cdt, aes(x=vn,y=or,ymin=orlci,ymax=oruci)) + 
  geom_point() + geom_errorbar(width=0.3) +
  geom_hline(aes(yintercept=1), linetype=2) +
  xlab(NULL) + 
  ylab("Odds Ratio with 95% Confidence Interval") + 
  coord_flip() + # Flip Plot
  theme_bw()

#'
#' # Predicted Probabilities
#'   

## Profiles
prof_baseD <- c(3, # ideology 
                6, # income 
                3, # education
                0, # ind (not)
                0, # gop (not) * means democrat
                0, # cue
                0) # gop_cue
names(prof_baseD) <- all.vars(ologit.soda$terms)[-1]
prof_cueD <- prof_baseR <- prof_cueR <- prof_baseD
prof_cueD[6] <- 1 # Receiving (dem) cues
prof_baseR[5] <- 1 # GOP member
prof_cueR[c(5,6,7)] <- 1 # Receiving R cues and GOP Member

## Function for Prediction
predologit <- function(model,profile) {
  
  # Parameters
  cf <- coef(model) # Coefficients
  z <- summary(model)$zeta # Thresholds
  
  xb <- sum(profile * cf) # Individual Estiamtes
  
  # Temporal Probabilities
  prtmp <- c(0,sapply(z, function(zi) 1 / (1 + exp(xb - zi))),1)

  # Predicted Probabilities
  pr <- rep(NA, length(z)+1) 
  for (i in seq(1,length(z)+1,1)) pr[i] <- prtmp[i+1] - prtmp[i]
  
  # Names
  names(pr) <- paste0("Pr.",seq(1,length(pr),1))

  return(pr)
  
}

# Make Prediction
pred_baseD <- predologit(ologit.soda,prof_baseD)
pred_cueD <- predologit(ologit.soda,prof_cueD)
pred_baseR <- predologit(ologit.soda,prof_baseR)
pred_cueR <- predologit(ologit.soda,prof_cueR)

# Plot Prediction
preddt <- rbind(cbind(pred_baseD,rbind(prof_baseD,prof_baseD,prof_baseD,prof_baseD)),
                cbind(pred_cueD,rbind(prof_cueD,prof_cueD,prof_cueD,prof_cueD)),
                cbind(pred_baseR,rbind(prof_baseR,prof_baseR,prof_baseR,prof_baseR)),
                cbind(pred_cueR,rbind(prof_cueR,prof_cueR,prof_cueR,prof_cueR)))
preddt <- as.data.frame(preddt)
colnames(preddt)[1] <- "pr"
preddt$cats <- as.factor(rep(seq(1,4,1),4))
## Label Party Membership
preddt$gop <- ifelse(preddt$gop==1,"Republican","Democrat")

# One By One
ggplot(preddt, aes(x=cue, y=pr)) + 
  geom_line(aes(linetype=cats), size=0.75) + 
  geom_point(aes(shape=cats), size=2) + 
  facet_grid(.~gop) +
  scale_x_continuous(limits=c(-0.25,1.25),
                     breaks=c(0,1),
                     labels=c("No Cue","Consistent\nCue")) +
  scale_shape_discrete(name="Tax Preference \nCategory") + 
  scale_linetype_discrete(name="Tax Preference \nCategory") + 
  ylab("Predicted Probability") + xlab(NULL) + 
  theme_bw()

# Cummulative
ggplot(preddt, aes(x=cue, y=pr)) + 
  geom_area(aes(fill=cats)) + 
  facet_grid(.~gop) +
  scale_x_continuous(limits=c(-0.25,1.25),
                     breaks=c(0,1),
                     labels=c("No Cue","Consistent\nCue")) +
  scale_fill_discrete(name="Tax Preference \nCategory") + 
  ylab("Predicted Probability") + xlab(NULL) + 
  theme_bw()

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
rmarkdown::render('TA_session_050919.R', 'pdf_document', encoding = 'UTF-8')
rmarkdown::render('TA_session_050919.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_050919.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_050919.R', 'pdf_document')"
