#' ---
#' title: "POL213 TA Session"
#' author: "Gento Kato"
#' date: "May 16, 2019"
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

#'
#' # Annotated (and Slightly Modified) Class Codes
#' 
#' ## Data Preparation
#' 

# Import Data
d <- read.table("ca_taxes_soda.txt", header=TRUE, sep="\t")
dim(d) # 9 variables, 795 cases

# Some Additional Variableseducate
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
#' ## Ordinal Logit (polr)
#'

# Ordinal Logit (polr)
require(MASS) # For polr function
ol1 <- polr(soda_tax2 ~ ideology + income + education + ind + gop + cue + gop_cue, 
            data = ca_soda, Hess = TRUE)
# Summary
(sum.ol1 <- summary(ol1))

# Significance Test
table.ol1 <- coef(sum.ol1)
p <- pnorm(abs(table.ol1[, "t value"]), lower.tail = FALSE) * 2
(table.ol1 <- cbind(table.ol1, "p value" = p))

# The Easier Way
require(lmtest)
(cft <- coeftest(ol1))

# Confidence Interval
(ci <- confint(ol1))

# Another ologit model (zelig)
require(Zelig)
require(ZeligChoice)
ol2 <- zelig(soda_tax2 ~ ideology + income + education + ind + gop + cue + gop_cue,
             data=ca_soda, model="ologit")
summary(ol2)

# Export Social-Scientific Table
require(texreg)
#+ eval=FALSE
htmlreg(list(ol1,ol2), # models in list
        # names of model
        custom.model.names = c("polr", "zelig"),
        # include tao values in output
        include.thresholds = TRUE,
        # drop deviance from output
        include.deviance = FALSE,
        # Set digits to 3 points
        digits = 3,
        # Stars for p < 0.01 & p < 0.05
        stars = c(0.01, 0.05),
        # Place coefficients & SE in single row (effective when few models)
        single.row = TRUE,
        # Set custom names of coefficients (and thresholds)
        custom.coef.names = c("Ideology","Income","Education",
                              "Independent", "Republican",
                              "Cue Reception", "Republican Cue",
                              "Threshold: Strongly|Somewhat Oppose",
                              "Threshold: Somewhat Oppose|Support",
                              "Threshold: Somewhat|Strongly Support"),
        # Title of Table
        caption = c("Compare polr and zelig outputs of ordinal logit"),
        # Place title above the table
        caption.above =TRUE,
        # Add Some Notes under the table
        custom.note = c("%stars. Standard Errors in Parentheses."),
        # Save the Output to File (Word Format)
        file = "ologit_table.doc")


#+
# Coefficient Plot (Extra) 
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
cdtor <- cdt[,c("or","orlci","oruci")]
rownames(cdtor) <- c("Ideology","Income","Education",
                     "Independent", "Republican",
                     "Cue Reception", "Republican Cue")
colnames(cdtor) <- c("Odds Ratio", "Lower 95% CI", "Upper 95% CI")
cdtor

## Export Odds Ratio Table in Word Pastable Format
require(stargazer)
#+ eval=FALSE
stargazer(cdtor, # Input (matrix or data.frame)
          # Control digits
          digits = 3,
          # Title
          title = "Odds Ratios from Ordinal Logit Model",
          # Export output "as is"
          summary = FALSE,
          # Export Type
          type = "html", out = "ologit_ortab.doc")

#+
## Fit Statistics
pR2(ol1) # Make sure you know (roughly) what they are

# Adjusted Statistics (Adjusted McFadden for Ordinal Logit)
adjMcFadden_ol <- function(model) {
  # Null Model
  nullmod <- polr( model$model[,all.vars(model$terms)[1]] ~ 1, Hess = TRUE)
  # Null Model Log-Likelihood
  L.base <- logLik(nullmod)
  # Full Model Log-Likelihood
  L.full <- logLik(model)
  # Degrees of Freedom in Full Moddel
  P3 <- attr(L.full, "df")
  # Standard McFadden
  McFadden.R2 <- 1 - (L.full / L.base)
  # Adjusted McFadden
  McFadden.Adj.R2 <- 1 - ((L.full - P3) / L.base)
  # Format Output
  out <- c(McFadden.R2,McFadden.Adj.R2)
  names(out) <- c("McFadden","Adjusted McFadden")
  out
}
adjMcFadden_ol(ol1)

#'
#' # Predicted Probabilities
#'   

## Profiles (No Intercept in Ordinal Logit)
prof_baseD <- c(3, # ideology 
                6, # income 
                3, # education
                0, # ind (not)
                0, # gop (not) * means democrat
                0, # cue
                0) # gop_cue
names(prof_baseD) <- all.vars(ol1$terms)[-1]
# prof_cueD <- prof_baseR <- prof_cueR <- prof_baseD
# prof_cueD[6] <- 1 # Receiving (dem) cues
# prof_baseR[5] <- 1 # GOP member
# prof_cueR[c(5,6,7)] <- 1 # Receiving R cues and GOP Member
# Vary only ideology
x <- seq(1,5, .1) # Moving Values
profide_D <- t(matrix(rep(prof_baseD,length(x)), ncol=length(x)))
profide_D[,1] <- x
head(profide_D)
dim(profide_D)

## Function for Predicted Probability
predologit <- function(model,profile) {
  
  # Coefficients
  coeffs <- c(coef(model), summary(model)$zeta)
  # Variance Covariance Matrix
  covmat <- vcov(model)
  # Number of Draws
  ndraws <- 1000
  # Draw
  require(MASS); set.seed(34)
  betadraw <- mvrnorm(ndraws, coeffs, covmat)
  
  # Profile * Coefficients
  nvars <- length(coef(model))
  xb <- profile %*% t(betadraw[,1:nvars])
  # Thresholds
  taos <- betadraw[,seq(nvars+1, length(coeffs), 1)]
  
  # Predicted Probabilities
  prlist <- list()
  for (i in 1:nrow(xb)) {
    tmp <- 
      cbind(rep(0,ndraws), 
          apply(taos, 2, function(taoi) 1/(1 + exp(xb[i,] - taoi))),
          rep(1, ndraws))
    pr <- matrix(NA, nrow=ndraws, ncol=ncol(taos)+1)
    for (j in seq(1,ncol(taos)+1,1)) pr[,j] <- tmp[,(j+1)] - tmp[,j]
    colnames(pr) <- paste0("Pr.",seq(1,ncol(pr),1))
    head(pr)
    prlist[[i]] <- pr
  }

  # Function to Summarize Result
  cirange <- c(0.5,0.025,0.975)
  sumres <- function(pr) {
    out <- cbind(colMeans(pr),
                 apply(pr, 2, function(k) sd(k)),
                 t(apply(pr, 2, function(k) quantile(k, probs=cirange))))
    colnames(out) <- c("mean","se","median","lowCI","upCI")
    out
  }
  
  # Export Summary of Prediction
  sumlist <- lapply(prlist, sumres)
  if (length(sumlist)==1) {
    sumlist <- sumlist[[1]]
  }

  return(sumlist)
  
}

# Make Prediction
predide_D <- predologit(ol1,profide_D)
predide_D <- as.data.frame(do.call("rbind",predide_D))
predide_D$cat <- rep(c("Strongly Oppose",
                       "Somewhat Oppose",
                       "Somewhat Support",
                       "Strongly Support"), 
                     length(seq(1,5, .1)))
predide_D$cat <- factor(predide_D$cat,
                        levels=unique(predide_D$cat))
predide_D$x <- rep(seq(1,5, .1), each=4)

# One By One with CI
ggplot(predide_D, aes(x=x, y=mean, ymin=lowCI, ymax=upCI)) + 
  geom_ribbon(aes(fill=cat), alpha=0.5) + 
  geom_line(aes(color=cat), size=0.75) + 
  scale_fill_brewer(name="Tax Preference", type="div") +
  scale_color_brewer(name="Tax Preference", type="div") +
  ylab("Predicted Probability") + 
  xlab("Ideology") + 
  ggtitle("Predicted Probabilities from Ordinal Logit") + 
  labs(caption="Other variables are fixed at median.") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=15))

# Cummulative (no CI possible)
ggplot(predide_D, aes(x=x, y=mean)) + 
  geom_area(aes(fill=cat)) + 
  scale_fill_brewer(name="Tax Preference", type="div") +
  ylab("Predicted Probability") + 
  xlab("Ideology") + 
  ggtitle("Cummulative Predicted Probability from Ordinal Logit") + 
  scale_x_continuous(expand=c(0,0)) + # No Expansion of axis
  scale_y_continuous(expand=c(0,0)) + # No Expansion of axis
  labs(caption="Other variables are fixed at median.") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=15))

#'
#' # Workshop: Use Zelig to replicate the predicted probability graphs.
#' Do not rely too much on default graphs.
#' 

# moving value of x
ol2.x <- setx(ol2, ideology = seq(1,5, .1), 
              income = 6, education = 3, 
              ind = 0, gop = 0, cue = 0, gop_cue = 0)

# Prediction
set.seed(34)
ol2.pred <- sim(ol2, x = ol2.x)

# Hard to see what you are looking at
plot(ol2.pred)

# Capture prediction output and create data
# (probably there is a better way. let me know if you know it)
## capture summary output
ol2.predsum <- capture.output(ol2.pred) 
## Extract Relevant Ouputs and Compile into matrix
ol2.predsum <- do.call("rbind", 
  str_split(ol2.predsum[grep("^[1-9] [0-9]",ol2.predsum)]," "))
## Make All variables Numeric
ol2.predsum <- apply(ol2.predsum, 2, as.numeric)
## Create Data.frame and add variables
ol2.predsum <- as.data.frame(ol2.predsum)
colnames(ol2.predsum) <- c("catn","mean","se","median","lowCI","upCI")
ol2.predsum$cat <- rep(c("Strongly Oppose",
                       "Somewhat Oppose",
                       "Somewhat Support",
                       "Strongly Support"), 
                     length(seq(1,5, .1)))
ol2.predsum$cat <- factor(ol2.predsum$cat,
                        levels=unique(ol2.predsum$cat))
ol2.predsum$x <- rep(seq(1,5, .1), each=4)

# One By One with CI (Confidence Intervals Look Very Different)
ggplot(ol2.predsum, aes(x=x, y=mean, ymin=lowCI, ymax=upCI)) + 
  geom_ribbon(aes(fill=cat), alpha=0.5) + 
  geom_line(aes(color=cat), size=0.75) + 
  scale_fill_brewer(name="Tax Preference", type="div") +
  scale_color_brewer(name="Tax Preference", type="div") +
  ylab("Predicted Probability") + 
  xlab("Ideology") + 
  ggtitle("Predicted Probabilities from Ordinal Logit (Zelig)") + 
  labs(caption="Other variables are fixed at median.") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=15))

# Cummulative (no CI possible)
ggplot(ol2.predsum, aes(x=x, y=mean)) + 
  geom_area(aes(fill=cat)) + 
  scale_fill_brewer(name="Tax Preference", type="div") +
  ylab("Predicted Probability") + 
  xlab("Ideology") + 
  ggtitle("Cummulative Predicted Probability from Ordinal Logit (Zelig)") + 
  scale_x_continuous(expand=c(0,0)) + # No Expansion of axis
  scale_y_continuous(expand=c(0,0)) + # No Expansion of axis
  labs(caption="Other variables are fixed at median.") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=15))


#'
#' # Generalized Ordered Logit
#'

# Brant Test of Propotional Odds Assumption
require(brant)
brant(ol1)
# Income seem to potentially violating the parallel regression assumption.

# R codes for generalized ordered logit.
# (Note that this function uses slightly different procedure than Stata's gologit2)
require(VGAM)

# Replicate polr function Result
ol3a <- 
  vglm(as.ordered(soda_tax2) ~ 
         ideology + income + education + ind + gop + cue + gop_cue, 
       data = ca_soda,
       family = cumulative(link = "logit", parallel = TRUE, reverse=TRUE))

# Relaxing Proportional Odds Assumption
ol3b <- 
  vglm(as.ordered(soda_tax2) ~ ideology + income + education + ind + gop + cue + gop_cue, 
       data = ca_soda,
       family = cumulative(link = "logit", parallel = FALSE, reverse=TRUE))

# Modifying extract function of texreg to export table
extract.vglm <- function (model, 
                          include.aic = TRUE,
                          include.bic = TRUE,
                          include.loglik = TRUE, 
                          include.df = FALSE, 
                          include.nobs = TRUE,
                          beside = TRUE,
                          resp.names = NA,
                          ...) 
{
  s <- summary(model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, AIC(model))
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, BIC(model))
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, VGAM::logLik.vlm(model))
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.df == TRUE) {
    gof <- c(gof, df <- s@df[2])
    gof.names <- c(gof.names, "DF")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, nobs(s))
    gof.names <- c(gof.names, "Num.\\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  besidereq <- nrow(s@coef3) > 
    length(s@extra$colnames.y) - 1 + length(all.vars(s@terms$terms)[-1])
  
  if (beside == TRUE & besidereq==TRUE) {
    trlist <- list()
    
    respcol <- s@extra$colnames.y
    if (is.na(resp.names)) resp.names <- respcol
    if (length(resp.names)!=length(respcol)) {
      warning("resp.names length does not match with number of response categories")
      resp.names <- respcol
    }
    
    for (i in 1:(length(respcol)-1)) {
      names <- rownames(coef(s))
      resploc <- grep(paste0(":",respcol[i],"$"),names)
      names <- gsub(paste0(":",respcol[i],"$"),"",names[resploc])
      co <- s@coef3[resploc, 1]
      se <- s@coef3[resploc, 2]
      pval <- s@coef3[resploc, 4]
      if (i==1) {
        tr <- createTexreg(coef.names = names, coef = co, se = se, 
                           pvalues = pval, gof.names = gof.names, 
                           gof = gof, gof.decimal = gof.decimal,
                           model.name = paste(resp.names[i],resp.names[i+1],sep="|"))
      } else {
        tr <- createTexreg(coef.names = names, coef = co, se = se, 
                           pvalues = pval, gof.names = character(), 
                           gof = numeric(), gof.decimal = logical(),
                           model.name = paste(resp.names[i],resp.names[i+1],sep="|"))
      }
      trlist <- c(trlist, tr)
    }
    if (length(trlist) == 1) {
      return(trlist[[1]])
    }
    else {
      return(trlist)
    }
  }
  else {
    names <- rownames(coef(s))
    co <- s@coef3[, 1]
    se <- s@coef3[, 2]
    pval <- s@coef3[, 4]
    tr <- createTexreg(coef.names = names, coef = co, se = se, 
                       pvalues = pval, gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  
}
setMethod("extract", signature = className("vglm"), definition = extract.vglm)

# The only difference between vglm & polr is the sign of threshold 
goltab1 <- htmlreg(list(ol1,ol3a), 
                   include.thresholds=TRUE,
                   include.deviance = FALSE,
                   single.row=TRUE, 
                   custom.model.names = c("polr","vglm"),
                   custom.coef.names = c("Ideology","Income","Education",
                                         "Independent", "Republican",
                                         "Cue Reception", "Republican Cue",
                                         "Threshold: Strongly|Somewhat Oppose",
                                         "Threshold: Somewhat Oppose|Support",
                                         "Threshold: Somewhat|Strongly Support",
                                         "Threshold: Strongly|Somewhat Oppose",
                                         "Threshold: Somewhat Oppose|Support",
                                         "Threshold: Somewhat|Strongly Support"),
                   caption = "Comparing Ordered Logit from polr and vglm",
                   caption.above = TRUE)

#+ results="asis"
goltab1

#+
# After relaxing the proportional odds assumption, 
# income has statistically significant impact only on 3|4
# (The movement from Somewhat Support to Strongly Support)
goltab2 <- htmlreg(list(ol3b),
          single.row = TRUE,
          custom.coef.names = c("(Threshold)",
                  "Ideology","Income","Education",
                  "Independent", "Republican",
                  "Cue Reception", "Republican Cue"),
          custom.note = "%stars. \nResponse ranges from 1 = Strongly Oppose to 4 = Strongly Supoport.",
          caption = "Generalized Ordered (Cumulative) Logit from vglm",
          caption.above = TRUE)

#+ results="asis"
goltab2

#' Examples for Poisson Regression and Negative Binomial Regression 
#' will be discussed next week.

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('TA_session_051619.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('TA_session_051619.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_051619.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_051619.R', 'pdf_document')"
