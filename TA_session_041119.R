#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "February 27, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



y <- rnorm(100, mean = 72, sd = 2.5)



## Required Package
library(readstata13)
library(haven)

#'
#' # Practice of Analysis
#'
#' 1. Download QOG Basic Data from https://qog.pol.gu.se/data/datadownloads/qogbasicdata


dtaloc <- "http://www.qogdata.pol.gu.se/data/qog_bas_cs_jan19.dta"
#dtaloc <- "D:/BoxSync/Data/QOG/basic/qog_bas_cs_jan19.dta"

d <- read_dta(dtaloc)

#' 2. Relevant Variables

nd <- data.frame(id = d$cname)
rownames(nd) <- d$cname

# bribe incidence (DV)
nd$bribe <- d$wdi_bribfirm
summary(nd$bribe)

# Ethnic Fractionalization
nd$efrac <- d$al_ethnic
summary(nd$efrac)

# Seconday Education Enrollment 
nd$eduyr <- d$wdi_ners
summary(nd$eduyr)

# GDP per Capita
nd$gdp <- d$mad_gdppc
summary(nd$gdp)

# omit Missing Cases
nd <- na.omit(nd)


#' 3. Standardization
#' 
#' Standardize each variable in "nd" dataset. Interpret what each means.
#'
 
# Standardizing 
nd$bribe.rs <- scale(nd$bribe)
summary(nd$bribe.rs)
# Does the same thing.
nd$bribe.rs <- (nd$bribe - mean(nd$bribe))/sd(nd$bribe)
summary(nd$bribe.rs)

# Other variables
nd$efrac.rs <- scale(nd$efrac)
nd$eduyr.rs <- scale(nd$eduyr)
nd$gdp.rs <- scale(nd$gdp)

#' 4. Run OLS
#' 
#' Run ols model with bribe as dependent variable

m <- lm(bribe.rs ~ efrac.rs + eduyr.rs + gdp.rs, data=nd)

#' 5. Find Influencial Cases
#' 
#' Find influencial cases using:
#' - Studentized residuals
#' - hat value
#' - Cook's distance

# Studentized Residual
influenceIndexPlot(m, vars="Studentized")

# Hat Value
influenceIndexPlot(m, vars="hat")

# Cook's D
influenceIndexPlot(m, vars="cook")

# All in One
influencePlot(m)

#'
#' Kyrgyztan, Bangladesh, Swaziland, and Sweden comes up as outliers that 
#' are influencial in predictions.
#'
#' 6. Assess Models
#' 
#' Assess if, OLS model you estimated has: 
#' - non-normal errors
#' - heteroskedasticity
#' - multicollinearlity
#' 

# Check Non-normality of Residuals
qqPlot(m)

#' The error distribution is not normal. It is right skewed. 
#' Because, for the right side, observed errors (studentized residuals) 
#' tend to have larger values than the thoeretical errors that is normally distributed.
#' You can see that in the following graph:

require(MASS)
require(ggplot2)

sr <- studres(m) # Observed Studentized Residuals
tr <- dt(seq(-2, 3.1, length=512), m$df.residual) # Theoretical Density
nn <- data.frame(x = rep(seq(-2, 3.1, length=512), 2),
                 y=c(density(sr, from=-2, to=3.1)$y, tr),
                 lab = rep(c("Observed","Theory"), each=512))
ggplot(nn, aes(x=x,y=y)) + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=lab), alpha=0.7) + 
  geom_line(aes(color=lab), size=1) +
  scale_color_brewer(name="Studentized Residual", type="qual", palette=2) + 
  scale_fill_brewer(name="Studentized Residual", type="qual", palette=2) + 
  xlab("T-values") + ylab("Density") +
  ggtitle("The Distribution of Observed Studentized Residual is Right Skewed \nCompared to Theoretical T Distribution") + 
  theme_bw()

# Check Heteroskedasticity
residualPlots(m, ~1, fitted=TRUE, tests=FALSE)

#' There is heteroskedasticity, The variance of residuals gets 
#' larger as the predicted value of Y increases.

# Check Non-linearlity
crPlots(m, order=2)

#' For GDP, there is a sign that the relationship with bribe 
#' is non-linear.

# Check Multicollinearity
vif(m)

#' No variable has a excessive value of VIF.

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
#rmarkdown::render('TA_session_022719.R', 'pdf_document')
#rmarkdown::render('TA_session_022719.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_022719.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_022719.R', 'pdf_document')"
