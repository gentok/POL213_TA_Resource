## Plot Multinomial-Logit Predicted Probabilities for 
## Continuous Predictors.

# Assuming that you have run TA_session_050219_ws.R until Line 363.

# Using "Not Treated" Profile as Baseline
profile0

# Move Hispanic Proportion from 25-75 percentile.
quantile(d$pprhispx, probs=c(0.25,0.75), na.rm=TRUE)
# 25% ~= 15 & 75% ~= 70
(pprhispxvals <- seq(15,70, length=6)) # Arbitrary chose 6 values to predict

# Create New Profiles
profile0_15 <- profile0_26 <- 
profile0_37 <- profile0_48 <- 
profile0_59 <- profile0_70 <- profile0
# Replace pprhispx values (the third value)
profile0_15[3] <- 15
profile0_26[3] <- 26
profile0_37[3] <- 37
profile0_48[3] <- 48
profile0_59[3] <- 59
profile0_70[3] <- 70

# Prediction
pred0_15 <- predictmlogit(m2, profile0_15)
pred0_26 <- predictmlogit(m2, profile0_26)
pred0_37 <- predictmlogit(m2, profile0_37)
pred0_48 <- predictmlogit(m2, profile0_48)
pred0_59 <- predictmlogit(m2, profile0_59)
pred0_70 <- predictmlogit(m2, profile0_70)

# Plot Prediction
preddt <- rbind(pred0_15,pred0_26,pred0_37,
                pred0_48,pred0_59,pred0_70)
preddt$pprhispx <- rep(pprhispxvals, each=nrow(pred0_15))

# Plot
ggplot(preddt, aes(x=pprhispx, y=mean)) + 
  geom_line(aes(color=choice)) + 
  geom_ribbon(aes(ymin=lci,ymax=uci, fill=choice), alpha=0.5) + 
  theme_bw() + xlab("Proportion of Hispanic Population") + 
  ylab("Predicted Probability") + 
  labs(caption="Other variables are fixed at median and untreated.") + 
  theme(legend.position="bottom")
