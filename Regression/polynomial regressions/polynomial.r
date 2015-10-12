Formaldehyde

# line
summary(lm1 <- lm(optden ~ 1 + carb, Formaldehyde))
(summary(lm1a <- lm(optden ~ 0 + carb,Formaldehyde)))
anova(lm1a,lm1)

# polynomial
coef(summary(lm1b <- lm(optden ~ 1 + carb + I(carb^2), Formaldehyde)))
coef(summary(lm1c <- lm(optden ~ poly(carb, 2), Formaldehyde)))


# log
coef(summary(lm2a <- lm(log(Volume) ~ log(Girth) + log(Height), trees)))

# V = k* d^2 *h
confint(lm2a)

## log(Vi )=b0+2*log(Gi )+ 1*log(Heighti )+e
lm2b <- lm(log(Volume) ~ 1 + offset(2*log(Girth) + log(Height)), trees)
#lm((log(Volume)-(2*log(Girth) + log(Height)))~1,data=trees)
anova(lm2b,lm2a)
# The large p-value indicates that the more complex model (generalvalues of the coefficients for log(Girth) and log(Height)) does
# not fit significantly better than the simpler model (assuming2*log(Girth) + log(Height)), thus we prefer the simpler
# model.

# sum(resid(lm2a)^2)
