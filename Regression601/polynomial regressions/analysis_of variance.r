# one-factor
## Yi,j = Mi(means) +Ei,j  
## Yi,j = M + Ai(effects) + Ei,j  
class(InsectSprays$spray)

summary(av1 <- aov(sqrt(count) ~ spray, InsectSprays))#spray is a factor/treatment

model.tables(av1, type="means")
coef(lm(sqrt(count) ~ spray -1, InsectSprays))

model.tables(av1)#effect  
(coef(lm(sqrt(count) ~ spray -1, InsectSprays)))-mean((coef(lm(sqrt(count) ~ spray -1, InsectSprays))))


# multi-factor  
(OrchardSprays)#“Latin square” design,
# two blocking factors, rowpos and
# colpos, and one experimental factor, treatment
summary(av2 <- aov(decrease ~ factor(rowpos) + factor(colpos) + treatment, OrchardSprays))#ANOVA table 

## 2-factor 
## Yijk = Mij +Eijk
## Yijk =M +Ai +Bj +Eijk
## Yijk =M +Ai +Bj +(AB)ij +Eijk

## Two-way anova with replications
str(warpbreaks);head(warpbreaks)
xtabs(~ wool + tension, warpbreaks)
summary(av3 <- aov(breaks ~ wool * tension, warpbreaks))
summary(av3a <- aov(1/breaks ~ wool * tension,warpbreaks))#reciprocal transformation



