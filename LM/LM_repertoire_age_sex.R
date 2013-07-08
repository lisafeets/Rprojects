#load and attach data
gt=read.table("gesturetokens.txt", h=T)
attach(gt)
gt
        sig token types obs rep repoverobs typesoverobs  age    sex
1     Brian    15     9  37   4 0.10810811    0.2432432 23.0   male
2  Claudine   139    29  98  23 0.23469388    0.2959184  9.0 female
3    Deidre   118    31  61  21 0.34426229    0.5081967  8.0 female
4    Elikia    91    17  43  12 0.27906977    0.3953488 12.0 female
5     Faith   201    37  79  26 0.32911392    0.4683544  7.0 female


rfmod=lm(gt$repoverobs~gt$age*gt$sex)
summary(rfmod)

#Call:
#lm(formula = gt$typesoverobs ~ gt$age * gt$sex)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.27207 -0.05977 -0.01097  0.11185  0.26973 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.391689   0.077956   5.025 0.000233 ***
#gt$age            -0.005026   0.004533  -1.109 0.287669    
#gt$sexmale        -0.414774   0.144030  -2.880 0.012899 *  
#gt$age:gt$sexmale  0.025096   0.006986   3.592 0.003280 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.1606 on 13 degrees of freedom
#Multiple R-squared:  0.5506,	Adjusted R-squared:  0.4469 
#F-statistic:  5.31 on 3 and 13 DF,  p-value: 0.0131


##plot fitted vs residuals
rfmod_resid <- residuals(rfmod)
rfmod_fitted <- fitted(rfmod)
plot(rfmod5_fitted, rfmod_resid, xlab = "Fitted values",ylab = "Residuals", type = "n",ylim = max(abs(rfmod_resid)) * c(-1, 1))
abline(h = 0, lty = 2)
text(rfmod5_fitted, rfmod_resid, labels = rownames(gt))

##scatter plot repoverobs x age, points colored by sex, method=lm draws lines for the 2 subsets of data
##this ggplot2 code creates a black and white graph with differntly shpaed points and lines
library(ggplot2)
ggplot(gt, aes(x=repoverobs, y=age, linetype=sex, shape=sex)) + geom_point(color=1, size=3) +scale_colour_hue(l=50) + geom_smooth(method=lm, se=FALSE, colour=1)+ ggtitle("Linear model for individual repertoire size as affected by age and sex")+ xlab("Individual Repertoire") +
   ylab("Age")

##test model for regression assumptions

library(gvlma)
gvmodel = gvlma(rfmod) 
summary(gvmodel)

#Call:
#lm(formula = gt$repoverobs ~ gt$age * gt$sex)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.12161 -0.05347 -0.01940  0.08056  0.12593 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.275546   0.043660   6.311  2.7e-05 ***
#gt$age            -0.003856   0.002539  -1.519  0.15277    
#gt$sexmale        -0.286456   0.080665  -3.551  0.00355 ** 
#gt$age:gt$sexmale  0.011356   0.003913   2.902  0.01235 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.08995 on 13 degrees of freedom
#Multiple R-squared:  0.4931,	Adjusted R-squared:  0.3761 
#F-statistic: 4.215 on 3 and 13 DF,  p-value: 0.02746


#ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
#USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#Level of Significance =  0.05 
#
#Call:
# gvlma(x = rfmod4) 
#
#                     Value p-value                   Decision
#Global Stat        5.22449 0.26503    Assumptions acceptable.
#Skewness           0.06393 0.80039    Assumptions acceptable.
#Kurtosis           0.87825 0.34868    Assumptions acceptable.
#Link Function      4.15094 0.04161 Assumptions NOT satisfied!
#Heteroscedasticity 0.13137 0.71702    Assumptions acceptable.



##qqplot for non-normality
qqPlot(rfmod, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(rfmod) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)