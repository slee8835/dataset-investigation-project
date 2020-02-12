setwd("c:/USF MSHI/HS 631/Final project")
load(file = "births.Rdat")

library(ggplot2)
library("pwr")
library(corrplot)
library("rgl")
library("pscl")
library("car")

##pt1
summary(births)

##impossible data that need cleaning: dad_age, len_preg, num_visits, mom_wt_gain
births$dad_age[births$dad_age == 999] <- NA
births$len_preg[births$len_preg == 999] <- NA
births$num_visits[births$num_visits == 999] <- NA
births$mom_wt_gain[births$mom_wt_gain == 999] <- NA

#looking into rows that have "unknown" levels 
births[births$smoke == "unknown", ]
#    dad_age mom_age maturity len_preg is_premie num_visits marital mom_wt_gain  bwt low_bwt    sex   smoke mom_white mom_age_level
#988      NA      41  advnced       NA   unknown         NA unknown          NA 3.63     low female unknown     white           35+
##omitting this data entry because it has lots of NA's and unknonwn
births <- births[-988, ]

births[births$mom_white == "unknown", ]  
births[births$is_premie == "unknown", ]

##summary statistics 
summary(births)
range(births$dad_age, na.rm = TRUE)       ##14 55
range(births$mom_age, na.rm = TRUE)       ##13 50
mean(births$mom_age, na.rm = TRUE)        ##26.98599
range(births$len_preg, na.rm = TRUE)      ##20 45
mean(births$len_preg, na.rm = TRUE)       ##38.33467
range(births$num_visits, na.rm = TRUE)    ## 0 30
range(births$mom_wt_gain, na.rm = TRUE)   ## 0 85
range(births$bwt, na.rm = TRUE)           ## 1.00 11.75
mean(births$bwt, na.rm = TRUE)            ##7.104474
IQR(births$bwt)                           ##1.68
quantile(births$bwt)                      ##   0%   25%   50%   75%  100% 
                                          ##1.00  6.38  7.31  8.06 11.75 

#dropping unknown levels unknown
is_premie_level <- c("fullterm", "premie")
births$is_premie <- factor(births$is_premie, levels = is_premie_level)
marital_level <- c("married", "unmarried")
births$marital <- factor(births$marital, levels = marital_level)
smoke_level <- c("nonsmoker", "smoker")
births$smoke <- factor(births$smoke, levels = smoke_level)
mom_white_level <- c("nonwhite", "white")
births$mom_white <- factor(births$mom_white, levels = mom_white_level)
summary(births)

#change levels for maturity and mom_age_level
maturity_level <- c("younger", "advnced")
births$maturity <- factor(births$maturity, levels = maturity_level)
age_level <- c("teens", "early20s", "late20s", "early30s", "35+")
births$mom_age_level <- factor(births$mom_age_level, levels = age_level)

##create appropriate plots for each variable
#dad's age
hist(births$dad_age, breaks = 50, main = "Dad's Age Distribution", 
     xlab = "dad age (years)", ylab = "Frequency", col = "green")

#mom's age
hist(births$mom_age, breaks = 50, main = "Mom's Age Distribution", 
     xlab = "mom age (years)", ylab = "Frequency", col = "sky blue")

#maturity
plot(births$maturity, main = "Mom's maternal age classification", 
     xlab = "mom's maternal age", ylab = "Frequency", col = "pink")

#pregnancy length
hist(births$len_preg, breaks = 20, main = "Pregnancy Length Distribution", 
     xlab = "length of pregnancy (weeks)", ylab = "Frequency", col = "yellow")

#is_premie
plot(births$is_premie, main = "Baby's maturity classification", 
     xlab = "baby's maturity", ylab = "Frequency", col = "light blue")

#num_visits
hist(births$num_visits, main = "Number of Hospital Visits Distribution", 
     xlab = "Number of Visits", ylab = "Frequency", col = "gray")

#marital
plot(births$marital, main = "Mom's Marital Status Classification", 
     xlab = "Mom's marital status", ylab = "Frequency", col = "maroon")

#mom_wt_gain
hist(births$mom_wt_gain, breaks = 20, main = "Mom's Weight Gain Distribution", 
     xlab = "Weight Gaine (pounds)", ylab = "Frequency", col = "purple")

#bwt
hist(births$bwt, breaks = 20, main = "Baby's Birth Weight Distribution", 
     xlab = "birth weight (pounds)", ylab = "Frequency", col = "orange")

#low_bwt
plot(births$low_bwt, main = "Baby's Low Birth Weight Classification", 
     xlab = "Birth Weight Classfication", ylab = "Frequency", col = "orange")

#sex
plot(births$sex, main = "Baby's Gender Distribution", xlab = "baby's gender", 
     ylab = "Frequency", col = "light green")

#smoke
plot(births$smoke, main = "Smoking Status of Mom Distribution", 
     xlab = "smoking status of mom", ylab = "Frequency", col = "gray")

#mom_white
plot(births$mom_white, main = "Mom's Racial Status Distribution", 
     xlab = "Mom's racial classification", ylab = "Frequency", col = "white")

#mom_age_level
plot(births$mom_age_level, main = "Mom's Age Level Distribution", 
     xlab = "mom's age level", ylab = "Frequency", col = "sky blue")

plot(births)

##bivariate and trivariate relationships 

#bwt and len_preg colored by low_bwt shows a nice correlation between two variables and
#cutoff of low vs not low bwt
g <- ggplot(data = births, aes(x = len_preg, y = bwt, color = low_bwt))
g + geom_point() + labs(title="Baby's birthweight and length of pregnancy colored by low_bwt",
                        x = "length of pregnancy (weeks)", y="baby birthweight (pounds)")

#bwt and len_preg colored by is_premie shows a nice correlation between two variables and
#cutoff a baby maturity
g2<- ggplot(data = births, aes(x = len_preg, y = bwt, color = is_premie))
g2 + geom_point() + labs(title="Baby's birthweight and length of pregnancy colored by is_premie",
                        x = "length of pregnancy (weeks)", y="baby birthweight (pounds)")

#no strong correlation between mom_wt_gain and bwt observed, gender of baby doesn't seem to 
#matter either
g3 <- ggplot(data = births, aes(x = mom_wt_gain, y = bwt, color = sex)) 
g3 + geom_point() + labs(title="Baby's birthweight and mom's weight gain during pregnancy colored by baby's gender",
                        x = "mom's weight gain", y="baby birthweight (pounds)")

#no obvious difference in bab'y bwt between younger and advanced mom age
g4 <- ggplot(data = births, aes(x = maturity, y = bwt, color = maturity)) 
g4 + geom_boxplot() + labs(title="Baby's birthweight and mom's age maturity",
                           x = "mom's age maturity", y="baby birthweight (pounds)")

#bwt and mom's age level colored by smoking status of mom- in older age level, smoking 
#moms have lower bwt compared to non-smoking moms in the same age level
g5 <- ggplot(data = births, aes(x = mom_age_level, y = bwt, color = smoke))
g5 + geom_boxplot() + labs(title="Baby's birthweight and mom's age level for smoke/non-smoker moms",
                           x = "mom's weight gain", y="baby birthweight (pounds)")

#marital status and mom's age level- for younger mom's, most are not married vs for older
#mom's majority are married
g6 <- ggplot(data = births, aes(x = mom_age_level, y = marital, fill = marital))
g6 + geom_bar(position="stack", stat = "identity") + 
        labs(title="Mom's marital status and age level", 
             x = "Mom's age level", y="marital status")

#low birthweight status and pregnancy length- most babies with not low bwt status had
#approximately 40 weeks pregnancy length
g7 <- ggplot(data = births, aes(x = low_bwt, y = len_preg, color = low_bwt))
g7 + geom_boxplot() + labs(title="Baby's birthweight status and gestation length",
                           x = "low birthweight status", y="length of pregnancy (weeks)")

##correlations 
#create new dataframe that holds only numeric variables 
births_num <- births[-c(3, 5, 7, 10, 11, 12:15)]
#change all factor variables to numeric 
births_num$maturity <- as.numeric(births$maturity)
births_num$is_premie <- as.numeric(births$is_premie)
births_num$marital <- as.numeric(births$marital)
births_num$low_bwt <- as.numeric(births$low_bwt)
births_num$sex <- as.numeric(births$sex)
births_num$smoke <- as.numeric(births$smoke)
births_num$mom_white <- as.numeric(births$mom_white)
births_num$mom_age_level <- as.numeric(births$mom_age_level)
#use spearman method when calculate correlation since our dataframe now has ranks 
birth_cor <- cor(births_num, use = "pairwise.complete.obs", method = "spearman")
birth_cor
#highly positively correlated: 
#1. mom age and dad age, dad age and maturity, dad age and mom age level (these are essentially the same 
#   because mom age, maturity and mom age level are the same)
#2. mom age and maturity, mom age and mom age level
#3. len_preg and bwt, len_pre and low_bwt
#4. bwt and low_bwt 
#highly negatively correlated:
#1. dad age and marital
#2. mom age and marital
#3. len_preg and is_premie
#4. bwt and is_premie
corrplot(birth_cor)

##statistical tests
t.test(births$bwt ~ births$sex)
#null hypothesis: there's no difference in bwt between female and male babies
#	Welch Two Sample t-test
#data:  births$bwt by births$sex
#t = -4.1483, df = 995.3, p-value = 3.635e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -0.5775921 -0.2066212
#sample estimates:
#mean in group female   mean in group male 
#    6.909402             7.301509 
#p-value proves data is statistically significant, reject null hypothesis

t.test(births$bwt ~ births$maturity)
#null hypothesis: there is no difference in bwt between younger and more advanced moms
#	Welch Two Sample t-test
#data:  births$bwt by births$maturity
#t = -0.36265, df = 165.51, p-value = 0.7173
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -0.3532357  0.2436085
#sample estimates:
#mean in group younger mean in group advnced 
#    7.097232              7.152045 
#large p-value, CI includes 0, don't reject null hypothesis

t.test(births$len_preg ~ births$low_bwt)
#null hypothesis: there is no difference in length of pregnancy between babies with 
#normal and low bwt 
#Welch Two Sample t-test
#data:  births$len_preg by births$low_bwt
#t = -12.188, df = 113.43, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-6.411808 -4.618782
#sample estimates:
#mean in group low mean in group notlow 
#    33.42727             38.94257 
#low p-value and CI doesn't include 0, reject null hypothesis

t.test(births$mom_age ~ births$low_bwt)
#there's no difference in mom's age for babies qith low or normal bwt
#	Welch Two Sample t-test
#data:  births$mom_age by births$low_bwt
#t = -0.25147, df = 132.9, p-value = 0.8018
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-1.490649  1.154378
#sample estimates:
#mean in group low mean in group notlow 
#    26.83636             27.00450 
#high p-value and CI includes 0, don't reject null hypothesis

t.test(births$bwt ~ births$mom_white)
#null hypothesis: there's no differnece in bwt between white and non-white moms
#	Welch Two Sample t-test
#data:  births$bwt by births$mom_white
#t = -4.8711, df = 467.28, p-value = 1.522e-06
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-0.7522261 -0.3197694
#sample estimates:
#mean in group nonwhite    mean in group white 
#    6.719542               7.255540
#low p-value and CI don't include 0, reject null hypothesis

t.test(births$bwt ~ births$smoke)
#null hypothesis: no difference in birthweight between smoking and non smoking moms
#	Welch Two Sample t-test
#data:  births$bwt by births$smoke
#t = 2.359, df = 171.32, p-value = 0.01945
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#0.05151165 0.57957328
#sample estimates:
#mean in group nonsmoker    mean in group smoker 
#    7.144273                6.828730 
#small p-value and CI doesn't include 0 - reject null hypothesis 

wilcox.test(births$bwt ~ births$smoke)
#	Wilcoxon rank sum test with continuity correction
#data:  births$bwt by births$smoke
#W = 63382, p-value = 0.005622
#alternative hypothesis: true location shift is not equal to 0
#reject null hypothesis

t <- table(births$marital, births$mom_age_level)
chisq.test(t)
#	Pearson's Chi-squared test

#data:  t
#X-squared = 228.64, df = 4, p-value < 2.2e-16


fit1 <- lm(bwt ~ mom_age_level, data = births)
anova(fit1)
#Analysis of Variance Table
#Response: bwt
#Df  Sum Sq Mean Sq F value Pr(>F)
#mom_age_level   4   15.05  3.7615  1.6637 0.1562
#Residuals     994 2247.28  2.2608  

##power
#summary(births$sex)
pwr.t2n.test(n1 = 502, n2 = 497, d = .2, sig.level = .05)
#     t test power calculation 
#       n1 = 502
#       n2 = 497
#       d = 0.2
#       sig.level = 0.05
#       power = 0.8844728
#       alternative = two.sided

#summary(births$maturity)
pwr.t2n.test(n1 = 132, n2 = 867, d = 0.2, sig.level = 0.05)
#     t test power calculation 
#       n1 = 132
#       n2 = 867
#       d = 0.2
#       sig.level = 0.05
#       power = 0.5709005
#       alternative = two.sided

#summary(births$low_bwt)
pwr.t2n.test(n1 = 110, n2 = 889, sig.level = 0.05, power = 0.8)
#     t test power calculation 
#     n1 = 110
#     n2 = 889
#     d = 0.2834073
#     sig.level = 0.05
#     power = 0.8
#     alternative = two.sided

#summary(births$smoke)
pwr.t2n.test(n1 = 110, n2 = 889, d = 0.2)
#     t test power calculation 
#       n1 = 110
#       n2 = 889
#       d = 0.2834073
#       sig.level = 0.05
#       power = 0.8
#       alternative = two.sided

pwr.chisq.test(w = 0.2, df = 4, N = 999)
#     Chi squared power calculation 
#     w = 0.2
#     N = 999
#     df = 4
#     sig.level = 0.05
#     power = 0.9998265
#NOTE: N is the number of observations

pwr.anova.test(k = 5, n = 200, f = 0.2)
#     Balanced one-way analysis of variance power calculation 
#     k = 5
#     n = 200
#     f = 0.2
#     sig.level = 0.05
#     power = 0.9998187
#NOTE: n is number in each group

##pt2
##linear fit
lmfit_all <- lm(bwt ~ ., data = births)
summary(lmfit_all)
plot(lmfit_all, 1)

#take out low_bwt, they are the same thing 
lmfit1 <- lm(bwt ~ dad_age + mom_age + len_preg + mom_wt_gain + maturity + 
                      is_premie + marital + sex + smoke + mom_white + mom_age_level 
              + num_visits, data = births)
summary(lmfit1)
plot(lmfit1, 1)

#take out num_visits 
lmfit2 <- lm(bwt ~ dad_age + mom_age + len_preg + mom_wt_gain + maturity + 
                   is_premie + marital + sex + smoke + mom_white + mom_age_level, 
                   data = births)
summary(lmfit2)

#take out mom_age_level
lmfit3 <- lm(bwt ~ dad_age + mom_age + len_preg + mom_wt_gain + maturity + 
                     is_premie + marital + sex + smoke + mom_white, data = births)
summary(lmfit3)

#take out maturity
lmfit4 <- lm(bwt ~ dad_age + mom_age + len_preg + mom_wt_gain + is_premie + 
                     marital + sex + smoke + mom_white, data = births)
summary(lmfit4)
plot(lmfit4, 1)

#take out mom_age
lmfit5 <- lm(bwt ~ dad_age + len_preg + mom_wt_gain + is_premie + marital + sex + 
                     smoke + mom_white, data = births)
summary(lmfit5)
plot(lmfit5, 1)

#take out dad_age
lmfit6 <- lm(bwt ~ len_preg + mom_wt_gain + is_premie + marital + sex + 
                     smoke + mom_white, data = births)
summary(lmfit6)
plot(lmfit6, 1)

#final linear regression model:
# bwt=0.28*len_preg+0.008*mom_wt_gain-0.52*is_premie-0.25*marital+
#0.40*sex-0.38*smoke+0.22*mom_white-4.10
#the final model seems a bit surprising for me for the fact that len_preg doesn't have 
#the largest fitted coefficient- I was expecting it to be the largest based on t-test 
#and the box plots I created prior. However, it is important to note that the adjusted 
#R-squared for the model is 0.49, which is not particularly high- indicating there's 
#a lot of variability that is not explained by this model

##logistic regression
births2 <- births
summary(births2)

getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}

#getting mean for numeric variables and mode for categorical variables
dad_age_mean <- mean(births2$dad_age, na.rm = TRUE)
len_preg_mean <- mean(births2$len_preg, na.rm = TRUE)
is_premie_md <- getmode(births2$is_premie)
num_visits_mean <- mean(births2$num_visits, na.rm = TRUE)
mom_wt_gain_mean <- mean(births2$mom_wt_gain, na.rm = TRUE)
mom_white_md <- getmode(births2$mom_white)

#imputing data- replacing NAs to mean or mode of the corresponding variable
births2$dad_age[is.na(births2$dad_age)] <- dad_age_mean
births2$len_preg[is.na(births2$len_preg)] <- len_preg_mean
births2$num_visits[is.na(births2$num_visits)] <- num_visits_mean
births2$mom_wt_gain[is.na(births2$mom_wt_gain)] <- mom_wt_gain_mean
births2$is_premie[is.na(births2$is_premie)] <- is_premie_md
births2$mom_white[is.na(births2$mom_white)] <- mom_white_md
summary(births2)

glmfit_all <- glm(low_bwt ~ ., family = binomial(),data = births2)
#Warning messages:
#1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#bwt is in the equation, casuing problems
summary(glmfit_all)
pR2(glmfit_all)
#          llh       llhNull            G2      McFadden          r2ML          r2CU 
#-2.271326e-07 -3.463987e+02  6.927975e+02  1.000000e+00  5.793680e-01  1.000000e+00 

#take out bwt
glmfit_1 <- glm(low_bwt ~ mom_age + maturity + len_preg + is_premie + num_visits + marital + 
                        mom_wt_gain + sex + smoke + mom_white + mom_age_level, family = binomial(), data = births2)
summary(glmfit_1)
pR2(glmfit_1)

glmfit_2 <- glm(low_bwt ~ mom_age + maturity + len_preg + is_premie + num_visits + marital + 
                        mom_wt_gain + sex + smoke + mom_white, family = binomial(), data = births2)
summary(glmfit_2)

glmfit_3 <- glm(low_bwt ~ mom_age + maturity + len_preg + is_premie + marital + mom_wt_gain + 
                        sex + smoke + mom_white, family = binomial(), data = births2)
summary(glmfit_3)

glmfit_4 <- glm(low_bwt ~ mom_age + maturity + len_preg + marital + mom_wt_gain + sex + smoke + 
                        mom_white, family = binomial(), data = births2)
summary(glmfit_4)

glmfit_5 <- glm(low_bwt ~ mom_age + maturity + len_preg + marital + mom_wt_gain + smoke + 
                        mom_white, family = binomial(), data = births2)
summary(glmfit_5)

glmfit_6 <- glm(low_bwt ~ len_preg + marital + smoke, family = binomial(), data = births2)
summary(glmfit_6)

#final logistic regression model 
glmfit_7 <- glm(low_bwt ~ len_preg + marital, family = binomial(), data = births2)
summary(glmfit_7)
pR2(glmfit_7)
#         llh      llhNull           G2     McFadden         r2ML         r2CU 
#-198.0756837 -346.3987413  296.6461151    0.4281859    0.2569137    0.5136508 
exp(coef(glmfit_7))
#for every week inrease in length of pregnancy, the chance of getting a baby with not 
#low birth weight is multiplied by 2.02


