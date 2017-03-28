###########################
#data analyses for a sample of 2500 loans, find a relation about Interes rate vs another varibles 
###########################

loansData<-read.csv("loansData.csv",header=T)
loansData

names(loansData) #variables
tolower(names(loansData))

#naming & munging variables
Amount.Requested<-loansData$Amount.Requested 
Amount.Requested  #numerical positive

Amount.Funded.By.Investors<-loansData$Amount.Funded.By.Investors 
Amount.Funded.By.Investors  #numerical positive

Interest.Rate<-loansData$Interest.Rate 
InterestRate<-as.numeric(sub("%","",Interest.Rate))
Irate<-InterestRate/100    
max(Irate)
min(Irate)
Irate #shows in percentage

LoanL<-loansData$Loan.Length
LoanLength<-as.numeric(sub("months","",Loan.Length))
loansData$LoanL[LoanLength==60]<-1
loansData$LoanL[LoanLength==36]<-0
LoanL<-loansData$LoanL
sum(LoanL==1) 
sum(LoanL==0)
LoanL... #categorical variable 1-60 months, 0-36 months  had change

Loan.Purpose<-loansData$Loan.Purpose
unique(sort(Loan.Purpose)) 
#14 levels of purpose

Debt.To.Income.Ratio<-loansData$Debt.To.Income.Ratio
DebtToIncomeRatio<-as.numeric(sub("%","",Debt.To.Income.Ratio))
dtiRate<-DebtToIncomeRatio/100    
dtiRate #shows in percentage

State<-loansData$State
State  #maybe categirize by area

Home.Ownership<-loansData$Home.Ownership
unique(sort(Home.Ownership)) 
loansData$HomeO[Home.Ownership=="NONE"]<-0
loansData$HomeO[Home.Ownership=="MORTGAGE"]<-1
loansData$HomeO[Home.Ownership=="OTHER"]<-2
loansData$HomeO[Home.Ownership=="OWN"]<-3
loansData$HomeO[Home.Ownership=="RENT"]<-4
HomeO<-loansData$HomeO
sum(HomeO==0) 
sum(HomeO==1)
sum(HomeO==2) 
sum(HomeO==3)
sum(HomeO==4)
HomeO  #possible to do proportions

Monthly.Income<-loansData$Monthly.Income
Monthly.Income  #numeric

FICO.Range<-loansData$FICO.Range
FICORange<-as.numeric(FICO.Range)
sort(unique(FICORange))
sFICO<-FICORange/max(FICORange)
sFICO  #score fico 1 to 100%, numerical

Open.CREDIT.Lines<-loansData$Open.CREDIT.Lines
Open.CREDIT.Lines #integer
sort(unique(Open.CREDIT.Lines))

Revolving.CREDIT.Balance<-loansData$Revolving.CREDIT.Balance
Revolving.CREDIT.Balance  #numerical

Inquiries.in.the.Last.6.Months<-loansData$Inquiries.in.the.Last.6.Months
Inquiries.in.the.Last.6.Months  #integer

Employment.Length<-loansData$Employment.Length
sort(unique(as.numeric(Employment.Length))) #12 levels

#interest variables 
Amount.Requested #cont
Amount.Funded.By.Investors #cont
Irate #% variable we want try
dtiRate #%
LoanL #cat 2 levels
State #cat 46 levels
Loan.Purpose #14 levels
Home.Ownership #cat
Monthly.Income #cont
sFICO #%
Open.CREDIT.Lines #cat 30levels
Revolving.CREDIT.Balance #cont 
Inquiries.in.the.Last.6.Months #cat
Employment.Length #cat

###########################
#parametric regression 0 whole variables -> Irate=Amount.Requested+Amount.Funded.By.Investors+dtiRate+LoanL+State+Loan.Purpose+Home.Ownership+Monthly.Income+sFICO+Open.CREDIT.Lines+Revolving.CREDIT.Balance+Inquiries.in.the.Last.6.Months+Employment.Length
###########################

#fitting model
lm0 <- lm(Irate~Amount.Requested+Amount.Funded.By.Investors+dtiRate+LoanL+State+Loan.Purpose+Home.Ownership+Monthly.Income+sFICO+Open.CREDIT.Lines+Revolving.CREDIT.Balance+Inquiries.in.the.Last.6.Months+Employment.Length)  
#get off->dtiRate, State, revolving credit balance

# Stepwise Regression
step0 <- stepAIC(lm0, direction="both")
step0$anova # display results
#Irate ~ Amount.Requested + Amount.Funded.By.Investors + LoanL + Loan.Purpose + Home.Ownership + Monthly.Income + sFICO + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months

# All Subsets Regression
leaps0<-regsubsets(Irate~Amount.Requested + Amount.Funded.By.Investors + LoanL + Loan.Purpose + Home.Ownership + Monthly.Income + sFICO + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months,nbest=10,data=loansData)
# view results 
summary(leaps0)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps0,scale="r2")

#model upgrade
lm00 <- lm(Irate~ Amount.Funded.By.Investors + LoanL + Loan.Purpose + Monthly.Income + sFICO + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months)
# Calculate Relative Importance for Each new Predictor
calc.relimp(lm00,c("lmg", "last", "first"),rela=TRUE)

s.lm00<-summary(lm00) #summary and significance
coef00<-lm00$coefficients #regression coefficents
compareCoefs(lm00)
vcov(lm00) #covariance matrix
par(mfrow=c(2,2))
plot(lm00,main="Regression",col=4) #weight tails distribution
mmps(lm00)#marginal model plots


#outliers
outlier.test(lm00,cutoff=Inf,n.mx=Inf) 
par(mfrow=c(1,1))
qqPlot(lm00, main="QQ Plot")
leveragePlots(lm00)

#influential observations
par(mfrow=c(3,3))
avPlots(lm00) #added variable plot
cutoff00 <- 4/((nrow(loansData)-length(lm00$coefficients)-2)) 
par(mfrow=c(1,1))
plot(lm00, which=4, cook.levels=cutoff00)  #identify values
influencePlot(lm00,id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#non normality errors
sres.lm00<-studres(lm00)  #distribution of studentized residuals
hist(sres.lm00, freq=FALSE, main="Distribution of Studentized Residuals")
xlm00<-seq(min(sres.lm00),max(sres.lm00)) 
ylm00<-dnorm(xlm00) 
lines(xlm00, ylm00)
shapiro.test(sres.lm00)  #residuals normality

#non constant error variance  Evaluate homoscedasticity
ncvTest(lm00) # non-constant error variance test
spreadLevelPlot(lm00)# plot studentized residuals vs. fitted values 

#evaluate multycollinearity
vif(lm00) # variance inflation factors 
sqrt(vif(lm00)) > 2 # problem?

# Evaluate Nonlinearity (ot clear)
crPlots(lm00) #component + residual plot 
ceresPlots(lm00)  # Ceres plots 

# Test for Autocorrelated Errors for evaluate non independence
durbin.watson(lm00)

#global results
gvlma(lm00)
s.lm00
res00<-lm00$resid #vector of residuals
lin00<-coef4[1]+coef4[2]*Amount.Requested+coef4[3]*Amount.Funded.By.Investors+coef3[4]*Monthly.Income+res4 #modelo (univarate)
par(mfrow=c(1,1))
plot(lin00,main="Fitted model")  #scatterplot
par(mfrow=c(3,3))
visreg(lm00, main="Regression") 

###########################
#parametric regression 1-> try one by one significant variables as, Amount.Funded.By.Investors + LoanL + Loan.Purpose + Monthly.Income + sFICO + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months
###########################

#fitting model
lm1a <- lm(Irate~ Amount.Funded.By.Investors)
lm1b <- lm(Irate~ LoanL)
lm1c <- lm(Irate~ Loan.Purpose)
lm1d <- lm(Irate~ Monthly.Income)
lm1e <- lm(Irate~ sFICO)
lm1f <- lm(Irate~ Open.CREDIT.Lines)
lm1g <- lm(Irate~ Inquiries.in.the.Last.6.Months)

#elements of model
s.lm1a<-summary(lm1a)
s.lm1b<-summary(lm1b)
s.lm1c<-summary(lm1c)
s.lm1d<-summary(lm1d)
s.lm1e<-summary(lm1e)
s.lm1f<-summary(lm1f)
s.lm1g<-summary(lm1g)
#regression coefficents
coef1a<-lm1a$coefficients
coef1b<-lm1b$coefficients
coef1c<-lm1c$coefficients
coef1e<-lm1d$coefficients
coef1f<-lm1f$coefficients
coef1g<-lm1g$coefficients
compareCoefs(lm1a)
compareCoefs(lm1b)
compareCoefs(lm1c)
compareCoefs(lm1d)
compareCoefs(lm1e)
compareCoefs(lm1f)
compareCoefs(lm1g)

par(mfrow=c(2,2))
plot(lm1a,main="Regression",col=1) 
plot(lm1b,main="Regression",col=1) 
plot(lm1c,main="Regression",col=1) 
plot(lm1d,main="Regression",col=1) 
plot(lm1e,main="Regression",col=1) 
plot(lm1f,main="Regression",col=1) 
plot(lm1g,main="Regression",col=1) 
mmps(lm1a)
mmps(lm1b)
mmps(lm1c)
mmps(lm1d)
mmps(lm1e)
mmps(lm1f)
mmps(lm1g)#marginal model plots

#outliers
par(mfrow=c(3,3))
qqPlot(lm1a, main="QQ Amount.Funded.By.Investors")
qqPlot(lm1b, main="QQ LoanL")
qqPlot(lm1c, main="QQ Loan.Purpose")
qqPlot(lm1d, main="QQ Monthly.Income")
qqPlot(lm1e, main="QQ sFICO")
qqPlot(lm1f, main="QQ Open.CREDIT.Lines")
qqPlot(lm1g, main="QQ Inquiries.in.the.Last.6.Months")
par(mfrow=c(3,3))
leveragePlots(lm1a)
leveragePlots(lm1b)
leveragePlots(lm1c)
leveragePlots(lm1d)
leveragePlots(lm1e)
leveragePlots(lm1f)
leveragePlots(lm1g)

#influential observations
par(mfrow=c(3,3))
cutoff1a <- 4/((nrow(loansData)-length(coef1a)-2)) 
plot(lm1a, which=4, cook.levels=cutoff1a)  #identify values
cutoff1b <- 4/((nrow(loansData)-length(coef1b)-2)) 
plot(lm1b, which=4, cook.levels=cutoff1b)  #identify values
cutoff1c <- 4/((nrow(loansData)-length(coef1c)-2)) 
plot(lm1c, which=4, cook.levels=cutoff1c)  #identify values
cutoff1d <- 4/((nrow(loansData)-length(coef1d)-2)) 
plot(lm1d, which=4, cook.levels=cutoff1d)  #identify values
cutoff1e <- 4/((nrow(loansData)-length(coef1e)-2)) 
plot(lm1e, which=4, cook.levels=cutoff1e)  #identify values
cutoff1f <- 4/((nrow(loansData)-length(coef1f)-2)) 
plot(lm1f, which=4, cook.levels=cutoff1f)  #identify values
cutoff1g <- 4/((nrow(loansData)-length(coef1g)-2)) 
plot(lm1g, which=4, cook.levels=cutoff1g)  #identify values

#non normality errors
par(mfrow=c(3,3))
sres.lm1a<-studres(lm1a)  #distribution of studentized residuals
hist(sres.lm1a, freq=FALSE, main="Histogram")
xlm1a<-seq(min(sres.lm1a),max(sres.lm1a)) 
ylm1a<-dnorm(xlm1a) 
lines(xlm1a, ylm1a)
sres.lm1b<-studres(lm1b)  #distribution of studentized residuals
hist(sres.lm1b, freq=FALSE, main="Histogram")
xlm1b<-seq(min(sres.lm1b),max(sres.lm1b)) 
ylm1b<-dnorm(xlm1b) 
lines(xlm1b, ylm1b)
sres.lm1c<-studres(lm1c)  #distribution of studentized residuals
hist(sres.lm1c, freq=FALSE, main="Histogram")
xlm1c<-seq(min(sres.lm1c),max(sres.lm1c)) 
ylm1c<-dnorm(xlm1c) 
lines(xlm1c, ylm1c)
sres.lm1d<-studres(lm1d)  #distribution of studentized residuals
hist(sres.lm1d, freq=FALSE, main="Histogram")
xlm1d<-seq(min(sres.lm1d),max(sres.lm1d)) 
ylm1d<-dnorm(xlm1d) 
lines(xlm1d, ylm1d)
sres.lm1e<-studres(lm1e)  #distribution of studentized residuals
hist(sres.lm1e, freq=FALSE, main="Histogram")
xlm1e<-seq(min(sres.lm1e),max(sres.lm1e)) 
ylm1e<-dnorm(xlm1e) 
lines(xlm1e, ylm1e)
sres.lm1f<-studres(lm1f)  #distribution of studentized residuals
hist(sres.lm1f, freq=FALSE, main="Histogram")
xlm1f<-seq(min(sres.lm1f),max(sres.lm1f)) 
ylm1f<-dnorm(xlm1f) 
lines(xlm1f, ylm1f)
sres.lm1g<-studres(lm1g)  #distribution of studentized residuals
hist(sres.lm1g, freq=FALSE, main="Histogram")
xlm1g<-seq(min(sres.lm1g),max(sres.lm1g)) 
ylm1g<-dnorm(xlm1g) 
lines(xlm1g, ylm1g)


#non constant error variance  Evaluate homoscedasticity
par(mfrow=c(3,3)) # plot studentized residuals vs. fitted values # non-constant error variance test
ncvTest(lm1a) 
spreadLevelPlot(lm1a) 
ncvTest(lm1b) 
spreadLevelPlot(lm1b) 
ncvTest(lm1c) 
spreadLevelPlot(lm1c) 
ncvTest(lm1d) 
spreadLevelPlot(lm1d) 
ncvTest(lm1e) 
spreadLevelPlot(lm1e) 
ncvTest(lm1f) 
spreadLevelPlot(lm1f) 
ncvTest(lm1g) 
spreadLevelPlot(lm1g) 

#global results
gvlma(lm1a)
s.lm1a
gvlma(lm1b)
s.lm1b
gvlma(lm1c)
s.lm1c
gvlma(lm1d)
s.lm1d
gvlma(lm1e)
s.lm1e
gvlma(lm1f)
s.lm1f
gvlma(lm1g)
s.lm1g

par(mfrow=c(3,3))
visreg(lm1a, main="Regression") 
visreg(lm1b, main="Regression") 
visreg(lm1c, main="Regression") 
visreg(lm1d, main="Regression") 
visreg(lm1e, main="Regression") 
visreg(lm1f, main="Regression") 
visreg(lm1g, main="Regression") 
#monthly income, loan purpose left

###########################
#parametric regression 2-> Irate=Amount.Funded.By.Investors + LoanL + sFICO + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months
###########################

#fitting model
lm2 <- lm(Irate~Amount.Funded.By.Investors+LoanL+sFICO+Inquiries.in.the.Last.6.Months)  

# Calculate Relative Importance for Each Predictor
calc.relimp(lm2,type=c("last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(lm2, b = 1000, type = c("lmg","last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

s.lm2<-summary(lm2) #summary and significance
coef2<-lm2$coefficients #regression coefficents
compareCoefs(lm2)
vcov(lm2) #covariance matrix
par(mfrow=c(2,2))
plot(lm2,main="Regression",col=1) #weight tails distribution
mmps(lm2)#marginal model plots

#outliers
outlier.test(lm2,cutoff=Inf,n.mx=Inf) 
par(mfrow=c(1,1))
qqPlot(lm2, main="QQ Plot")
leveragePlots(lm2)

#influential observations
avPlots(lm2) #added variable plot
cutoff2 <- 4/((nrow(loansData)-length(coef2)-2)) 
par(mfrow=c(1,2))
plot(lm2, which=4, cook.levels=cutoff2)  #identify values
influencePlot(lm2,id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#non normality errors
sres.lm2<-studres(lm2)  #distribution of studentized residuals
par(mfrow=c(1,1))
hist(sres.lm2, freq=FALSE, main="Distribution of Studentized Residuals")
xlm2<-seq(min(sres.lm2),max(sres.lm2)) 
ylm2<-dnorm(xlm2) 
lines(xlm2, ylm2)
shapiro.test(sres.lm2)  #residuals normality

#non constant error variance  Evaluate homoscedasticity
ncvTest(lm2) # non-constant error variance test
spreadLevelPlot(lm2)# plot studentized residuals vs. fitted values 

#evaluate multycollinearity
vif(lm2) # variance inflation factors 
sqrt(vif(lm2)) > 2 # problem?

# Evaluate Nonlinearity (ot clear)
crPlots(lm2) #component + residual plot 
# Test for Autocorrelated Errors for evaluate non independence
durbin.watson(lm2)

#global results
gvlma(lm2)
s.lm2
res2<-lm2$resid #vector of residuals
lin2<-coef2[1]+coef2[2]*Amount.Funded.By.Investors+coef2[3]*LoanL+coef2[4]*sFICO+coef2[4]*Inquiries.in.the.Last.6.Months+res2 #modelo (univarate)
par(mfrow=c(1,1))
plot(lin2,main="Fitted model")  #scatterplot
par(mfrow=c(2,2))
visreg(lm2, main="Regression") 

inverseResponsePlot(lm2)
cor.test(sFICO,Irate,type="pearson")
cor.test(Amount.Funded.By.Investors,Irate,type="pearson")
cor.test(Inquiries.in.the.Last.6.Months,Irate,type="pearson")

###########################
#parametric regression 3-> (without sFICO) Irate=Amount.Funded.By.Investors + LoanL + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months
###########################

#fitting model
lm3 <- lm(Irate~(log10(Amount.Funded.By.Investors+1)+LoanL+Inquiries.in.the.Last.6.Months))  

# Calculate Relative Importance for Each Predictor
calc.relimp(lm3,type=c("last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples) 
boot3 <- boot.relimp(lm3, b = 1000, type = c("lmg","last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot3) # print result
plot(booteval.relimp(boot3,sort=TRUE)) # plot result

s.lm3<-summary(lm3) #summary and significance
coef3<-lm3$coefficients #regression coefficents
compareCoefs(lm3)
vcov(lm3) #covariance matrix
par(mfrow=c(2,2))
plot(lm3,main="Regression",col=1) #weight tails distribution
mmps(lm3)#marginal model plots

#outliers
outlier.test(lm3,cutoff=Inf,n.mx=Inf) 
par(mfrow=c(1,1))
qqPlot(lm3, main="QQ Plot")
leveragePlots(lm3)

#influential observations
avPlots(lm3) #added variable plot
cutoff3 <- 4/((nrow(loansData)-length(coef3)-2)) 
par(mfrow=c(1,2))
plot(lm3, which=4, cook.levels=cutoff3)  #identify values
influencePlot(lm3,id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#non normality errors
sres.lm3<-studres(lm3)  #distribution of studentized residuals
par(mfrow=c(1,1))
hist(sres.lm3, freq=FALSE, main="Distribution of Studentized Residuals")
xlm3<-seq(min(sres.lm3),max(sres.lm3)) 
ylm3<-dnorm(xlm3) 
lines(xlm3, ylm3)
shapiro.test(sres.lm3)  #residuals normality

#non constant error variance  Evaluate homoscedasticity
ncvTest(lm3) # non-constant error variance test
spreadLevelPlot(lm3)# plot studentized residuals vs. fitted values 

#evaluate multycollinearity
vif(lm3) # variance inflation factors 
sqrt(vif(lm3)) > 2 # problem?

# Evaluate Nonlinearity (ot clear)
crPlots(lm3) #component + residual plot 
# Test for Autocorrelated Errors for evaluate non independence
durbin.watson(lm3)

#global results
gvlma(lm3)
s.lm3
lin3<-predict(lm3) #modelo 
lin2<-predict(lm2)
plot(lin2,main="Fitted model")  #scatterplot
par(mfrow=c(1,1))
lines(lin3,main="Fitted model",col=2)  #scatterplot

par(mfrow=c(2,2))
visreg(lm3, main="Regression") 

head(predict(lm3))
head(predict(lm2))
inverseResponsePlot(lm2)
cor.test(sFICO,Irate,type="pearson")
cor.test(Amount.Funded.By.Investors,Irate,type="pearson")
cor.test(Inquiries.in.the.Last.6.Months,Irate,type="pearson")