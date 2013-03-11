library(gbm)
library(randomForest)
library(gplots)

# Load data
loans=read.csv(file='LoanStats.csv', header=T)
newloans=read.csv(file='InFunding2StatsNew.csv', header=T, quote="\"")

loans=loans[loans$Status=='Charged Off' || loans$Status=='Fully Paid',]
loans$DV=as.numeric(loans$Status=='Charged Off')

# make sure factors are ok
oldlength=nrow(loans)
newlength=nrow(newloans)
combinedfactor=as.factor(c(as.character(loans$CREDIT.Grade), as.character(newloans$sub_grade)))
loans$CREDIT.Grade=combinedfactor[1:oldlength]
newloans$sub_grade=combinedfactor[(oldlength+1):(oldlength+newlength)]

# Derived features
loans$Employment.Length.num   = as.numeric(loans$Employment.Length)
loans$Amount.Requested.binned = round(as.numeric(loans$Amount.Requested)/1000)*1000
loans$Amount.Requested.mod500 = as.numeric(loans$Amount.Requested %% 500)
loans$Amount.Requested.mod100 = as.numeric(loans$Amount.Requested %% 100)
loans$Amount.Requested.mod1000z = as.numeric(0==as.numeric(loans$Amount.Requested %% 1000))
#,"Debt.To.Income.Ratio"
#,"Revolving.Line.Utilization"
#,"Employment.Length")
#,"City"
whitelist=c(
  "State"
  ,"CREDIT.Grade"
  ,"Amount.Requested"
  ,"Amount.Requested.binned"
  ,"Amount.Requested.mod1000z"
  ,"Amount.Requested.mod100"
  ,"Amount.Requested.mod500"
  #                               ,"Loan.Purpose"
  #                               ,"Delinquencies..Last.2.yrs."
  #                               ,"FICO.Range"
  ,"Home.Ownership" 
  #                               ,"Open.CREDIT.Lines"
  #                               ,"Inquiries.in.the.Last.6.Months" 
  ,"Employment.Length"
)

# Individual predictors w/ CI
# plotmeans(DV ~ loans$Amount.Requested.binned, data=loans)
# plotmeans(DV ~ loans$Home.Ownership, data=loans)
# plotmeans(DV ~ loans$Loan.Purpose, data=loans)
# plotmeans(DV ~ loans$State, data=loans)
# plotmeans(DV ~ loans$CREDIT.Grade, data=loans)
# plotmeans(DV ~ loans$Open.CREDIT.Lines, data=loans)
# plotmeans(DV ~ loans$Employment.Length, data=loans)
# plotmeans(DV ~ loans$Inquiries.in.the.Last.6.Months, data=loans)
# plotmeans(DV ~ loans$FICO.Range, data=loans)
# plotmeans(DV ~ loans$Amount.Requested.mod500, data=loans)
# plotmeans(DV ~ loans$Amount.Requested.mod100, data=loans)
# plotmeans(DV ~ loans$Amount.Requested.mod1000z, data=loans)
# plotmeans(DV ~ loans$Loan.Length, data=loans)

traindata=loans[,c(whitelist,'DV')]

# Fit GBM
gbmmodel = gbm(DV ~ .
               , data=traindata
               , distribution="bernoulli"
               , n.trees = 100
               , shrinkage = 0.01
               , interaction.depth = 5
               , bag.fraction = 0.5
               , n.minobsinnode = 5
               , keep.data=TRUE
               , verbose=TRUE
)
save(file='gbm.model', list=c('gbmmodel','traindata'))
summary(gbmmodel)
