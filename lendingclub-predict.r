library(gbm)
library(gplots)

# Load data
newloans=read.csv(file='InFunding2StatsNew.csv', header=T, quote="\"")
load(file='gbm.model')

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

# Convert new loans to the same format as old loans
newloans$CREDIT.Grade=newloans$sub_grade
newloans$Home.Ownership=newloans$home_ownership
newloans$State=newloans$addr_state
newloans$Employment.Length=newloans$emp_length
newloans$Amount.Requested=newloans$loan_amnt
#newloans$Loan.Purpose=newloans$purpose

# derived features
newloans$Amount.Requested.binned = round(as.numeric(newloans$Amount.Requested)/1000)*1000
newloans$Amount.Requested.mod500 = as.numeric(newloans$Amount.Requested %% 500)
newloans$Amount.Requested.mod100 = as.numeric(newloans$Amount.Requested %% 100)
newloans$Amount.Requested.mod1000z = as.numeric(0==as.numeric(newloans$Amount.Requested %% 1000))

testdata=newloans[,whitelist]
newloans$pred_default_rate=predict.gbm(gbmmodel,newdata=testdata,n.trees=100,type='response')
newloans$profitability=newloans$int_rate-(1/(1-newloans$pred_default_rate))
orderedloans = newloans[order(newloans$profitability,decreasing=T),]
head(orderedloans[orderedloans$term=='36',c('term','profitability','pred_default_rate','Amount.Requested', "funded_amnt",'CREDIT.Grade', 'url')])
