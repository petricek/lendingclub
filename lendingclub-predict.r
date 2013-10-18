library(gbm)
library(gplots)

# Load data
newloans=read.csv(file='InFundingStats3.csv', header=T, quote="\"")
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

classify_purpose = function(purpose)
{  
  # PAST NEW
  "car","Car financing"
  "credit_card","Credit card refinancing"
  classified=as.character(purpose)
  classified[classified=="Debt consolidation"]="debt_consolidation"
  classified[classified=="Home improvement"]="home_improvement"
  classified[classified=="Home buying"]="house"
  classified[classified=="Debt consolidation"]="debt_consolidation"
  classified[classified=="Major purchase"]="major_purchase"
  classified[classified=="Medical expenses"]="medical"
  classified[classified=="Major purchase"]="major_purchase"
  classified[classified=="Moving and relocation"]="moving"
  classified[classified=="Other"]="other"
  classified[classified=="Business"]="small_business"
  classified[classified=="Vacation"]="vacation"
  #  "educational",
  #  "renewable_energy",  
  #  "wedding"
  
  return(classified)
}


# Convert new loans to the same format as old loans
newloans$CREDIT.Grade=newloans$sub_grade
newloans$Home.Ownership=newloans$home_ownership
newloans$State=newloans$addr_state
newloans$Employment.Length=newloans$emp_length
newloans$Amount.Requested=newloans$loan_amnt
newloans$Loan.Purpose=classify_purpose(newloans$purpose)

# derived features
newloans$Amount.Requested.binned = round(as.numeric(newloans$Amount.Requested)/1000)*1000
newloans$Amount.Requested.mod500 = as.numeric(newloans$Amount.Requested %% 500)
newloans$Amount.Requested.mod100 = as.numeric(newloans$Amount.Requested %% 100)
newloans$Amount.Requested.mod1000z = as.numeric(0==as.numeric(newloans$Amount.Requested %% 1000))

testdata=newloans[,whitelist]
newloans$pred_default_rate=predict.gbm(gbmmodel,newdata=testdata,n.trees=100,type='response')
# pdefault * (-1) + (1-pdefault) * (1) * interest = 0 for "break even rate"
# interest = pdefault / (1-pdefault)
newloans$profitability=newloans$int_rate-(newloans$pred_default_rate/(1-newloans$pred_default_rate))
orderedloans = newloans[order(newloans$profitability,decreasing=T),]
head(orderedloans[orderedloans$term=='36',c('term','profitability','pred_default_rate','Amount.Requested', "funded_amnt",'CREDIT.Grade', 'url')])
