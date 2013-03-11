library(gbm)
library(randomForest)
library(gplots)

classify_purpose = function(purpose)
{  
  # PAST NEW
  #"car","Car financing"
  classified=as.character(purpose)
  classified[classified=="Wedding expenses"]="wedding"
  classified[classified=="Car financing"]="car"
  classified[classified=="Credit card refinancing"]="credit_card"
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


# Load data
pastloans=read.csv(file='LoanStats.csv', header=T)
newloans=read.csv(file='InFunding2StatsNew.csv', header=T, quote="\"")

pastloans=pastloans[pastloans$Status=='Charged Off' || pastloans$Status=='Fully Paid',]
pastloans$DV=as.numeric(pastloans$Status=='Charged Off')

# make sure factors are ok
pastlength=nrow(pastloans)
newlength=nrow(newloans)

trainrows=c(rep(T,pastlength),rep(F,newlength))
testrows=c(rep(F,pastlength),rep(T,newlength))

combined=data.frame(list(DV=c(pastloans$DV,rep(NA,newlength))))
combined$sub_grade=as.factor(c(as.character(pastloans$CREDIT.Grade), as.character(newloans$sub_grade)))
combined$emp_len=as.factor(c(as.character(pastloans$Employment.Length), as.character(newloans$emp_length)))
combined$loan_amt=as.numeric(c(pastloans$Amount.Requested, newloans$loan_amnt))
combined$addr_state=as.factor(c(as.character(pastloans$State), as.character(newloans$addr_state)))
combined$home_ownership=as.factor(c(as.character(pastloans$Home.Ownership), as.character(newloans$home_ownership)))
combined$term=as.factor(c(substr(pastloans$Loan.Length,0,2), as.character(newloans$term)))
combined$int_rate=as.numeric(
  c(
    substr(as.character(pastloans$Interest.Rate),1, nchar(as.character(pastloans$Interest.Rate))-1)
    , as.numeric(as.character(newloans$int_rate))
    )
  )
combined$purpose=as.factor(
  c(
    as.character(pastloans$Loan.Purpose)
    , classify_purpose(newloans$purpose)
  )
)
combined$income=c(as.numeric(pastloans$Monthly.Income)
                  , as.numeric(newloans$annual_inc/12)
                  )
combined$fico_range=as.factor(
  c(
    as.character(pastloans$FICO.Range)
    , paste(newloans$fico_range_low,newloans$fico_range_high,sep='-')
  )
)
combined$revolving_line_utilization=x=c(
  as.numeric(substr(as.character(pastloans$Revolving.Line.Utilization),1,nchar(as.character(pastloans$Revolving.Line.Utilization))-1))/100
  ,as.numeric(as.character(newloans$revol_util))
  )

# New only
combined$url=''
combined$url[testrows]=as.character(newloans$url)
combined$funded_amnt=''
combined$funded_amnt[testrows]=as.character(newloans$funded_amnt)

# Derived features
combined$loan_amt.binned = round(as.numeric(combined$loan_amt)/1000)*1000
combined$loan_amt.mod500 = as.numeric(combined$loan_amt %% 500)
combined$loan_amt.mod100 = as.numeric(combined$loan_amt %% 100)
combined$loan_amt.mod1000z = as.numeric(0==as.numeric(combined$loan_amt %% 1000))

#,"Debt.To.Income.Ratio"
#,"Revolving.Line.Utilization"
#,"City"
whitelist=c(
  "addr_state"
#   ,"sub_grade"
  ,"loan_amt"
  ,"loan_amt.binned"
  ,"loan_amt.mod1000z"
  ,"loan_amt.mod100"
  ,"loan_amt.mod500"
  ,"term"
  ,"income"
  ,"purpose"
  ,"fico_range"
  ,"revolving_line_utilization"
  #                               ,"Delinquencies..Last.2.yrs."
  #                               ,"FICO.Range"
  ,"home_ownership" 
  #                               ,"Open.CREDIT.Lines"
  #                               ,"Inquiries.in.the.Last.6.Months" 
  ,"emp_len"
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

traindata=combined[trainrows,]

# Fit GBM
gbmmodel = gbm(DV ~ .
               , data=traindata[,c(whitelist,'DV')]
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

testdata=combined[testrows,]

testdata$pred_default_rate=predict.gbm(gbmmodel
                                       ,newdata=testdata[,whitelist]
                                       ,n.trees=100
                                       ,type='response'
                                       )
# pdefault * (-1) + (1-pdefault) * (1) * interest = 0 for "break even rate"
# interest = pdefault / (1-pdefault)
testdata$profitability=(testdata$int_rate/100)-(testdata$pred_default_rate/(1-testdata$pred_default_rate))
orderedloans = testdata[order(testdata$profitability,decreasing=T),]
head(orderedloans[orderedloans$term=='36',
                  c('term'
                    ,'profitability'
                    ,'pred_default_rate'
                    ,'int_rate'
                    ,'loan_amt'
                    , "purpose"
                    , "income"
                    #                      #, "funded_amnt"
                    ,'sub_grade'
                    , 'url'
                  )
                  ])
