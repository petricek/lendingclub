library(gbm)
library(randomForest)
library(gplots)

# Load data
loans=read.csv(file='LoanStats.csv', header=T, skip=1)
newloans=read.csv(file='InFunding2StatsNew-20130225.csv', header=T, quote="\"")

loans=loans[loans$Status=='Charged Off' || loans$Status=='Fully Paid',]
loans$DV=as.numeric(loans$Status=='Charged Off')

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

# md=mean(loans$DV)
# loans_ss=loans[(runif(nrow(loans)) <= md/(1-md)) | (loans$DV==1),]

# Fit GBM
gbmmodel = gbm(DV ~ .
               , data=loans[,c(whitelist,'DV')],
               distribution="bernoulli"
               , n.trees = 100
               , shrinkage = 0.01
               , interaction.depth = 5
               , bag.fraction = 0.5
               , n.minobsinnode = 5
               , keep.data=TRUE
               , verbose=TRUE
)
summary(gbmmodel)

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

# [1] "acc_now_delinq"                 "acc_open_past_24mths"          
# [3] "accept_d"                       "addr_city"                     
# [5] "addr_state"                     "annual_inc"                    
# [7] "apr"                            "bc_open_to_buy"                
# [9] "bc_util"                        "chargeoff_within_12_mths"      
# [11] "collections_12_mths_ex_med"     "credit_pull_d"                 
# [13] "delinq_2yrs"                    "delinq_amnt"                   
# [15] "desc"                           "dti"                           
# [17] "earliest_cr_line"               "emp_length"                    
# [19] "emp_name"                       "emp_status"                    
# [21] "exp_d"                          "exp_default_rate"              
# [23] "fico_range_high"                "fico_range_low"                
# [25] "funded_amnt"                    "grade"                         
# [27] "home_ownership"                 "id"                            
# [29] "ils_exp_d"                      "initial_list_status"           
# [31] "inq_last_6mths"                 "installment"                   
# [33] "int_rate"                       "is_inc_v"                      
# [35] "list_d"                         "loan_amnt"                     
# [37] "member_id"                      "mort_acc"                      
# [39] "msa"                            "mths_since_last_delinq"        
# [41] "mths_since_last_major_derog"    "mths_since_last_record"        
# [43] "mths_since_oldest_il_open"      "mths_since_recent_bc"          
# [45] "mths_since_recent_bc_dlq"       "mths_since_recent_inq"         
# [47] "mths_since_recent_loan_delinq"  "mths_since_recent_revol_delinq"
# [49] "num_accts_ever_120_pd"          "num_rev_accts"                 
# [51] "open_acc"                       "percent_bc_gt_75"              
# [53] "pub_rec"                        "pub_rec_bankruptcies"          
# [55] "pub_rec_gt_100"                 "purpose"                       
# [57] "review_status"                  "review_status_d"               
# [59] "revol_bal"                      "revol_util"                    
# [61] "service_fee_rate"               "sub_grade"                     
# [63] "tax_liens"                      "term"                          
# [65] "title"                          "total_acc"                     
# [67] "total_bal_ex_mort"              "total_bc_limit"                
# [69] "total_il_high_credit_limit"     "url"                           

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
