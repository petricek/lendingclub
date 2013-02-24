library(gbm)
library(randomForest)

loans=read.csv(file='LoanStats.csv', header=T, skip=1)
#summary(loans)
#head(loans)

loans=loans[loans$Status=='Charged Off' || loans$Status=='Fully Paid',]
loans$DV=as.numeric(loans$Status=='Charged Off')

#,"Debt.To.Income.Ratio"
#,"Revolving.Line.Utilization"
#,"Employment.Length")
#,"City"
md=mean(loans$DV)
loans_ss=loans[(runif(nrow(loans)) <= md/(1-md)) | (loans$DV==1),]
gbmmodel = gbm(DV ~ .
            , data=loans[,c("DV"
                               ,"State"
                               ,"CREDIT.Grade"
                               ,"Loan.Purpose"
                               ,"Delinquencies..Last.2.yrs."
                               ,"FICO.Range"
                               ,"Home.Ownership" 
                               ,"Open.CREDIT.Lines"
                               ,"Inquiries.in.the.Last.6.Months" 
                               ,"Employment.Length"
            )
                            ],
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

loans$Employment.Length.num=(as.numeric(loans$Employment.Length))
loans$Amount.Requested.binned = round(as.numeric(loans$Amount.Requested)/1000)*1000
loans$Amount.Requested.mod500 = as.numeric(loans$Amount.Requested %% 500)
loans$Amount.Requested.mod100 = as.numeric(loans$Amount.Requested %% 100)
loans$Amount.Requested.mod1000z = (0==as.numeric(loans$Amount.Requested %% 1000))
rfmodel = randomForest(DV ~ .
                       , data=na.roughfix(loans_ss[,c("DV"
                                                      #                                                   ,"State"
                                                      #                                                   ,"CREDIT.Grade"
                                                      ,"Loan.Purpose"
                                                      #                                                   ,"Delinquencies..Last.2.yrs."
                                                      #                                                   ,"FICO.Range"
                                                      ,"Home.Ownership" 
                                                      #                                                   ,"Open.CREDIT.Lines"
                                                      #                                                   ,"Inquiries.in.the.Last.6.Months" 
                                                      ,"Employment.Length"
                                                      #                                                         ,"Employment.Length.num"
                                                      ,"Amount.Requested.binned"
                                                      ,"Requested.mod500"
                       )
                                                   ])
)
varImpPlot(rfmodel)

# Write-offs by amount
plotmeans(DV ~ loans$Amount.Requested.binned, data=loans)

plotmeans(DV ~ loans$Home.Ownership, data=loans)
plotmeans(DV ~ loans$Loan.Purpose, data=loans)
plotmeans(DV ~ loans$State, data=loans)
plotmeans(DV ~ loans$CREDIT.Grade, data=loans)
plotmeans(DV ~ loans$Open.CREDIT.Lines, data=loans)
plotmeans(DV ~ loans$Employment.Length, data=loans)
plotmeans(DV ~ loans$Inquiries.in.the.Last.6.Months, data=loans)
plotmeans(DV ~ loans$FICO.Range, data=loans)
plotmeans(DV ~ loans$Amount.Requested.mod500, data=loans)
plotmeans(DV ~ loans$Amount.Requested.mod100, data=loans)
plotmeans(DV ~ loans$Amount.Requested.mod1000z, data=loans)
plotmeans(DV ~ loans$Loan.Length, data=loans)
