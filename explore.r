library(gplots)

# Load data
pastpastloans=read.csv(file='pastloanstats.csv', header=T)

# Individual predictors w/ CI
plotmeans(pastloans$DV ~ pastpastloans$Amount.Requested.binned)
plotmeans(pastloans$DV ~ pastloans$Home.Ownership)
plotmeans(pastloans$DV ~ pastloans$Loan.Purpose)
plotmeans(pastloans$DV ~ pastloans$State)
plotmeans(pastloans$DV ~ pastloans$CREDIT.Grade)
plotmeans(pastloans$DV ~ pastloans$Open.CREDIT.Lines)
plotmeans(pastloans$DV ~ pastloans$Employment.Length)
plotmeans(pastloans$DV ~ pastloans$Inquiries.in.the.Last.6.Months)
plotmeans(pastloans$DV ~ pastloans$FICO.Range)
plotmeans(pastloans$DV ~ pastloans$Amount.Requested.mod500)
plotmeans(pastloans$DV ~ pastloans$Amount.Requested.mod100)
plotmeans(pastloans$DV ~ pastloans$Amount.Requested.mod1000z)
plotmeans(pastloans$DV ~ pastloans$Loan.Length)
