# Lending Club Notes scoring

Allows people to lend to others in $25 increments by buying Notes. Lending Club takes a small cut (compared to traditional banks).

The code here trains GBM model in R on historical data that aims to predict probability of default for each Note based on some of its attributes. The model is then applied to loans in funding and these are then sorted in descending order by the expected profitability ~ interest rate adjusted by the chance of default. According to the model the Notes have different probabilities of default despite being in the same "risk" category which could potentially allow one to get a higher return than choosing randomly.

the ``go`` script fetches data from Lending Club site, trains a model on past oans, scores the new 'in-funding' loans and sorts them by expected profitability.

# Caveats

Use at your own risk!

Depends on many assumptions which are not necessarily satisfied. Such as that the attributes in training data (past loans) and test data (loans in funding) have the same meaning and distribution. For some attributes mapping is necessary which could have introduced noise or systematic bias. The risk category is a strong feature but the scoring by Lending club probably changed over time. Also the economy influences the overall rates of default so past performance cannot be guarantee of future performance.
