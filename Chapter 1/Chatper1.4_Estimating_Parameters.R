# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 1 Discrete Case
# Section   : 1.4

##################################
# Estimating the Parameters: cpt #
##################################

# The data file comes in .gz format and the script is modify to read the zipped file
surveyfile <- gzfile('survey.txt.gz', 'rt')
# Also, per Dr. Scutari, R 4.0 doesn't convert characters to factors by default
# Add stringsAsFactors = TRUE to the read.table command to covert the levels to factors
survey <- read.table(surveyfile, header = TRUE, stringsAsFactors = TRUE)
#View data table and columns
head(survey)
ncol(survey)
nrow(survey)

# The classic frequentist view and maximum likelihood estimates would use
# the number of observations from the variable to estimate the conditional prob.
# This step create an object bn.mle by assigning the maximum likelihood estimator
# For this code, we assume the structure of the network is known.The data is fitted to the dag object
bn.mle <- bn.fit(dag, data = survey, method = "mle")
bn.mle #This will provides all the variables conditional probability table estimated from the data

# For didactic purposes, the cpt estimates can also be computed manually from the data
# This is an example for O and E variables
prop.table(table(survey[, c("O", "E")]), margin = 2)
bn.mle$O # This is the same code as above and is used to check if the result is the same

# The parameters can also be estimated by using the posterior distributions with a Bayesian setting
# This is done by using the "bayes" method, similar to the "mle" code
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 10)
bn.bayes$O
# iss is an optional argument, for imaginary sample size to determine how much weight to assign
# to the prior distribution

bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 20)
bn.bayes$O
# The higher the iss value, makes the posterior distribution curve flatter which push it
# towards the uniform distribution used a the prior distribution.

# Descriptive Summary of the data
summary(survey)
#      A           R          E          O       S           T
#  adult:236   big  :379   high:365   emp :483   F:201   car  :290
#  old  :104   small:121   uni :135   self: 17   M:299   other: 85
#  young:160                                             train:125