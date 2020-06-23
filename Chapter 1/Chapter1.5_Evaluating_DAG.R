# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 1 Discrete Case
# Section   : 1.5

#######################################
# 1.5.1 Conditional Independence Test #
#######################################

# Conditional Independence Tests focus on individual arcs.
# Tests whether a probabilistic dependence is supported by the data
# Null Hypothesis Testing (of the Conditional Independence)
# H0: "Travel is probabilistically independent from Education conditional on its parents"
# If H0 is rejected, it means that Education does influence travel, can should be included in the DAG.
# H1: ""Travel is probabilistically not independent from Education conditional on its parents"
# H1 is the alternative hypothesis, which means there is no significant difference on the dependence relationship

# We can test this hypothesis by log-likelihood ratio G-squared or Pearson's X-squared
# The test statistics formula is displayed in p.15 to p.16
# (nlevels(survey[, "T"] - 1) * nlevels(survey[, "E"]) - 1) * nlevels(survey[, "O"]) * nlevels(survey[, "R"])
# Gave a warning in which "-" is not meaningful for factors
# [T ~ E | O + R]

ci.test("T", "E", c("O", "R"), test = "mi", data = survey)
# G-squared test, which is equivalent to the mutual information test from information theory is "mi"

ci.test("T", "E", c("O", "R"), test = "x2", data = survey)
# Pearson's X-squared is "x2"

# Another example to test the dependence relationship between Occupation to Travel
# [T|O:R]
ci.test("T", "O", "R", test = "mi", data = survey)
# G-squared test, which is equivalent to the mutual information test from information theory is "mi"

ci.test("T", "O", "R", test = "x2", data = survey)
# Pearson's X-squared is "x2"


# Test statistics can be automated using arc.strength function
arc.strength(dag, data = survey, criterion = "x2")
# The reported strength here is the resulting p-value
# Using p =< 0.05, only the O -> T relationship is insignificant

########################
# 1.5.2 Network Scores #
########################

# Network scores is the goodness-of-fit statistics
# It measures how well the DAG mirrors the dependence structure of the data
# Bayesian Information Criterion (BIC)
# (Page 18) formula that display how to compute BIC from local distributions
# Bayesian Dirichlet equivalent uniform (BDe) posterios probability of the DAG
# Both BIC and BDe assign high scores to the DAGs that fits the data better

score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)
score(dag, data = survey, type = "bde", iss = 1)
# iss is used for the imaginary sample size.
# which represents the weight assigned to the (flat) prior distribution
# for small value of iss to show large observed samples,
# log BDe and BIC scores yield similar values

# Compare the DAG before and after adding arcs to fit the data
dag4 <- set.arc(dag, from = "E", to = "T")
nparams(dag4, survey)
score(dag4, data = survey, type = "bic")
# previously the original DAG with [A][S][E|A:S][O|E][R|E][T|O:R] produced a BIC score of -2012.687
# After adding E -> T to dag4, the BIC score is -2032.603
# So therefore -2012 is a larger BIC score which mean DAG is a better model to fit the data

# BIC van also be used to compare completely different networks unlike conditional independence tests
# The following example generates a random graph for comparison

rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type = "bic")
# The random model got a -2064 BIC score which is worse than the previous two models

# To find a strcuture that best fit the data, there is one simple appraoch called hill-climbing algorithm
# It starts the DAG with no arcs, and then modify the arcs until the network score increase to most
# Using the hc function, it will take the data and generate the highest score network with BIC
learned <- hc(survey) #hill-climbing algorithm
modelstring(learned) #It gives [R][E|R][T|R][A|E][O|E][S|E]
score(learned, data = survey, type = "bic") #gives -1998.432 score

learned2 <- hc(survey, score = "bde") # To specify the BDe score for hill-climbing algorithm
learned2 # Tells you the Bayesian network model learned by score based methods
modelstring(learned2) #It gives [R][E|R][T|R][A|E][O|E][S|E]
score(learned2, data = survey, type = "bde") # gives -2002.002 score

score(dag, data = survey, type = "bic") #gives a score of -2012.687 in BIC

# Test to see if the BIC score can improve by removing an arc (Page20)
# use the function arc.strength to test
arc.strength(learned, data = survey, criterion = "bic")
# result strength reports the score change caused by arc removal when criterion is a network score
arc.strength(dag, data = survey, criterion = "bic")
# comparing dag with learned in which they have different arcs
# results indicated from O to T, the strength is 10.046 which is increase in BIC score
# The p-values is insignifiant from previous X-squared calculation

# To conclude, we used the HC algorithm to model the data gives a better BIC score
# However, it is not possible to test the significance of the paths
# Running both the path evaluation and network model will allow comparison
# of paths (for hypothesis testing) or overall structure (network score)

