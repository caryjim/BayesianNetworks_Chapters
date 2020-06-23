# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 1 Discrete Case
# Section   : 1.6

####################
# 1.6 Discrete BNs #
####################

# X, Y, and Z
# If X and Y are separated by Z(conditioning variables), X, Y|Z
# Direct separation (d-separation) which implies probabilistic independence in a BN
# therefore, all paths between X and Y are blocked, X and Y are independent conditionally
# Not all conditional independence is reflected in the DAG

# We can investigate if the nodes in BN are direct separation
# use dsep function to test
dsep(dag, x = "S", y = "R")  # FALSE
dsep(dag, x = "O", y = "R")  # FALSE

# Review the original graph, Sex and Age is directed to Education
# Education is then pointing at Travel and Residence
# Then S is associated with R because of E,  S -> E, E-> R
# use the path function to check the paths
path(dag, from = "S", to = "R")  # True

# After checking the path between S to R, then we have to condition E for correct setup
# Use dsep to set up E to condition to block the path between S and R
# S and R become independent
dsep(dag, x = "S", y = "R", z = "E")
# To reflect the correct relationship
# Pr(S,R|E) = Pr(S|E)Pr(R|E)
# Serial Connection
# Global distinction decompose that a part depends on S and a part depends on R
# once E is known. S -> E, E-> R

# The same for R, O and E in this DAG
# E -> O, E -> R
dsep(dag, x = "R", y = "O", z = "E")
# To reflect the correct relationship
# Pr(O,R|E) = Pr(O|E)Pr(R|E)
# Divergent Connection

# For A, S on E - Convergent Connection
# Conditioning a specific node can make the two other nodes dependent
# A and S conditional on E
dsep(dag, x = "A", y = "S")  # True
dsep(dag, x = "A", y = "S", z = "E")
# E depends on a joint distribution of A and S Pr(E|A,S)
# Using Bayes' theorem, when E is known, we cannot decompose the joint distribution of A
# and S in a part that depends only on A and in a part that only depends on S

#############################
## Exact Inference
#############################

# Two most common inferences with BN are conditional probability queries and
# most likely explanation queries, which can be answered by exact inference or approx. inference

# Exact Inference transofrm the BN into a specially crafted tree to speed up the computation
# of conditional probabilities

# To install the graphical modeling Packages, these are not in the book due to R4.0 version
# http://people.math.aau.dk/~sorenh/software/gR/#installation
#setRepositories()
#install.packages("gRain", dependencies = TRUE)
#install.packages("gRbase", dependencies = TRUE)

# Then get Bioc Manager and associated packages for library gRain to work
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
#BiocManager::install(version = "3.11")

# https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html
BiocManager::install("Rgraphviz")
# # http://www.bioconductor.org/packages/release/bioc/html/graph.html
BiocManager::install("graph")
# https://bioconductor.org/packages/3.11/bioc/html/RBGL.html
BiocManager::install("RBGL")

# Finally, load the libraries
library(gRbase)
library(gRain)

# Junction Tree is the tree that can be constructed with the bn object
# First build the junction tree variable and then input evidence into the tree
junction <- compile(as.grain(bn))
# The changes of local distribution are propagated through the tree
# For example, we are assessing the attitudes of women towards car and train in the survey
querygrain(junction, nodes = "T")$T   # Pr(T)
# The querygrain function extracts the distribution of the modes of interest from the tree
jsex <- setEvidence(junction, nodes = "S", states = "F")
# Create a variable jsex to record the women in the tree under S node
querygrain(jsex, nodes = "T")$T  # Pr(T|S = Female)
# The query assess the probability table of females in the survey data
# The result show that there is little differences with the probabilities derived from
# the whole sample vs. female.

# Another example is looking at the people that lives in a small and large town
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T   # Pr(T| R = small)
jres2 <- setEvidence(junction, nodes = "R", states = "big")
querygrain(jres2, nodes = "T")$T  # Pr(T| R = big)
# The probability of each type of transportation varied among the residence

# (Page 25) Another Example to examine sex and transportation given that education is high
# This is a conditional probaility queries that can aslo assess the conditional independence
# We are looking at the joint probability distribution of S and T given E = high
# Pr(S,T|E = high)

jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S", "T"), # Joint S and T
             type = "joint")
SxT.cpt # Call the probability table
# The function querygrain determine which possible distributions of the nodes is returned.
# In a joint type, the whole table (rows and column) sums up to one.
# The type of distribution can be assigned, joint is used above, marginal is the default
querygrain(jedu, nodes = c("S", "T"), type = "marginal")
# The output table is seperated by S and by T, in which 1 is split by M/F or the levels in T
# Conditional is the joint distribution of the first node conditional on the other nodes
# setEvidence specified the evidence we want the conditioning on. (Page 26)
querygrain(jedu, nodes = c("S", "T"), type = "conditional")
# Only with the conditional type, the columns of variables will sums up to one
# Pr(S=M|T=t, E=high), E is not shown the columns and rows, but is the third dimension
# The outcome of this shown that sex is not informative of a person's preference if we know their education.
# In another word, sex is independent from Transportation conditioning on Education
#    T (t: car, train, or other)
# S        car    train    other
#   M 0.612557 0.612557 0.612557
#   F 0.387443 0.387443 0.387443
jedu2 <- setEvidence(junction, nodes = "E", states = "uni")
querygrain(jedu2, nodes = c("S", "T"), type = "conditional")
# The same for the education level of "university"
dsep(bn, x = "S", y = "T", z = "E")
# The graphical separation indicated that S and T are directly seperated by education
# This method confirm the conditional independence of the joint distribution of S and T
# Another method to confirm the conditional independence is to use test statistics Pearson's X2

SxT.ct <- SxT.cpt * nrow(survey)
chisq.test(SxT.ct)  #chisq.test function comes with base R
# Pearson's chi-squared test is a statistical test applied to a set of categorical data
# nrow will determine the sample size by counting each row in the survey data
# Null hypothesis for this is there is no difference between S and T, in which they are
# independence of each other. When p-values equal to 1, we accept the null hypothesis.

#################################
# 1.6.2.2 Approximate Inference #
#################################

# Use Monte Carlo simulations to randomly generate observations from the BN
# This is computationally expensive, however, it allows for complex specifications
# of the evidence, and also scales better to BN with large number of nodes

# For discrete BNs, a simple way to implement this is to use rejection sampling
# We generate random independent observations from BN in rejection sampling
# Then, we count how many evidence we are conditioning on matches
# and how many observations also match the event in which we compute the probability
# The estimated conditional probability is the ratio between the observations to the evidence.

# cpquery is the function that returns the probability of a specific event given the evidence
# For example, to recompute the SxT table for Male, high education, and car transport
cpquery(bn, event = (S == "M") & (T == "car"),
          evidence = (E == "high"))             # 0.3374
# By default this function calculate the number of parameters to 5000
# Increase the random observation to one million
cpquery(bn, event = (S == "M") & (T == "car"),
            evidence = (E == "high"), n = 10^6)   #0.3425
# With the increased number of random observations, the estimated probability is closer to its true value.
# The true value was calculated by the querygrain Pr(S=M, T=car | E=high)
# querygrain is a function in gRain
querygrain(jedu, nodes = c("S", "T"), # Joint S and T
             type = "joint")                      # 0.3426
# When evidence has a low probability, having increase precision doesn't improve precision

# A better approach is likelihood weighting
# Likelihood weighting generates random observations in a way that match to the evidence
# Then, it re-weights it when computing the conditional probability for query
cpquery(bn, event = (S == "M") & (T == "car"),
            evidence = list(E = "high"), method = "lw")
# Without generating a large random observations, the weighting methods estimated closer to 0.342

# Another example of a complex query
# Pr(S=M, T=car| {A=young, E=uni} U {A=adult})
# Male with car transportation given that he is a young uni Edu or Adult
# U is the union between young and university or Adults only
cpquery(bn, event = (S == "M") & (T == "car"),
  evidence = ((A == "young") & (E == "uni")) | (A == "adult"))
# cpquery function is not flexible enough to compute a query with composite evidence
# this is the same limitation as in the gRain package

# cpdist function has a similar to cpquery and returns a data frame
# containing the random observations for the variables in nodes that match the evidence
SxT <- cpdist(bn, nodes = c("S", "T"),
         evidence = (E == "high"))
head(SxT)
# The new dataframe can then be used for any kind of inference
options(digits = 3)
# First, use table to produce a contingency table from SxT
prop.table(table(SxT))
# Place it in the prop.table function to transform the counts into probability
# To conclude, we can look for a combination of different states of S and T
# and look for the highest probability
# In this example, the most common gender and travel combination with high education is male car driver

