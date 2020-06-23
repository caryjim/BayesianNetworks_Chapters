# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 1 Discrete Case
# Section   : 1.1 to 1.3

# Bayesian Network Learning package to create and manipulate DAGs in BNs
# install(bnlearn) is it has been installed
library(bnlearn)

###############
## Section 1 ##
###############

# First, create an empty directed acyclic graph (DAG)
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
dag  #This store as an object which is a list of elements corresponding to the nodes in DN.
# Within each node: there are mb, nbr, parents, and children

# Second, add the arcs to encode the direct dependencies between the variables
# Variables that don't have inflence will not have arcs
# Age and Sex have direct influence on Education
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
# Education have direct influence on Occupation and Residence
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")
# Occupation and Residence directly influence means of transport
# Transport is the target variable of this example
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")

dag # read the dag object to see the model information
modelstring(dag) # representation of graph structure is used to recall a product of conditional probabilities

nodes(dag) # Call the names of the nodes in the DAG
arcs(dag)  # Call the arcs of the nodes in the DAG

# Another way to set the arcs to a DAG with a matrix will be faster
# dag2 <-empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
# arc.set <- matrix(c("A", "E",
#                     "S", "E",
#                     "E", "O",
#                     "E", "R",
#                     "O", "T",
#                     "R", "T"),
#                   byrow = TRUE, ncol = 2,
#                   dimnames = list(NULL, c("from", "to")))
# arcs(dag2) <-arc.set # Assign the matrix of arcs to the DAG
# dag2 # Call the object to see its properties
# modelstring(dag2) # Call the strcuture of dependence among the nodes
# If there are multiple DAG, you can check the structures of the target and current DAG
# all.equal(dag, dag2)

# Third, specifying the levels within each variable
# All variables in this expample are discrete with multinomial distribution
# The discrete variables have levels within each
A.lv <- c("young","adult","old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

# (page7) In the context of BNs, the joint distribution is called the global distribution
# The number of parameters is very high even in a small proble like this. THe DAG
# breaks down the global distribution into a set of smaller local distributions (one for each variable)
# Variables that are not linked by an arc are conditionally independent in this example
# from the original representation : "[A][S][E|A:S][O|E][R|E][T|O:R]"

# Factor the global distribution can be written as:
# Pr(A,S,E,O,R,T) = Pr(A)Pr(S)Pr(E|A,S)Pr(O|E)Pr(R|E)Pr(T|O,R)
# Each variable depends on its parents and the distribution is univarite and has a smaller parameteres comparetively.
# Pr(A,S,E,O,R,T) is defined as a nested model or a submodel of global distribution

# Fourth, Assign the probability table and dimensions to each variables
# Age and Sex don't have parents and can be modeled by simple undimensional probability tables
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3, dimnames = list(A = A.lv))
A.prob

S.prob <- array(c(0.60, 0.40), dim = 2, dimnames = list(S = S.lv))
S.prob

# The variables that have dependence can be modeled by a two dimensional conditional probability tables
# Each column is one level of the parents which holds the distribution conditional on that particular level
# Therefore, probability sum up to 1 for each column
# "[A][S][E|A:S][O|E][R|E][T|O:R]"

O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2,2), dimnames = list(O = O.lv, E = E.lv))
O.prob

R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2,2), dimnames = list(R = R.lv, E = E.lv))
R.prob

# matrix function can also be used to create these distribution table if there is only 1 or 2 dimensions
# R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol =2, dimnames = list(R = R.lv, E = E.lv)

#"[A][S][E|A:S][O|E][R|E][T|O:R]" Education and Travel are 3D probability tables
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2,3,2),
                  dimnames = list(E = E.lv, A = A.lv, S = S.lv))
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08,
                  0.58, 0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3,2,2),
                  dimnames = list(T = T.lv, O = O.lv, R = R.lv))
E.prob
T.prob

# Overall the local distributions have just 21 parameters instead of the 143 from the global distribution.
# The reduction in dimension makes BN application feasible for high dimensional problems.
# Section 1 defined both the DAG and the local distribution for each variable.

###############
## Section 2 ##
###############
# Combine the DAG and the local distriction for each varible from section 1 to form a fully specified BN
# First, recreate a DAG with the modelstring method to call the syntax to create DAG3

modelstring(dag)
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
dag3
all.equal(dag, dag3)

# Second, create a list call cpt to store all the probability tables
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

# Third, create an object with cpt and the dag
bn <- custom.fit(dag, cpt)

# Identify the number of parameters in the BN
nparams(bn)
# Identify the arcs in the BN
arcs(bn)
#      from to
# [1,] "A"  "E"
# [2,] "S"  "E"
# [3,] "E"  "O"
# [4,] "E"  "R"
# [5,] "O"  "T"
# [6,] "R"  "T"
# Identify the nodes in the BN
nodes(bn)
# Identify the parents in the BN for node "T"
parents(bn, "T")
# Identify the children in the BN for node "O"
children(bn, "O")
# To print the conditional proability table of the nodes
bn$R
# Or to assign a table using coef function, then when calling bn, all the conditional tables will be printed.
A.cpt <-coef(bn$A)
bn

# Using summary function on DAG
summary(dag)
#          Length Class  Mode
# learning  6     -none- list
# nodes     6     -none- list
# arcs     12     -none- character

summary(dag$nodes)
#   Length Class  Mode
# A 4      -none- list
# S 4      -none- list
# E 4      -none- list
# O 4      -none- list
# R 4      -none- list
# T 4      -none- list
