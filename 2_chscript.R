# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 2 The Continous Case: Gaussian Bayesian Networks 
# Section   : 2.1 Introductory Example_Crop Analysis- 


setwd("D:/Documents/R/BayesianNetworks/BayesianNetworks_Chapters/Chapter_2")

library(bnlearn)
dag.bnlearn <- model2network("[G][E][V|G:E][N|V][W|V][C|N:W]")
dag.bnlearn 
# graphviz.plot(dag.bnlearn, groups = None)
# error with Rgraphviz 

#Which pair is independent?
nano <- nodes(dag.bnlearn)
for (n1 in nano) {
  for (n2 in nano) {
    if (dsep(dag.bnlearn, n1, n2))
      cat(n1, "and", n2, "are independent.\n")
  }#FOR
}#FOR

#Which pairs are conditionally independent given V (vegetation mass)
for (n1 in nano[nano != "V"]) { 
  for (n2 in nano[nano != "V"]) {
    if (n1 < n2) {
      if (dsep(dag.bnlearn, n1, n2, "V")) 
        cat(n1, "and", n2, "are independent given V.\n")
    }#THEN
  }#FOR
}#FOR

#Test to see if a path of one subset depends on the former
bnlearn::path(dag.bnlearn, from = "E", to = "C")

#2.3 Probabilistic Representation
# Proposed probability local distributions for the DAG 
disE <- list(coef = c("(Intercept)" = 50), sd = 10)
disG <- list(coef = c("(Intercept)" = 50), sd = 10)
disV <- list(coef = c("(Intercept)" = -10.35534, 
                      E = 0.70711, G = 0.5), sd = 5)
disN <- list(coef = c("(Intercept)" = 45, V = 0.1),
             sd = 9.949874)
disW <- list(coef = c("(Intercept)" = 15, V = 0.7),
             sd = 7.141428)
disC <- list(coef = c("(Intercept)" = 0, N = 0.3, W = 0.7), 
             sd = 6.25)
dis.list = list(E = disE, G = disG, V = disV, N = disN, 
                W = disW, C = disC)
#Use a custom fit function to create an object with the local distribution
gbn.bnlearn <- custom.fit(dag.bnlearn, dist = dis.list)

gbn.bnlearn$G

gbn.bnlearn$C

gbn.bnlearn$E

gbn.bnlearn$N

gbn.bnlearn$V
# Linear Gaussian Bayesian Networks
# Every node follows a normal distribution
# Nodes without any parents are root nodes, and described by respective marginal distributions
# The conditioning effect of the parent nodes is by additive linear term in the mean, and not the variance
# Each node has a variance of its own and it doesn't depend on the parents
# Each local distribution in the node are Gaussian Linear Model with an intercept
# Node's parents are explanatory variables and no interaction term 

#---------------------------------------------------
#rbmn package
library(rbmn)
gbn.rbmn <- bnfit2nbn(gbn.bnlearn)
# Assumed that local distribution imply a joint distribution of all nodes
# is multivariate normal
# Derived the multivariate normal distribution numerically 
gema.rbmn <- nbn2gema(gbn.rbmn)
mn.rbmn <- gema2mn(gema.rbmn)
print8mn(mn.rbmn)
# marginal expectations (mu)
# marginal standard deviation (s.d.)
# correlation matrix
# making the mean and variation the same to simplify interpretation
str(mn.rbmn) # gamma is the covariance matrix 

#-------------------------
#2.4 Estimating the parameters

#Set up data
set.seed(4567)
cropdata1 <- rbn(gbn.bnlearn, n = 200) #Create a dataset of 200
set.seed(1234)
cropdata2 <- rbn(gbn.bnlearn, n = 20000) #Create a dataset of 20000

dim(cropdata1)
round(head(cropdata1), 2)

est.para <- bn.fit(dag.bnlearn, data = cropdata1) # Produce parameter estimate to bnfit with ML estimator
options(digits=3)

#Also, we can assign a particular estimator to one of the node
# est.para$C <- lm(C ~ N + W, data = cropdata1)

#p.46-47 for penalization
# library(penalized)
# est.para$C <- penalized(C ~ N + W, lambda1 = 0, lambda2 = 1.5, 
#                 data = cropdata1)

est.para$E
est.para$C #Estimated value use to compare to the previous local distribution

lmC <- lm(C ~ N + W, data = cropdata1[, c("N", "W", "C")])
coef(lmC)
#The quality of the estimates depends on the sample size
confint(lmC) #Confidence Intervals

# 2.5 Learning the DAG structure 
#Tests and scores specific to GBNs

#Testing of the partial correlation between C and W given N. 
# C| W:N

# First we need to estimate the correlation matrix for C, W and N
cormat <- cor(cropdata1[, c("C", "W","N")])

#Second we compute the inverse correlation
library(corpcor)
invcor <- cor2pcor(cormat,)
dimnames(invcor) <- dimnames(cormat)

invcor

ci.test("C", "W", "N", test = "cor", data = cropdata1,)
#The distribution for the test under the null is a Student t distribution

stru1 <- iamb(cropdata1, test = "cor")

wl <- matrix(c("V", "N"), ncol = 2)
wl
stru2 <- iamb(cropdata1, test = "cor", whitelist = wl)
all.equal(dag.bnlearn, stru2)

# Suppose we use a larger sample size
dim(cropdata2)
stru3 <- iamb(cropdata2, test = "cor")
all.equal(dag.bnlearn, stru3)

# Plotting
library(igraph)
igraph.options(print.full = TRUE)
dag0.igraph <- graph.formula(G-+V, E-+V, V-+N, V-+W, 
                             N-+C, W-+C)
dag0.igraph
typeof(dag0.igraph)

V(dag0.igraph)
E(dag0.igraph)

par(mfrow = c(2, 2), mar = rep(3, 4), cex.main = 2)
plot(dag0.igraph, main = "\n1: defaults")
dag2 <- dag0.igraph
V(dag2)$label <- V(dag2)$name
plot(dag2, main = "\n2: with labels")
ly <- matrix(c(2, 3, 1, 1, 2, 3,
               1, 4, 4, 2, 3, 2), 6)
plot(dag2, layout = ly, main = "\n3: positioning")
colo <- c("black", "darkgrey", "darkgrey", rep(NA, 3))
lcolo <- c(rep("white", 3), rep(NA, 3))
par(mar = rep(0, 4), lwd = 1.5)
plot(dag2, layout = ly, frame = TRUE,
     main = "\n4: final",
     vertex.color = colo, vertex.label.color = lcolo,
     vertex.label.cex = 3, vertex.size = 50,
     edge.arrow.size = 0.8, edge.color = "black")