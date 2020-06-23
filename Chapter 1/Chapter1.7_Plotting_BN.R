# Title     : Bayesian Networks with examples in R ~ Scutari & Denis (2014) ebook
# Source    : https://www.bnlearn.com/book-crc/
# Objective : Chapter 1 Discrete Case
# Section   : 1.7

####################
# 1.7 Plotting BNs #
####################

# The ability to plot a BN effectively is the key tool to BN inference
# The Rgraphviz package has a strucutral graph function
# This plot the DAG graph itself
graphviz.plot(dag)
# By default it gives a dot layout with parents above their children, with arcs point downward
# There are three layout types: dot, fdp, circo in the Rgraphviz package
# You can also use the highlight argument to mark a path or nodes
# For example, All nodes and arcs plotted in grey, except S->E->R
hlight <- list(nodes = nodes(dag), arcs = arcs(dag),
                  col = "grey", textCol = "grey")
# Pass hlight to the graphviz.plot function
pp <- graphviz.plot(dag, highlight = hlight)
pp # Now all path and nodes are in grey color

# Use the edgeRenderInfo function from the Rgraphviz package,
# It render certain paths and nodes to black
# Be sure to load "graph" from the BiocManager::install("graph")
# The attributes being modified are colour "col" and line width "lwd"
edgeRenderInfo(pp) <-
  list(col = c("S~E" = "black", "E~R" = "black"),
       lwd = c("S~E" = 3, "E~R" = 3))
# The attributes being modified are nodes for color "col" and "fill"
nodeRenderInfo(pp) <-
  list(col = c("S" = "black", "E" = "black", "R" = "black"),
    textCol = c("S" = "black", "E" = "black", "R" = "black"),
    fill = c("E" = "grey"))
# Then we can plot after making all the modification.
# Be sure to load "grid" package from BiocManager::install("grid")
renderGraph(pp)

# 1.7.2 Plotting Conditional Probability Distributions
# The following are bar graphs of Residence or Occuptation on Travel
bn.fit.barchart(bn.mle$T, main = "Travel",
  xlab = "Pr(T |R,O)", ylab = "")
# Table of Pr(T | R,O)
bn.mle$T
# bn.mle is the conditional probability estimated from the data
# Select T from that table will display the parents on this node (O, R)
# O, R is the parent and T is the child

# Construct a new table with selected evidence for plotting
Evidence <-
  factor(c(rep("Unconditional",3), rep("Female", 3),
           rep("Small City",3)),
         levels = c("Unconditional", "Female", "Small City"))
Travel <- factor(rep(c("car", "train", "other"), 3),
           levels = c("other", "train", "car"))
distr <- data.frame(Evidence = Evidence, Travel = Travel,
           Prob = c(0.5618, 0.2808, 0.15730, 0.5620, 0.2806,
                    0.1573, 0.4838, 0.4170, 0.0990))
# View the table of the three edidence, unconditional, female, small city
head(distr)
# Combined plots of three probability tables with lines
# Load lattice
library(lattice)
barchart(Travel ~ Prob | Evidence, data = distr,
   layout = c(3, 1), xlab = "probability",
   scales = list(alternating = 1, tck = c(1, 0)),
   strip = strip.custom(factor.levels =
     c(expression(Pr(T)),
       expression(Pr({T} * " | " * {S == F})),
       expression(Pr({T} * " | " * {R == small})))),
   panel = function(...) {
     panel.barchart(...)
     panel.grid(h = 0, v = -1)
   })

# The queries that generated the tables
querygrain(junction, nodes = "T")$T   # Pr(T)
jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")$T  # Pr(T|S = Female)
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T   # Pr(T| R = small)
