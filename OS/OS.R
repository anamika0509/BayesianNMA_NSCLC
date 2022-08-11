library(dmetar)
library(netmeta)
library(openxlsx)
NETWORK_PLOT <- read.xlsx("NETWORK_PLOT_OSHR.xlsx")

m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
treat1 = treat1,
treat2 = treat2,
studlab = study,
data = NETWORK_PLOT,
sm = "HR",
comb.fixed = FALSE,
comb.random = TRUE,
reference.group = "comp",
details.chkmultiarm = TRUE,
sep.trts = " vs ")

m.netmeta 

forest(m.netmeta,
       reference.group = "comp",
       sortvar = TE,
       smlab = paste("ICI vs Chemo"),
       drop.reference.group = TRUE,
       label.left = "HR",
       label.right = "95% CrI")


#### meta gen ###

NETWORK_PLOT_OSHR <- read.xlsx("NETWORK_PLOT_OSHR.xlsx")
glimpse(NETWORK_PLOT_OSHR)

m.gen <- metagen(TE = TE,
seTE = seTE,
studlab = study,
data = NETWORK_PLOT_OSHR,
sm = "HR",
comb.fixed = FALSE,
comb.random = TRUE,
method.tau = "REML",
hakn = TRUE,
title = "OS - HR")

# forest plot
forest.meta(m.gen,sortvar = TE)

# funnel plot
# Produce funnel plot
funnel.meta(m.gen,
studlab = TRUE)
# Add title
title("Funnel Plot OS (ICI vs Chemo)")

# egger's test
eggers.test(m.gen)

# Eggers' test of the intercept 
# ============================= 

#  intercept       95% CI      t         p
#      -3.12 -6.75 - 0.51 -1.683 0.1161844

# Eggers' test does not indicate the presence of funnel plot asymmetry. 

#######

# risk of bias assessment
library(devtools)
library(robvis)
library(openxlsx)
mydata <- read.xlsx("ROB2.xlsx")
rob_traffic_light(mydata, "ROB2")
rob_summary(mydata, "ROB2")

league0 <- netleague(
m.netmeta,
fixed = m.netmeta$fixed,
random = m.netmeta$random,
seq = m.netmeta$seq,
ci = TRUE,
backtransf = TRUE,
direct = FALSE,
digits = gs("digits"),
bracket = gs("CIbracket"),
separator = gs("CIseparator"),
text.NA = ".",
big.mark = gs("big.mark"),
warn.deprecated = gs("warn.deprecated"))
# S3 method for netleague
print(
netmeta,
fixed = m.netmeta$m.netmeta$fixed,
random = m.netmeta$m.netmeta$random,
warn.deprecated = gs("warn.deprecated"))

write.table(league0$random, file = "league0-OS.csv",
row.names = FALSE, col.names = FALSE,
sep = ",")




############################################################################
################### Bayesian network meta analysis ###################
############################################################################
library(rjags)
library(tidyverse)
library(openxlsx)
library(gemtc)

NetworkGraphID <- read.xlsx("NetworkGraphID.xlsx")
NetworkGraphOS <- read.xlsx("NetworkGraphOS.xlsx")
network <- mtc.network(data.re = NetworkGraphOS,
treatments = NetworkGraphID)

summary(network)

plot(network,use.description = TRUE)

# better plot
library(igraph)
set.seed(12345) # set seed for reproducibility
plot(network,
use.description = TRUE, # Use full treatment names
vertex.color = "red", # node color
vertex.label.color = "gray10", # treatment label color
vertex.shape = "sphere", # shape of the node
vertex.label.family = "Helvetica", # label font
vertex.size = 20, # size of the node
vertex.label.dist = 3, # distance label-node center
vertex.label.cex = 1.0, # node label size
edge.curved = 0.1, # edge curvature
layout = layout.fruchterman.reingold)

# We give our compiled model the name `model`.
model <- mtc.model(network,
likelihood = "binom",
link = "cloglog",
linearModel = "random",
n.chain = 4)

#mcmc
mcmc1 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)
mcmc2 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)
mcmc3 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)
mcmc4 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)
mcmc5 <- mtc.run(model, n.adapt = 5000, n.iter = 1e5, thin = 10)

gelman.diag(mcmc1)$mpsrf
gelman.diag(mcmc2)$mpsrf
gelman.diag(mcmc3)$mpsrf
gelman.diag(mcmc4)$mpsrf
gelman.diag(mcmc5)$mpsrf

gelman.plot(mcmc4)
plot(mcmc4)

# rank probability
rank <- rank.probability(mcmc4, preferredDirection = 1)
plot(rank, beside=TRUE, xlab="Treatment Regiments OS", ylab="Probability")

#forest plot
forest(relative.effect(mcmc4, t1 = "comp"),
use.description = TRUE) # Use long treatment names)

# sucra
library(dmetar)
rank.probability <- rank.probability(mcmc4)
sucra <- dmetar::sucra(rank.probability, lower.is.better = FALSE)
sucra

plot(sucra)

#         SUCRA
# pil  0.927105
# ddt  0.619805
# pem  0.539265
# pilc 0.480915
# pdm  0.431035
# comp 0.001875


results <- relative.effect.table(mcmc4)
save(results, file = "results.csv")