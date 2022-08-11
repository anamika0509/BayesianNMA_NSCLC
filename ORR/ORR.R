# oddsratio -> metabin -> forest plot

library(openxlsx)
library(dmetar)
library(tidyverse)
library(meta)

PoolingData <- read.xlsx("PoolingData.xlsx")
glimpse(PoolingData)

m.bin <- metabin(event.e = event.e,
n.e = n.e,
event.c = event.c,
n.c = n.c,
studlab = STUDYNAME,
data = PoolingData,
sm = "OR",
method = "MH",
MH.exact = TRUE,
comb.fixed = FALSE,
comb.random = TRUE,
method.tau = "PM",
hakn = TRUE,
title = "ICI vs Chemo")

m.bin

# forest plot
forest.meta(m.bin,sortvar = TE)

# funnel plot
# Produce funnel plot
funnel.meta(m.bin,
studlab = TRUE)
# Add title
title("Funnel Plot OR (ICI vs Chemo)")

# egger's test
eggers.test(m.gen)
# Eggers' test of the intercept 
# ============================= 

#  intercept   95% CI     t          p
#      3.615 0 - 7.23 1.962 0.06735807

# Eggers' test does not indicate the presence of funnel plot asymmetry. 



############################################################################
################### Bayesian network meta analysis ###################
############################################################################
library(rjags)
library(tidyverse)
library(openxlsx)
library(gemtc)

file2read <- "NetworkGraphID.xlsx"
NetworkGraphID <- read.xlsx(file2read)
file2read <- "NetworkGraphOR.xlsx"
NetworkGraphOR <- read.xlsx(file2read)
network <- mtc.network(data.ab = NetworkGraphOR,
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
link = "logit",
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
plot(rank, beside=TRUE, xlab="Treatment Regiments ORR", ylab="Probability")

#forest plot
forest(relative.effect(mcmc4, t1 = "comp"),
use.description = TRUE) # Use long treatment names)

# sucra
library(dmetar)
rank.probability <- rank.probability(mcmc4)
sucra <- dmetar::sucra(rank.probability, lower.is.better = FALSE)
sucra

#         SUCRA
# pdm  0.775520
# pil  0.675250
# pem  0.663615
# pilc 0.537165
# ddt  0.181085
# comp 0.167365


results <- relative.effect.table(mcmc4)
save(results, file = "results.csv")


# library(rjags)
# library(tidyverse)
# library(openxlsx)
# library(gemtc)

# #### regression analysis ####
# NetworkGraphID <- read.xlsx("NetworkGraphID.xlsx")
# NetworkGraphOR <- read.xlsx("NetworkGraphOR.xlsx")
# NetworkGraphOR_study_risk <- read.xlsx("NetworkGraphOR_study_risk.xlsx")

# network.mr <- mtc.network(data.ab = NetworkGraphOR,
# studies = NetworkGraphOR_study_risk,
# treatments = NetworkGraphID)

# regressor <- list(coefficient = "shared",
# variable = "rob",
# control = "comp")

# model.mr <- mtc.model(network.mr,
# likelihood = "binom",
# link = "logit",
# type = "regression",
# regressor = regressor)

# mcmc1 <- mtc.run(model.mr, n.adapt = 5000, n.iter = 1e5, thin = 10)

# summary(mcmc1)

# forest(relative.effect(mcmc1, t1 = "comp", covariate = 1),
# use.description = TRUE)
# title("High Risk of Bias")
# forest(relative.effect(mcmc1, t1 = "comp", covariate = 0),
# use.description = TRUE)
# title("Low Risk of Bias")