# pfshr
library(dmetar)
library(netmeta)
library(openxlsx)
NETWORK_PLOT <- read.xlsx("NetworkGraphPFS_freq.xlsx")

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

netrank(m.netmeta, small.values = "good")

#      P-score
# pil   0.9969
# ddt   0.7509
# pem   0.5321
# pilc  0.4589
# pdm   0.2608
# comp  0.0003