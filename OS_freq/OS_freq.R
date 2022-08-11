# pfshr
library(dmetar)
library(netmeta)
library(openxlsx)
NETWORK_PLOT <- read.xlsx("NetworkGraphOS_freq.xlsx")

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
# pil   0.9999
# ddt   0.5973
# pem   0.5289
# pilc  0.4589
# pdm   0.4148
# comp  0.0001