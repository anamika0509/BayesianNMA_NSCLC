# pfshr
library(dmetar)
library(netmeta)
library(openxlsx)
NETWORK_PLOT <- read.xlsx("NETWORK_PLOT_ORR.xlsx")

m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
treat1 = treat1,
treat2 = treat2,
studlab = study,
data = NETWORK_PLOT,
sm = "OR",
comb.fixed = FALSE,
comb.random = TRUE,
reference.group = "comp",
details.chkmultiarm = TRUE,
sep.trts = " vs ")

m.netmeta 

netrank(m.netmeta, small.values = "good")

#      P-score
# pil   0.8720
# pdm   0.7091
# pem   0.6511
# pilc  0.4509
# ddt   0.2565
# comp  0.0604