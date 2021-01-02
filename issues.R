

setwd("~/Dropbox/Personal computer/Independent studies/Permanent websites/Metasens website/metasens_website (git)/metasens_website/Main site/tests_human_inspection/Datasets for website test")
d = read.csv("kodama_prepped.csv")


# most recent code version
setwd("~/Dropbox/Personal computer/Independent studies/Sensitivity analysis for publication bias (SAPB)/Linked to OSF (SAPB)/R package PublicationBias/PublicationBias/R")
source("functions.R")

library(robumeta)
library(metafor)

x = svalue( yi = d$yi,
        vi = d$vi,
        q = 0,
        #clustervar = cluster,
        model = "robust",
        favor.positive = TRUE,
        alpha.select = 0.50,
        return.worst.meta = TRUE )

x$stats$sval.est
