library(tidyverse)

#IGFdata = readRDS("data/observational_igf.RData")

sampledata = read_csv("data/obs_data.csv")

sampledata$U = rnorm(nrow(sampledata))

source("ContinuousPGM.R")

ggplot(sampledata,aes(x=PI3K,y=AKT)) + geom_point() + geom_smooth(method="gam")

fig_5_DAG = c("U~1",
              "SOS~U",
              "Ras ~ SOS",
              "PI3K~ U + s(Ras)",
              "AKT ~ s(PI3K)",
              "Raf ~ AKT",
              "Mek ~ Raf",
              "Erk ~ Mek")

fig_5_methods = rep("glm",length(fig_5_DAG))
fig_5_methods[[4]] = "gam"
fig_5_methods[[5]] = "gam"

BN_Fig5 = BayesianNetwork$new(formvec=fig_5_DAG,methods=fig_5_methods)

BN_Fig5$fit(sampledata)
BN_Fig5$sample(1000)->a

plot(a$SOS,a$Ras)
plot(sampledata$SOS,sampledata$Ras)
hist(sampledata$PI3K)
qqnorm(sampledata$SOS)
qqline(sampledata$SOS)
