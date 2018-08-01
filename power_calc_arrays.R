#####################################################
###Implementation of the SSPA bioconductor package###
#####################################################

#####################################################
##See SSPA vignette for more details              ###
#####################################################

source("https://bioconductor.org/biocLite.R")

biocLite("SSPA")

biocLite("genefilter")

library(SSPA)

library(genefilter)

set.seed(854379)

# two group experiment is assumed.
# m: number of features, J: number of samples per group

m <-1300

J <-12

# pi0 the proportion of non-differentially regulated genes
pi0 <-0.95

m0 <-as.integer(m*pi0)

# values for a, b and m were derived from the pilot proteomic experiment

mu<-rbitri(m-m0, a=log2(5.86), b=log2(10.87), m= log2(7.469))

data<- simdat(mu, m=m, pi0=pi0, J=J, noise=0.01)

statistics<-rowttests(data, factor(rep(c(0,1), each=J)))$statistic

pdD<-pilotData(statistics=statistics,
               samplesize = sqrt(1/(1/J + 1/J)),
               distribution ="norm")
pdD

plot(pdD)

ssD <- sampleSize(pdD, control=list(from=-6, to=6))
ssD

plot(ssD, panel = function(x, y, ...)
  {
  panel.xyplot(x, y)
  panel.curve(dbitri(x), lwd =2, lty=2, n=500)
  
}, ylim=c(0, 0.6))

Jpred<-seq(10,20, by=2)

N<-sqrt(Jpred/2)

pwrD<-predictpower(ssD, samplesizes = N, alpha=0.05)

matplot(Jpred, pwrD, type="b", pch=16, ylim=c(0,1),
        ylab="predicted power", xlab="sample size (per group)")

grid()
