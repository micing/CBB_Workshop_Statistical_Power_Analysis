library("pwr")
source("effect_size_transformation_functions.R")

#### t-test ####
# t-test of two independent groups: d=.20, n=20 * 2 & alpha=.05
pwr.t.test(n=20, d=.20, power=NULL, sig.level=.05)

# n=100*2
pwr.t.test(n=100, d=.20,  power=NULL)

# sample size calculation to achive 90% power
pwr.t.test(n=NULL, power=.90, d=.20)

# power for unequal size groups n1=150, n2=50
pwr.t2n.test(n1=150, n2=50, d=.20,  power=NULL)

# one tailed test alpha=.01
pwr.t2n.test(n1=150, n2=50, d=.20, power=NULL, sig.level=.01, alternative="greater")

# sample size calculation for paired t-test
pwr.t.test(n=NULL, power=.90, d=.20, type="paired")

# one sample differ from 0
pwr.t.test(n=NULL, power=.90, d=.20, type="one.sample")

#### Correlations ####

# sample size to achieve 80% power for assumed correlation r=.20
pwr.r.test(n=NULL, r=.20, power=.80)

# power for n=100, r=.40 with alpha=.001
pwr.r.test(n=100, r=.40, power=NULL, sig.level=.001)

# report only the alpha to achieve 80% power with n=100, r=.3
pwr.r.test(n=100, r=.30, power=.80, sig.level=NULL)$sig.level

#### ANOVA ####
#power of oneway anova with 4 groups and 20 subjects per group with 10% explained variance
f=f_from_eta2(.10)
pwr.anova.test(k=4,n=20,f=f, power=NULL)

#sample size to achieve 90% power
pwr.anova.test(k=4,n=NULL,f=f, power=.90)

#### Proportions ####

# power to detect a difference between two equal sized groups 
# with 10% and 20% prevalence at alpha=.01 and n=100

h=es.h(.10, .20)
pwr.2p.test(h=h, n=100, power=NULL)
pwr.2p2n.test(h=h, n1=100, n2=100, power=NULL)

# Samplesize to detect a difference between two equal sized groups 
# with 10% and 20% prevalence at alpha=.01 and 90% power

h=es.h(.10, .20)
pwr.2p.test(h=h, n=NULL, power=.90, sig.level=.01)

#45 & 55%
h=es.h(.45, .55)
pwr.2p.test(h=h, n=NULL, power=.90, sig.level=.01)

#33% and 66%
h=es.h(.33, .66)
pwr.2p.test(h=h, n=NULL, power=.90, sig.level=.01)

# power to detect difference between 20% & 30% in groups of 30 and 50 at alpha=.05
h=es.h(.20, .30)
pwr.2p2n.test(h=h, n1=30, n2=50, power=NULL)

#### Multiple regression ####

# approximate power for linear regression with 10% explained variance
# 1 predictor and 30 observations

f2=f2_from_R2(.10)
pwr.f2.test(u=1, v=30-(1+1), f2=f2, power=NULL)










