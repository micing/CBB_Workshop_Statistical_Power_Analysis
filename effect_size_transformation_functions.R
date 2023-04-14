#### References ####

# Lakens (2013) can be found here:
# https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full

# Polanin & Snilstveit (2016):
# https://onlinelibrary.wiley.com/doi/full/10.4073/cmpn.2016.3

# G*Power and it's manual can be downloaded here:
# https://www.psychologie.hhu.de/arbeitsgruppen/allgemeine-psychologie-und-arbeitspsychologie/gpower.html
# https://www.psychologie.hhu.de/fileadmin/redaktion/Fakultaeten/Mathematisch-Naturwissenschaftliche_Fakultaet/Psychologie/AAP/gpower/GPowerManual.pdf

#### Functions to transform between different effect sizes #####

# generic calculation of Cohen's d effect size for means
es.d <- function(delta, sd) {delta/sd} 

# replication of ES.h from "pwr" package, effect size for proportions, ref Cohen (1988)
es.h <- function(p1, p2) {2*asin(sqrt(p1))-2*asin(sqrt(p2))}

# Lakens equation 1
cohens_d <- function(m1,m2,sd1,sd2,n1,n2) { 
  (m1-m2) / sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2) )
}

# Lakens equation 2
d_from_t <- function(t, n1, n2) {t*sqrt(1/n1+1/n2)} 

# Lakens equation 3
r_from_d <- function(d, n1, n2) { 
  a=((n1+n2)^2-2*(n1+n2))/(n1*n2)
  d/sqrt(d^2+a)
}

# Lakens equation 4
hedges_g <- function(d, n1, n2) {d*(1-3/(4*(n1+n2)-9))} 

# Lakens equation 7
d_from_t_within <- function(t, n) {t/sqrt(n)} 

# lakens equation 11
eta2 <- function(SS_effect, SS_total) {SS_effect/SS_total}

# lakens equation 12
eta2p <- function(SS_effect, SS_error) {SS_effect/(SS_effect+SS_error)}

# lakens equation 13
eta2p_from_F <- function(F, df_effect, df_error) {(F*df_effect)/(F*df_effect+df_error)}

# G*power manual chapter 10
f_from_eta2 <- function(eta2) { sqrt(eta2/(1-eta2)) }
eta2_from_f <- function(f){ f^2/(1+f^2) }

# G*power manual chapter 13
f2_from_R2 <- function(R2) {R2/(1-R2)}
R2_from_f2 <- function(f2) {f2/(1+f2)}

# Standarized mean difference and odds ratios
# Polanin & Snilstveit (2016):
d_from_or <- function(or) {log(or)*sqrt(3)/pi}
or_from_d <- function(d) {exp(d*pi/sqrt(3))}

RR_from_OR <- function(or, p) {or / (1 - p + (p * or))}







