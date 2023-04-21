library("tidyverse")
library("gridExtra")
library("epiR")

#### Definitions ####

irr=c(1.1, 1.2, 1.3, 1.4) # incidence rate ratios for exposed/unexposed
ir = .01 # incidence rate in the unexposed group
ft = 100 # followup time
conf.level = 0.95 # The confidence interval to use for the test
sided.test = 2 # one or two sided test
power = .90 # statistical power needed
re = 1 # ratio exposed/unexposed
n=seq(1, 5000, 10) # range of total sample sizes to use in calculations

#### Sample size and Power calculations ####

sample_size_needed = c()
for (r in irr) { sample_size_needed=c(sample_size_needed, epi.sscohortt(FT = ft, irexp1 =ir*r, irexp0 = ir, n = NA, power = power,
                                                           r = re, sided.test = sided.test, conf.level = conf.level)$n.total) }

statistical_power=expand.grid(n=n, irr=irr) %>% rowwise() %>%
  mutate(power=epi.sscohortt(FT = ft, irexp1 =ir*irr, irexp0 = ir, n = n, power = NA,
                             r = re, sided.test = sided.test, conf.level = conf.level)$power) %>%
  mutate(irr=factor(irr)) %>% as_tibble()

#### Create plot ####

p1=statistical_power %>% ggplot(aes(x=n, group=irr), linewidth=.7) +
  geom_line(aes(y=power, color=irr)) +
  theme_minimal() + xlab("Total sample size") + ylab("Statistical power") +
  geom_abline(intercept=power, slope=0, linewidth=.2) +
  labs(color="Assumed\nIncidence Rate Ratios") +
  theme(legend.position = c(0.80, 0.15)) +
  ggtitle(paste0("Statistical power @ ",conf.level*100,"% Confidence Intervals"))


#### Create summary table ####

summary = tribble(~Variable, ~Estimate,
                  "Statistical Power:", power %>% format(digits=2),
                  "Confidence level:", conf.level %>% format(digits=2),
                  "One or two\nsided test:", sided.test %>% format(digits=2),
                  "Exposed/unexposed", re %>% format(digits=2),
                  "Incidence Rate\nfor unexposed", ir %>% format(digits=2),
                  "Followup time", ft %>% format(digits=2),
                  "Assumed IRRs:", paste(irr %>% format(digits=2), collapse=", "),
                  "Sample size needed:", paste(sample_size_needed %>% format(digits=2), collapse=", "))

p2 = tableGrob(summary, rows=NULL)

#### Print results ####

grid.arrange(arrangeGrob(p1, p2, ncol=2))
