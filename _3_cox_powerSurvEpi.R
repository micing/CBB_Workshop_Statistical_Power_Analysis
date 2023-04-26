library("tidyverse")
library("gridExtra")
library("powerSurvEpi")

#### Definitions ####

hr=c(.5, .6, .7, .8) # assumed Hazard Ratios
alpha = .05 # The tests type-1 error rate
power = .80 # statistical power needed
ptreat = .50 # .proportion of subjects treated/exposed
rho2 = .00 # confounding: square of the correlation between covariate of interest and other covariates 

cases = seq(1,1000) # the range of cases (outcomes) to calculate and plot
n=10000 # total number of subjects (not used by this function, 
        # it is based only on the number cases and suitable for rare diseases/events. 
        # Consider the limitations for other types of events)

#### Sample size (i.e. cases/deaths needed) and Power calculations ####

cases_needed = c()
for (r in hr) { 
  cases_needed=c(cases_needed, 
                 numDEpi.default(power=power, theta=r, p=ptreat, rho2=rho2, alpha = alpha)) 
  }

statistical_power = expand.grid(cases=cases, hr=hr) %>% 
  mutate(power=powerEpi.default(n=n, theta=hr, p=ptreat, psi=cases/n, rho2=rho2, alpha=alpha)) %>%
  mutate(hr=factor(hr)) %>% as_tibble()
        
#### Create plot ####

p1=statistical_power %>% ggplot(aes(x=cases, group=hr), linewidth=.7) +
           geom_line(aes(y=power, color=hr)) +
           theme_minimal() + xlab("Number of cases") + ylab("Statistical power") +
           geom_abline(intercept=power, slope=0, linewidth=.2) +
           labs(color="Assumed\nHazard Ratio") +
           theme(legend.position = c(0.80, 0.15)) +
           ggtitle(paste0("Statistical power @ alpha=", alpha))


#### Create summary table ####

summary = tribble(~Variable, ~Estimate,
                  "Statistical Power:", power %>% format(digits=2),
                  "Type-1 error rate:", alpha %>% format(digits=2),
                  "Confounding:", rho2 %>% format(digits=2),
                  "Assumed HRs:", paste(hr %>% format(digits=2), collapse=", "),
                  "Cases needed:", paste(cases_needed %>% format(digits=2), collapse=", "))

p2 = tableGrob(summary, rows=NULL)

#### Print results ####

grid.arrange(arrangeGrob(p1, p2, ncol=2))




