library("tidyverse")
library("gridExtra")
library("pwr")

#### Definitions ####

ds = c(.4, .45, .5, .55) -.25 # assumed group difference (Cohen's d)
alpha = .05 # The tests type-1 error rate
power = .90 # statistical power needed
alternative = "two.sided" # alternate hypothesis: two.sided or less or greater

n = seq(5,1000) # the range of sample sizes to calculate and plot

#### Sample size and Power calculations ####

sample_size_needed = c()
for (d in ds) { 
  sample_size_needed = c(sample_size_needed, 
                         pwr.t.test(n = NULL, d = d, sig.level = alpha, 
                                    power = power, alternative = alternative)$n %>% 
                           ceiling()) 
}

statistical_power = expand.grid(n=n, d=ds) %>% 
  mutate(power=pwr.t.test(n = n, d = d, sig.level = alpha, 
                          power = NULL, alternative = alternative)$power) %>%
  mutate(d=factor(d)) %>% as_tibble()

#### Create plot ####

p1=statistical_power %>% ggplot(aes(x=n, group=d), linewidth=.7) +
  geom_line(aes(y=power, color=d)) +
  theme_minimal() + xlab("Sample size") + ylab("Statistical power") +
  geom_abline(intercept=power, slope=0, linewidth=.2) +
  labs(color="Assumed\ndifference (d)") +
  theme(legend.position = c(0.80, 0.15)) +
  ggtitle(paste0("Statistical power @ alpha=", alpha))


#### Create summary table ####

summary = tribble(~Variable, ~Estimate,
                  "Statistical Power:", power %>% format(digits=2),
                  "Type-1 error rate:", alpha %>% format(digits=2),
                  "Alternate hypothesis:", alternative,
                  "Assumed difference (d):", paste(ds %>% format(digits=2), collapse=", "),
                  "Sample size needed:", paste(sample_size_needed %>% format(digits=2), collapse=", "))

p2 = tableGrob(summary, rows=NULL)

#### Print results ####

grid.arrange(arrangeGrob(p1, p2, ncol=2))




