library(tidyverse)

## Confidence Interval simulation
## ------------------------------
##
## Demonstrate properties of (95%) confidence intervals and how to
## calculate them. Note that some confidence intervals _will not_
## contain the true value.
##
##
## Definition of a confidence interval (CI) is an interval generated
## by a procedure that produces CIs that cover true parameter value
## with a desired frequency over repeated samples.
##
## That is,
##    1. sample repeatedly from the population
##    2. calculate a CI for each sample (given a CI procedure)
##    3. X% of these CIs will contain true value
##
## Notably, an individual (realized) CI may not contain the true
## value. In practice, it is not possible to know hwhether the CI from
## your data covers the true value or not.
##
## People often say that a 95% CI has a 95% probability of containing
## the true parameter value. This is wrong. A realized CI either
## contains the true value or not.


## population distribution
pop_mean <- 5
pop_sd <- 2


serr <- function(x) sd(x)/sqrt(length(x)) # standard error
is_covered <- function(mu, lo, hi) {      # does CI cover true value?
  lo <= mu && mu <= hi
  }


t <-
  # take 100 samples from the population
  tibble(s = replicate(100,
                       rnorm(1000, mean=pop_mean, sd=pop_sd),
                       simplify=FALSE)) %>%
  # summarize sample mean and standard error for each sample
  mutate(mu = map_dbl(s, mean),
         se = map_dbl(s, serr)) %>%
  ## calculate 95% CI using Wald procedure (+/- 2*SE)
  ## and test if CI covers true mean
  ## `rowwise()` is needed for `is_covered` to operate on one row at a time
  rowwise() %>%
  mutate(lo = mu - 2*se,
         hi = mu + 2*se,
         cover = is_covered(pop_mean, lo, hi))

## plot sample means with CIs. Red CIs do not contain the true mean
ggplot(t) +
  geom_hline(yintercept=pop_mean, linetype="dashed", size=0.2) +
  geom_pointrange(
    aes(x=seq_along(mu),
        y=mu, ymin=lo, ymax=hi,
        color=cover),
    fatten=2) +
  theme_void() +
  theme(legend.position="none", aspect.ratio = 0.1) +
  scale_color_manual(values = c("red", "black"))
