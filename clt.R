## Central Limit Theorem (CLT)
## ------------------------------
##
## Demonstrate features of the CLT using simulation
##
## CLT defintion:
##
## Let $X_1$, $X_2$, $X_3$, ... be i.i.d.~random variables, each with
## mean $\mu$ and variance $\sigma^2$.
##
## Then, the sample mean $\bar{X}_n$ of $n$ such variables is itself a
## random variable.
##
## The CLT describes (a property of) the distribution of this random
## variable $\bar{X}_n$. Namely, in the limit as $n \to \infty$,
## $\bar{X}_n$ follows a _normal distribution_ with mean $\mu$ and
## variance $\sigma^2 / n$. That is,
##
## $$ \bar{X}_n \sim \mathcal{N}(\mu,\,\sigma^2/n) $$
##
## This fact holds _regardless of the distribution of the original
## random variables $X_i$._ Note, $\mu$ and $\sigma$ must be finite.
##
## The values of the mean and variance of the sampling distribution
## are known from the properties of expectation and variance, not from
## the CLT. The CLT adds the information about the distribution
## apporaching Normality.
##
## Nonetheless, its useful to now list all the facts about the sample
## mean together:
##
##   * $\mathop{\mathbb{E}} \bar{X}_n = \mu$
##   * $\mathop{Var}\bar{X}_n = \sigma^2/n$
##   * $\bar{X}_n \sim $\mathcal{N}(\mu,\, \sigma^2/n)
##
## The variance, notably, depends on both the variance of the
## population distribution $\sigma$ _and_ the sample size _n_. Larger
## sample sizes lead to smaller variance in our estimate of the
## population mean $\mu$.
##
##
## An example: Roll a die 10 times and average the results. This
## average is the _sample mean_ for one iteration of this experiment,
## namely the experiment of rolling a die 10 times. If we repeat this
## measurement many times, we will get different mean values each
## time. If we plot a histogram of these values, we will see that
## their _distribution_ approaches normal as we repeat the experiment
## more and more. That is, the values of this averaging measure follow
## a distribution, and that distribution is approximately
## normal. Notably, the spread of the distribution also depends on the
## value $n$ (in this case, 10), which is the number of values in each
## average.


## The following code recreates Figure 10.5 from Blitzstein and Hwang,
## Introduction to Probability.

## Demonstration of approach to normality of sample mean as we
## increase number of random variates per trial, even for variates
## that do not come from a normal distribution.

## generate random variates from Binomial, Poission, Exponential, and Beta distributions
fbin  <- function(N) { rbinom(N, size=10, prob=0.9) }
fpois <- function(N) { rpois(N, lambda=2) }
fexpo <- function(N) { rexp(N, rate=1) }
fbeta <- function(N) { rbeta(N, shape1=0.8, shape2=0.8) }

## Each plot contains 10,000 simulated trials
ntrials <- 10000

## Each row contains simulated samples from one non-normal distribution...
dists   <- c(fbin, fpois, fexpo, fbeta)
ylabels <- c("Bin(10, 0.9)", "Pois(2)", "Expo(1)", "Beta(0.8, 0.8)")

## ...for each of 4 different values for the number of r.v's taken for each sample mean
## e.g. in the last column, each trial is the average of 100 realized r.v. values
n <- c(1, 5, 30, 100)

## broken out for-loop steps for clarity
par(mfrow=c(4, 4), oma=c(2, 6, 1, 0), mar=c(2,1,3,1), yaxt="n")
i <- 1
mapply(function(dist, label, i) {
  lapply(n, function (n.) {
    ## simulate trials, get sample mean
    draws <- dist(n. * ntrials)          # simulate draws from the distribution
    trials <- matrix(draws, ntrials, n.) # each row of matrix is one experiment of n. draws
    means <- apply(trials, 1, mean)      # take the sample mean for each trial

    ## plot
    hist(means, main="", xlab="")

    ## labels
    if (n. == 1)
      mtext(label, line=1, side=2, adj=1, las=2, cex=0.7)
    if (i == 1)
      mtext(paste0("n=", n.), side=3, line=1, outer=F, adj=0.5, cex=0.7)
  })
}, dists, ylabels, 1:4)
