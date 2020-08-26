# BAYESIAN DATA ANALYSIS FUNDAMENTALS
# -----------------------------------
library(ggplot2)
library(dplyr)
library(purrr)
library(forcats)
library(ggridges)

# 1. What is Bayesian Data Analysis
# ---------------------------------
# A method for figuring out unobservable quantities given known facts that uses probability to describe
# the uncertainty over what the values of the unknown quantities could be.
# probabilistic inference := bayesian inference
prop_model <- function(data = c(), 
                       prior_prop = c(1, 1), 
                       n_draws = 10000, 
                       show_plot = TRUE) {
  check_factor <- function(f) {
    if (is.character(f)) {
      factor(f)
    } else if (is.factor(f)) {
      f
    } else {
      stop("`f` must be a factor (or character vector or numeric vector).", call. = FALSE)
    }
  }
  lvls_seq <- function(f) {
    seq_along(levels(f))
  }
  fct_rev<- function(f) {
    f <- check_factor(f)
    forcats::lvls_reorder(f, rev(lvls_seq(f)))
  }
  
  data <- as.logical(data)
  proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 
                                                                1, 20)))
  post_curves <- purrr::map_dfr(data_indices, function(i) {
    value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", 
                                            "Failure"))
    label <- paste0("n=", i)
    probability <- dbeta(proportion_success, prior_prop[1] + 
                           sum(data[seq_len(i)]), prior_prop[2] + sum(!data[seq_len(i)]))
    probability <- probability/max(probability)
    data_frame(value, label, proportion_success, probability)
  })
  post_curves$label <- fct_rev(factor(post_curves$label, levels = paste0("n=", 
                                                                         data_indices)))
  post_curves$value <- factor(post_curves$value, levels = c("Prior", 
                                                            "Success", "Failure"))
  p <- ggplot(post_curves, aes(x = proportion_success, y = label, 
                               height = probability, fill = value)) + ggjoy::geom_joy(stat = "identity", 
                                                                             color = "white", alpha = 0.8, panel_scaling = TRUE, size = 1) + 
    scale_y_discrete("", expand = c(0.01, 0)) + scale_x_continuous("Underlying proportion of success") + 
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), 
                      name = "", drop = FALSE, labels = c("Prior   ", "Success   ", 
                                                          "Failure   ")) + theme_light(base_size = 18) + 
    theme(legend.position = "top")
  if (show_plot) {
    print(p)
  }
  invisible(rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + 
                    sum(!data)))
}
# implements a bayesian model that assumes that:
# - the data is a vector of successes and failures represented by 1s and 0s
# - there is an unknown underlying proportion of success
# - prior to being updated with data any underlying proportion of success is equally likely
# ..assume you just flipped a coin 13x and the result was heads, tails, tails, heads, tails etc.:
data <- c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
prop_model(data)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")
median(posterior)
quantile(posterior, c(0.05, 0.95)) # 95% CI
sum(posterior>0.07) / length(posterior)
# output of prop_model is a plot showing what the model learns about the underlying proportion of 
# success from each data point in the order you entered them. At n=0 there is no data, and all the 
# model knows is that it's equally probable that the proportion of success is anything from 0% to 100%. 
# At n=4 all data has been added, and the model knows a little bit more.
# remarks:
# prior: a probability distribution that represents what the model knows before seeing the data
# posterior:a probability distribution that represents what the model knows after having seen the data


# 2. How does Bayesian inference work
# -----------------------------------
# 3 inputs for bayesian inference: 1. data 2. generative model 3. priors

# generative model
# ---
# simulation: assuming the underlying proportion of success of curing a zombie is 42% and that you 
# administer the drug to 100 zombies.
# parameters:
prop_success <- 0.42
n_zombies <- 100
# simulating data
data <- c()
for(zombie in 1:n_zombies) {
  data[zombie] <- runif(1, min = 0, max = 1) < prop_success
}
data <- as.numeric(data)
sum(data) / length(data)

# .. this is a binomial process:
rbinom(n = 1, size = 100, prob = 0.42) / 100
# repeat 200x:
data2 <- rbinom(n = 200, size = 100, prob = 0.42) / 100

# representing uncertainty with priors..
# ---
# not so sure that your ad will get clicked on exactly 10% of the time. Instead of assigning proportion_clicks
# a single value you are now going to assign it a large number of values drawn from a probability distribution.
n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n = n_samples, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)
prior<- data.frame(n_visitors, proportion_clicks)
hist(proportion_clicks)
hist(n_visitors)
plot(prior)

# data
# ---
# You ran your ad campaign, and 13 people clicked and visited your site when the ad was shown a 100 times. 
# You would now like to use this new information to update the Bayesian model.
posterior <- prior[prior$n_visitors == 13, ]
hist(posterior$proportion_clicks)
# .. now we want to use this updated proportion_clicks to predict how many visitors we would get if we reran 
# the ad campaign
prior <- posterior
# replace prior$n_visitors with a new sample and visualize the result
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown, prob = prior$proportion_clicks)
hist(prior$n_visitors)
# calculate the probability that you will get 5 or more visitors
sum(prior$n_visitors >= 5)/length(prior$n_visitors)


# 3. Why use Bayesian Data Analysis
# ---------------------------------

# 4 good things with bayes:
# - you can include information sources in addition to data (expert opinion, background info, common knowledge)
# - you can make any comparisons between groups or data sets
# - you can use the result of bayesian analysis to do decision analysis
# - you can change the underlying statistical model

# including other information sources
# ---
# use beta distribution which is bound from 0-1 but can take many different shapes to estimate prior..
# A Beta(1,1) distribution is the same as a uniform distribution between 0 and 1. It is useful 
# as a so-called non-informative prior as it expresses than any value from 0 to 1 is equally likely
beta_sample <- stats::rbeta(n = 1000000, shape1 = 100, shape2 = 20)
hist(beta_sample, breaks = 5)
# Pick the prior that best captures the information:
# .. Most ads get clicked on 5% of the time, but for some ads it is as low as 2% and for others as high as 8%.
beta_sample28 <- stats::rbeta(n = 1000000, shape1 = 5, shape2 = 95)
hist(beta_sample28, breaks = 500)

# previous example with beta-distribution as underlying model..
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- stats::rbeta(n_draws, shape1 = 5, shape2 = 95)
n_visitors <- rbinom(n_draws, size = n_ads_shown, prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

# plot the prior and the posterior in the same plot..
par(mfcol = c(2, 1))
hist(prior$proportion_clicks, xlim = c(0, 0.25))
hist(posterior$proportion_clicks, xlim = c(0, 0.25))


# comparisons between groups
# ---
# for example, video vs text adds comparison..


# changing underlying distribution: poisson distribution
# ---
# 1 parameter: average number of events per unit
x <- rpois(n = 10000, lambda = 3)
# Let's say that you run an ice cream stand and on cloudy days you on 
# average sell 11.5 ice creams. It's a cloudy day. Change the rpois call 
# to visualize the probability distribution over how many ice creams you'll sell.
x <- rpois(n = 10000, lambda = 11.5)
# It's still a cloudy day, and unfortunately, you won't break even unless you 
# sell 15 or more ice creams. Assuming the Poisson model is reasonable, use x 
# to calculate the probability that you'll break even.
sum(x >= 15) / length(x)


# 4. Bayesian inference with Bayes' theorem
# -----------------------------------------
# probability review with 52 card deck..
# prob(4 aces):
4/52 * 3/51 * 2/50 * 1/49 # using product rule
# fast calculation of probabilities: prob(visitors=13 | prob_success=0.10) given 100 adds
x <- rbinom(n = 10000, size = 100, prob = 0.1)
sum(x == 13) / length(x)
# alternatively, use dbinom:
dbinom(x = 13, size = 100, prob = 0.1)
# probabilities for a range of visitors:
v <- seq(0, 100, by = 1)
y <- dbinom(x = v, size = 100, prob = 0.1)
plot(v, y, type = "h")


# 5. More parameters, more data, and more Bayes
# ---------------------------------------------






