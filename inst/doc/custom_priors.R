## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 7)

## -----------------------------------------------------------------------------
library(dscore)
builtin_keys[, c("key", "base_population")]

## -----------------------------------------------------------------------------
get_mu(t = c(0:12)/12, key = "gsed2406")

## -----------------------------------------------------------------------------
# Calculate the custom prior mean by adding 5 to the default prior mean
data <- milestones
mymean <- get_mu(t = data$age, key = "gsed2406") + 5

# Calculate default D-scores
def <- dscore(data)
head(def)

# Custom prior, direct specification
adj1 <- dscore(data, prior_mean = mymean)
head(adj1)

# Custom prior, column specification
adj2 <- dscore(cbind(data, mymean), prior_mean = "mymean")
head(adj2)

identical(adj1, adj2)

## -----------------------------------------------------------------------------
# Plot the difference between adjusted and default D-scores
plot(y = adj1$d - def$d, x = def$p, 
     xlab = "Proportion of items passed by the child", 
     ylab = "Upward drift of D-score", 
     pch = 16, main = "Impact of Custom Prior Mean on D-score")

# Add a smoothed line to visualize the trend
lines(lowess(x = def$p, y = adj1$d - def$d, f = 0.5), col = "grey", lwd = 2)

## -----------------------------------------------------------------------------
# Filter data for a specific child
boy <- milestones[milestones$id == 111, ] 

# Calculate default D-scores
def <- dscore(boy)
def

## -----------------------------------------------------------------------------
# Calculate expected D-scores and standard deviations
exp_d <- zad(z = c(0, def$daz[1:3]), x = def$a)
exp_sd <- c(5, def$sem[1:3])

# Calculate adjusted D-scores using the custom prior mean and standard deviation
adj1 <- dscore(boy, prior_mean = exp_d, prior_sd = exp_sd)

## ----fig.height=4-------------------------------------------------------------
# Plotting the raw and informed DAZ trajectories
plot(x = def$a, y = def$daz, type = "b", pch = 16, 
     ylab = "DAZ", xlab = "Age (years)", 
     main = "Standard (black) and Informed (red) DAZ-trajectory for child 111")
points(x = adj1$a, y = adj1$daz, col = "red", type = "b", lwd = 2, pch = 16)

## -----------------------------------------------------------------------------
# Set missing ages for specific observations
boy$age[2:3] <- NA

# Calculate D-scores using default
def <- dscore(boy)
def

## -----------------------------------------------------------------------------
# Calculate D-scores for missing ages using age-independent priors
adj1 <- dscore(boy, prior_mean_NA = 50, prior_sd_NA = 20)
adj1

## -----------------------------------------------------------------------------
# Forcing D-scores for missing ages to value -1
adj2 <- dscore(boy, prior_mean_NA = -1, prior_sd_NA = 0.001)
adj2

