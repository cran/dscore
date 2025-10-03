## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 7)

## ----eval=FALSE---------------------------------------------------------------
# # Example (pseudo-code): compute a D-score given a key
# # library(dscore)
# # d1 <- dscore(items = x, key = "gsed2406")
# # d2 <- dscore(items = x, key = "gsed2510")
# # cbind(d1, d2, diff = d2 - d1)

## ----eval=FALSE---------------------------------------------------------------
# # Check the package default key (example; replace with your function)
# # dscore::get_default_key()

## ----eval=FALSE---------------------------------------------------------------
# # Example (replace with the actual function name)
# # keys <- dscore::list_keys()
# # keys

## ----eval=FALSE---------------------------------------------------------------
# # Example usage
# # ds <- dscore(data, key = "gsed2406")

## ----eval=FALSE---------------------------------------------------------------
# # Session-level default (illustrative; adapt to your API)
# # options(dscore.default_key = "gsed2510")
# 
# # Confirm
# # getOption("dscore.default_key")

## ----eval=FALSE---------------------------------------------------------------
# # Example skeleton
# # ds1 <- dscore(dat, key = "gsed2406")
# # ds2 <- dscore(dat, key = "gsed2510")
# # comp <- transform(dat, d1 = ds1$D, d2 = ds2$D, diff = ds2$D - ds1$D)
# # head(comp)

## ----eval=FALSE---------------------------------------------------------------
# # Histogram of differences
# # hist(comp$diff, main = "D-score differences (new - old)", xlab = "Difference")
# 
# # Correlation and summary
# # cor(comp$d1, comp$d2, use = "complete.obs")
# # summary(comp$diff)

## ----eval=FALSE---------------------------------------------------------------
# # Example summary
# # mad <- median(abs(comp$diff), na.rm = TRUE)
# # quant <- quantile(comp$diff, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
# # list(median_abs_diff = mad, quantiles = quant)

## ----eval=FALSE---------------------------------------------------------------
# # dat <- read.csv("your_data.csv")
# # ds_default <- dscore(dat) # implicit default key
# # head(ds_default)

## ----eval=FALSE---------------------------------------------------------------
# # ds_old <- dscore(dat, key = "gsed2406")
# # ds_new <- dscore(dat, key = "gsed2510")
# # delta <- ds_new$D - ds_old$D
# # summary(delta)

## ----eval=FALSE---------------------------------------------------------------
# # plot(ds_old$D, ds_new$D,
# #      xlab = "Old key D-score",
# #      ylab = "New key D-score",
# #      main = "D-scores under two keys")
# # abline(0, 1, lty = 2)

## ----session-info, echo=FALSE-------------------------------------------------
sessionInfo()

