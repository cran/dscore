## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(dscore)

## ----daz----------------------------------------------------------------------
# Create a dataset of five 13-month old children scoring 5 GSED items 
dm <- matrix(
  c(
    13, 0, 0, 0, 0, 0,
    13, 1, 0, 0, 0, 0,
    13, 1, 1, 1, 0, 0,
    13, 1, 1, 1, 1, 0,
    13, 1, 1, 1, 1, 1
  ), 
  ncol = 6, byrow = TRUE)
colnames(dm) <- c("age", "gs1moc060", "gs1moc061", "gs1lgc062", "gs1sec063", "gs1moc064")

# Score the data using dscore function
output <- dscore(dm, xunit = "months")

# Add centile rankings to the output
output$centile <- round(100 * pnorm(output$daz), 1)

# View the scored data
head(output)


## ----NA-----------------------------------------------------------------------
# Create a dataset of five children of different ages with the same scores
dm <- matrix(
  c(
    NA, 1, 1, 1, 1, 1,
    12, 1, 1, 1, 1, 1,
    18, 1, 1, 1, 1, 1,
    48, 1, 1, 1, 1, 1,
    -1, 1, 1, 1, 1, 1
  ), 
  ncol = 6, byrow = TRUE)
colnames(dm) <- c("age", "gs1moc060", "gs1moc061", "gs1lgc062", "gs1sec063", "gs1moc064")

dscore(dm, xunit = "months")


## ----inf----------------------------------------------------------------------
### Get a list of all GSED item names 
gsed_names <- get_itemnames(instrument = "gs1")

### Create a sample dataframe where all responses are 1
df <- as.data.frame(setNames(as.list(rep(1, length(gsed_names))), gsed_names)) 

### Add an age (in months) of 3
df$age <- 3

dscore(df, xunit = "months")

