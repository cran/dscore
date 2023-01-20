## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("dscore")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("d-score/dscore")

## ----include=FALSE------------------------------------------------------------
stopifnot(packageVersion("dscore") >= "1.8.0")

## ----getlabels----------------------------------------------------------------
library(dscore)
get_labels("by3cgd018")

## -----------------------------------------------------------------------------
instrument <- "gs1"
items <- get_itemnames(instrument = instrument, order = "indm")
length(items)
head(items)

## -----------------------------------------------------------------------------
labels <- get_labels(items)
head(cbind(items, substr(labels, 1, 50)))

## -----------------------------------------------------------------------------
sf <- dscore::sample_sf
head(sf[, c(1:2, 101:105)])

## -----------------------------------------------------------------------------
colnames(sf)[3:141] <- items
head(sf[, c(1:2, 101:105)])

## -----------------------------------------------------------------------------
results <- dscore(sf, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
sf2 <- data.frame(sf, results)

## -----------------------------------------------------------------------------
items_motor <- get_itemnames(instrument = c("gs1", "gl1"), domain = c("mo", "gm"))
results <- dscore(sf, items = items_motor, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
instrument <- "gl1"
items <- get_itemnames(instrument = instrument)
length(items)
head(items)

## -----------------------------------------------------------------------------
items <- items[c(55:155, 1:54)]
head(items)

## -----------------------------------------------------------------------------
labels <- get_labels(items)
head(cbind(items, substr(labels, 1, 50)))

## -----------------------------------------------------------------------------
lf <- dscore::sample_lf
head(lf[, c(1:2, 60:64)])

## -----------------------------------------------------------------------------
colnames(lf)[3:157] <- items
head(lf[, c(1:2, 60:64)])

## -----------------------------------------------------------------------------
results <- dscore(lf, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
lf2 <- data.frame(lf, results)

## -----------------------------------------------------------------------------
items_motor <- get_itemnames(instrument = c("gs1", "gl1"), domain = c("mo", "gm"))
results <- dscore(lf, items = items_motor, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
instrument <- "gh1"
items <- get_itemnames(instrument = instrument, order = "indm")
length(items)
head(items)

## -----------------------------------------------------------------------------
labels <- get_labels(items)
head(cbind(items, substr(labels, 1, 50)))

## -----------------------------------------------------------------------------
hf <- dscore::sample_hf
head(hf[, c(1:2, 30:35)])

## -----------------------------------------------------------------------------
colnames(hf)[3:57] <- items
head(hf[, c(1:2, 30:35)])

## -----------------------------------------------------------------------------
results <- dscore(hf, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
hf2 <- data.frame(hf, results)

## -----------------------------------------------------------------------------
items_motor <- get_itemnames(instrument = c("gs1", "gl1", "gh1"), domain = c("mo", "gm"))
results <- dscore(hf, items = items_motor, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
instrument <- "gpa"
items <- get_itemnames(instrument = instrument, order = "indm")
length(items)
head(items)

## -----------------------------------------------------------------------------
labels <- get_labels(items)
head(cbind(items, substr(labels, 1, 50)))

## -----------------------------------------------------------------------------
sf <- dscore::sample_sf
head(sf[, c(1:2, 101:105)])

## -----------------------------------------------------------------------------
colnames(sf)[3:141] <- items
head(sf[, c(1:2, 101:105)])

## -----------------------------------------------------------------------------
results <- dscore(sf, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
sf3 <- data.frame(sf, results)

## -----------------------------------------------------------------------------
items_motor <- get_itemnames(instrument = c("gpa", "gto"), domain = c("mo", "gm"))
results <- dscore(sf, items = items_motor, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
instrument <- "gto"
items <- get_itemnames(instrument = instrument)
length(items)
head(items)

## -----------------------------------------------------------------------------
items <- items[c(55:155, 1:54)]
head(items)

## -----------------------------------------------------------------------------
labels <- get_labels(items)
head(cbind(items, substr(labels, 1, 50)))

## -----------------------------------------------------------------------------
lf <- dscore::sample_lf
head(lf[, c(1:2, 60:64)])

## -----------------------------------------------------------------------------
colnames(lf)[3:157] <- items
head(lf[, c(1:2, 60:64)])

## -----------------------------------------------------------------------------
results <- dscore(lf, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
lf3 <- data.frame(lf, results)

## -----------------------------------------------------------------------------
items_motor <- get_itemnames(instrument = c("gpa", "gto"), domain = c("mo", "gm"))
results <- dscore(lf, items = items_motor, xname = "agedays", xunit = "days")
head(results)

## -----------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
ref <- builtin_references %>% 
  filter(pop == "phase1") %>% 
  select(pop, age, mu, sigma, nu, tau, SDM2, SD0, SDP2)
head(ref)

## ----fig.height=5, fig.width=10, warning=FALSE--------------------------------
library(ggplot2)
library(patchwork)

r <- builtin_references %>% 
  filter(pop == "phase1" & age <= 3.5) %>% 
  mutate(m = age * 12)

lf2$ins <- "lf"; lf2$m <- lf2$a * 12
sf2$ins <- "sf"; sf2$m <- sf2$a * 12
data <- bind_rows(lf2, sf2)
g1 <- ggplot(data, aes(x = m, y = d, group = ins, color = ins)) + 
  theme_light() +
  annotate("polygon", x = c(r$age, rev(r$age)),
           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.06, fill = "#C5EDDE") +
  annotate("line", x = r$m, y = r$SDM2, lwd = 0.5, color = "#C5EDDE") +
  annotate("line", x = r$m, y = r$SDP2, lwd = 0.5, color = "#C5EDDE") +
  annotate("line", x = r$m, y = r$SD0, lwd = 1, color = "#C5EDDE") +
  scale_x_continuous("Age (in months)",
                     limits = c(0, 42),
                     breaks = seq(0, 42, 12)) +
  scale_y_continuous(
    expression(paste(italic(D), "-score", sep = "")),
    breaks = seq(0, 80, 20),
    limits = c(0, 90)) +
  geom_point(size = 2) +
  theme(legend.position = "none")
g2 <- ggplot(data, aes(x = m, y = daz, group = ins, color = ins)) + 
  theme_light() +
  scale_x_continuous("Age (in months)",
                     limits = c(0, 42),
                     breaks = seq(0, 42, 12)) +
  scale_y_continuous(
    "DAZ",
    breaks = seq(-4, 4, 2),
    limits = c(-5, 5)) +
  geom_point(size = 2) +
  theme(legend.position = "none")
g1 + g2

