library(basejump)
library(knitr)
library(ggplot2)
library(tidyverse)

opts_chunk[["set"]](
    audodep = TRUE,
    cache = TRUE,
    cache.lazy = FALSE,
    error = FALSE,
    fig.height = 10L,
    fig.retina = 2L,
    fig.width = 10L,
    message = FALSE,
    tidy = TRUE,
    warning = FALSE)

theme_set(theme_gray(base_size = 14L))
theme_update(legend.position = "bottom")
