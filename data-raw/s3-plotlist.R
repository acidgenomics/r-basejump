library(ggplot2)
library(cowplot)

# Examples from the ggplot2 cheatsheet
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
continuous <- ggplot(mpg, aes(hwy)) +
    geom_histogram(binwidth = 5)
discrete <- ggplot(mpg, aes(fl)) +
    geom_bar()

plotlist <- list(
    continuous = continuous,
    discrete = discrete
)

# Plotting example using the plotlist with cowplot
plot_grid(plotlist = plotlist)

saveData(plotlist, dir = "tests/testthat", compress = "xz")
