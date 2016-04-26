install_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(install_pkg) > 0) {
  install.packages(install_pkg)
}
lapply(pkg, require, character.only = TRUE)
rm(install_pkg, pkg)
(.packages())
