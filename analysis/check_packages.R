## check_packages.R

#Run this script to check for packages that the other R scripts will use. If missing, try to install.
#code borrowed from here:
#http://www.vikram-baliga.com/blog/2015/7/19/a-hassle-free-way-to-verify-that-r-packages-are-installed-and-loaded

#add new packages to the chain here
packages = c("readr","ggplot2","texreg","dplyr","purrr","stringr","broom",
             "ggrepel","purrr", "kableExtra", "tibble","scales","janitor",
             "survey","modmarg","sandwich","psych","rmdformats","scales",
             "fastDummies","janitor","srvyr", "gt","tidyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
