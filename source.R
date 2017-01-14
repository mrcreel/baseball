library("tidyverse")
library("lubridate")
library("stringr")

# By http://stackoverflow.com/users/2572423/jasonaizkalns
# From http://stackoverflow.com/a/33525389/6269439
is_outlier <- function(x) {
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
