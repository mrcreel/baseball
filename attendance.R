library("tidyverse")

# By http://stackoverflow.com/users/2572423/jasonaizkalns
# From http://stackoverflow.com/a/33525389/6269439
is_outlier <- function(x) {
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


team <- read.csv("./data/team.csv")
park <- read_csv("./data/park.csv")

tbl_team <- team %>%
    filter(!is.na(attendance),
           !is.na(ghome),
           park != "") %>%
    select(year:ws_win,
           name:attendance) %>%
    mutate(avg_attendance = as.integer(attendance / ghome))

tbl_park <- park %>%
    select(park_id, park_name)

tbl_team %>% 
    group_by(park) %>% 
    mutate(n=n()) %>%
    mutate(outlier = ifelse(is_outlier(avg_attendance), year, as.numeric(NA))) %>%
    filter(year>1969, n >2) %>%
    ggplot(., aes(x = factor(park), y = avg_attendance)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = outlier), na.rm = TRUE, size = 3, hjust = -0.1)
