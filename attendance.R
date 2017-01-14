source("source.R")

team <- read.csv("./data/team.csv")
park <- read_csv("./data/park.csv")

tbl_team <- team %>%
    filter(!is.na(attendance),
           !is.na(ghome),
           park != "") %>%
    select(year:ws_win,
           name:attendance) %>%
    mutate(att_game = as.integer(attendance / ghome))

tbl_park <- park %>%
    select(park_id, park_name)

tbl_team %>% 
    group_by(park) %>% 
    mutate(n=n()) %>%
    mutate(outlier = ifelse(is_outlier(att_game), year, as.numeric(NA))) %>%
    filter(year>1969, n >2) %>%
    ggplot(., aes(x = factor(park), y = att_game)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = outlier), na.rm = TRUE, size = 3, hjust = -0.1)
