library("tidyverse")

team <- read.csv("./data/team.csv", stringsAsFactors = FALSE)
team$park[team$park == "The Ballpark at Arlington"] <-"The Ballpark in Arlington"
team$park[team$park == "Petco Park"] <-"PETCO Park"
team$park[team$park == "Fenway Park II"] <-"Fenway Park"
team$park[team$park == "Angel Stadium"] <-"Angel Stadium of Anaheim"
team$park[team$park == "Hubert H Humphrey Metrodome"] <-"Hubert H. Humphrey Metrodome"
team$park[team$park == "Comiskey Park"] <-"Comiskey Park I"

park <- read.csv("./data/park.csv", stringsAsFactors = FALSE)

tbl_park <- bind_rows(
    park %>%
        select(park_id,park_name),
    park %>%
        filter(park_alias !="") %>%
        select(park_id, park_name = park_alias)
) %>%
    mutate(park_name = strsplit(park_name, ";")) %>%
    unnest(park_name) %>%
    mutate(park_name = str_trim(park_name))
tbl_park$park_name[tbl_park$park_name == "Great American Ballpark"] <- "Great American Ball Park"
tbl_park$park_name[tbl_park$park_name == "Edison Field"] <- "Edison International Field"
tbl_park$park_name[tbl_park$park_name == "Robert F. Kennedy Stadium"] <- "R.F.K. Stadium"
tbl_park$park_name[tbl_park$park_name == "Oakland-Alameda County Coliseum"] <- "Oakland Coliseum"
tbl_park$park_name[tbl_park$park_name == "San Diego/Jack Murphy Stadium"] <- "Jack Murphy Stadium"
tbl_park <- rbind(tbl_park, c("NYC22", "Yankee Stadium III"))
    
   
tbl_team <- team %>%
    filter(!is.na(attendance),
           !is.na(ghome),
           park != "") %>%
    select(year:ws_win,
           name:attendance) %>%
    mutate(avg_attendance = as.integer(attendance / ghome)) %>%
    rename(park_name = park) %>%
    left_join(tbl_park) %>%
    group_by(park_id) %>%
    mutate(seasons = n()) %>%
    filter(year > 1968,
           seasons > 4)

tbl_team %>% 
    ggplot(., aes(x = factor(park_id), y = avg_attendance)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

