source("source.R")

players <- read.csv("./data/player.csv", stringsAsFactors = FALSE)

tbl_players <- players %>%
    mutate(birth_month = str_pad(birth_month, 2, pad = "0"),
           birth_day = str_pad(birth_day, 2, pad = "0")) %>%
    unite(name, c(name_first, name_last), sep = " ") %>%
    unite(birth_date, c(birth_year, birth_month, birth_day), sep = "-") %>% 
    select(player_id, name, birth_date, debut) %>%
    mutate(debut = ymd(debut),
           birth_date = ymd(birth_date))
 
   
salary <- read.csv("./data/salary.csv", stringsAsFactors = FALSE) 

tbl_salary <- salary %>%
    group_by(player_id, year) %>%
    summarise(salary = sum(salary)) %>%
    inner_join(tbl_players, by = "player_id")


all_star <- read.csv("./data/all_star.csv", stringsAsFactors = FALSE) 

tbl_all_star <- all_star %>%
    select(player_id, year) %>%
    inner_join(tbl_players, by = "player_id")


hall_of_fame <- read.csv("./data/hall_of_fame.csv", stringsAsFactors = FALSE)

tbl_hall_of_fame <- hall_of_fame  %>%
    filter(inducted == "Y",
           category == "Player") %>%
    select(player_id) %>%
    inner_join(tbl_players, by = "player_id")



