library("tidyverse")
library("lubridate")

salary <- read.csv("./data/salary.csv", stringsAsFactors = FALSE)
all_star <- read.csv("./data/all_star.csv", stringsAsFactors = FALSE)
master <- read.csv("./data/player.csv") %>%
    unite(birth_date, c(birth_year, birth_month, birth_day), sep = "-") %>%
    mutate(birth_date = ymd(birth_date),
           debut = ymd(debut),
           final_game = ymd(final_game))
hall_of_fame <- read.csv("./data/hall_of_fame.csv", stringsAsFactors = FALSE)

hof_salary <-inner_join(hall_of_fame %>% 
                      filter(inducted == "Y"),
                  salary, by = "player_id") %>%
    mutate(year = as.factor(year))
    
plot.hof_salary <- ggplot(hof_salary) +
    geom_boxplot(aes(x = factor(year), y = salary)) + 
    scale_y_continuous(labels = scales::dollar,
                       limits = c(0, 35000000))

as_salary <- inner_join(all_star, salary) %>%
    select(player_id, year, salary) %>%
    mutate(year = as.factor(year))

plot.as_salary <- ggplot(as_salary) +
    geom_boxplot(aes(x = year, y = salary)) +
    scale_y_continuous(labels = scales::dollar,
                       limits = c(0, 35000000))
