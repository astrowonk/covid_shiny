require(readr)
require(dplyr)
require(magrittr)
require(jsonlite)
get_state_data_nyt <- function() {
    state_data <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
    state_data <- state_data %>%
        group_by(state) %>%
        mutate(case_growth = cases - lag(cases))
    state_data$date <- state_data$date %>% as.Date()
    return(state_data)
}

get_state_data_covid_tracking <- function() {
    state_json <- fromJSON(txt = url("https://covidtracking.com/api/states/daily"))
    state_json$date <- as.Date(state_json$dateChecked)
    state_json <- state_json %>% rename(state_abbr = state)
    state_info <- read_csv("state_info.csv") %>% select(state_abbr = state, state = name)
    state_json <- merge(state_json, state_info, by = "state_abbr", all.x = TRUE) %>% rename(case_growth = positiveIncrease)
    return(state_json)
}