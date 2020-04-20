require(readr)
require(dplyr)
require(magrittr)
require(jsonlite)
get_state_data_nyt <- function() {
    state_data <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
    state_data$date <- state_data$date %>% as.Date()
    state_data <- state_data[order(state_data$date),]
    state_data <- state_data %>%
        group_by(state) %>%
        mutate(case_growth = cases - lag(cases))
    pop_data <- read_csv('SCPRC-EST2019-18+POP-RES.csv')
    state_data <- merge(state_data,pop_data,by.x='state',by.y='NAME',all.x=TRUE)
    state_data <- mutate(state_data,cases_per_100K = 100000 * (cases / POPESTIMATE2019), case_growth_per_100K = 100000 * (case_growth / POPESTIMATE2019))
    state_data <- state_data[order(state_data$date),]
    return(state_data)
}

get_state_data_covid_tracking <- function() {
    state_json <- fromJSON(txt = url("https://covidtracking.com/api/states/daily"))
    state_json$date <- as.Date(state_json$dateChecked)
    state_json <- state_json %>% rename(state_abbr = state)
    state_info <- read_csv("state_info.csv") %>% select(state_abbr = state, state = name)
    state_json <- merge(state_json, state_info, by = "state_abbr", all.x = TRUE) %>% rename(case_growth = positiveIncrease, cases=positive)
    #merge population data, create new columns
    pop_data <- read_csv('SCPRC-EST2019-18+POP-RES.csv')
    state_data <- merge(state_data,pop_data,by.x='state',by.y='NAME',all.x=TRUE)
    state_data <- mutate(state_data,cases_per_100K = 100000 * (cases / POPESTIMATE2019), case_growth_per_100K = 100000 * (case_growth / POPESTIMATE2019))
    state_data <- state_data[order(state_data$date),]
    
    return(state_json)
}

make_state_comparison <- function(state_data) {
    my_list <- c()
    state_data <- state_data %>% group_by(state) %>% mutate(case_growth_rolled_100K = roll_meanr(case_growth_per_100K,7)) %>% select(state,case_growth_rolled_100K,cases_per_100K,date) %>% na.omit()
    for (my_state in unique(state_data$state)) {
        my_data <- filter(state_data, state == my_state ) %>% tail(14)
        out <- lm(case_growth_rolled_100K ~ date,my_data %>% select(case_growth_rolled_100K,date)) %>% na.omit()
        my_list[[my_state]] <- c(out$coefficients['date'],tail(my_data$cases_per_100K,1))

    }
    return (my_list)
}
