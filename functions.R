require(readr)
require(dplyr)
require(magrittr)
require(jsonlite)
require(RcppRoll)
require(ggpubr)
require(ggplot2)
require(plotly)


plot_state <- function(state_data, my_state, roll_days) {
    plot_data <-
        state_data %>% filter(state == my_state) %>% select (case_growth, state, date) %>% na.omit
    plot_data <- plot_data[order(plot_data$date), ]
    plot_data <-
        plot_data %>% mutate(
            rolled_average_case_count = roll_mean(
                case_growth,
                roll_days,
                na.rm = TRUE,
                align = 'right',
                fill = NA
            )
        ) %>% select(date, rolled_average_case_count) %>% na.omit
    ggbarplot(
        plot_data,
        'date',
        'rolled_average_case_count',
        fill = "dark blue",
        ggtheme = theme_minimal(),
        ylab = 'New Cases',
        xlab = 'Date',
        title = paste("All", my_state)
    ) + theme(text = element_text(size = 12))
}

plot_state2 <- function(state_data, my_state, roll_days) {
    plot_data <-
        state_data %>% filter(state == my_state) %>% select (case_growth, state, date) %>% na.omit
    plot_data <- plot_data[order(plot_data$date), ]
    plot_data <-
        plot_data %>% mutate(
            rolled_average_case_count = roll_mean(
                case_growth,
                roll_days,
                na.rm = TRUE,
                align = 'right',
                fill = 0
            )
        ) %>% select(date, rolled_average_case_count,case_growth)

    legend_name <- paste(roll_days,'Day Average')
    color_vector <- c('red')
    names(color_vector) <- c(legend_name)
    ggbarplot(
        plot_data,
        'date',
        'case_growth',
        fill = "dark blue",
        ggtheme = theme_minimal(),
        ylab = 'New Cases',
        xlab = 'Date',
        
        title = paste("All", my_state)
    ) + geom_line(data=plot_data,aes(x=date,y=rolled_average_case_count,color=legend_name),size=1) + 
        scale_color_manual(name="",values=color_vector) +
        theme(text = element_text(size = 12),legend.position = "bottom")
}



plot_county_roll <-
    function(county_data,
             my_county,
             my_state,
             roll_days) {
        plot_data <-
            county_data %>% filter(county == my_county &
                                       state == my_state) %>% na.omit
        
        plot_data <-
            plot_data %>% mutate(
                rolled_average_case_count = roll_mean(
                    case_growth,
                    roll_days,
                    na.rm = TRUE,
                    align = 'right',
                    fill = NA
                )
            ) %>% na.omit
        
        ggbarplot(
            plot_data,
            'date',
            'rolled_average_case_count',
            fill = "light blue",
            ggtheme = theme_minimal(),
            ylab = 'New Cases',
            xlab = 'Date',
            title = paste(my_county, my_state, sep = ", ")
        ) + theme(text = element_text(size = 12),legend.position = 'bottom')
    }

plot_county2 <-
    function(county_data,
             my_county,
             my_state,
             roll_days) {
        plot_data <-
            county_data %>% filter(county == my_county &
                                       state == my_state) %>% na.omit
        
        plot_data <-
            plot_data %>% mutate(
                rolled_average_case_count = roll_mean(
                    case_growth,
                    roll_days,
                    na.rm = TRUE,
                    align = 'right',
                    fill = NA
                )
            ) 
        legend_name <- paste(roll_days,'Day Average')
        color_vector <- c('red')
        names(color_vector) <- c(legend_name)
        ggbarplot(
            plot_data,
            'date',
            'case_growth',
            fill = "dark blue",
            ggtheme = theme_minimal(),
            ylab = 'New Cases',
            xlab = 'Date',
            
            title = paste(my_county, my_state, sep = ", ")
        ) + geom_line(data=plot_data,aes(x=date,y=rolled_average_case_count,color=legend_name),size=1) + 
            scale_color_manual(name="",values=color_vector) +
            theme(text = element_text(size = 12),legend.position = "bottom")
        
    }

get_state_data_nyt <- function() {
    state_data <-
        read_csv(
            url(
                "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
            )
        )
    state_data$date <- state_data$date %>% as.Date()
    state_data <- state_data[order(state_data$date), ]
    state_data <- state_data %>%
        group_by(state) %>%
        mutate(case_growth = cases - lag(cases))
    pop_data <- read_csv('SCPRC-EST2019-18+POP-RES.csv')
    state_data <-
        merge(
            state_data,
            pop_data,
            by.x = 'state',
            by.y = 'NAME',
            all.x = TRUE
        )
    state_data <-
        mutate(
            state_data,
            cases_per_100K = 100000 * (cases / POPESTIMATE2019),
            case_growth_per_100K = 100000 * (case_growth / POPESTIMATE2019)
        )
    state_data <- state_data[order(state_data$date), ]
    return(state_data)
}

get_state_data_covid_tracking <- function() {
    state_json <-
        fromJSON(txt = url("https://covidtracking.com/api/states/daily"))
    state_json$date <- as.Date(state_json$dateChecked)
    state_json <- state_json %>% rename(state_abbr = state)
    state_info <-
        read_csv("state_info.csv") %>% select(state_abbr = state, state = name)
    state_json <-
        merge(state_json, state_info, by = "state_abbr", all.x = TRUE) %>% rename(case_growth = positiveIncrease, cases =
                                                                                      positive)
    #merge population data, create new columns
    pop_data <- read_csv('SCPRC-EST2019-18+POP-RES.csv')
    state_json <-
        merge(
            state_json,
            pop_data,
            by.x = 'state',
            by.y = 'NAME',
            all.x = TRUE
        )
    state_json <-
        mutate(
            state_json,
            cases_per_100K = 100000 * (cases / POPESTIMATE2019),
            case_growth_per_100K = 100000 * (case_growth / POPESTIMATE2019)
        )
    state_json <- state_json[order(state_json$date), ]
    
    return(state_json)
}

make_state_comparison <-
    function(state_data, tail_days, rounding_days) {
        my_list <- c()
        state_data <-
            state_data %>% group_by(state) %>% mutate(case_growth_rolled_100K = roll_meanr(case_growth_per_100K, rounding_days)) %>% select(state, case_growth_rolled_100K, cases_per_100K, date) %>% na.omit()
        for (my_state in unique(state_data$state)) {
            my_data <-
                filter(state_data, state == my_state) %>% tail(tail_days)
            out <-
                lm(
                    case_growth_rolled_100K ~ date,
                    my_data %>% select(case_growth_rolled_100K, date)
                ) %>% na.omit()
            my_list[[my_state]] <-
                c(out$coefficients['date'],
                  tail(my_data$case_growth_rolled_100K, 1))
            
        }
        return (
            my_list %>% data.frame() %>% t() %>% data.frame() %>% data.table::setDT(keep.rownames = TRUE)
        )
    }
