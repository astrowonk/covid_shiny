require(readr)
require(dplyr)
require(magrittr)
require(jsonlite)
require(RcppRoll)
require(ggpubr)
require(ggplot2)
require(plotly)



get_county_nyt <- function () {
    
    county_data <-  read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
    county_data$date <- as.Date(county_data$date)
    county_data <- county_data[order(county_data$date),]
    county_data <- county_data %>% group_by(county,state) %>% mutate(case_growth = cases - lag(cases))
    return(county_data)
}




merge_county_pop <- function (county_data,pop_data) {
    county_data <-
        merge(
            county_data,
            select(pop_data,fips,population),
            by = 'fips',
            all.x = TRUE
        )
    county_data <-
        mutate(
            county_data,
            cases_per_100K = 100000 * (cases / population),
            case_growth_per_100K = 100000 * (case_growth / population)
        )
    county_data <- mutate(county_data,state_county = paste(county,state,sep=", "))
    return(county_data)
    
}

get_virginia <- function() {
    raw <- read_csv('data_cache/va_data.txt')
    raw$date <- raw$`Report Date` %>% as.Date(format="%m/%d/%Y")
    county_data <- raw[order(raw$date),]
    county_data <- county_data %>% rename(cases = `Total Cases`, county=Locality)
    county_data <- county_data %>% group_by(county) %>% mutate(case_growth = cases - lag(cases))
    county_data <- county_data %>% group_by(county) %>% mutate(hospital_growth = Hospitalizations - lag(Hospitalizations))
    
    
    
    county_data$state = 'Virginia'
    return(county_data)
    
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



plot_county2 <-
    function(county_data,
             my_county,
             my_state,
             roll_days) {
        plot_data <-
            county_data %>% filter(state_county == my_county &
                                       state == my_state) 
        
        plot_data <- plot_data[order(plot_data$date), ]
        
        plot_data <-
            plot_data %>% group_by (state_county) %>%  mutate(
                rolled_average_case_count = roll_mean(
                    case_growth,
                    roll_days,
                    na.rm = TRUE,
                    align = 'right',
                    fill = NA
                )
            ) %>% select(rolled_average_case_count,case_growth,date,state,county) 
        legend_name <- paste(roll_days,'Day Average')
        color_vector <- c('purple')
        names(color_vector) <- c(legend_name)
        ggbarplot(
            plot_data,
            'date',
            'case_growth',
            fill = "light blue",
            ggtheme = theme_minimal(),
            ylab = 'New Cases',
            xlab = 'Date',
            
            title = my_county
        ) + geom_line(data=plot_data,aes(x=date,y=rolled_average_case_count,color=legend_name),size=1) + 
            scale_color_manual(name="",values=color_vector) +
            theme(text = element_text(size = 12),legend.position = "bottom")
        
    }

plot_county_hospital <-
    function(county_data,
             my_county,
             my_state,
             roll_days) {
        plot_data <-
            county_data %>% filter(county == my_county &
                                       state == my_state) 
        
        plot_data <-
            plot_data %>% mutate(
                rolled_data_to_plot = roll_mean(
                    hospital_growth,
                    roll_days,
                    na.rm = TRUE,
                    align = 'right',
                    fill = NA
                )
            ) %>% select(rolled_data_to_plot,hospital_growth,date,state,county) 
        legend_name <- paste(roll_days,'Day Average')
        color_vector <- c('purple')
        names(color_vector) <- c(legend_name)
        ggbarplot(
            plot_data,
            'date',
            'hospital_growth',
            fill = "light blue",
            ggtheme = theme_minimal(),
            ylab = 'New Hospitalizations',
            xlab = 'Date',
            
            title = paste(my_county, my_state, sep = ", ")
        ) + geom_line(data=plot_data,aes(x=date,y=rolled_data_to_plot,color=legend_name),size=1) + 
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


plot_county_per_capita <-
    function(plot_data,counties,
             roll_days) {
 
        plot_data <- plot_data %>% filter(state_county %in% counties)
        plot_data <- plot_data[order(plot_data$date),]
        plot_data <-
            plot_data %>% group_by(state_county) %>%  mutate(
                rolled_average_case_count = roll_mean(
                    case_growth_per_100K,
                    roll_days,
                    na.rm = TRUE,
                    align = 'right',
                    fill = NA
                )
            ) %>% select(rolled_average_case_count,case_growth,date,state_county,case_growth_per_100K) 
        legend_name <- paste(roll_days,'Day Average')
        color_vector <- c('purple')
        names(color_vector) <- c(legend_name)
        ggline(
            plot_data,
            'date',
            'rolled_average_case_count',
            ggtheme = theme_minimal(),
            ylab = 'New Cases per 100K',
            xlab = 'Date',color='state_county',
            plot_type='l',
            
            title = 'County Per Capita'
        ) 
        
    }

plot_compare_states <- function(plot_data,state_list,roll_days) {
    
    plot_data <- plot_data %>% filter(state %in% state_list)
    plot_data <- plot_data[order(plot_data$date),]
    plot_data <-
        plot_data %>% group_by(state) %>%  mutate(
            rolled_average_case_count = roll_mean(
                case_growth_per_100K,
                roll_days,
                na.rm = TRUE,
                align = 'right',
                fill = NA
            )
        ) %>% select(rolled_average_case_count,case_growth,date,state,case_growth_per_100K) 
    legend_name <- paste(roll_days,'Day Average')
    color_vector <- c('purple')
    names(color_vector) <- c(legend_name)
    ggline(
        plot_data,
        'date',
        'rolled_average_case_count',
        ggtheme = theme_minimal(),
        ylab = 'New Cases per 100K',
        xlab = 'Date',color='state',
        plot_type='l',
        
        title = 'Compare States'
    ) 
    
}
