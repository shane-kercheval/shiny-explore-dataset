
dataset <- data.frame(nycflights13::flights %>%
               mutate(date = make_date(year, month, day)) %>%
               select(-year, -month, -day) %>%
               select(date, everything()))
dataset <- dataset %>% mutate(origin = factor(origin))


as.character((as.data.frame(table(as.character(dataset[, 'tailnum']))) %>%
                  arrange(desc(Freq)))$Var1)

imap(dataset, ~ {
    input_id <- paste0('dynamic_filter_variable_plots_', .y)
    
    if(is.Date(.x)) {
        #'date'
        min_index <- which.min(.x)
        max_index <- which.max(.x)
        min_value <- .x[min_index]
        max_value <- .x[max_index]

        dateRangeInput(inputId=input_id,
                    label=.y,
                    start=min_value,
                    end=max_value)
    } else if (is.POSIXct(.x) || is.POSIXlt(.x)) {
        
        # TODO: factor if this works good and is the same as is.Date
        
        #'POSIX.t'
        min_index <- which.min(.x)
        max_index <- which.max(.x)
        min_value <- .x[min_index]
        max_value <- .x[max_index]

        dateRangeInput(inputId=input_id,
                       label=.y,
                       start=min_value,
                       end=max_value)
    } else if(is.factor(.x)) {
        #'factor'
        selectInput(inputId=input_id, label=.y, choices=levels(.x), selected = NULL, multiple = TRUE)
    } else if(is.numeric(.x)) {
        #'numeric'
        min_value <- min(.x, na.rm = TRUE)
        max_value <- max(.x, na.rm = TRUE)

        sliderInput(inputId=input_id, label=.y, min=min_value, max=max_value, value=c(min_value, max_value))
    } else if(is.character(.x)) {
        values_ordered_by_frequency <- as.character((as.data.frame(table(as.character(.x))) %>%
            arrange(desc(Freq)))$Var1)
        
        selectInput(inputId=input_id,
                    label=.y,
                    choices=values_ordered_by_frequency,
                    selected = NULL,
                    multiple = TRUE)
            
    } else {
        #class(.)[1]
        stopifnot(FALSE)
    }
})

as.Date(dataset$time_hour)

as_datetime(dataset$time_hour)

summary(dataset)


temp <- list()
temp <- append(temp, 'a')
temp <- append(temp, 'b')




l = list()
length(l)=5

names(l) <- c('a', 'b', 'c', 'd', 'e')
l

l['c'] <- 1


l <- list()
any(map_lgl(l, ~ !is.null(.)))









