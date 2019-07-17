library(lubridate)

# defualt placement is 'bottom', but I want the default to be 'top'
add_tooltip <- function(element, tooltip_text, placement='top', trigger='hover') {

    return ( tipify(element, title=tooltip_text, placement=placement, trigger=trigger) )
}

dataset_or_null <- function(file) {
    # loads the file if it exists, otherwise returns NULL.    

    if(file.exists(file)) {

        return (read.csv(file, header=TRUE))

    } else {

        return (NULL)
    }
}

is_date_type <- function(x) {
    return (is.Date(x) || is.POSIXct(x) || is.POSIXlt(x))
}

is_categoric <- function(x) {
    return (is.character(x) || is.factor(x))
}

null_if_select_variable_optional <- function(value) {

    if(is.null(value) || value == global__select_variable_optional) {

        value <- NULL
    }

    return (value)
}

is_null_or_empty_string <- function(value) {

    return(is.null(value) || value == "")
}

default_if_null_or_empty_string <- function(value, string_values_as_null=NULL, default=NULL) {

    if(is_null_or_empty_string(value) || value %in% string_values_as_null) {

        return (default)

    } else {

        return (value)
    }
}

mutate_factor_lump <- function(dataset, factor_lump_number=NULL, ignore_columns=NULL) {

    if(!is.null(factor_lump_number) && !is.na(factor_lump_number)) {

        column_names <- colnames(dataset)

        if(!is.null(ignore_columns)) {

            temp <- dataset %>% select(ignore_columns)
        }

        dataset <- dataset %>% select(rt_remove_val(column_names, ignore_columns)) %>%
            mutate_if(is.character, as.factor) %>%
            mutate_if(is.factor, ~fct_lump(.x, n=factor_lump_number))

        if(!is.null(ignore_columns)) {

            dataset <- cbind(dataset, temp)
            dataset <- dataset %>% select(column_names)
        }
    }

    return (dataset)
}

#' mutates a categoricvariable in order (factor) to order it (and display) it on a graph in that particular order
#' @param dataset the dataset
#' @param variable_to_order_by: "Default", "Frequency", or the name of the variable
#' @param variable_to_order: the variable to mutate the factor order
mutate_factor_reorder <- function(dataset, variable_to_order_by, variable_to_order) {

    if(is.null(variable_to_order)) {
        return (dataset)
    }

    stopifnot(is_categoric(dataset[[variable_to_order]]))
    
    if (variable_to_order_by == "Frequency") {

        dataset[, variable_to_order] <- fct_infreq(dataset[[variable_to_order]], ordered = TRUE)
    
    } else if (variable_to_order_by != "Default") {

        rt_stopif(is.null(variable_to_order_by))
        stopifnot(is.numeric(dataset[[variable_to_order_by]]))
        
        symbol_variable_to_order <- sym(variable_to_order)
        symbol_variable_to_order_by <- sym(variable_to_order_by)
        new_levels <- dataset %>%
            group_by(!!symbol_variable_to_order) %>%
            summarise(n = n(), median_v = median(!!symbol_variable_to_order_by, na.rm = TRUE)) %>%
            arrange(desc(median_v), desc(n)) %>%
            rt_get_vector(variable_to_order) %>%
            as.character()

        dataset[, variable_to_order] <- factor(dataset[[variable_to_order]],
                                               levels = new_levels,
                                               ordered = TRUE)
    }

    return (dataset)
}

capture_messages_warnings <- function(func) {
    
    messages <- list()
    withCallingHandlers(
        warning = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        message = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        func()
    )
    return (paste0(messages, collapse = '\n'))
}

#' filters the dataset based on a list of filters
#' 
#' @param filter_list a named list with names as columns and values as filter values e.g.
#' 
#'         ```
#'         $carat
#'         [1] 0.20 5.01
#'         $cut
#'         [1] "Good"
#'         $color
#'         NULL
#'         ```
#'      `filter_list` should only contain the variables that the user is filtering on
#' @param callback a callback function that get's executed at the beginning of each loop for e.g. providing a
#'      way to `incProgress`
#'      Has the params `(index, num_columns, column_name)`
#' @return a list
#'      index 1: filtered dataset
#'      index 2: string containing information about what was filtered
filter_data <- function(dataset, filter_list, callback=NULL) {

    end_message <- function(message, num_is_na, num_removing) {
        if(num_is_na > 0) {
        
            message <- paste0(message, "; Removing ", num_is_na, " rows with missing values", ")")
        } else {
            message <- paste0(message, ")")
        }
    }

    filter_messages <- list()
    columns <- names(filter_list)
    index <- 1
    for(column_name in columns) {
        # column_name <- columns[2]
        message <- NULL
        filter_values <- filter_list[[column_name]]
        
        if(!is.null(callback)) {

            callback(index, length(columns), column_name)
        }

        if(is.null(filter_values)) {

            message <- paste0(column_name, ": Not Filtering")

        } else {

            symbol_column_name <- sym(column_name)
        
            if(is_date_type(dataset[, column_name])) {
                #'date'
                # for numerics/etc. need to remove NA values and then filter
                num_is_na <- sum(is.na(dataset[, column_name]))

                # need to convert possible date/times to days so that if someone selects 11/01 as an end-date
                # then it includes all date/times on 11/01 (even e.g 'XXXX-11-01 23:23:23')
                day_column <- floor_date(dataset[, column_name], unit='days')

                num_removing <- sum(!is.na(day_column) & # already counting is.na() above
                                    (day_column < filter_values[1] | day_column > filter_values[2]))

                message <- paste0(column_name, ": ", filter_values[1], " <= x <= ", filter_values[2], " (Removing ", num_removing, " rows") %>%
                    end_message(num_is_na, num_removing)

                valid_indexes <- which(!is.na(day_column) & day_column >= filter_values[1] & day_column <= filter_values[2])
                dataset <- dataset[valid_indexes, ]
                
            } else if(is.numeric(dataset[, column_name])) {

                # for numerics/etc. need to remove NA values and then filter
                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    (dataset[, column_name] < filter_values[1] | dataset[, column_name] > filter_values[2]))

                message <- paste0(column_name, ": ", filter_values[1], " <= x <= ", filter_values[2], " (Removing ", num_removing, " rows") %>%
                    end_message(num_is_na, num_removing)

                dataset <- dataset %>%
                    filter(!is.na(!!symbol_column_name)) %>%
                    filter(!!symbol_column_name >= filter_values[1] & !!symbol_column_name <= filter_values[2])

            } else if(is.factor(dataset[, column_name]) ||
                        is.character(dataset[, column_name])) {
                
                missing_value_string <- "<Missing Values (NA)>"
        
                if(missing_value_string %in% filter_values) {
                    
                    num_is_na <- 0
                    num_removing <- sum(!is.na(dataset[, column_name]) &
                                            !dataset[, column_name] %in% filter_values)
                    
                    dataset <- dataset %>%
                        filter(is.na(!!symbol_column_name) | !!symbol_column_name %in% filter_values)

                } else {
                
                    num_is_na <- sum(is.na(dataset[, column_name]))
                    num_removing <- sum(!is.na(dataset[, column_name]) &
                                            !dataset[, column_name] %in% filter_values)

                    dataset <- dataset %>%
                        filter(!!symbol_column_name %in% filter_values)
                }
                
                message <- paste0(column_name, ": ", paste0(filter_values, collapse=", "), " (Removing ", num_removing, " rows") %>%
                    end_message(num_is_na, num_removing)
                
            } else if(is.logical(dataset[, column_name])) {

                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    !dataset[, column_name] %in% filter_values)

                message <- paste0(column_name, ": ", paste0(filter_values, collapse=", "), " (Removing ", num_removing, " rows") %>%
                    end_message(num_is_na, num_removing)

                #'logical'
                dataset <- dataset %>%
                    filter(!!symbol_column_name %in% filter_values)

            } else if("hms" %in% class(dataset[, column_name])) {

                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    (dataset[, column_name] < hm(filter_values[1]) | dataset[, column_name] > hm(filter_values[2])))

                message <- paste0(column_name, ": ", filter_values[1], " <= x <= ", filter_values[2], " (Removing ", num_removing, " rows") %>%
                    end_message(num_is_na, num_removing)

                # hours minutes seconds
                dataset <- dataset %>%
                    filter(!is.na(!!symbol_column_name)) %>%
                    filter(!!symbol_column_name >= hm(filter_values[1]) & !!symbol_column_name <= hm(filter_values[2]))

            } else {

                print("ERROR: unknown class")
                print(paste0(class(dataset[, column_name]), collapse=', '))
                stopifnot(FALSE)

            }
        }
 
        filter_messages <- append(filter_messages, message)
        index <- index + 1
    }

    return (list(dataset, filter_messages))
}
