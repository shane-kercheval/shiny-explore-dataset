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
    return (is.character(x) || is.factor(x) || is.logical(x))
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

        dataset[, variable_to_order] <- fct_infreq(as.character(dataset[[variable_to_order]]), ordered = TRUE)
    
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

    end_message <- function(message, num_is_na) {
        if(num_is_na > 0) {
        
            message <- paste0(message, "; Removing ", my_number_format(num_is_na), " rows with missing values", ")")
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

                message <- paste0(column_name, ": ", filter_values[1], " <= x <= ", filter_values[2], " (Removing ", my_number_format(num_removing), " rows") %>%
                    end_message(num_is_na)

                valid_indexes <- which(!is.na(day_column) & day_column >= filter_values[1] & day_column <= filter_values[2])
                dataset <- dataset[valid_indexes, ]
                
            } else if(is.numeric(dataset[, column_name])) {

                # for numerics/etc. need to remove NA values and then filter
                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    (dataset[, column_name] < filter_values[1] | dataset[, column_name] > filter_values[2]))

                message <- paste0(column_name, ": ", my_number_format(filter_values[1]), " <= x <= ", my_number_format(filter_values[2]), " (Removing ", my_number_format(num_removing), " rows") %>%
                    end_message(num_is_na)

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
                
                message <- paste0(column_name, ": ", paste0(filter_values, collapse=", "), " (Removing ", my_number_format(num_removing), " rows") %>%
                    end_message(num_is_na)
                
            } else if(is.logical(dataset[, column_name])) {

                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    !dataset[, column_name] %in% filter_values)

                message <- paste0(column_name, ": ", paste0(filter_values, collapse=", "), " (Removing ", my_number_format(num_removing), " rows") %>%
                    end_message(num_is_na)

                #'logical'
                dataset <- dataset %>%
                    filter(!!symbol_column_name %in% filter_values)

            } else if("hms" %in% class(dataset[, column_name])) {

                num_is_na <- sum(is.na(dataset[, column_name]))
                num_removing <- sum(!is.na(dataset[, column_name]) & 
                                    (dataset[, column_name] < hm(filter_values[1]) | dataset[, column_name] > hm(filter_values[2])))

                message <- paste0(column_name, ": ", filter_values[1], " <= x <= ", filter_values[2], " (Removing ", my_number_format(num_removing), " rows") %>%
                    end_message(num_is_na)

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

my_number_format <- function(x) {
    format_format(big.mark=",", preserve.width="none", digits=4, scientific=FALSE)(x)
}

#' @param vertical_annotations list of vectors; each list item is an annotation; first value of vector is x location of line; second value is text annotation
#' @param y_location y location of text
add_vertical_annotations <- function(ggplot_object, vertical_annotations, y_location=0, is_date=FALSE) {
    
    if(!is.null(vertical_annotations) &&
           vertical_annotations != "" &&
           length(vertical_annotations) > 0) {
        
        for(annotation in vertical_annotations) {
            
            if(is_date) {
                x_location <- ymd(annotation[1])
            } else {
                x_location <- as.numeric(annotation[1])
            }
            
            ggplot_object <- ggplot_object +
                geom_vline(xintercept = x_location, color='red') +
                annotate(geom="text",
                         x=x_location,
                         y=y_location,
                         label=annotation[2],
                         color="red",
                         angle=90,
                         hjust=0,
                         vjust=-0.5)
        }
    }

    return (ggplot_object)
}

#' @param horizontal_annotations list of vectors; each list item is an annotation; first value of vector is y location of line; second value is text annotation
#' @param x_location x location of text
#' @param x_location_is_date true if the x-axis is a date. Because we need to convert e.g. POSIXct types to Date type
add_horizontal_annotations <- function(ggplot_object, horizontal_annotations, x_location=0, x_location_is_date=FALSE) {
    
    if(!is.null(horizontal_annotations) &&
           horizontal_annotations != "" &&
           length(horizontal_annotations) > 0) {
        
        for(annotation in horizontal_annotations) {
            
            y_location <- as.numeric(annotation[1])

            if(x_location_is_date) {

                # need to do this because even if it is e.g. POSIXct it will actually cause an error
                x_location <- as.Date(x_location)
            }

            ggplot_object <- ggplot_object +
                geom_hline(yintercept = y_location, color='red') +
                annotate(geom="text",
                         y=y_location,
                         x=x_location,
                         label=annotation[2],
                         color="red",
                         hjust=-0.2,
                         vjust=-0.5)
        }
    }
    return (ggplot_object)
}

#' gets the current URL of the app
#' 
#' @param session the session provided by the shiny app
get_base_url <- function(session) {
    url_string <- paste0(session$clientData$url_protocol,
                         "//",
                         session$clientData$url_hostname,
                         ":",
                         session$clientData$url_port)
    url <- httr::parse_url(url_string)
    return (httr::build_url(url))
}

#' builds a url based on the base_url of the shiny app and a list of parameters to change to a query string
#' 
#' @param base_url base url of the shiny app
#' @param parameters_list a named list of values 
build_custom_url <- function(base_url, parameters_list) {
    url <- httr::parse_url(base_url)
    url$query <- parameters_list
    return (httr::build_url(url))
}

#' takes a list that has repeated named list elements with single values and transfers them to single named value with multiple values
#' i.e. list('a'='b', 'a'='c') -> list('a'=c('b', 'c'))
#' 
#' @param list 
mergeUrlArgs <- function(x) sapply(unique(names(x)), function(z) unlist(x[names(x) == z], use.names=FALSE), simplify=FALSE)

#' opposite of mergeUrlArgs
#' i.e. list('a'='b', 'a'='c') -> list('a'=c('b', 'c'))
#' 
#' @param list 
expandUrlArgs <- function(x) structure(do.call(c, lapply(x, function(z) as.list(z))), names=rep(names(x), sapply(x, length)))


#' helper function to determine if the variable/value is the defualt value
private__variable_value_is_default <- function(variable_name, variable_value) {

    default_value <- var_plots__input_list_default_values[[variable_name]]

    if(is.null(default_value)) {

        return (is.null(variable_value))

    } else if(is.na(default_value)) {

        return (is.na(variable_value))

    } else {

        return (all(default_value == variable_value))
    }
}

#' takes an input (provided by the server) and extracts all of the non-default values and builds a named list
#' which will be used to build the url; only want non-defaults in order to limit the length of our url
#' 
#' @param input input object from the app
#' @param preloaded_dataset the name of the preloaded-dataset to load
build_parameters_list <- function(input, preloaded_dataset) {

    # parameters_list <- reactiveValuesToList(input)
    # #my_list <- list("var_plots__asdf"=1, "dddd"=2, "var_plots__ffff"=3)
    # parameters_list <- parameters_list[grep("var_plots__", names(parameters_list))]
    parameters_list <- list("data"=preloaded_dataset,
                            "tab"="Graphs")

    # lets loop through list of default values (real input$ will contain main irrevant variables)
    for(variable_name in names(var_plots__input_list_default_values)) {
        #print(variable_name)
        variable_value <- input[[variable_name]]
        #print(variable_value)
        # if it exists in the input and is not the default value, then add it to our list
        if(!private__variable_value_is_default(variable_name, variable_value)) {

            param_name <- str_replace(variable_name, 'var_plots__', '')
            parameters_list[[param_name]] <- variable_value
        }
    }

    # change multi-value parameters to single-value but occuring multiple times e.g. list('a'=c('b', 'c')) -> list('a'='b', 'a'='c')
    return (expandUrlArgs(parameters_list))
}

#' takes a url search string e.g. ?data=flights&tab=Graphs&variable=Test and extracts all of the parameters into a named list
#' transforms all parameters except 'data' and 'tab' to 'var_plots__xxx'
#' 
#' @param url_search the search part of the url 
extract_url_parameters <- function(url_search) {

    parameters_list <- mergeUrlArgs(shiny::parseQueryString(url_search))
    default_params <- c('data', 'tab')
    names(parameters_list) <- c(default_params, paste0('var_plots__', names(parameters_list) %>% rt_remove_val(default_params)))
    parameters_list <- type.convert(parameters_list, as.is=TRUE)

    return (parameters_list)
}
