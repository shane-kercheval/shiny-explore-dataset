##########################################################################################################
# main dataset
# initialize with small default dataset or upload from file, by user
##########################################################################################################
reactive__dataset <- function(input, output, session) {
    reactive({

        req(input$preloaded_dataset)

        # reactive data
        upload_file_path <- input$uploadFile$datapath
        local_preloaded_dataset <- input$preloaded_dataset

        log_message_block_start('Loading Dataset')
        log_message_variable('upload_file_path', upload_file_path)
        log_message_variable('preloaded_dataset', local_preloaded_dataset)

        if(is.null(upload_file_path)) {
            
            if(local_preloaded_dataset == 'Credit') {

                return (dataset_or_null('example_datasets/credit.csv'))

            } else if(local_preloaded_dataset == 'Housing') {

                return (dataset_or_null('example_datasets/housing.csv'))

            } else if(local_preloaded_dataset == 'Insurance') {

                return (dataset_or_null('example_datasets/insurance.csv'))

            } else if(local_preloaded_dataset == 'Iris') {

                return (data.frame(iris))

            } else if(local_preloaded_dataset == 'Diamonds') {

                return (data.frame(diamonds))

            } else if(local_preloaded_dataset == 'Flights') {

                return (
                    data.frame(nycflights13::flights %>%
                        mutate(date = make_date(year, month, day)) %>%
                        select(-year, -month, -day) %>%
                        select(date, everything())))

            } else if(local_preloaded_dataset == 'Gapminder') {

                return (data.frame(gapminder::gapminder))

            } else {

                return (NULL)
            }
        } else {

            if(str_sub(upload_file_path, -4) == '.csv') {
                
                return (read.csv(upload_file_path, header=TRUE))

            } else if(str_sub(upload_file_path, -4) == '.RDS') {
            
                return (readRDS(file=upload_file_path))

            } else {

                showModal(
                    modalDialog(title = 'Unknown File Type',
                                'Only `.csv` and `.RDS` files are supported at this time.'))
                return (NULL)
            }
        }
    })
}

##############################################################################################################
# calculate the numeric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__numeric_summary <- function(input, output, session, dataset) {

    reactive({

        # typically I would do the progress in the while rendering the UI, but this is used while updating
        # the summary options and i'm not sure which will be called first
        withProgress(value=1/2, message='Calculating Numeric Summary',{

            log_message_block_start('Calculating Numeric Summary')
            return (rt_explore_numeric_summary(dataset=dataset()))
        })
    })
}

##############################################################################################################
# calculate the categoric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__categoric_summary <- function(input, output, session, dataset) {

    reactive({

        withProgress(value=1/2, message='Calculating Categoric Summary',{

            log_message_block_start('Calculating Categoric Summary')
            return (rt_explore_categoric_summary(dataset=dataset()))
        })
    })
}

##############################################################################################################
# Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
##############################################################################################################
reactive__variable_plots_filtered_dataset <- function(input, dataset) {

    reactive({

        local_dataset <- dataset()  # clear on new datasets

        if(!is.null(input$variable_plots_filter_use) && input$variable_plots_filter_use) {

            input$variable_plots_filter_apply  # trigger for the "apply" button
            

            column_names <- colnames(local_dataset)
            num_columns <- length(column_names)
            withProgress(value=1 / num_columns, message='Applying Filters',{

                log_message_block_start('Filtering...')

                #### APPLY FILTERS

                # list with selections for each dynamic filter, and list names are the column names
                dynamic_filter_selections <- get_dynamic_filter_selections(input, column_names)

                index = 1
                for(column_name in column_names) {

                    incProgress(index / num_columns, detail = column_name)


                    filter_selection <- dynamic_filter_selections[[column_name]]

                    

                    if(is.null(filter_selection)) {

                        log_message_generic(column_name, 'skipping...')

                    } else {

                        symbol_column_name <- sym(column_name)
                    
                        log_message_generic(column_name,
                                             paste('filtering -', paste0(filter_selection, collapse = '; ')))

                        if(is.Date(local_dataset[, column_name]) ||
                            is.POSIXct(local_dataset[, column_name]) ||
                            is.POSIXlt(local_dataset[, column_name]) ||
                            is.numeric(local_dataset[, column_name])) {
                            #'date'
                            # for numerics/etc. need to remove NA values and then filter
                            local_dataset <- local_dataset %>%
                                filter(!is.na(!!symbol_column_name)) %>%
                                filter(!!symbol_column_name >= filter_selection[1] & !!symbol_column_name <= filter_selection[2])
                            
                        } else if(is.factor(local_dataset[, column_name]) ||
                                    is.character(local_dataset[, column_name])) {
                            #'factor'
                            local_dataset <- local_dataset %>%
                                filter(!!symbol_column_name %in% filter_selection)
                            
                        } else {
                            #class(.)[1]
                            stopifnot(FALSE)
                        }
                    }
                    index <- index + 1
                }
                log_message('Done Filtering\n')
            })
        } else {
            log_message_block_start('Not Filtering')
        }

        return (local_dataset)
    })
}

##############################################################################################################
# build the UI controls for the filters
##############################################################################################################
reactive__filter_controls_list <- function(input, dataset) {

    reactive({

        input$variable_plots_filter_clear
        req(dataset())

        # local_filter_options_data <- filter_options_data()

        withProgress(value=1/2, message='Generating Filters',{
            
            ui_list <- imap(dataset(), ~ {

                #log_message_variable('class', class(.x)[1])

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

        })
        ui_list
    })
}


##########################################################################################################
# Run Regression when user clicks Run button
##########################################################################################################    
eventReactive__regression_results <- function(input, output, session, dataset) {

    eventReactive(input$regression_run_button, {

        if(input$regression_dependent_variable == select_variable) {
            return (NULL)
        }

        local_interaction_term1 <- input$regression_interaction_term1
        local_interaction_term2 <- input$regression_interaction_term2

        withProgress(value=1/2, message='Running Regression',{

            interaction_variables <- NULL

            if(!is.null(local_interaction_term1) && local_interaction_term1 != select_variable &&
               !is.null(local_interaction_term2) && local_interaction_term2 != select_variable) {

                interaction_variables <- list(c(local_interaction_term1,
                                                local_interaction_term2))
            }

            # updates to reactive variables will not trigger an update here, only regression_run_button
            results <- easy_regression(dataset=dataset(),
                                       dependent_variable=input$regression_dependent_variable,
                                       independent_variables=input$regression_independent_variables,
                                       # list of vectors, each element in the list is a pair of interaction terms
                                       # only supporting two interaction variables at the moment
                                       interaction_variables=interaction_variables)

            shinyjs::show('regression_formula_header')
            shinyjs::show('regression_summary_header_UI')
            shinyjs::show('regression_vif_header')
            
            return (results)
        })
    })
}
