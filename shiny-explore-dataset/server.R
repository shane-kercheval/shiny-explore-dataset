#devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(stringr)
library(tidyverse)
library(scales)
library(lattice)
library(lubridate)

source('helper_scripts/definitions.R')
source('helper_scripts/logging_functions.R')
source('helper_scripts/generic_helpers.R')
source('helper_scripts/plot_helpers.R')
source('helper_scripts/dynamic_show_hide_controls.R')
source('helper_scripts/reactive_data_helpers.R')
source('helper_scripts/reactive_input_helpers.R')
source('helper_scripts/reactive_output_helpers.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # main dataset
    ##########################################################################################################
    dataset <- reactive__dataset(input, output, session)
    output$dataset_head_table <- renderDataTable__dataset_head_table(dataset)
    output$dataset_types_table <- renderDataTable__dataset_types_table(dataset)

    ##########################################################################################################
    # numeric summary data
    ##########################################################################################################
    numeric_summary_data <- reactive__numeric_summary(input, output, session, dataset)
    output$numeric_summary_table <- renderDataTable__numeric_summary_table(input, numeric_summary_data)
    output$numeric_summary_options_UI <- renderUI__numeric_summary_options_UI(numeric_summary_data)

    ##########################################################################################################
    # categoric summary data
    ##########################################################################################################
    categoric_summary_data <- reactive__categoric_summary(input, output, session, dataset)
    output$categoric_summary_table <- renderDataTable__categoric_summary_table(categoric_summary_data)
    output$categoric_summary_text <- renderPrint__categoric_summary_text(dataset, categoric_summary_data)

    ##########################################################################################################
    # RENDER OUTPUT
    ##########################################################################################################
    output$correlation_plot <- renderPlot__correlation_plot(input, output, session, dataset)

    ##########################################################################################################
    # RENDER OUTPUT
    ##########################################################################################################

    filter_controls_list <- reactive({

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
    })

    output$variable_plots_filter_bscollapse_UI <- renderUI({
        tagList(list=filter_controls_list())
    })

    # duplicate dataset (which i don't like for large datasets) so that that the filters don't have to be reapplied every time.
    variable_plots_filtered_dataset <- reactive({

        input$variable_plots_filter_apply


        local_dataset <- dataset()
        column_names <- colnames(local_dataset)

        withProgress(value=1/length(column_names), message='Applying Filters',{

            log_message_block_start('Filtering...')

            #### APPLY FILTERS

            # list with selections for each dynamic filter, and list names are the column names
            dynamic_filter_selections <- get_dynamic_filter_selections(input, column_names)

            index = 1
            for(column_name in column_names) {

                incProgress(index/length(column_names), detail = column_name)


                filter_selection <- dynamic_filter_selections[[column_name]]

                log_message_generic(paste('Filtering on', column_name), 
                                    paste0(filter_selection, collapse = '; '))

                if(!is.null(filter_selection)) {
                    symbol_column_name <- sym(column_name)
                    
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
            log_message('Done Filter\n')
        })

        return (local_dataset)
    })


    observeEvent(input$variable_plots_filter_clear, ({
        updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
    }))

    observeEvent(input$variable_plots_filter_apply, ({
        updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'default'))
    }))

    observe(({

        req(dataset())
        req(filter_controls_list())

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(column_name in colnames(dataset())) {
            value <- input[[paste0('dynamic_filter_variable_plots_', column_name)]]
            selections <- append(selections, value)
        }

        # if any of the selections are not null, that means they have been initialized and we can begin to mark as being changed
        # otherwise, the filter section hasn't even been opened
        if(any(map_lgl(selections, ~ !is.null(.)))) {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
        }
    }))

    output$variable_plots <- renderPlot__variable_plot(input, output, session, variable_plots_filtered_dataset)
    output$variable_plots_variable_UI <- renderUI__variable_plots_variable_UI(dataset)
    output$variable_plots_comparison_UI <- renderUI__variable_plots_comparison_UI(dataset)
    output$variable_plots_sum_by_variable_UI <- renderUI__variable_plots_sum_by_variable_UI(dataset)
    output$variable_plots_point_color_UI <- renderUI__variable_plots_point_color_UI(dataset)
    output$variable_plots_point_size_UI <- renderUI__variable_plots_point_size_UI(dataset)
    observe__variable_plots__hide_show_uncollapse_on_primary_vars(input, output, session)

    

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    # Run Regression when user clicks Run button
    regression_results <- eventReactive__regression_results(input, output, session, dataset)
    output$regression_summary_output <- renderPrint__regression_summary_output(regression_results)
    output$regression_number_of_rows_missing_removed <- renderText__regression_number_of_rows_missing_removed(regression_results)
    output$regression_formula <- renderText__regression_formula(regression_results)
    output$regression_summary_vif <- renderPrint__regression_summary_vif(regression_results)
    output$regression_diagnostic_actual_vs_predicted <- render_diagnostic_plot__regression_diagnostic_actual_vs_predicted(input, session, dataset, regression_results)
    output$regression_diagnostic_residuals_vs_fitted <- render_diagnostic_plot__regression_diagnostic_residuals_vs_fitted(input, session, dataset, regression_results)
    output$regression_diagnostic_actual_vs_observed <- render_diagnostic_plot__regression_diagnostic_actual_vs_observed(input, session, dataset, regression_results)
    output$regression_diagnostic_normal_qq <- render_diagnostic_plot__regression_diagnostic_normal_qq(input, session, dataset, regression_results)
    output$regression_diagnostic_scale_location <- render_diagnostic_plot__regression_diagnostic_scale_location(input, session, dataset, regression_results)
    output$regression_diagnostic_cooks_distance <- render_diagnostic_plot__regression_diagnostic_cooks_distance(input, session, dataset, regression_results)
    output$regression_diagnostic_residuals_vs_leverage <- render_diagnostic_plot__regression_diagnostic_residuals_vs_leverage(input, session, dataset, regression_results)
    output$regression_diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot__regression_diagnostic_cooks_distance_vs_leverage(input, session, dataset, regression_results)
    # Regression Reactive UI
    output$regression_dependent_variable_UI <- renderUI__regression_dependent_variable_UI(dataset)
    output$regression_independent_variables_UI <- renderUI__regression_independent_variables_UI(input, dataset)
    output$regression_summary_header_UI <- renderUI__regression_summary_header_UI(regression_results)
    output$regression_interaction_term1_UI <- renderUI__regression_interaction_term1_UI(input, dataset)
    output$regression_interaction_term2_UI <- renderUI__regression_interaction_term2_UI(input, dataset)
    observeEvent__regression_toggle_all_ind_variables(input, dataset, session)
})
