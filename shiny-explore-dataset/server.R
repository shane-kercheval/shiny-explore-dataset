#devtools::install_github('shane-kercheval/rtools', force=TRUE)

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(fpp2)
#library(ggfortify)
library(stringr)
library(tidyverse)
library(scales)
library(lattice)
library(lubridate)
library(maps)
library(mapproj)

source('helper_scripts/definitions.R')
source('helper_scripts/logging_functions.R')
source('helper_scripts/generic_helpers.R')
source('helper_scripts/plot_helpers.R')
source('helper_scripts/dataset_loading_helpers.R')
source('helper_scripts/numeric_summary_helpers.R')
source('helper_scripts/categoric_summary_helpers.R')
source('helper_scripts/correlation_helpers.R')
source('helper_scripts/variable_plots_helpers.R')
source('helper_scripts/url_parameter_helpers.R')
source('helper_scripts/regression_helpers.R')

options(shiny.maxRequestSize=200*1024^2)
global__variable_lookup <- c()


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    log_message_block_start("##########################################################################################\nStarting Server\n##########################################################################################")
    
    parameter_info <- reactiveValues(params=NULL,
                                     step=NULL,
                                     has_plotted=FALSE)
    
    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################

    reactive__source_data <- reactiveValues(data=NULL)

    # initially suspended, resume depending on url parameters
    observeEvent_preloaded_dataset <-  observeEvent__source_data__preloaded(session, input, output, reactive__source_data, parameter_info)

    observeEvent__source_data__upload(session, input, output, reactive__source_data)
    observeEvent__source_data__csv_url(session, input, output, reactive__source_data)
    # shows the first 500 rows of the data
    output$source_data__head_table <- renderDataTable__source_data__head(reactive__source_data)
    # shows the types of the data's variables/columns
    output$source_data__types_table <- renderDataTable__source_data__types(reactive__source_data)

    observeEvent__load_data__r_code_apply(reactive__source_data, input, output)
    ##########################################################################################################
    # numeric summary data
    ##########################################################################################################
    reactive__numeric_summary <- reactive__numeric_summary__creator(reactive__source_data)
    output$numeric_summary__table <- renderDataTable__numeric_summary__table(input, reactive__numeric_summary)
    output$numeric_summary__options__UI <- renderUI__numeric_summary__options__UI(reactive__numeric_summary)

    ##########################################################################################################
    # categoric summary data
    ##########################################################################################################
    reactive__categoric_summary <- reactive__categoric_summary__creator(reactive__source_data)
    output$categoric_summary__table <- renderDataTable__categoric_summary__table(reactive__categoric_summary)
    output$categoric_summary__text <- renderPrint__categoric_summary__text(reactive__source_data,
                                                                           reactive__categoric_summary)

    ##########################################################################################################
    # Correlation Plot
    ##########################################################################################################
    output$correlation__plot <- renderPlot__correlation__plot(input, session, reactive__source_data)

    ##########################################################################################################
    # Viarable Plot
    ##########################################################################################################
    # cached dataset after the filters have been applied (which is bad for large datasets :( ) so that the
    # filters don't have to be reapplied every time; sacrificing memory for speed 
    reactiveValues__vp_filtering_message <- reactiveValues(value=NULL)
    
    reactive__var_plots__filtered_data <- reactive__var_plots__filtered_data__creator(input,
                                                                                      reactive__source_data,
                                                                                      reactiveValues__vp_filtering_message)
    output$var_plots__filtering_messages <- renderPrint__reactiveValues__vp_filtering_message(reactiveValues__vp_filtering_message,
                                                                                              reactive__var_plots__filtered_data)

    temp_observer <- observeEvent(parameter_info$step, {

        log_message_block_start('Parameters-Step Observe-Event')

        log_message_variable('Detected Parameter-Info Step', as.character(parameter_info$step))

        if(parameter_info$step == create_url_param_step("Updated Categoric View Variable")) {

            parameter_info$step = create_url_param_step("Can Create Graph from URL Parameters")
            log_message_variable('Changed To Parameter-Info Step', as.character(parameter_info$step))

        } else if(parameter_info$step == create_url_param_step("Successfully Created Graph from URL Parameters")) {
            
        
            log_message('Finished with URL Parameters')

            #parameter_info$params <- NULL
            # can't set step to null because then it will trigger graph and graph won't know it is being re-created for nothing
            #parameter_info$step <- NULL
        }
    })

    observeEvent(parameter_info$has_plotted, {
        if(parameter_info$has_plotted) {
            log_message_block_start("Suspending Observers")
            temp_observer$suspend()
        }
    })
    
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        session,
                                                                        reactive__var_plots__filtered_data,
                                                                        parameter_info)
    # stores any messages/warnings that ggplot produces when rendering the plot (outputs below the graph
    #(var_plots__ggplot_messages))
    reactiveValues__vp__ggplot_message <- reactiveValues(value=NULL)
    output$var_plots__ggplot_messages <- renderPrint__reactiveValues__vp__ggplot_message(reactiveValues__vp__ggplot_message)

    # this builds up the filters based on the dataset column types and dynamically adds the controls to var_plots__filter_bscollapse__UI
    reactive__filter_controls_list <- reactive__filter_controls_list__creator(input, reactive__source_data)
    # area in Filters area where all of the filters are stored and then shown/hidden based on selections
    output$var_plots__filter_bscollapse__UI <- renderUI__var_plots__filter_bscollapse__UI(input, reactive__source_data, reactive__filter_controls_list)
    # where the user selects the variables they want to filter
    output$var_plots__filter_controls_selections__UI <- renderUI__var_plots__filter_controls_selections__UI(input, reactive__source_data)
    # contains the logic to show/hide filters based on selections
    observeEvent__var_plots__show_hide_dynamic_filters(input, session, reactive__source_data)

    observeEvent__var_plots__filter_clear(input, session)
    observeEvent__var_plots__filter_apply(input, session)
    observe__var_plots__bscollapse__dynamic_inputs(input, session, reactive__source_data)
    observeEvent__var_plots__filter_use(input, session)

    observeEvent__var_plots__custom_labels_clear(input, session)
    observeEvent__var_plots__graph_options_clear(input, session)
    observeEvent__var_plots__graph_options_apply(input, session)
    observeEvent__var_plots__graph_options__any_used(input, session)
    observeEvent__var_plots__custom_labels_apply(input, session)
    observeEvent__var_plots__other_options__any_used(input, session)

    # main plot
    output$var_plots <- renderPlot__variable_plot(session,
                                                  reactive__var_plots__ggplot,
                                                  reactiveValues__vp__ggplot_message)

    observe__var_plots__hide_show_uncollapse_on_primary_vars(session, input)
    observeEvent__var_plots__variables_buttons_clear_swap(session, input)

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    # Run Regression when user clicks Run button
    eventReactive__regression__results <- eventReactive__regression__results__creator(input, reactive__source_data)
    output$regression__summary_output <- renderPrint__regression__summary_output(eventReactive__regression__results)
    output$regression__number_of_rows_missing_removed <- renderText__regression__number_of_rows_missing_removed(eventReactive__regression__results)
    output$regression__formula <- renderText__regression__formula(eventReactive__regression__results)
    output$regression__summary_vif <- renderPrint__regression__summary_vif(eventReactive__regression__results)
    output$regression__diagnostic_actual_vs_predicted <- render_diagnostic_plot__actual_vs_predicted(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_residuals_vs_fitted <- render_diagnostic_plot__residuals_vs_fitted(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_residuals_vs_predictors <- render_diagnostic_plot__residuals_vs_predictors(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_actual_vs_observed <- render_diagnostic_plot__actual_vs_observed(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_normal_qq <- render_diagnostic_plot__normal_qq(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_scale_location <- render_diagnostic_plot__scale_location(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_cooks_distance <- render_diagnostic_plot__cooks_distance(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_residuals_vs_leverage <- render_diagnostic_plot__residuals_vs_leverage(input, session, reactive__source_data, eventReactive__regression__results)
    output$regression__diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot__cooks_distance_vs_leverage(input, session, reactive__source_data, eventReactive__regression__results)
    # Regression Reactive UI
    output$regression__dependent_variable__UI <- renderUI__regression__dependent_variable__UI(reactive__source_data)
    output$regression__independent_variables__UI <- renderUI__regression__independent_variables__UI(input, reactive__source_data)
    output$regression__summary_header__UI <- renderUI__regression__summary_header__UI(eventReactive__regression__results)
    output$regression__interaction_term1__UI <- renderUI__regression__interaction_term1__UI(input, reactive__source_data)
    output$regression__interaction_term2__UI <- renderUI__regression__interaction_term2__UI(input, reactive__source_data)
    observeEvent__regression__toggle_all_ind_variables(input, reactive__source_data, session)
    output$regression__residuals_vs_predictors_var__UI <- renderUI__regression__residuals_vs_predictors_var__UI(input, eventReactive__regression__results)

    # suspend() <- function(pausableReactive) {
    #   obs <- attr(pausableReactive, "pauseObserver", exact = TRUE)
    #   obs$suspend()
    # }
    # 
    # resume() <- function(pausableReactive) {
    #   obs <- attr(pausableReactive, "pauseObserver", exact = TRUE)
    #   obs$resume()
    # }
    
    # vals <- shiny::reactiveValues(value = NULL)
    # obsB <- observe({ print(values$A + 1) })
    # obsE <- observeEvent(vals$value, { print(values$A + 1) })
    # obsE$suspend()
    # vals
    # attributes(obsB)
    # attr(vals$value, "Observer", exact = TRUE)


    ##########################################################################################################
    # UPDATE FROM URL
    ##########################################################################################################

    url_search__observeEvent <- observeEvent(session$clientData$url_search, {
        
        url_search <- session$clientData$url_search
        
        log_message_variable('base url', get_base_url(session))

        if(is.null(url_search) || url_search == '') {

            log_message_block_start("No URL Parameters Detected, loading initial dataset")

            # resume data loading and dynamic variables, then trigger loading with updateSelectInput
            observeEvent_preloaded_dataset$resume()

#TODO resume dynamic variables

            log_message_variable('Initial Dataset', isolate(input$preloaded_dataset))
            updateSelectInput(session, 'preloaded_dataset', selected=isolate(input$preloaded_dataset))

        } else {

            log_message_block_start("Detected URL Parameters")
            log_message_variable('URL Search', url_search)


            params <- parseQueryString(url_search)

            if(is.null(params) || length(params) == 0 || is.null(params[['data']]) || is.null(params[['tab']])) {

                log_message("Detected URL Parameters But Unable To Create Parameter List")
                log_message_variable('data', params[['data']])
                log_message_variable('tab', params[['tab']])

                log_message("Loading initial dataset")
                log_message_variable('Initial Dataset', isolate(input$preloaded_dataset))
                updateSelectInput(session, 'preloaded_dataset', selected=isolate(input$preloaded_dataset))

            } else {

                log_message_variable('param_names', paste0(names(params), collapse='; '))

                parameter_info$params <- params
                parameter_info$possible_steps <- global__url_param_possible_steps
                parameter_info$step <- create_url_param_step("Parsed Parameters")

                rt_stopif(is.null(params[['data']]))
                rt_stopif(is.null(params[['tab']]))

                parameter_info$step <- create_url_param_step("Loaded Dataset")


                observeEvent_preloaded_dataset$resume()
                updateSelectInput(session, 'preloaded_dataset', selected=params[['data']])
                updateNavbarPage(session, 'navbar_page_app', selected = params[['tab']])
                update_var_plot_variables_from_url_params(session, params)
                
                shiny::updateQueryString(get_base_url(session), mode = "replace")
                
                # `max` so we don't override a higher level step (e.g. Updated Dataset)
                parameter_info$step <- max(parameter_info$step,
                                           create_url_param_step("Updated Non-Dynamic Parameters"))
            }
        }
    })
    
    observeEvent(reactive__source_data$data, {

        # step exists and is <2 then we are still loading shit
        if(!is.null(parameter_info$step) && parameter_info$step < create_url_param_step("Loaded Dataset")) {

            log_message_block_start("Ignoring Variable Update")

        } else {

            column_names <- colnames(reactive__source_data$data)
            numeric_column_names <- colnames(reactive__source_data$data %>% select_if(is.numeric))
            categoric_column_names <- colnames(reactive__source_data$data %>% select_if(purrr::negate(is.numeric)))

            choices__variable <- c(global__select_variable, column_names)
            choices__comparison <- c(global__select_variable_optional, column_names)
            choices__sum_by_variable <- c(global__select_variable_optional, numeric_column_names)
            choices__facet_variable <- c(global__select_variable_optional, categoric_column_names)
            choices__size_variable <- c(global__select_variable_optional, column_names)
            choices__color_variable <- c(global__select_variable_optional, categoric_column_names)
            choices__label_variables <- c(global__select_variable_optional, column_names)
            choices__order_by_variable <- c("Default", "Frequency", numeric_column_names)

            selection__variable <- global__select_variable
            selection__comparison <- global__select_variable_optional
            selection__sum_by_variable <- global__select_variable_optional
            selection__facet_variable <- global__select_variable_optional
            selection__size_variable <- global__select_variable_optional
            selection__color_variable <- global__select_variable_optional
            selection__label_variables <- character(0)
            selection__order_by_variable <- "Default"

            # everything is loaded, and there should be parameters
            if(!is.null(parameter_info$step) && parameter_info$step >= create_url_param_step("Loaded Dataset")) {

                params <- parameter_info$params
                rt_stopif(is.null(params))

                log_message_block_start("Updating variables based URL parameters")

                if (!is.null(params[['variable']])) {

                    log_message_variable('variable', params[['variable']])
                    selection__variable <- params[['variable']]
                }
                if (!is.null(params[['comparison']])) {

                    log_message_variable('comparison', params[['comparison']])
                    selection__comparison <- params[['comparison']]
                }
                if (!is.null(params[['sum_by_variable']])) {

                    log_message_variable('sum_by_variable', params[['sum_by_variable']])
                    selection__sum_by_variable <- params[['sum_by_variable']]
                }
                if (!is.null(params[['color_variable']])) {

                    log_message_variable('color_variable', params[['color_variable']])
                    selection__color_variable <- params[['color_variable']]
                }
                if (!is.null(params[['facet_variable']])) {

                    log_message_variable('facet_variable', params[['facet_variable']])
                    selection__facet_variable <- params[['facet_variable']]
                }
                if (!is.null(params[['size_variable']])) {

                    log_message_variable('size_variable', params[['size_variable']])
                    selection__size_variable <- params[['size_variable']]
                }
# TODO: label_have multiple values.
                if (!is.null(params[['label_variables']])) {

                    log_message_variable('label_variables', params[['label_variables']])
                    selection__label_variables <- params[['label_variables']]
                }
                if (!is.null(params[['order_by_variable']])) {

                    log_message_variable('order_by_variable', params[['order_by_variable']])
                    selection__order_by_variable <- params[['order_by_variable']]
                }

                parameter_info$step <- max(parameter_info$step,
                                           create_url_param_step("Updated Dynamic Variables (Triggered by Dataset)"))
            } else {

                log_message_block_start("Updating variables based on new dataset")
            }
            updateSelectInput(session, 'var_plots__variable',
                              choices=choices__variable,
                              selected=selection__variable)
            updateSelectInput(session, 'var_plots__comparison',
                              choices=choices__comparison,
                              selected=selection__comparison)
            updateSelectInput(session, 'var_plots__sum_by_variable',
                              choices=choices__sum_by_variable,
                              selected=selection__sum_by_variable)
            updateSelectInput(session, 'var_plots__facet_variable',
                              choices=choices__facet_variable,
                              selected=selection__facet_variable)
            updateSelectInput(session, 'var_plots__size_variable',
                              choices=choices__size_variable,
                              selected=selection__size_variable)
            updateSelectInput(session, 'var_plots__color_variable',
                              choices=choices__color_variable,
                              selected=selection__color_variable)
            updateSelectInput(session, 'var_plots__label_variables',
                              choices=choices__label_variables,
                              selected=selection__label_variables)
            updateSelectInput(session, 'var_plots__order_by_variable',
                              choices=choices__order_by_variable,
                              selected=selection__order_by_variable)

            # updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=FALSE)
            # updateSelectInput(session,
            #                   'var_plots__numeric_aggregation_function',
            #                   selected=global__num_num_aggregation_function_default)
            # updateSelectInput(session,
            #                   'var_plots__numeric_aggregation',
            #                   selected=global__var_plots__numeric_aggregation_default)
            # updateTextInput(session, 'var_plots__multi_value_delimiter', value="")

            if(!is.null(parameter_info$step)) {

                    parameter_info$step <- max(parameter_info$step,
                                               create_url_param_step("Updated Dynamic Variables (Triggered by Dataset)"))
            }
        }
    })

    ######
    # COMPARISON
    ######
    observeEvent(input$var_plots__variable, {

        if(!is.null(parameter_info$step) && parameter_info$step < create_url_param_step("Loaded Dataset")) {
            # Should be equal to 3 right after all the variables have been loaded, no need to reload comparison

            log_message_block_start("Ignoring Comparison Variable Update")

        } else {

            column_names <- colnames(reactive__source_data$data)

            if(!is.null(input$var_plots__variable) &&
                    (input$var_plots__variable == global__select_variable ||
                        input$var_plots__variable %in% column_names)) {

                log_message_block_start("Updating Comparison Variables based on new Variable")
                log_message_variable('var_plots__variable', input$var_plots__variable)
                log_message_variable('var_plots__comparison', input$var_plots__comparison)
           
                if(is_date_type(reactive__source_data$data[[input$var_plots__variable]])) {

                    column_names <- colnames(reactive__source_data$data %>% select_if(is.numeric))
                }

                current_selection <- input$var_plots__comparison
                if(!is.null(parameter_info$step) && !is.null(parameter_info$params[['comparison']])) {
                    # if parameter$step is not null, then regardless of the primary variable/etc., we want that value

                    current_selection <- parameter_info$params[['comparison']]

                } else if(input$var_plots__variable == global__select_variable) {

                    current_selection <- NULL
                }
                selected_variable <- default_if_null_or_empty_string(current_selection,
                                                                     global__select_variable_optional)

                updateSelectInput(session, 'var_plots__comparison',
                                  choices=c(global__select_variable_optional, column_names),
                                  selected=selected_variable)

                log_message_variable('selected_variable', selected_variable)

                if(!is.null(parameter_info$step)) {

                    parameter_info$step <- max(parameter_info$step,
                                               create_url_param_step("Updated Comparison Variable"))
                }
            }
        }
    })

    ######
    # COLOR
    ######
    observeEvent(c(input$var_plots__variable, input$var_plots__comparison), {

        if(!is.null(parameter_info$step) && parameter_info$step < create_url_param_step("Loaded Dataset")) {

            log_message_block_start("Ignoring Color Variable Update")

        } else {

            column_names <- colnames(reactive__source_data$data)

            if(!is.null(input$var_plots__variable) &&
                    input$var_plots__variable != global__select_variable &&
                    input$var_plots__variable %in% column_names) {

                log_message_block_start("Updating Color based on new variable/comparison")
                log_message_variable('var_plots__comparison', input$color_variable)

                if(is_date_type(reactive__source_data$data[[input$var_plots__variable]])) {

                    column_names <- colnames(reactive__source_data$data %>% select_if(purrr::negate(is.numeric)))

                } else if(xor(is.numeric(reactive__source_data$data[[input$var_plots__variable]]),
                          is.numeric(reactive__source_data$data[[input$var_plots__comparison]]))) {

                    column_names <- colnames(reactive__source_data$data %>% select_if(purrr::negate(is.numeric)))

                }

                current_selection <- input$var_plots__color_variable
                if(!is.null(parameter_info$step) && !is.null(parameter_info$params[['color_variable']])) {
                    # if parameter$step is not null, then regardless of the primary variable/etc., we want that value

                    current_selection <- parameter_info$params[['color_variable']]

                } else if(input$var_plots__variable == global__select_variable) {

                    current_selection <- NULL
                }
                selected_variable <- default_if_null_or_empty_string(current_selection,
                                                                     global__select_variable_optional)

                updateSelectInput(session, 'var_plots__color_variable',
                                  choices=c(global__select_variable_optional, column_names),
                                  selected=selected_variable)
                
                log_message_variable('selected_variable', selected_variable)

                if(!is.null(parameter_info$step)) {

                    parameter_info$step <- max(parameter_info$step,
                                               create_url_param_step("Updated Color Variable"))
                }
            }
        }
    })

    ##########################################
    # Categoric Variable
    ##########################################
    observeEvent(c(#input$var_plots__categoric_view_type,
                   input$var_plots__comparison,
                   input$var_plots__sum_by_variable), {

        if(!is.null(parameter_info$step) && parameter_info$step < create_url_param_step("Updated Comparison Variable")) {

            log_message_block_start("Ignoring Categoric Variable Update")

        } else {

            # used for Categoric Primary and optionally Categoric Secondary variables
            log_message_block_start("Creating Categoric View Type")

            log_message_variable('input$var_plots__categoric_view_type', input$var_plots__categoric_view_type)
            log_message_variable('input$var_plots__comparison', input$var_plots__comparison)
            log_message_variable('input$var_plots__sum_by_variable', input$var_plots__sum_by_variable)

            previous_selection <- input$var_plots__categoric_view_type
            local_comparison_variable <- null_if_select_variable_optional(input$var_plots__comparison)
            local_sum_by_variable <- null_if_select_variable_optional(input$var_plots__sum_by_variable)                    

            view_type_options <- NULL
            if(is.null(local_comparison_variable) && is.null(local_sum_by_variable)) {

                view_type_options <- c("Bar", "Confidence Interval")

            } else if(is.null(local_comparison_variable) && !is.null(local_sum_by_variable)) {

                view_type_options <- c("Bar")

            } else if(!is.null(local_comparison_variable) && is.null(local_sum_by_variable)) {

                view_type_options <- c("Bar",
                                       "Confidence Interval",
                                       "Facet by Comparison",
                                       "Confidence Interval - within Variable",
                                       "Stack")

            } else { # both are not null
                
                view_type_options <- c("Bar", "Facet by Comparison", "Stack")
            }

            if(!is.null(previous_selection) && previous_selection %in% view_type_options) {
                
                selected_option <- previous_selection

            } else {

                selected_option <- "Bar"
            }

            updateSelectInput(session,
                              'var_plots__categoric_view_type',
                              choices = view_type_options,
                              selected = selected_option)
                log_message_variable('selected_option', selected_option)


            if(!is.null(parameter_info$step)) {

                    parameter_info$step <- max(parameter_info$step,
                                               create_url_param_step("Updated Categoric View Variable"))
            }
        }
    })

    observeEvent(input$var_plots__generate_link, {

        custom_link <- build_custom_url(get_base_url(session), build_parameters_list(input, input$preloaded_dataset))
        
        if(nchar(custom_link) > 2000) {

            log_message_variable('custom_link length', nchar(custom_link))
            log_message_variable('custom_link', custom_link)

            showModal(modalDialog(title='Error, link too long from too many options.'))

        } else {

            showModal(urlModal(custom_link, title = "Link to Regenerate Settings & Graph",
                               subtitle = "Links only work with pre-loaded & non-filtered datasets."))
        }
        
    })
})

#http://127.0.0.1:3158/?data=Flights&tab=Graphs&variable=date&comparison=arr_delay&color_variable=dest&facet_variable=origin&ts_date_floor=month
#http://127.0.0.1:3158/?data=Flights&tab=Graphs&custom_title=This%20is%20my%20title%20from%20URL%20parameter&variable=date&comparison=arr_delay&color_variable=dest&facet_variable=origin&ts_date_floor=month

