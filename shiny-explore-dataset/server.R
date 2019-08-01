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
    
    url_parameter_info <- reactiveValues(params=NULL,
                                         currently_updating=FALSE,
                                         preloaded_dataset_var_updated=FALSE,
                                         has_updated_variables=FALSE,
                                         can_plot=FALSE,
                                         has_plotted=FALSE)
    
    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################

    reactive__source_data <- reactiveValues(data=NULL)

    # initially suspended, resume depending on url parameters
    observeEvent_preloaded_dataset <-  observeEvent__source_data__preloaded(session, input, output, reactive__source_data, url_parameter_info)

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
    
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        session,
                                                                        reactive__var_plots__filtered_data,
                                                                        url_parameter_info)
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

    observeEvent_dynamic_variables <- observeEvent(reactive__source_data$data, {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        log_message_block_start("Updating Dynamic Variables (Triggered from new Dataset)")

        column_names <- colnames(reactive__source_data$data)
        numeric_column_names <- colnames(reactive__source_data$data %>% select_if(is.numeric))
        categoric_column_names <- colnames(reactive__source_data$data %>% select_if(purrr::negate(is.numeric)))

        updateSelectInput(session, 'var_plots__variable',
                          choices=c(global__select_variable, column_names),
                          selected=global__select_variable)
        updateSelectInput(session, 'var_plots__comparison',
                          choices=c(global__select_variable_optional, column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__sum_by_variable',
                          choices=c(global__select_variable_optional, numeric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__facet_variable',
                          choices=c(global__select_variable_optional, categoric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__size_variable',
                          choices=c(global__select_variable_optional, column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__color_variable',
                          choices=c(global__select_variable_optional, categoric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__label_variables',
                          choices=column_names,
                          selected=character(0))
        updateSelectInput(session, 'var_plots__order_by_variable',
                          choices=c("Default", "Frequency", numeric_column_names),
                          selected="Default")
    }, suspended=TRUE)

    observeEvent_comparison <- observeEvent(input$var_plots__variable, {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        column_names <- colnames(reactive__source_data$data)

        if(!is.null(input$var_plots__variable) &&
                (input$var_plots__variable == global__select_variable ||
                    input$var_plots__variable %in% column_names)) {

            log_message_block_start("Updating var_plots__comparison based on var_plots__variable")

            results <- var_plots__comparison__logic(dataset=reactive__source_data$data,
                                                    primary_variable=input$var_plots__variable,
                                                    current_value=input$var_plots__comparison)

            updateSelectInput(session, 'var_plots__comparison',
                              choices=results$choices,
                              selected=results$selected)
        }
    }, suspended=TRUE)

    observeEvent_color <- observeEvent(c(input$var_plots__variable, input$var_plots__comparison), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        column_names <- colnames(reactive__source_data$data)

        if(!is.null(input$var_plots__variable) &&
                input$var_plots__variable != global__select_variable &&
                input$var_plots__variable %in% column_names) {

            results <- var_plots__color__logic(dataset=reactive__source_data$data,
                                               primary_variable=input$var_plots__variable,
                                               comparison_variable=input$var_plots__comparison,
                                               current_value=input$var_plots__color_variable)

            updateSelectInput(session, 'var_plots__color_variable',
                              choices=results$choices,
                              selected=results$selected)
        }
    }, suspended=TRUE)

    observeEvent_categoric <- observeEvent(c(input$var_plots__comparison,
                                             input$var_plots__sum_by_variable), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        results <- var_plots__categoric_view_type__logic(dataset=reactive__source_data$data,
                                                         comparison_variable=input$var_plots__comparison,
                                                         sum_by_variable=input$var_plots__sum_by_variable,
                                                         current_value=input$var_plots__categoric_view_type)

        updateSelectInput(session, 'var_plots__categoric_view_type',
                          choices=results$choices,
                          selected=results$selected)

    }, suspended=TRUE)

    resume_dynamic_observers_load_dataset <- function(session,
                                                      input,
                                                      observeEvent_preloaded_dataset,
                                                      observeEvent_dynamic_variables,
                                                      observeEvent_comparison,
                                                      observeEvent_color,
                                                      observeEvent_categoric) {
        # resume data loading and dynamic variables, then trigger loading with updateSelectInput
            observeEvent_preloaded_dataset$resume()
            observeEvent_dynamic_variables$resume()
            observeEvent_comparison$resume()
            observeEvent_color$resume()
            observeEvent_categoric$resume()

#TODO resume dynamic variables
            log_message("Loading initial dataset")
            log_message_variable('Initial Dataset', isolate(input$preloaded_dataset))
            updateSelectInput(session, 'preloaded_dataset', selected=isolate(input$preloaded_dataset))
    }

    ##########################################################################################################
    # UPDATE FROM URL
    ##########################################################################################################
    url_search__observeEvent <- observeEvent(session$clientData$url_search, {
        
        url_search <- session$clientData$url_search
        
        log_message_variable('base url', get_base_url(session))

        if(is.null(url_search) || url_search == '') {

            log_message_block_start("No URL Parameters Detected, loading initial dataset")

            resume_dynamic_observers_load_dataset(session,
                                                  input,
                                                  observeEvent_preloaded_dataset,
                                                  observeEvent_dynamic_variables,
                                                  observeEvent_comparison,
                                                  observeEvent_color,
                                                  observeEvent_categoric)

        } else {

            log_message_block_start("Detected URL Parameters")
            log_message_variable("URL Search", url_search)

            params <- parseQueryString(url_search)

            if(is.null(params) || length(params) == 0 || is.null(params[['data']]) || is.null(params[['tab']])) {

                log_message("Detected URL Parameters But Unable To Create Parameter List")
                log_message_variable("data", params[['data']])
                log_message_variable("tab", params[['tab']])

                resume_dynamic_observers_load_dataset(session,
                                                      input,
                                                      observeEvent_preloaded_dataset,
                                                      observeEvent_dynamic_variables,
                                                      observeEvent_comparison,
                                                      observeEvent_color,
                                                      observeEvent_categoric)

            } else {

                log_message_variable("param_names", paste0(names(params), collapse="; "))

                url_parameter_info$params <- params
                url_parameter_info$currently_updating <- TRUE

                rt_stopif(is.null(params[['data']]))
                rt_stopif(is.null(params[['tab']]))
                
                log_message("Clearing the URL from Browser")
                shiny::updateQueryString(get_base_url(session), mode = "replace")
                
                reactive__source_data$data <- select_preloaded_dataset(session=session,
                                                                       output=output,
                                                                       dataset_name=params[['data']],
                                                                       message="Loaded Dataset from URL params")


                log_message_block_start("Continuing Processing Url Parameter")

                if(input$preloaded_dataset == params[['data']]) {

                    url_parameter_info$preloaded_dataset_var_updated <- TRUE

                } else {
                    # This will trigger an update to input$preloaded_dataset
                    # but we need to wait until input$preloaded_dataset is updated before we can resume
                    # the obseerver for observeEvent_preloaded_dataset
                    log_message("Updating Preloaded Dataset Dropdown Selection")
                    updateSelectInput(session, 'preloaded_dataset', selected=params[['data']])
                }

                log_message("Updating Navbar Tab")
                updateNavbarPage(session, 'navbar_page_app', selected = params[['tab']])
            }
        }
    })

    observeEvent(input$preloaded_dataset, {

        req(input$preloaded_dataset)  # only run when there is a value
        req(url_parameter_info$currently_updating)  # only run when we have params (otherwise there is nothing to syncronize)
        req(!url_parameter_info$preloaded_dataset_var_updated)  # only care about resuming observer if input$preloaded_data has been updated

        # once preloaded_dataset has updated, set this trigger
        if(input$preloaded_dataset == url_parameter_info$params[['data']]) {

            log_message_block_start("Preloaded Dataset Done Changing")
            log_message_variable("preloaded_dataset", input$preloaded_dataset)

            log_message("Resuming observer for Preloaded Dataset Dropdown")
            observeEvent_preloaded_dataset$resume()
            url_parameter_info$preloaded_dataset_var_updated <- TRUE


            log_message_block_start("Updating all variables From URL Params")
            update_var_plot_variables_from_url_params(session, url_parameter_info$params, reactive__source_data$data, input)

            url_parameter_info$has_updated_variables <- TRUE

        }
    })
    
    # observeEvent(url_parameter_info$init_finished, {
        
    #     log_message_variable('WTF', url_parameter_info$params[['data']])
    #     log_message_variable('WTF input$preloaded_dataset', input$preloaded_dataset)
    #     updateSelectInput(session, 'preloaded_dataset', selected=url_parameter_info$params[['data']])
    # })


    # observeEvent(url_parameter_info$preloaded_dataset_var_updated, {
    #     # preloaded_dataset_var_updated will be TRUE when the dataset to load form the URL parameter is the 
    #     # same as the default selection, or after input$preloaded_dataset_var_updated is updated

    #     # once the input$preloaded_dataset has been updated, we can update the variables with the url params
    #     if(url_parameter_info$preloaded_dataset_var_updated) {

    #         log_message_block_start('Detected Dataset Loader Has Finished - Updating Variable From URL Params')
    #         log_message_variable('WTF', input$preloaded_dataset)
    #         update_var_plot_variables_from_url_params(session, url_parameter_info$params, reactive__source_data$data)

    #         url_parameter_info$has_updated_variables <- TRUE
    #     }
    # })

    observeEvent(url_parameter_info$has_updated_variables, {

        if(url_parameter_info$has_updated_variables) {

            log_message_block_start('Detected Has Updated Variables - Resuming Dynamic Variable Observers')

            # by now, we have have loaded the dataset and called update_var_plot_variables_from_url_params
            # so we can resume all of the various dynamic variable loaders, which should not run because
            # nothing new at this point should trigger them, until after the plot has been created
            observeEvent_dynamic_variables$resume()
            observeEvent_comparison$resume()
            observeEvent_color$resume()
            observeEvent_categoric$resume()

            log_message('Setting `can_plot` to TRUE')
            url_parameter_info$can_plot <- TRUE  # should trigger the plot
        }
    })

    observeEvent(url_parameter_info$has_plotted, {

        if(url_parameter_info$has_plotted) {

            url_parameter_info$currently_updating <- FALSE
            log_message_block_start("Finished URL Parameter Process")
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
