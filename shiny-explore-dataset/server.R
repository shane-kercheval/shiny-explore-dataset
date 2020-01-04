#devtools::install_github('shane-kercheval/rtools', force=TRUE)

library(DT)
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(ggExtra)
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
source('helper_scripts/graph_functions.R')
source('helper_scripts/regression_helpers.R')

options(shiny.maxRequestSize=200*1024^2)
# options(shiny.reactlog=TRUE) 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    log_message_block_start("##########################################################################################\nStarting Server\n##########################################################################################")
    log_message_variable('Last Sync Date', global__reference_date)

    url_parameter_info <- reactiveValues(params=NULL,
                                         filter_params=NULL,
                                         currently_updating=FALSE,
                                         preloaded_dataset_var_updated=FALSE,
                                         has_filter_params=FALSE,
                                         has_created_filter_controls=FALSE,
                                         has_displayed_filter_controls=FALSE,
                                         has_set_filter_controls=FALSE,
                                         has_filter_ran=FALSE,
                                         has_updated_variables=FALSE,
                                         can_plot=FALSE,
                                         has_plotted=FALSE,
                                         progress=NULL)

    var_plots_graph_options_can_dirty <- reactiveVal(TRUE)
    var_plots__convert_numerics_to_categoric__clicked <- reactiveVal(FALSE)
    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################
    reactive__source_data <- reactiveValues(data=NULL, source=NULL)

    observeEvent(reactive__source_data$data, {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        log_message_block_start('Clearing Filter triggered by new dataset')

        updateCheckboxInput(session, inputId='var_plots__filter_use', value=FALSE)
        updateCollapse(session, 'var_plots__bscollapse', close="Filter")
        updateCollapse(session, "var_plots__bscollapse", style = list('Filter' = 'default'))

    }, ignoreInit=TRUE)

    observeEvent(c(reactive__source_data$data, # clear with new dataset
                   input$var_plots__clear_all_settings), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        if(!is.null(reactive__source_data$data)) {
            
            log_message_block_start('Clearing All Settings')
    
            clear_variables(session, input, swap_primary_and_comparison=FALSE)
            helper__restore_defaults_graph_options(session)
            helper__restore_defaults_other_options(session)
            helper__restore_defaults_map_options(session)
            updateCollapse(session, 'var_plots__bscollapse', close="Graph Options")
            updateCollapse(session, 'var_plots__bscollapse', close="Other Options")
            updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
            updateCollapse(session, "var_plots__bscollapse", style = list('Graph Options' = 'default',
                                                                          'Other Options' = 'default',
                                                                          'Map Options' = 'default'))
        }
    }, ignoreInit=TRUE)

    observeEvent_preloaded_dataset <-  observeEvent__source_data__preloaded(session,
                                                                            input,
                                                                            output,
                                                                            reactive__source_data,
                                                                            url_parameter_info)

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

    # reactive__var_plots__final_dataset takes the master->filtered dataset and applies any additional 
    # changes to it before it passes it on to be plotted
    # DO NOT DO ANY ADDITIONAL FILTERING
    reactive__var_plots__final_dataset <- reactive({

        # update when filtering changes
        local_dataset <- reactive__var_plots__filtered_data()
        input$var_plots__convert_numerics_to_categoric__cut_seq_apply

        if(is_date_type(local_dataset[[input$var_plots__variable]]) && input$var_plots__convert_primary_date_to_categoric) {

            local_dataset[[input$var_plots__variable]] <- rt_floor_date_factor(local_dataset[[input$var_plots__variable]],
                                                                               date_floor=input$var_plots__ts_date_floor)
        }

        if(is.numeric(local_dataset[[input$var_plots__variable]]) &&
            is.numeric(local_dataset[[input$var_plots__comparison]]) &&
            input$var_plots__convert_numerics_to_categoric) {

            n_cuts <- input$var_plots__convert_numerics_to_categoric__num_groups + 1
            x_cut_sequence <- str_trim(isolate(input$var_plots__convert_numerics_to_categoric__x_cut_sequence))
            y_cut_sequence <- str_trim(isolate(input$var_plots__convert_numerics_to_categoric__y_cut_sequence))

            convert_variable <- function(dataset, variable, cut_sequence, n_cuts) {

                if(is_null_or_empty_string(cut_sequence)) {

                    cut_sequence <- seq(min(dataset[[variable]], na.rm = TRUE),
                                        max(dataset[[variable]], na.rm = TRUE),
                                        length.out = n_cuts)
                } else {

                    cut_sequence <- as.numeric(str_split(cut_sequence, ",", simplify = TRUE))
                }

                return (cut(dataset[[variable]],
                            breaks = cut_sequence,
                            include.lowest = TRUE, ordered_result = TRUE,
                            dig.lab = 5))
            }

            local_dataset[[input$var_plots__variable]] <- convert_variable(local_dataset,
                                                                           input$var_plots__variable,
                                                                           y_cut_sequence,
                                                                           n_cuts)

            local_dataset[[input$var_plots__comparison]] <- convert_variable(local_dataset,
                                                                             input$var_plots__comparison,
                                                                             x_cut_sequence,
                                                                             n_cuts)
        }

        return (local_dataset)
    })

    observeEvent(c(input$var_plots__convert_numerics_to_categoric), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        if(!is.null(input$var_plots__convert_numerics_to_categoric) &&
            input$var_plots__convert_numerics_to_categoric) {
            
            updateSelectInput(session, 'var_plots__categoric_view_type',
                              selected="Heatmap")
            var_plots__convert_numerics_to_categoric__clicked(TRUE)
        }
    }, ignoreInit=TRUE)
    
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        session,
                                                                        reactive__var_plots__final_dataset,
                                                                        reactive__source_data,
                                                                        url_parameter_info,
                                                                        var_plots_graph_options_can_dirty,
                                                                        var_plots__convert_numerics_to_categoric__clicked)
    # stores any messages/warnings that ggplot produces when rendering the plot (outputs below the graph
    #(var_plots__ggplot_messages))
    reactiveValues__vp__ggplot_message <- reactiveValues(value=NULL)
    output$var_plots__ggplot_messages <- renderPrint__reactiveValues__vp__ggplot_message(reactiveValues__vp__ggplot_message)

    # this builds up the filters based on the dataset column types and dynamically adds the controls to var_plots__filter_bscollapse__UI
    reactive__filter_controls_list <- reactive__filter_controls_list__creator(input, reactive__source_data)
    # area in Filters area where all of the filters are stored and then shown/hidden based on selections
    output$var_plots__filter_bscollapse__UI <- renderUI__var_plots__filter_bscollapse__UI(input, reactive__source_data, reactive__filter_controls_list, url_parameter_info)
    # where the user selects the variables they want to filter
    output$var_plots__filter_controls_selections__UI <- renderUI__var_plots__filter_controls_selections__UI(input, reactive__source_data, url_parameter_info)
    # contains the logic to show/hide filters based on selections
    observeEvent__var_plots__show_hide_dynamic_filters(input, session, reactive__source_data)

    observeEvent__var_plots__filter_clear(input, session)
    observeEvent__var_plots__filter_apply(input, session)
    observe__var_plots__bscollapse__dynamic_inputs(input, session, reactive__source_data)
    observeEvent__var_plots__filter_use(input, session)

    observeEvent__var_plots__custom_labels_clear(input, session)
    observeEvent__var_plots__graph_options_clear(input, session)
    observeEvent__var_plots__graph_options_apply(input, session)
    observeEvent__var_plots__graph_options__any_used__function(input, session, url_parameter_info, var_plots_graph_options_can_dirty)
    observeEvent__var_plots__custom_labels_apply(input, session)
    observeEvent__var_plots__other_options__any_used__function(input, session, url_parameter_info)
    
    observeEvent(input$var_plots__date_cr__plot_type, {

        req(!is.null(input$var_plots__date_cr__plot_type) &&
            !is.null(input$var_plots__date_conversion_variable) &&
            input$var_plots__date_conversion_variable != global__select_variable_optional)

        log_message_block_start("Updating var_plots__date_cr__plot_type")
        hide_show_date_cr_options(session, input)
    })

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

    ##########################################################################################################
    # observeEvent's for updating the dynamic variables
    # initially suspended so that when we set the graph variables/options via url parameters those settings
    # don't get overrided; resumed half-way through the process so everything can flush (and be ignored) by
    # the time the process is finished.
    ##########################################################################################################
    observeEvent_dynamic_variables <- observeEvent(reactive__source_data$data, {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        log_message_block_start("Updating Dynamic Variables (Triggered from new Dataset)")

        column_names <- colnames(reactive__source_data$data)
        numeric_column_names <- colnames(reactive__source_data$data %>% select_if(is.numeric))
        categoric_column_names <- colnames(reactive__source_data$data %>% select_if(is_categoric))
        date_column_names <- colnames(reactive__source_data$data %>% select_if(is_date_type))

        updateSelectInput(session, 'var_plots__variable',
                          choices=c(global__select_variable, column_names),
                          selected=global__select_variable)
        updateSelectInput(session, 'var_plots__comparison',
                          choices=c(global__select_variable_optional, column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__sum_by_variable',
                          choices=c(global__select_variable_optional, numeric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__count_distinct_variable',
                          choices=c(global__select_variable_optional, categoric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__facet_variable',
                          choices=c(global__select_variable_optional, categoric_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__size_variable',
                          choices=c(global__select_variable_optional, column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__date_conversion_variable',
                          choices=c(global__select_variable_optional, date_column_names),
                          selected=global__select_variable_optional)
        updateSelectInput(session, 'var_plots__date_cr__snapshots__group_variable',
                          choices=c(global__select_variable_optional, categoric_column_names),
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

    observeEvent_comparison <- observeEvent(c(input$var_plots__variable,
                                              input$var_plots__convert_primary_date_to_categoric), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)
        req(reactive__source_data$data)

        column_names <- colnames(reactive__source_data$data)

        if(!is.null(input$var_plots__variable) &&
                (input$var_plots__variable == global__select_variable ||
                    input$var_plots__variable %in% column_names)) {

            log_message_block_start("Updating Comparison Logic")

            results <- var_plots__comparison__logic(dataset=reactive__source_data$data,
                                                    primary_variable=input$var_plots__variable,
                                                    current_value=input$var_plots__comparison,
                                                    primary_date_converted_to_categoric=input$var_plots__convert_primary_date_to_categoric)

            updateSelectInput(session, 'var_plots__comparison',
                              choices=results$choices,
                              selected=results$selected)
        }
    }, suspended=TRUE)

     observeEvent_date_conversion_variable <- observeEvent(input$var_plots__variable, {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)
        req(reactive__source_data$data)

        column_names <- colnames(reactive__source_data$data)

        if(!is.null(input$var_plots__variable) &&
                input$var_plots__variable %in% column_names &&
                is_date_type(reactive__source_data$data[[input$var_plots__variable]])) {

            log_message_block_start("Updating Conversion Variable Logic")

            results <- var_plots__date_conversion_variable__logic(dataset=reactive__source_data$data,
                                                                  primary_variable=input$var_plots__variable,
                                                                  current_value=input$var_plots__date_conversion_variable)

            updateSelectInput(session, 'var_plots__date_conversion_variable',
                              choices=results$choices,
                              selected=results$selected)
        }
    }, suspended=TRUE)


    observeEvent_color <- observeEvent(c(input$var_plots__variable, input$var_plots__comparison), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)
        req(reactive__source_data$data)

        column_names <- colnames(reactive__source_data$data)

        if(!is.null(input$var_plots__variable) &&
                input$var_plots__variable != global__select_variable &&
                input$var_plots__variable %in% column_names) {

            log_message_block_start("Updating Color Logic")

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
                                             input$var_plots__sum_by_variable,
                                             input$var_plots__count_distinct_variable,
                                             input$var_plots__facet_variable), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)
        req(reactive__source_data$data)

        # if(is_categoric(reactive__source_data$data[[input$var_plots__variable]])) {

            log_message_block_start("Updating Categoric View Logic")
            results <- var_plots__categoric_view_type__logic(dataset=reactive__source_data$data,
                                                             comparison_variable=input$var_plots__comparison,
                                                             sum_by_variable=input$var_plots__sum_by_variable,
                                                             count_distinct_variable=input$var_plots__count_distinct_variable,
                                                             facet_variable=input$var_plots__facet_variable,
                                                             current_value=input$var_plots__categoric_view_type)

            updateSelectInput(session, 'var_plots__categoric_view_type',
                              choices=results$choices,
                              selected=results$selected)
        # }
    }, suspended=TRUE)

    # hide/show trend_extend_date
    observeEvent(input$var_plots__trend_line, {

        if(!is.null(input$var_plots__trend_line) && input$var_plots__trend_line == "Projection") {

            log_message_block_start("Updating Trend Extend Date Logic")

            results <- var_plots__trend_extend_date__logic(dataset=reactive__source_data$data,
                                                           primary_variable=input$var_plots__variable,
                                                           current_value=input$var_plots__trend_extend_date)

            updateDateInput(session, inputId='var_plots__trend_extend_date', value = results)
            shinyjs::show('var_plots__trend_extend_date')

        } else {

            reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
        }
    }, ignoreInit=TRUE)

    resume_dynamic_observers <- function(session,
                                         input,
                                         observeEvent_preloaded_dataset,
                                         observeEvent_dynamic_variables,
                                         observeEvent_comparison,
                                         observeEvent_date_conversion_variable,
                                         observeEvent_color,
                                         observeEvent_categoric) {
        # resume data loading and dynamic variables, then trigger loading with updateSelectInput
        observeEvent_preloaded_dataset$resume()
        observeEvent_dynamic_variables$resume()
        observeEvent_comparison$resume()
        observeEvent_date_conversion_variable$resume()
        observeEvent_color$resume()
        observeEvent_categoric$resume()

        log_message("Updating input$preloaded_dataset")
        log_message_variable('Initial Dataset', isolate(input$preloaded_dataset))
        updateSelectInput(session, 'preloaded_dataset', selected=isolate(input$preloaded_dataset))
    }

    ##########################################################################################################
    # UPDATE FROM URL
    # 
    # How it works:
    # ============
    # There are various observeEvents for e.g. loading the dataset and updating the selectInput choices for 
    # the variables based on the dataset; these are initially suspended when the app starts. 

    # There is an observeEvent for the url_search which parses the url-parameters into a list of
    # parameter/values; if no parameters are found, then all of the observeEvents (e.g. loading dataset and
    # updating selectInputs) are resumed and the application proceeds as usual. If parameters are found, then
    # 
    # The dataset is loaded, the variable selections and graph options are updated, and the data is filtered
    # if applible. This is done by a somewhat complex event-chain, which is explained below.
    # 
    # 
    # Dataset & Variables
    # ----------
    # The dataset is loaded directly rather than from the selectInput. We then set the preloaded_dataset
    # selectInput to the appropriate value.
    # 
    # We have to wait for the preloaded_dataset selectInput to get set via the reactive system (and of course
    # after we manully set the values) before resuming all the observeEvents, otherwise our variables (which
    # have been manually set) will be overwritten. To do that I use `preloaded_dataset_var_updated`. If we are
    # loading the default dataset, then the preloaded_dataset selectInput already has the correct value and 
    # `preloaded_dataset_var_updated` is set to TRUE and we immediately update all of the variables & graph 
    # options based on the url parameters; otherwise, we have to rely on our observeEvent for 
    # preloaded_dataset, which will set `preloaded_dataset_var_updated` to TRUE and update all of the
    # variables/options only when input$preloaded_dataset matches our parameter value (again, so nothing is
    # overwritten, which will happen when input$preloaded_dataset is udpated because many reactive events will
    # be triggered)
    # 
    # 
    # Once the variables & graph options have been set, the `has_updated_variables` is set to TRUE. 
    # Before we can create the graph, `has_updated_variables` has to be TRUE and IF the url parameters
    # include variables to filter, we have to be done filtering (i.e. `has_filter_ran` has to be TRUE).
    # Once both of these things are done, we resume all of the observeEvents if we haven't already, and set
    # `can_plot` to TRUE.
    #
    # `can_plot` is referenced in the code that creates the ggplot so it will triggered once this value is set
    # to TRUE (if the app is generating the graph from url parmaeters; via 
    # isolate(url_parameter_info$currently_updating))
    # 
    # Once the graph is finished being created, `has_plotted` gets set to TRUE, which triggers an event to 
    # set `url_parameter_infocurrently_updating` to FALSE. This is important because we only do a lot of the
    # previously mentioned logic once, while we are processing/updating from the url-parameters. Once the
    # graph has been created, the process is finished and we should shut that off to avoid unnecessary code
    # execution and side-effects. 
    # 
    # Filtering
    # ----------
    # 
    # The basic steps to filtering are:
    #     expand "Filters" collapsable menu, which will trigger generating the filter controls and building
    #         "Filters" selectInput control
    #     once the controls have been created `has_created_filter_controls` is set to TRUE from within the
    #         logic that creates the control
    #     once `has_created_filter_controls` is set to TRUE, we can update the "Filters" selectInput control
    #         to all of the dataset columns that we need to filter by. This will automatically trigger the
    #         underlying dynamic filter controls to be added to the menu. 
    #     we have to wait for the controls to be added to the menu, which is done in a somewhat hacky way
    #         since these controls are dynamic (see code that sets `has_displayed_filter_controls` to TRUE)
    #     once the dynamic filter controls are added, we can set the values according to the url-parameters
    #     again, we have to wait until the reactive events have processed and the controls have actually been
    #         set; refer to the logic around the `has_set_filter_controls` variables
    #     once `has_set_filter_controls` is TRUE, then we can update the "Use Filters" checkbox, which will
    #         automatically filter the dataset. Again, we have to wait for this to happen, so this is
    #         accomplished by an observeEvent on this checkbox.
    #     once the "Use Filters" checkbox is successfully updated, we can set `has_filter_ran` to TRUE which
    #         will trigger `can_plot` to be set to TRUE, which will trigger the graph to be completed.
    # 
    ##########################################################################################################
    url_search__observeEvent <- observeEvent(session$clientData$url_search, {

        shinyjs::hide('div__loading_page')
        shinyjs::show('div__main_content')
 
        url_parameter_info$progress <- Progress$new(session, min=0, max=2)
        url_parameter_info$progress$set(message = 'Initializing...')
        url_parameter_info$progress$set(value = 1)
        
        url_search <- session$clientData$url_search
        
        log_message_block_start("Executing URL Search Observer")
        
        log_message_variable('url_protocol', session$clientData$url_protocol)
        log_message_variable('url_hostname', session$clientData$url_hostname)
        log_message_variable('url_port', session$clientData$url_port)
        log_message_variable('url_pathname', session$clientData$url_pathname)
        log_message_variable('url_search', session$clientData$url_search)
        log_message_variable('url_hash_initial', session$clientData$url_hash_initial)
        log_message_variable('url_hash', session$clientData$url_hash)

        log_message_variable('generated url', get_base_url(session))

        if(is.null(url_search) || url_search == '') {

            log_message_block_start("No URL Parameters Detected, loading initial dataset")

            resume_dynamic_observers(session,
                                     input,
                                     observeEvent_preloaded_dataset,
                                     observeEvent_dynamic_variables,
                                     observeEvent_comparison,
                                     observeEvent_date_conversion_variable,
                                     observeEvent_color,
                                     observeEvent_categoric)

            url_parameter_info$progress$close()

        } else {
            
            log_message_block_start("Detected URL Parameters")
            log_message_variable("URL Search", url_search)

            params <- extract_url_parameters(url_search)

            if(is.null(params) || length(params) == 0 || is.null(params[['data']]) || is.null(params[['tab']])) {

                log_message("Detected URL Parameters But Unable To Create Parameter List")
                log_message_variable("data", params[['data']])
                log_message_variable("tab", params[['tab']])

                resume_dynamic_observers(session,
                                         input,
                                         observeEvent_preloaded_dataset,
                                         observeEvent_dynamic_variables,
                                         observeEvent_comparison,
                                         observeEvent_date_conversion_variable,
                                         observeEvent_color,
                                         observeEvent_categoric)

                url_parameter_info$progress$close()

            } else {

                url_parameter_info$progress$set(message = 'Processing URL Parameters')
                log_message_variable("param_names", paste0(names(params), collapse="; "))

                url_parameter_info$currently_updating <- TRUE
                url_parameter_info$params <- params

                rt_stopif(is.null(params[['data']]))
                rt_stopif(is.null(params[['tab']]))
                
                log_message("Clearing the URL from Browser")
                shiny::updateQueryString(get_base_url(session), mode = "replace")
                
                withProgress(value=1/2, message='Loading Dataset',{

                    log_message_block_start("Loaded Dataset from URL params")
                    log_message_variable("params[['data']]", params[['data']])

                    reactive__source_data$data <- select_preloaded_dataset(dataset_name=params[['data']])$dataset
                    reactive__source_data$source <- "preloaded"
                })

                log_message_block_start("Continuing Processing Url Parameter")
                log_message("Updating Navbar Tab")
                updateNavbarPage(session, 'navbar_page_app', selected = params[['tab']])

                if(input$preloaded_dataset == params[['data']]) {

                    observeEvent_preloaded_dataset$resume()
                    url_parameter_info$preloaded_dataset_var_updated <- TRUE
                    # don't need to wait for the observe
                    helper__update_var_plot_variables_from_url_params(session, url_parameter_info, reactive__source_data, input)

                } else {
                    # This will trigger an update to input$preloaded_dataset
                    # but we need to wait until input$preloaded_dataset is updated before we can resume
                    # the obseerver for observeEvent_preloaded_dataset
                    log_message("Updating Preloaded Dataset Dropdown Selection")
                    updateSelectInput(session, 'preloaded_dataset', selected=params[['data']])
                }

                filter_params <- params[which(str_starts(names(params), global__url_params_filter_prefix))]
                if(!is.null(filter_params) && length(filter_params) > 0) {
                    log_message('Detected filter parameters')

                    url_parameter_info$has_filter_params <- TRUE

                    names(filter_params) <- str_replace(names(filter_params), global__url_params_filter_prefix, '')
                    url_parameter_info$filter_params <- filter_params

                    log_message_variable('Filtering variables', paste0(names(filter_params), collapse='; '))
                    updateCollapse(session, 'var_plots__bscollapse', open="Filters")
                }
            }
        }
    })

    helper__update_var_plot_variables_from_url_params <- function(session, url_parameter_info, reactive__source_data, input) {
        log_message_block_start("Updating all variables From URL Params")
        update_var_plot_variables_from_url_params(session, url_parameter_info$params, reactive__source_data$data, input)
        url_parameter_info$has_updated_variables <- TRUE
    }

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

            helper__update_var_plot_variables_from_url_params(session, url_parameter_info, reactive__source_data, input)
        }
    })

    #' detects when the filter controls have been created and updates the filter selectInput selections
    observeEvent(url_parameter_info$has_created_filter_controls, {

        if(url_parameter_info$has_created_filter_controls) {
            log_message_block_start('Detected has_created_filter_controls')
            log_message_variable('Filter param names', paste0(names(url_parameter_info$filter_params), collapse="; "))

            updateSelectInput(session, 'var_plots__filter_controls_selections', selected=names(url_parameter_info$filter_params))
        }
    })

    #' this is a way to detect that we have actually shown the controls
    #' It is also used to update the "Use Filter" checkbox after we have displayed and done setting the values
    #' of all the filters
    observe({

        req(isolate(url_parameter_info$currently_updating))
        req(isolate(url_parameter_info$filter_params))

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(variable_name in names(url_parameter_info$filter_params)) {
            value <- input[[paste0('var_plots__dynamic_filter__', variable_name)]]

            log_message_variable(paste0('var_plots__dynamic_filter__', variable_name), value)
            selections <- append(selections, value)
        }

        # only update if we are using the filter
        # also, if any of the selections are not null, that means they have been initialized and we can begin
        # to mark as being changed otherwise, the filter section hasn't even been opened
        if(any(map_lgl(selections, ~ !is.null(.)))) {

            log_message_block_start('Detected that the filter controls have rendered i.e. have values.')
            url_parameter_info$has_displayed_filter_controls <- TRUE

            # This will get triggered again change to the input control values from our hacky registration above
            # once this happens and once we have has_set_filter_controls then we know we can turn the filter controls on
            if(isolate(url_parameter_info$has_set_filter_controls)) {

                updateCheckboxInput(session, inputId='var_plots__filter_use', value = TRUE)
            }
        }
    })

    observeEvent(input$var_plots__filter_use, { 

        if(url_parameter_info$currently_updating &&
                url_parameter_info$has_filter_params && 
                !is.null(input$var_plots__filter_use) && input$var_plots__filter_use) {

            log_message_block_start("Setting has_filter_ran to TRUE")
            url_parameter_info$has_filter_ran <- TRUE
        }
    })

    #' this triggers after we have displayed all of the controls
    #' it is the main logic that goes through each url parmaeter and updates the filter control values
    observeEvent(url_parameter_info$has_displayed_filter_controls, {

        if(url_parameter_info$has_displayed_filter_controls) {
            log_message_block_start("Setting Filter Control Values from URL Parameters")

            for(variable_name in names(url_parameter_info$filter_params)) {
                
                dynamic_filter_name <- paste0('var_plots__dynamic_filter__', variable_name)
                filter_values <- url_parameter_info$filter_params[[variable_name]]

                log_message_variable(paste0("updating `", variable_name, "`"),
                                     paste0(filter_values, collapse='; '))

                log_message_variable('class', class(reactive__source_data$data[[variable_name]]))    

                if(is_date_type(reactive__source_data$data[[variable_name]])) {

                    log_message('updating dateRangeInput (date)')
                    stopifnot(length(filter_values) == 2)
                    updateDateRangeInput(session, inputId=dynamic_filter_name,
                                         start=filter_values[1], end=filter_values[2])
                    
                } else if(is.factor(reactive__source_data$data[[variable_name]])) {

                    log_message('updating updateSelectInput (factor)')
                    updateSelectInput(session, inputId=dynamic_filter_name, selected=filter_values)

                } else if(is.character(reactive__source_data$data[[variable_name]])) {
                    
                    log_message('updating updateSelectInput (character)')
                    updateSelectInput(session, inputId=dynamic_filter_name, selected=filter_values)

                } else if(is.numeric(reactive__source_data$data[[variable_name]])) {

                    log_message('updating sliderInput (numeric)')
                    stopifnot(length(filter_values) == 2)
                    updateSliderInput(session, inputId=dynamic_filter_name,
                                      value=filter_values)

                } else if(is.logical(reactive__source_data$data[[variable_name]])) {

                    log_message('updating updateSelectInput (logical)')
                    updateSelectInput(session, inputId=dynamic_filter_name, selected=filter_values)

                } else if("hms" %in% class(reactive__source_data$data[[variable_name]])) {

                    log_message('updating sliderTextInput (hms)')
                    stopifnot(length(filter_values) == 2)
                    updateSliderTextInput(session, inputId=dynamic_filter_name, selected=filter_values)

                } else {
                    #class(.)[1]
                    stopifnot(FALSE)
                }
            }

            url_parameter_info$has_set_filter_controls <- TRUE
        }
    })

    #' this tracks whether we have updated the variable selectInput controls and done filtering;
    #' only then can we plot
    observeEvent(c(url_parameter_info$has_updated_variables,
                   url_parameter_info$has_filter_ran), {

        if(url_parameter_info$has_updated_variables &&
                (!url_parameter_info$has_filter_params || url_parameter_info$has_filter_ran)) {

            log_message_block_start('Detected Has Updated Variables - Resuming Dynamic Variable Observers')

            # by now, we have have loaded the dataset and called update_var_plot_variables_from_url_params
            # so we can resume all of the various dynamic variable loaders, which should not run because
            # nothing new at this point should trigger them, until after the plot has been created
            observeEvent_dynamic_variables$resume()
            observeEvent_comparison$resume()
            observeEvent_date_conversion_variable$resume()
            observeEvent_color$resume()
            observeEvent_categoric$resume()

            log_message('Setting `can_plot` to TRUE')
            url_parameter_info$can_plot <- TRUE  # should trigger the plot
        }
    })

    #' once we are finished plotting, we can set `currently_updating` to FALSE which other parts of the app
    #' use to know if they should execute logic related to URL params. This makes it so we stop executing
    #' various pieces of code and avoid random side-effects in the app
    observeEvent(url_parameter_info$has_plotted, {

        if(url_parameter_info$has_plotted) {

            url_parameter_info$currently_updating <- FALSE
            log_message_block_start("Finished URL Parameter Process")
            url_parameter_info$progress$close()
        }
    })

    # this is the code that generates the link for a particular graph based on the selections and filtering
    observeEvent(input$var_plots__generate_link, {
        log_message_block_start("Generating URL Parameter Link")

        filter_list <- NULL
        if(!is.null(isolate(input$var_plots__filter_use)) && isolate(input$var_plots__filter_use)) {
            log_message("Including filters in link")

            column_names <- colnames(reactive__source_data$data)

            # these are the columns we want to filter on; if the column is not in the selection, don't filter
            filter_controls_selections <- isolate(input$var_plots__filter_controls_selections)
            if('All Variables' %in% filter_controls_selections) {

                filter_controls_selections <- colnames(local_dataset)
            }

            all_filter_values <- get_dynamic_filter_values(input, column_names)
            filter_list <- all_filter_values[filter_controls_selections]
        }

        custom_link <- build_custom_url(get_base_url(session),
                                        build_parameters_list(input=input,
                                                              preloaded_dataset=input$preloaded_dataset,
                                                              filter_list=filter_list))
        if(nchar(custom_link) > 2000) {

            log_message_variable("custom_link length", nchar(custom_link))
            log_message_variable("custom_link", custom_link)

            showModal(modalDialog(title=paste0("Error: The link is too long (",
                                               nchar(custom_link),
                                               ") from too many options. Max link size is 2000.")))

        } else if(is.null(reactive__source_data$source) || reactive__source_data$source != 'preloaded') {

            showModal(modalDialog(title=paste0("Error: Links can only be generated from Pre-loaded datasets. The current dataset's source is `",
                                               reactive__source_data$source,
                                               "`.")))

        } else {

            showModal(urlModal(custom_link, title = "Link to Regenerate Settings, Filters, & Graph",
                               subtitle = "Links only work with pre-loaded datasets. Additional R code that has been ran will not be re-applied."))
        }
        
    })
})

#http://127.0.0.1:3158/?data=Flights&tab=Graphs&variable=date&comparison=arr_delay&color_variable=dest&facet_variable=origin&ts_date_floor=month
#http://127.0.0.1:3158/?data=Flights&tab=Graphs&custom_title=This%20is%20my%20title%20from%20URL%20parameter&variable=date&comparison=arr_delay&color_variable=dest&facet_variable=origin&ts_date_floor=month
