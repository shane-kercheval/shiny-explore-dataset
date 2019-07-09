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
source('helper_scripts/regression_helpers.R')

options(shiny.maxRequestSize=100*1024^2)  # increase upload limit to 30MB


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################
    custom_triggers <- reactiveValues(reload_source_data=0)
    # loads dataset depending on drop down or upload
    reactive__source_data <- reactive__source_data__creator(session, input, custom_triggers)
    # shows the first 500 rows of the data
    output$source_data__head_table <- renderDataTable__source_data__head(reactive__source_data)
    # shows the types of the data's variables/columns
    output$source_data__types_table <- renderDataTable__source_data__types(reactive__source_data)
    output$source_data__add_date_fields__UI <- renderUI__source_data__add_date_fields__UI(reactive__source_data)

    observeEvent(input$source_data__add_date_fields, {
        # this should trigger a reload of the dataset (perhaps not the best approach, but TBD on alternatives)
        # however, it shouldn't trigger a reload if it is set back to the default global__select_variable_optional
        # which will trigger false positive loadings
        if(!is.null(reactive__source_data()) &&
                !is.null(input$source_data__add_date_fields) &&
                input$source_data__add_date_fields != global__select_variable_optional &&
                input$source_data__add_date_fields %in% colnames(reactive__source_data())) {

            custom_triggers$reload_source_data <- runif(1, 1, 1000000)
        }
    })

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
    output$var_plots__filtering_messages <- renderPrint__reactiveValues__vp_filtering_message(reactiveValues__vp_filtering_message)
    
    reactive__var_plots__filtered_data <- reactive__var_plots__filtered_data__creator(input,
                                                                                      reactive__source_data,
                                                                                      reactiveValues__vp_filtering_message)
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot__creator(input,
                                                                        session,
                                                                        reactive__var_plots__filtered_data)
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
    output$var_plots__variable__UI <- renderUI__var_plots__variable__UI(reactive__source_data)
    output$var_plots__comparison__UI <- renderUI__var_plots__comparison__UI(input, reactive__source_data)
    output$var_plots__sum_by_variable__UI <- renderUI__var_plots__sum_by_variable__UI(reactive__source_data)
    output$var_plots__color_variable__UI <- renderUI__var_plots__color_variable__UI(input, reactive__source_data)
    output$var_plots__size_variable__UI <- renderUI__var_plots__size_variable__UI(reactive__source_data)
    output$var_plots__label_variables__UI <- renderUI__var_plots__label_variables__UI(reactive__source_data)
    output$var_plots__order_by_variable__UI <- renderUI__var_plots__order_by_variable__UI(reactive__source_data)
    observeEvent__var_plots__categoric_view_type(input, session)


    # the `outputOptions` options code below makes it so that these variables/selectInput update even if they
    # are hidden; why? e.g. if using the "Pretty Text" option, these variables are accessed whether hidden
    # or not. If the dataset has been changed, then the variables might refer to a column that doesn't exist.
    # So, even if they are hidden, we need them to update behind the scenes.
    outputOptions(output, "var_plots__variable__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__comparison__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__sum_by_variable__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__color_variable__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__size_variable__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__label_variables__UI", suspendWhenHidden = FALSE)
    outputOptions(output, "var_plots__order_by_variable__UI", suspendWhenHidden = FALSE)

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
})
