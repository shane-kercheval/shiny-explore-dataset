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
source('helper_scripts/dataset_loading_helpers.R')
source('helper_scripts/numeric_summary_helpers.R')
source('helper_scripts/categoric_summary_helpers.R')
source('helper_scripts/correlation_helpers.R')
source('helper_scripts/variable_plots_helpers.R')
source('helper_scripts/regression_helpers.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################
    # loads dataset depending on drop down or upload
    reactive__source_data <- reactive__source_data__creator(input)
    # shows the first 500 rows of the data
    output$source_data__head_table <- renderDataTable__source_data__head(reactive__source_data)
    # shows the types of the data's variables/columns
    output$source_data__types_table <- renderDataTable__source_data__types(reactive__source_data)

    ##########################################################################################################
    # numeric summary data
    ##########################################################################################################
    reactive__numeric_summary <- reactive__numeric_summary_creator(reactive__source_data)
    output$numeric_summary__table <- renderDataTable__numeric_summary__table(input, reactive__numeric_summary)
    output$numeric_summary__options__UI <- renderUI__numeric_summary__options__UI(reactive__numeric_summary)

    ##########################################################################################################
    # categoric summary data
    ##########################################################################################################
    reactive__categoric_summary <- reactive__categoric_summary(reactive__source_data)
    output$categoric_summary__table <- renderDataTable__categoric_summary__table(reactive__categoric_summary)
    output$categoric_summary__text <- renderPrint__categoric_summary__text(reactive__source_data, reactive__categoric_summary)

    ##########################################################################################################
    # Correlation Plot
    ##########################################################################################################
    output$correlation__plot <- renderPlot__correlation__plot(input, session, reactive__source_data)

    ##########################################################################################################
    # Viarable Plot
    ##########################################################################################################
    # cached dataset after the filters have been applied (which is bad for large datasets :( ) so that the
    # filters don't have to be reapplied every time; sacrificing memory for speed 
    reactive__var_plots__filtered_data <- reactive__var_plots__filtered_data_creator(input, reactive__source_data)
    # creates the ggplot object
    reactive__var_plots__ggplot <- reactive__var_plots__ggplot_creator(input, session, reactive__var_plots__filtered_data)
    # stores any messages/warnings that ggplot produces when rendering the plot (outputs below the graph (var_plots__ggplot_messages))
    reactiveValues__vp__ggplot_message <- reactiveValues(value=NULL)
    output$var_plots__ggplot_messages <- renderPrint__reactiveValues__vp__ggplot_message(reactiveValues__vp__ggplot_message)

    # Viarable Plot - Filters - this builds up the filters based on the dataset column types and dynamically 
    # adds the controls to var_plots__filter_bscollapse_UI
    filter_controls_list <- reactive__filter_controls_list(input, reactive__source_data)
    output$var_plots__filter_bscollapse_UI <- renderUI__var_plots__filter_bscollapse_UI(filter_controls_list)
    observeEvent__var_plots__filter_clear(input, session)
    observeEvent__var_plots__filter_apply(input, session)
    observe__var_plots__bscollapse__dynamic_inputs(input, session, reactive__source_data)
    observeEvent__var_plots__filter_use(input, session)

    # main plot
    output$var_plots <- renderPlot__variable_plot(session, reactive__var_plots__ggplot, reactiveValues__vp__ggplot_message)
    output$var_plots__variable_UI <- renderUI__var_plots__variable_UI(reactive__source_data)
    output$var_plots__comparison_UI <- renderUI__var_plots__comparison_UI(reactive__source_data)
    output$var_plots__sum_by_variable_UI <- renderUI__var_plots__sum_by_variable_UI(reactive__source_data)
    output$var_plots__point_color_UI <- renderUI__var_plots__point_color_UI(reactive__source_data)
    output$var_plots__point_size_UI <- renderUI__var_plots__point_size_UI(reactive__source_data)
    observe__var_plots__hide_show_uncollapse_on_primary_vars(input, session)

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    # Run Regression when user clicks Run button
    regression_results <- eventReactive__regression_results(input, output, session, reactive__source_data)
    output$regression_summary_output <- renderPrint__regression_summary_output(regression_results)
    output$regression_number_of_rows_missing_removed <- renderText__regression_number_of_rows_missing_removed(regression_results)
    output$regression_formula <- renderText__regression_formula(regression_results)
    output$regression_summary_vif <- renderPrint__regression_summary_vif(regression_results)
    output$regression_diagnostic_actual_vs_predicted <- render_diagnostic_plot__regression_diagnostic_actual_vs_predicted(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_residuals_vs_fitted <- render_diagnostic_plot__regression_diagnostic_residuals_vs_fitted(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_actual_vs_observed <- render_diagnostic_plot__regression_diagnostic_actual_vs_observed(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_normal_qq <- render_diagnostic_plot__regression_diagnostic_normal_qq(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_scale_location <- render_diagnostic_plot__regression_diagnostic_scale_location(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_cooks_distance <- render_diagnostic_plot__regression_diagnostic_cooks_distance(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_residuals_vs_leverage <- render_diagnostic_plot__regression_diagnostic_residuals_vs_leverage(input, session, reactive__source_data, regression_results)
    output$regression_diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot__regression_diagnostic_cooks_distance_vs_leverage(input, session, reactive__source_data, regression_results)
    # Regression Reactive UI
    output$regression_dependent_variable_UI <- renderUI__regression_dependent_variable_UI(reactive__source_data)
    output$regression_independent_variables_UI <- renderUI__regression_independent_variables_UI(input, reactive__source_data)
    output$regression_summary_header_UI <- renderUI__regression_summary_header_UI(regression_results)
    output$regression_interaction_term1_UI <- renderUI__regression_interaction_term1_UI(input, reactive__source_data)
    output$regression_interaction_term2_UI <- renderUI__regression_interaction_term2_UI(input, reactive__source_data)
    observeEvent__regression_toggle_all_ind_variables(input, reactive__source_data, session)
})
