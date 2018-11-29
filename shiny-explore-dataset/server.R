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
    # Correlation Plot
    ##########################################################################################################
    output$correlation_plot <- renderPlot__correlation_plot(input, output, session, dataset)

    ##########################################################################################################
    # Viarable Plot
    ##########################################################################################################

    # Viarable Plot - Filters
    filter_controls_list <- reactive__filter_controls_list(input, dataset)
    output$variable_plots_filter_bscollapse_UI <- renderUI__variable_plots_filter_bscollapse_UI(filter_controls_list)
    variable_plots_filtered_dataset <- reactive__variable_plots_filtered_dataset(input, dataset)  # duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
    observeEvent__variable_plots_filter_clear(input, session)
    observeEvent__variable_plots_filter_apply(input, session)
    observe__variable_plots_bscollapse__color(input, session, dataset)
    observeEvent__variable_plots_filter_use(input, session)

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
