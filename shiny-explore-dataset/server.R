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
source('helper_scripts/ui_helpers.R')
source('helper_scripts/dynamic_show_hide_controls.R')
source('helper_scripts/generic_helpers.R')
source('helper_scripts/reactive_helpers.R')
source('helper_scripts/renderUI_control_helpers.R')
source('helper_scripts/render_output_helpers.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # REACTIVE DATASETS
    ##########################################################################################################

    # main dataset
    dataset <- reactive__dataset(input, output, session)
    # calculate the numeric summary; it is an expensive operation for large datasets
    numeric_summary_data <- reactive__numeric_summary(input, output, session, dataset)
    # calculate the categoric summary; it is an expensive operation for large datasets
    categoric_summary_data <- reactive__categoric_summary(input, output, session, dataset)
    # Run Regression when user clicks Run button
    regression_results <- eventReactive__regression_results(input, output, session, dataset)

    ##########################################################################################################
    # REACTIVE UI CONTROLS
    ##########################################################################################################

    # summaries
    output$selected_numeric_summary_options_UI <- renderUI__numeric_summary_options_UI(numeric_summary_data)

    # Reactive UI for Variable Plot Controls
    output$selected_variable_plot_variable_UI <- renderUI__variable_plot_variable_UI(dataset)
    output$selected_variable_plot_comparison_UI <- renderUI__variable_plot_comparison_UI(dataset)
    output$selected_variable_plot_point_color_UI <- renderUI__variable_plot_point_color_UI(dataset)
    output$selected_variable_plot_point_size_UI <- renderUI__variable_plot_point_size_UI(dataset)
    observe__variable_plot__hide_show_uncollapse_on_primary_vars(input, output, session)

    # Regression Reactive UI
    output$regression_selected_dependent_variable_UI <- renderUI__regression_selected_dependent_variable_UI(dataset)
    output$regression_selected_independent_variables_UI <- renderUI__regression_selected_independent_variables_UI(input, dataset)
    output$regression_summary_header_UI <- renderUI__regression_summary_header_UI(regression_results)
    output$regression_selected_interaction_term1_UI <- renderUI__regression_selected_interaction_term1_UI(input, dataset)
    output$regression_selected_interaction_term2_UI <- renderUI__regression_selected_interaction_term2_UI(input, dataset)
    observeEvent__regression_toggle_all_ind_variables(input, dataset, session)

    ##########################################################################################################
    # RENDER OUTPUT
    ##########################################################################################################
    output$dataset_head_table <- renderDataTable__dataset_head_table(dataset)
    output$dataset_types_table <- renderDataTable__dataset_types_table(dataset)
    output$numeric_summary_table <- renderDataTable__numeric_summary_table(input, numeric_summary_data)
    output$categoric_summary_table <- renderDataTable__categoric_summary_table(categoric_summary_data)
    output$categoric_summary_text <- renderPrint__categoric_summary_text(dataset, categoric_summary_data)
    output$correlation_plot <- renderPlot__correlation_plot(input, output, session, dataset)

    ##########################################################################################################
    # Variable Plot
    # NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
    # withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
    # shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
    # not affecting return of the plot from the function or it being displayed in shiny
    ##########################################################################################################
    output$variable_plot <- renderPlot__variable_plot(input, output, session, dataset)

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    output$regression_summary_output <- renderPrint({

        req(regression_results())

        summary(regression_results()$results)
    })

    output$regression_number_of_rows_missing_removed <- renderText({

        req(regression_results())

        paste('Number of missing/removed rows from dataset:', length(regression_results()$rows_excluded))
    })

    output$regression_formula <- renderText({

        req(regression_results())

        regression_results()$formula
    })

    output$regression_summary_vif <- renderPrint({

        req(regression_results())

        car::vif(regression_results()$results)
    })

    render_diagnostic_plot <- function(graph_function, graph_width_function) {

        return (
            renderPlot({

                req(regression_results())

                withProgress(value=1/2, message='Creating Regression Diagnostic Graph',{

                    graph_function()
                })
        
            }, height = graph_width_function)
        )
    }

    output$regression_diagnostic_actual_vs_predicted <- render_diagnostic_plot(
        graph_function=function() {
            xyplot(dataset()[, isolate({input$regression_selected_dependent_variable})] ~ predict(regression_results()$results),
                   type=c('p', 'g'),
                   xlab='Predicted', ylab='Actual')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_predicted_width})
    output$regression_diagnostic_residuals_vs_fitted <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=1) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_fitted_width})
    output$regression_diagnostic_actual_vs_observed <- render_diagnostic_plot(
        graph_function=function() {
            xyplot(predict(regression_results()$results) ~ 1:nrow(dataset()),
                   type=c('p', 'g'),
                   xlab='Observation Number', ylab='Predicted')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_observed_width})
    output$regression_diagnostic_normal_qq <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=2) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_normal_qq_width})
    output$regression_diagnostic_scale_location <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=3) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_scale_location_width})
    output$regression_diagnostic_cooks_distance <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=4) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_width})
    output$regression_diagnostic_residuals_vs_leverage <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=5) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_leverage_width})
    output$regression_diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=6) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_vs_leverage_width})
})
