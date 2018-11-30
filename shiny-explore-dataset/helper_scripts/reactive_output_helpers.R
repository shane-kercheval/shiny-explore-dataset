############################################################################################################## 
# Variable Plot Output
##############################################################################################################
renderDataTable__dataset_head_table <- function(dataset) {

    renderDataTable({

        return (head(dataset(), 500))
    })
}

renderDataTable__dataset_types_table <- function(dataset) {

    renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            local_dataset <- dataset()

            types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])

            return (data.frame(variable=colnames(local_dataset), type=types))
        })
    })
}

renderDataTable__numeric_summary_table <- function(input, numeric_summary_data) {

    renderDataTable({

        local_numeric_summary <- numeric_summary_data()
        local_numeric_options <- input$numeric_summary_options
        return (local_numeric_summary[, c('feature', local_numeric_options)])
    })
}

renderDataTable__categoric_summary_table <- function(categoric_summary_data) {

    renderDataTable({
        
        return (categoric_summary_data())
    })
}

renderPrint__categoric_summary_text <- function(dataset, categoric_summary_data) {

    renderPrint({
        
        # get R's summary of the categoric data
        return (summary(dataset()[, as.character(categoric_summary_data()$feature)]))
    })
}

renderPlot__correlation_plot <- function(input, output, session, dataset) {

    renderPlot({

        withProgress(value=1/2, message='Calculating Correlations', {

            local_dataset <- dataset()

            if(input$correlation_pretty_text) {

                local_dataset <- rt_pretty_dataset(local_dataset)
            }

            log_message_block_start('Calculating Correlations & Creating Plot')
            log_message_variable('correlation_corr_threshold', input$correlation_corr_threshold)
            log_message_variable('correlation_p_value_threshold', input$correlation_p_value_threshold)
            log_message_variable('correlation_base_size', input$correlation_base_size)
            log_message_variable('correlation_pretty_text', input$correlation_pretty_text)

            # see note about why I use print, in `variable plot` section below.
            return (
                print(rt_explore_plot_correlations(dataset=local_dataset,
                                                   corr_threshold=input$correlation_corr_threshold,
                                                   p_value_threshold=input$correlation_p_value_threshold,
                                                   base_size=input$correlation_base_size,
                                                   type='pearson'))
            )
        })
    }, height = function() {

        session$clientData$output_correlation_plot_width * 0.66  # set height to % of width
    })
}

##############################################################################################################
# Variable Plot
# NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
# withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
# shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
# not affecting return of the plot from the function or it being displayed in shiny
##############################################################################################################
get_dynamic_filter_selections <- function(input, columns) {

    # get all of the selections from the dynamic filters without triggering refresh for the first time
    selections_list <- map(columns, ~ isolate(input[[paste0('dynamic_filter_variable_plots_', .)]]))
    names(selections_list) <- columns

    return (selections_list)

}

renderPlot__variable_plot <- function(input, output, session, reactive__variable_plots__ggplot, messages) {

    renderPlot({
        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(reactive__variable_plots__ggplot()))

           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_variable_plots_width * 0.66  # set height to % of width
    })
}

##############################################################################################################
# Regression
##############################################################################################################
renderPrint__regression_summary_output <- function(regression_results) {

    renderPrint({

        req(regression_results())
        summary(regression_results()$results)
    })
}

renderText__regression_number_of_rows_missing_removed <- function(regression_results) {

    renderText({

        req(regression_results())
        paste('Number of missing/removed rows from dataset:', length(regression_results()$rows_excluded))
    })
}

renderText__regression_formula <- function(regression_results) {

    renderText({

        req(regression_results())
        regression_results()$formula
    })
}

renderPrint__regression_summary_vif <- function(regression_results) {

    renderPrint({

        req(regression_results())
        car::vif(regression_results()$results)
    })
}

render_diagnostic_plot <- function(regression_results, graph_function, graph_width_function) {

    return (
        renderPlot({

            req(regression_results())
            withProgress(value=1/2, message='Creating Regression Diagnostic Graph',{

                graph_function()
            })
    
        }, height = graph_width_function)
    )
}

render_diagnostic_plot__regression_diagnostic_actual_vs_predicted <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() {
            xyplot(dataset()[, isolate({input$regression_dependent_variable})] ~ predict(regression_results()$results),
                   type=c('p', 'g'),
                   xlab='Predicted', ylab='Actual')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_predicted_width}
    )
}

render_diagnostic_plot__regression_diagnostic_residuals_vs_fitted <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=1) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_fitted_width}
    )
}

render_diagnostic_plot__regression_diagnostic_actual_vs_observed <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() {
            xyplot(predict(regression_results()$results) ~ 1:nrow(dataset()),
                   type=c('p', 'g'),
                   xlab='Observation Number', ylab='Predicted')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_observed_width}
    )
}

render_diagnostic_plot__regression_diagnostic_normal_qq <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=2) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_normal_qq_width}
    )
}

render_diagnostic_plot__regression_diagnostic_scale_location <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=3) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_scale_location_width}
    )
}

render_diagnostic_plot__regression_diagnostic_cooks_distance <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=4) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_width}
    )
}

render_diagnostic_plot__regression_diagnostic_residuals_vs_leverage <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=5) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_leverage_width}
    )
}

render_diagnostic_plot__regression_diagnostic_cooks_distance_vs_leverage <- function(input, session, dataset, regression_results) {

    render_diagnostic_plot(
        regression_results,
        graph_function=function() { plot(regression_results()$results, which=6) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_vs_leverage_width}
    )
}
