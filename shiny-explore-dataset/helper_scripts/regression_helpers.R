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

##############################################################################################################
# Regression Reactive UI
##############################################################################################################
renderUI__regression_dependent_variable_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='regression_dependent_variable',
                    label='Dependent Variable',
                    choices=c(select_variable, colnames(dataset())),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression_independent_variables_UI <- function(input, dataset) {

    renderUI({

        req(input$regression_dependent_variable)

        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% input$regression_dependent_variable]        

        checkboxGroupInput(inputId='regression_independent_variables',
                           label='Independent Variables',
                           choices=possible_variables,
                           selected=possible_variables,
                           inline=FALSE,
                           width=NULL)
    })
}

renderUI__regression_summary_header_UI <- function(regression_results) {

    renderUI({

        req(regression_results())

        local_regression_results <- regression_results()

        if(is.null(local_regression_results$reference)) {  # reference is filled for logistic regression

            reference <- ''            

        } else {

            reference <- paste0('(reference: `', local_regression_results$reference, '`)')
        }

        tags$h4(paste(regression_results()$type, 'Summary', reference))
    })
}

renderUI__regression_interaction_term1_UI <- function(input, dataset) {

    renderUI({

        req(input$regression_dependent_variable)

        # cannot select dependent_variable
        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% input$regression_dependent_variable]

        selectInput(inputId='regression_interaction_term1',
                    label='Interaction Variable 1',
                    choices=c(select_variable, possible_variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression_interaction_term2_UI <- function(input, dataset) {

    renderUI({

        req(input$regression_dependent_variable)
        req(input$regression_interaction_term1)

        # cannot select dependent_variable or the first term
        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% c(input$regression_dependent_variable,
                                                                 input$regression_interaction_term1)]

        selectInput(inputId='regression_interaction_term2',
                    label='Interaction Variable 2',
                    choices=c(select_variable, possible_variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

observeEvent__regression_toggle_all_ind_variables <- function(input, dataset, session) {

    observeEvent(input$regression_toggle_all_ind_variables, {

        # if none selected, select all, otherwise (if any selected); unselect all
        if(length(input$regression_independent_variables) == 0) {

            column_names <- colnames(dataset())
            possible_variables <- column_names[! column_names %in% input$regression_dependent_variable]

            updateCheckboxGroupInput(session=session,
                                     inputId='regression_independent_variables',
                                     selected=possible_variables)

        } else {

            updateCheckboxGroupInput(session=session,
                                     inputId='regression_independent_variables',
                                     selected=character(0))
        }
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
