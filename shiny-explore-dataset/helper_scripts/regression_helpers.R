##########################################################################################################
# Regression Results - Run Regression when user clicks Run button
##########################################################################################################    
eventReactive__regression__results__creator <- function(input, dataset) {

    eventReactive(input$regression__run_button, {

        if(input$regression__dependent_variable == global__select_variable) {

            return (NULL)
        }

        local_interaction_term1 <- input$regression__interaction_term1
        local_interaction_term2 <- input$regression__interaction_term2

        withProgress(value=1/2, message='Running Regression',{

            interaction_variables <- NULL

            if(!is.null(local_interaction_term1) && local_interaction_term1 != global__select_variable &&
               !is.null(local_interaction_term2) && local_interaction_term2 != global__select_variable) {

                interaction_variables <- list(c(local_interaction_term1,
                                                local_interaction_term2))
            }

            # updates to reactive variables will not trigger an update here, only regression__run_button
            results <- rt_regression(dataset=dataset$data,
                                     dependent_variable=input$regression__dependent_variable,
                                     independent_variables=input$regression__independent_variables,
                                     # list of vectors, each element in the list is a pair of interaction terms
                                     # only supporting two interaction variables at the moment
                                     interaction_variables=interaction_variables)

            shinyjs::show('regression__formula_header')
            shinyjs::show('regression__summary_header__UI')
            shinyjs::show('regression__vif_header')
            
            return (results)
        })
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__regression__dependent_variable__UI <- function(dataset) {

    renderUI({

        selectInput(inputId='regression__dependent_variable',
                    label='Dependent Variable',
                    choices=c(global__select_variable, colnames(dataset$data)),
                    selected=global__select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression__independent_variables__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)

        column_names <- colnames(dataset$data)
        possible_variables <- column_names[! column_names %in% input$regression__dependent_variable]        

        selectInput(inputId='regression__independent_variables',
                    label = 'Independent Variables',
                    choices = possible_variables,
                    selected = NULL,
                    multiple = TRUE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__regression__summary_header__UI <- function(regression__results) {

    renderUI({

        req(regression__results())

        local_regression__results <- regression__results()

        if(is.null(local_regression__results$reference)) {  # reference is filled for logistic regression

            reference <- ''            

        } else {

            reference <- paste0('(reference: `', local_regression__results$reference, '`)')
        }

        tags$h4(paste(regression__results()$type, 'Summary', reference))
    })
}

renderUI__regression__interaction_term1__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)

        # cannot select dependent_variable
        column_names <- colnames(dataset$data)
        possible_variables <- column_names[! column_names %in% input$regression__dependent_variable]

        selectInput(inputId='regression__interaction_term1',
                    label='Interaction Variable 1',
                    choices=c(global__select_variable, possible_variables),
                    selected=global__select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderUI__regression__interaction_term2__UI <- function(input, dataset) {

    renderUI({

        req(input$regression__dependent_variable)
        req(input$regression__interaction_term1)

        # cannot select dependent_variable or the first term
        column_names <- colnames(dataset$data)
        possible_variables <- column_names[! column_names %in% c(input$regression__dependent_variable,
                                                                 input$regression__interaction_term1)]

        selectInput(inputId='regression__interaction_term2',
                    label='Interaction Variable 2',
                    choices=c(global__select_variable, possible_variables),
                    selected=global__select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

observeEvent__regression__toggle_all_ind_variables <- function(input, dataset, session) {

    observeEvent(input$regression__toggle_all_ind_variables, {

        # if none selected, select all, otherwise (if any selected); unselect all
        if(length(input$regression__independent_variables) == 0) {

            column_names <- colnames(dataset$data)
            possible_variables <- column_names[! column_names %in% input$regression__dependent_variable]

            updateSelectInput(session=session,
                              inputId='regression__independent_variables',
                              selected=possible_variables)

        } else {

            updateSelectInput(session=session,
                              inputId='regression__independent_variables',
                              selected=character(0))
        }
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderUI__regression__residuals_vs_predictors_var__UI <- function(input, regression_results) {

    renderUI({

        choices <- rt_regression_get_ind_var_options(regression_results()$model,
                                                     input$regression__dependent_variable,
                                                     input$regression__independent_variables)
        selectInput(inputId='regression__residuals_vs_predictors_var',
                    label=NULL,
                    choices=choices,
                    selected=choices[1],
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })
}

renderPrint__regression__summary_output <- function(regression__results) {

    renderPrint({

        req(regression__results())
        summary(regression__results()$model)
    })
}

renderText__regression__number_of_rows_missing_removed <- function(regression__results) {

    renderText({

        req(regression__results())
        paste('Number of missing/removed rows from dataset:', length(regression__results()$rows_excluded))
    })
}

renderText__regression__formula <- function(regression__results) {

    renderText({

        req(regression__results())
        regression__results()$formula
    })
}

renderPrint__regression__summary_vif <- function(regression__results) {

    renderPrint({

        req(regression__results())
        car::vif(regression__results()$model)
    })
}

render_diagnostic_plot <- function(regression__results, graph_function, graph_width_function) {

    return (
        renderPlot({

            req(regression__results())
            withProgress(value=1/2, message='Creating Regression Diagnostic Graph',{

                graph_function()
            })
    
        }, height = graph_width_function)
    )
}

render_diagnostic_plot__actual_vs_predicted <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() {
            rt_regression_plot_actual_vs_predicted(regression__results()$model)
            # xyplot(dataset$data[, isolate({input$regression__dependent_variable})] ~ predict(regression__results()$model),
            #        type=c('p', 'g'),
            #        xlab='Predicted', ylab='Actual')
        },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_actual_vs_predicted_width}
    )
}

render_diagnostic_plot__residuals_vs_fitted <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { rt_regression_plot_residual_vs_predicted(regression__results()$model) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_residuals_vs_fitted_width}
    )
}

render_diagnostic_plot__residuals_vs_predictors <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() {

            req(input$regression__residuals_vs_predictors_var)
            rt_regression_plot_residual_vs_variable(regression__results()$model,
                                                    input$regression__residuals_vs_predictors_var,
                                                    dataset$data)
        },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_residuals_vs_predictors_width}
    )
}

render_diagnostic_plot__actual_vs_observed <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() {
            xyplot(predict(regression__results()$model) ~ 1:nrow(dataset$data),
                   type=c('p', 'g'),
                   xlab='Observation Number', ylab='Predicted')
        },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_actual_vs_observed_width}
    )
}

render_diagnostic_plot__normal_qq <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$model, which=2) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_normal_qq_width}
    )
}

render_diagnostic_plot__scale_location <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$model, which=3) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_scale_location_width}
    )
}

render_diagnostic_plot__cooks_distance <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$model, which=4) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_cooks_distance_width}
    )
}

render_diagnostic_plot__residuals_vs_leverage <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$model, which=5) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_residuals_vs_leverage_width}
    )
}

render_diagnostic_plot__cooks_distance_vs_leverage <- function(input, session, dataset, regression__results) {

    render_diagnostic_plot(
        regression__results,
        graph_function=function() { plot(regression__results()$model, which=6) },
        graph_width_function=function() {0.55 * session$clientData$output_regression__diagnostic_cooks_distance_vs_leverage_width}
    )
}
