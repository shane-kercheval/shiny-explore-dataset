renderUI__numeric_summary_options_UI <- function(numeric_summary_data) {
    renderUI({

        option_values <- colnames(numeric_summary_data())

        option_values <- option_values[option_values != 'feature']
        checkboxGroupInput(inputId='numeric_summary_options',
                           label='Summary Options',
                           choices=option_values,
                           selected=c('perc_nulls', 'perc_zeros', 'mean', 'coef_of_var', 'skewness', 'min', 
                                      'percentile_50', 'max'),
                           inline=FALSE,
                           width=NULL)
    })
}

renderUI__variable_plots_variable_UI <- function(dataset) {

    renderUI({
        selectInput(inputId='variable_plots_variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_comparison_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_comparison',
                    label = 'Comparison Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_sum_by_variable_UI <- function(numeric_summary_data) {

    renderUI({

        selectInput(inputId='variable_plots_sum_by_variable',
                    label = 'Sum By Variable',
                    choices = c(select_variable_optional, as.character(numeric_summary_data()$feature)),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_color_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_color',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_size_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
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
