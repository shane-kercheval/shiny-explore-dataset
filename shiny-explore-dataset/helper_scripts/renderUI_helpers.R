renderUI_selected_numeric_summary_options_UI <- function(numeric_summary_data) {
    renderUI({

        # reactive data
        option_values <- colnames(numeric_summary_data())

        option_values <- option_values[option_values != 'feature']
        checkboxGroupInput(inputId='selected_numeric_summary_options',
                           label='Summary Options',
                           choices=option_values,
                           selected=c('perc_nulls', 'perc_zeros', 'mean', 'coef_of_var', 'skewness', 'min', 
                                      'percentile_50', 'max'),
                           inline=FALSE,
                           width=NULL)
    })
}

renderUI_selected_variable_plot_variable_UI <- function(dataset) {

    renderUI({
        selectInput(inputId='selected_variable_plot_variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI_selected_variable_plot_comparison_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='selected_variable_plot_comparison',
                    label = 'Comparison Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI_selected_variable_plot_point_color_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='selected_variable_plot_point_color',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI_selected_variable_plot_point_size_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='selected_variable_plot_point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}
