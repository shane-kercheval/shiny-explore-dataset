renderDataTable__numeric_summary_table <- function(input, numeric_summary_data) {

    renderDataTable({

        local_numeric_summary <- numeric_summary_data()
        local_numeric_options <- input$numeric_summary_options
        return (local_numeric_summary[, c('feature', local_numeric_options)])
    })
}

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
