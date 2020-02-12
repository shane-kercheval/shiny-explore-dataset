##############################################################################################################
# NUMERIC SUMMARY - calculate the numeric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__numeric_summary__creator <- function(dataset) {

    reactive({

        # typically I would do the progress in the while rendering the UI, but this is used while updating
        # the summary options and i'm not sure which will be called first
        withProgress(value=1/2, message='Calculating Numeric Summary',{

            log_message_block_start('Calculating Numeric Summary')
            return (rt_explore_numeric_summary(dataset=dataset$data))
        })
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__numeric_summary__options__UI <- function(numeric_summary) {
    renderUI({

        option_values <- colnames(numeric_summary())

        option_values <- option_values[option_values != 'feature']
        checkboxGroupInput(inputId='numeric_summary__options',
                           label='Summary Options',
                           choices=option_values,
                           selected=c('nulls', 'perc_nulls', 'perc_zeros', 'mean', 'coef_of_var', 'skewness',
                                      'min', 'percentile_50', 'max'),
                           inline=FALSE,
                           width=NULL)
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderDataTable__numeric_summary__table <- function(input, numeric_summary) {

    DT::renderDataTable({

        local_summary <- numeric_summary()
        local_options <- input$numeric_summary__options

        return (local_summary %>% rt_select_all_of(c('feature', local_options)))
    })
}
