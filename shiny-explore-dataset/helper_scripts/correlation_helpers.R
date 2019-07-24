##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__correlation__plot <- function(input, session, dataset) {

    renderPlot({

        withProgress(value=1/2, message='Calculating Correlations', {

            local_dataset <- dataset$data

            if(input$correlation__pretty_text) {

                local_dataset <- rt_pretty_dataset(local_dataset)
            }

            local_correlation__max_missing_column_perc <- input$correlation__max_missing_column_perc / 100
            log_message_block_start('Calculating Correlations & Creating Plot')
            log_message_variable('correlation__max_missing_column_perc', local_correlation__max_missing_column_perc)
            log_message_variable('correlation__corr_threshold', input$correlation__corr_threshold)
            log_message_variable('correlation__p_value_threshold', input$correlation__p_value_threshold)
            log_message_variable('correlation__base_size', input$correlation__base_size)
            log_message_variable('correlation__pretty_text', input$correlation__pretty_text)

            # see note about why I use print, in `variable plot` section below.
            return (
                print(rt_explore_plot_correlations(dataset=local_dataset,
                                                   max_missing_column_perc=local_correlation__max_missing_column_perc,
                                                   corr_threshold=input$correlation__corr_threshold,
                                                   p_value_threshold=input$correlation__p_value_threshold,
                                                   base_size=input$correlation__base_size,
                                                   type='pearson'))
            )
        })
    }, height = function() {

        session$clientData$output_correlation__plot_width * 0.66  # set height to % of width
    })
}
