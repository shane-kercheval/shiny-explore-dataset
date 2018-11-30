##############################################################################################################
# calculate the categoric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__categoric_summary <- function(dataset) {

    reactive({

        withProgress(value=1/2, message='Calculating Categoric Summary',{

            log_message_block_start('Calculating Categoric Summary')
            return (rt_explore_categoric_summary(dataset=dataset()))
        })
    })
}

renderDataTable__categoric_summary__table <- function(categoric_summary) {

    renderDataTable({
        
        return (categoric_summary())
    })
}

renderPrint__categoric_summary__text <- function(dataset, categoric_summary) {

    renderPrint({
        
        # get R's summary of the categoric data
        return (summary(dataset()[, as.character(categoric_summary()$feature)]))
    })
}
