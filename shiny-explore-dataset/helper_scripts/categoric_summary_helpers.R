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
