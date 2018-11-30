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
