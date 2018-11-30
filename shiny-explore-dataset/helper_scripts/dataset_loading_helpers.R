##########################################################################################################
# main dataset
# initialize with small default dataset or upload from file, by user
##########################################################################################################
reactive__source_data__creator <- function(input) {
    reactive({

        req(input$preloaded_dataset)

        # reactive data
        upload_file_path <- input$uploadFile$datapath
        local_preloaded_dataset <- input$preloaded_dataset

        log_message_block_start('Loading Dataset')
        log_message_variable('upload_file_path', upload_file_path)
        log_message_variable('preloaded_dataset', local_preloaded_dataset)

        if(is.null(upload_file_path)) {
            
            if(local_preloaded_dataset == 'Credit') {

                return (dataset_or_null('example_datasets/credit.csv'))

            } else if(local_preloaded_dataset == 'Housing') {

                return (dataset_or_null('example_datasets/housing.csv'))

            } else if(local_preloaded_dataset == 'Insurance') {

                return (dataset_or_null('example_datasets/insurance.csv'))

            } else if(local_preloaded_dataset == 'Iris') {

                return (data.frame(iris))

            } else if(local_preloaded_dataset == 'Diamonds') {

                return (data.frame(diamonds))

            } else if(local_preloaded_dataset == 'Flights') {

                return (
                    data.frame(nycflights13::flights %>%
                        mutate(date = make_date(year, month, day)) %>%
                        select(-year, -month, -day) %>%
                        select(date, everything())))

            } else if(local_preloaded_dataset == 'Gapminder') {

                return (data.frame(gapminder::gapminder))

            } else {

                return (NULL)
            }
        } else {

            if(str_sub(upload_file_path, -4) == '.csv') {
                
                return (read.csv(upload_file_path, header=TRUE))

            } else if(str_sub(upload_file_path, -4) == '.RDS') {
            
                return (readRDS(file=upload_file_path))

            } else {

                showModal(
                    modalDialog(title = 'Unknown File Type',
                                'Only `.csv` and `.RDS` files are supported at this time.'))
                return (NULL)
            }
        }
    })
}

renderDataTable__source_data__head <- function(dataset) {

    renderDataTable({

        return (head(dataset(), 500))
    })
}

renderDataTable__source_data__types <- function(dataset) {

    renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            local_dataset <- dataset()
            types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])
            return (data.frame(variable=colnames(local_dataset), type=types))
        })
    })
}
