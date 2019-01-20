##########################################################################################################
# MAIN DATASET
# initialize with small default dataset or upload from file, by user
##########################################################################################################
reactive__source_data__creator <- function(input, custom_triggers) {
    reactive({

        withProgress(value=1/2, message='Loading Data',{

            req(input$preloaded_dataset)

            input$load_data__url_csv_button  # reload data if this button is pressed
            custom_triggers$reload_source_data  # update based on changes to this reactiveValue

            # reactive data
            upload_file_path <- input$uploadFile$datapath
            local_preloaded_dataset <- input$preloaded_dataset
            local_add_date_column <- isolate(input$source_data__add_date_fields)
            local_csv_url <- isolate(input$load_data__url_csv)

            log_message_block_start('Loading Dataset')
            log_message_variable('input$uploadFile$datapath', upload_file_path)
            log_message_variable('input$load_data__url_csv', local_csv_url)
            log_message_variable('input$preloaded_dataset', local_preloaded_dataset)
            log_message_variable('input$source_data__add_date_fields', local_add_date_column)

            loaded_dataset <- NULL

            if(!is.null(local_csv_url) && local_csv_url != "") {

                loaded_dataset <- read_csv(local_csv_url)

            } else if(is.null(upload_file_path)) {
                
                if(local_preloaded_dataset == 'Credit') {

                    loaded_dataset <- dataset_or_null('example_datasets/credit.csv')

                } else if(local_preloaded_dataset == 'Housing') {

                    loaded_dataset <- dataset_or_null('example_datasets/housing.csv')

                } else if(local_preloaded_dataset == 'Insurance') {

                    loaded_dataset <- dataset_or_null('example_datasets/insurance.csv')

                } else if(local_preloaded_dataset == 'Iris') {

                    loaded_dataset <- data.frame(iris)

                } else if(local_preloaded_dataset == 'Diamonds') {

                    loaded_dataset <- data.frame(diamonds)

                } else if(local_preloaded_dataset == 'Flights') {

                    loaded_dataset <-
                        data.frame(nycflights13::flights %>%
                            mutate(date = make_date(year, month, day)) %>%
                            select(-year, -month, -day) %>%
                            select(date, everything()))

                } else if(local_preloaded_dataset == 'Gapminder') {

                    loaded_dataset <- data.frame(gapminder::gapminder)
                }
            } else {

                if(str_sub(upload_file_path, -4) == '.csv') {
                    
                    loaded_dataset <- read.csv(upload_file_path, header=TRUE)

                } else if(str_sub(upload_file_path, -4) == '.RDS') {
                
                    loaded_dataset <- readRDS(file=upload_file_path)

                } else {

                    showModal(
                        modalDialog(title = 'Unknown File Type',
                                    'Only `.csv` and `.RDS` files are supported at this time.'))
                }
            }

            if(!is.null(loaded_dataset) &&
                    !is.null(local_add_date_column) &&
                    local_add_date_column != select_variable_optional &&
                    local_add_date_column %in% colnames(loaded_dataset)) {

                log_message('Adding date fields...')

                if(is.numeric(loaded_dataset[, local_add_date_column])) {

                    log_message('Adding date fields...numeric')
                    is_invalid <- TRUE

                } else {

                    is_invalid <- tryCatch({

                        log_message('Adding date fields...tryCatch()')
                        (all(is.na(lubridate::as_date(loaded_dataset[, local_add_date_column]))))

                    }, error = function(error_condition) {

                        log_message(paste('Adding date fields...ERROR CONVERTING', local_add_date_column, 'TO DATE'))                    
                        TRUE
                    })
                }

                log_message_variable('Adding date fields...is_invalid', is_invalid)

                if(is_invalid) {

                    showModal(
                        modalDialog(title = 'Cannot Convert to Date',
                                    paste0('Cannot convert `', local_add_date_column, '` to a date.')))
                    
                } else {

                    loaded_dataset <- cbind(loaded_dataset,
                                    rt_get_date_fields(date_vector = loaded_dataset[, local_add_date_column]))
                }
            }
        })

        return (loaded_dataset)
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__source_data__add_date_fields__UI <- function(dataset) {
    renderUI({

        selectInput(inputId='source_data__add_date_fields',
                    label = 'Add Date Fields based on Date Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

##############################################################################################################
# OUTPUT
##############################################################################################################
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
