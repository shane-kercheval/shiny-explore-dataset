##########################################################################################################
# main dataset
# initialize with small default dataset or upload from file, by user
##########################################################################################################
reactive__dataset <- function(input, output, session) {
    reactive({

        req(input$selected_preloaded_dataset)

        # reactive data
        upload_file_path <- input$uploadFile$datapath
        local_selected_preloaded_dataset <- input$selected_preloaded_dataset

        log_message_block_start('Loading Dataset')
        log_message_variable('upload_file_path', upload_file_path)
        log_message_variable('selected_preloaded_dataset', local_selected_preloaded_dataset)

        if(is.null(upload_file_path)) {
            
            if(local_selected_preloaded_dataset == 'Credit') {

                dataset_or_null('example_datasets/credit.csv')

            } else if(local_selected_preloaded_dataset == 'Housing') {

                dataset_or_null('example_datasets/housing.csv')

            } else if(local_selected_preloaded_dataset == 'Insurance') {

                dataset_or_null('example_datasets/insurance.csv')

            } else if(local_selected_preloaded_dataset == 'Iris') {

                return (data.frame(iris))

            } else if(local_selected_preloaded_dataset == 'Diamonds') {

                return (data.frame(diamonds))

            } else if(local_selected_preloaded_dataset == 'Flights') {

                return (
                    data.frame(nycflights13::flights %>%
                        mutate(date = make_date(year, month, day)) %>%
                        select(-year, -month, -day) %>%
                        select(date, everything())))

            } else if(local_selected_preloaded_dataset == 'Gapminder') {

                return (data.frame(gapminder::gapminder))

            } else {

                return (NULL)
            }
        } else {

            if(str_sub(upload_file_path, -4) == '.csv') {
                
                read.csv(upload_file_path, header=TRUE)

            } else if(str_sub(upload_file_path, -4) == '.RDS') {
            
                readRDS(file=upload_file_path)

            } else {

                showModal(
                    modalDialog(title = 'Unknown File Type',
                                'Only `.csv` and `.RDS` files are supported at this time.'))
                NULL
            }
        }
    })
}


##############################################################################################################
# calculate the numeric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__numeric_summary <- function(input, output, session, dataset) {

    reactive({

        # typically I would do the progress in the while rendering the UI, but this is used while updating
        # the summary options and i'm not sure which will be called first
        withProgress(value=1/2, message='Calculating Numeric Summary',{

            log_message_block_start('Calculating Numeric Summary')
            rt_explore_numeric_summary(dataset=dataset())
        })
    })
}

##############################################################################################################
# calculate the categoric summary; it is an expensive operation for large datasets
##############################################################################################################
reactive__categoric_summary <- function(input, output, session, dataset) {

    reactive({

        withProgress(value=1/2, message='Calculating Categoric Summary',{

            log_message_block_start('Calculating Categoric Summary')
            rt_explore_categoric_summary(dataset=dataset())
        })
    })
}

##########################################################################################################
# Run Regression when user clicks Run button
##########################################################################################################    
eventReactive__regression_results <- function(input, output, session, dataset) {

    eventReactive(input$regression_run_button, {

        if(input$regression_selected_dependent_variable == select_variable) {
            return (NULL)
        }

        local_interaction_term1 <- input$regression_selected_interaction_term1
        local_interaction_term2 <- input$regression_selected_interaction_term2

        withProgress(value=1/2, message='Running Regression',{

            interaction_variables <- NULL

            if(!is.null(local_interaction_term1) && local_interaction_term1 != select_variable &&
               !is.null(local_interaction_term2) && local_interaction_term2 != select_variable) {

                interaction_variables <- list(c(local_interaction_term1,
                                                local_interaction_term2))
            }

            # updates to reactive variables will not trigger an update here, only regression_run_button
            results <- easy_regression(dataset=dataset(),
                                       dependent_variable=input$regression_selected_dependent_variable,
                                       independent_variables=input$regression_selected_independent_variables,
                                       # list of vectors, each element in the list is a pair of interaction terms
                                       # only supporting two interaction variables at the moment
                                       interaction_variables=interaction_variables)

            shinyjs::show('regression_formula_header')
            shinyjs::show('regression_summary_header_UI')
            shinyjs::show('regression_vif_header')
            
            return (results)
        })
    })
}
