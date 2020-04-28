library(hms)
library(tidyverse)
library(dplyr)

##########################################################################################################
# MAIN DATASET
# initialize with small default dataset or upload from file, by user
##########################################################################################################
observeEvent__source_data__upload <- function(session, input, output, reactive__source_data) {
    observeEvent(input$uploadFile, {

        withProgress(value=1/2, message='Loading Data',{

            upload_file_path <- input$uploadFile$datapath
            log_message_variable('input$uploadFile$datapath', upload_file_path)

            if(!is.null(upload_file_path)) {

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
            shinyjs::hide('load_data__description')
        })
        reactive__source_data$data <- loaded_dataset
        reactive__source_data$source <- "File Upload"
    })
}

observeEvent__source_data__csv_url <- function(session, input, output, reactive__source_data) {
    observeEvent(input$load_data__url_csv_button, {
        
        loaded_dataset <- NULL
        withProgress(value=1/2, message='Loading Data',{

            local_csv_url <- isolate(input$load_data__url_csv)
            log_message_variable('input$load_data__url_csv', local_csv_url)

            if(!is.null(local_csv_url) && local_csv_url != "") {

                loaded_dataset <- as.data.frame(read_csv(local_csv_url)) %>% mutate_if(is.character, factor)
            }
            shinyjs::hide('load_data__description')
        })
        updateTextInput(session, inputId='load_data__url_csv', value = '')
        reactive__source_data$data <- loaded_dataset
        reactive__source_data$source <- "URL (csv)"
    })
}

select_preloaded_dataset <- function(dataset_name, defualt_path='') {

    log_message_variable('Dataset Name', dataset_name)

    loaded_dataset <- NULL
    data_description <- NULL

    if(dataset_name == "Jane Austen") {
        
        library(janeaustenr)
        loaded_dataset <- austen_books() %>%
            group_by(book) %>%
            mutate(linenumber = row_number(),
                   chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                           ignore_case = TRUE)))) %>%
            ungroup() %>%
            mutate(text = str_remove_all(text, "_"))

    } else if(dataset_name == 'Credit') {

        loaded_dataset <- dataset_or_null(paste0(defualt_path, 'example_datasets/credit.csv')) %>%
            mutate(default = ifelse(default == 'yes', TRUE, FALSE),
                   purpose = ifelse(as.character(purpose) == 'car0', 'car', as.character(purpose)),
                   checking_balance = factor(checking_balance,
                                             levels = c("< 0 DM", "1 - 200 DM", "> 200 DM", "unknown"),
                                             ordered = TRUE),
                   credit_history = factor(credit_history,
                                           levels = c("critical", "poor", "good", "very good", "perfect"),
                                           ordered = TRUE),
                   savings_balance = factor(savings_balance,
                                            levels = c("< 100 DM", "100 - 500 DM", "500 - 1000 DM",
                                                       "> 1000 DM", "unknown"),
                                            ordered = TRUE),
                   employment_duration = factor(employment_duration,
                                             levels = c("unemployed", "< 1 year", "1 - 4 years",
                                                        "4 - 7 years", "> 7 years"),
                                             ordered = TRUE))

        data_description <- "This is where a description of the Credit dataset should be given."

    } else if(dataset_name == 'Mock Conversions') {

        loaded_dataset <- dataset_or_null_RDS(paste0(defualt_path, 'example_datasets/conversions.RDS'))
        data_description <- "Example dataset of conversion events."

    } else if(dataset_name == 'Housing') {

        loaded_dataset <- dataset_or_null(paste0(defualt_path, 'example_datasets/housing.csv'))
        data_description <- "This is where a description of the Housing dataset should be given."

    } else if(dataset_name == 'Insurance') {

        loaded_dataset <- dataset_or_null(paste0(defualt_path, 'example_datasets/insurance.csv'))
        data_description <- "This is where a description of the Insurance dataset should be given."

    } else if(dataset_name == 'Iris') {

        loaded_dataset <- data.frame(iris)
        data_description <- "This is where a description of the Insurance dataset should be given."

    } else if(dataset_name == 'Diamonds') {

        loaded_dataset <- data.frame(diamonds)
        loaded_dataset[1:500, 'cut'] <- NA
        data_description <- "This is where a description of the Insurance dataset should be given."

    } else if(dataset_name == 'Flights') {

        loaded_dataset <-
            data.frame(nycflights13::flights %>%
                #mutate(date = make_date(year, month, day)) %>%
                rename(takeoff_datetime = time_hour) %>%
                mutate(land_datetime = takeoff_datetime + minutes(air_time)) %>%
                select(-year, -month, -day) %>%
                select(takeoff_datetime, land_datetime, everything()))

        loaded_dataset$hms <- as_hms(with_tz(loaded_dataset$takeoff_datetime, tzone='UTC'))
        data_description <- "This is where a description of the Flights dataset should be given."

    } else if(dataset_name == 'Wine Ratings') {

        # originally from 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv'
        loaded_dataset <- readRDS('example_datasets/wine_ratings.RDS') %>%
            as.data.frame() %>%
            extract(title, 'year', '([20]\\d\\d\\d)', convert=TRUE, remove=FALSE) %>%
            mutate(year = ifelse(year < 1900, NA, year),
                   points_per_price = points / price)
        
        us_red_varieties <- c(
            
            'Cabernet Sauvignon',
            'Pinot Noir',
            'Syrah',
            'Red Blend',
            'Merlot',
            'Bordeaux-style Red Blend',
            'Petite Sirah',
            'Rhône-style Red Blend',
            'Cabernet Franc',
            'Grenache',
            'Malbec',
            'Sangiovese',
            'Tempranillo',
            'Meritage',
            'Mourvèdre',
            'Petit Verdot',
            'G-S-M',
            'Cabernet Sauvignon-Syrah',
            'Shiraz',
            'Carmenère',
            'Syrah-Grenache',
            'Syrah-Cabernet Sauvignon',
            'Cabernet Sauvignon-Merlot',
            'Grenache-Syrah',
            'Cabernet Blend',
            'Petite Verdot',
            'Syrah-Petite Sirah',
            'Merlot-Cabernet Sauvignon',
            'Cabernet Sauvignon-Cabernet Franc',
            'Merlot-Cabernet Franc',
            'Cabernet Franc-Merlot',
            'Syrah-Mourvèdre',
            'Syrah-Viognier',
            'Cabernet Merlot',
            'Cabernet Sauvignon-Sangiovese',
            'Syrah-Tempranillo',
            'Tempranillo Blend',
            'Grenache-Carignan',
            'Grenache-Mourvèdre',
            'Merlot-Cabernet',
            'Sangiovese-Cabernet Sauvignon',
            'Sangiovese-Syrah',
            'Syrah-Cabernet',
            'Cabernet-Syrah',
            'Grenache Blend',
            'Sangiovese Cabernet',
            'Syrah-Cabernet Franc',
            'Cabernet Sauvignon-Malbec',
            'Malbec-Merlot',
            'Carignan-Grenache',
            'Malbec-Syrah',
            'Mourvèdre-Syrah',
            'Tempranillo-Cabernet Sauvignon',
            'Cabernet',
            'Cabernet Sauvignon-Barbera',
            'Cabernet Sauvignon-Carmenère',
            'Cabernet Sauvignon-Shiraz',
            'Cabernet Sauvignon-Tempranillo',
            'Cabernet-Shiraz',
            'Malbec-Cabernet Sauvignon',
            'Merlot-Malbec',
            'Rosado',
            'Syrah-Grenache-Viognier',
            'Syrah-Merlot',
            'Syrah-Petit Verdot',
            'Tannat-Syrah'
        )

        loaded_dataset$type <- map_chr(loaded_dataset$variety, ~ ifelse(. %in% us_red_varieties, 'Red', 'White/Other'))
        loaded_dataset$type <- ifelse(loaded_dataset$country == "US", loaded_dataset$type, NA)
        
        loaded_dataset <- loaded_dataset %>%
            select(-X1) %>%
            select(country, province, variety, type, year, points, price, points_per_price, title, winery, everything())

        data_description <- "This was a Tidy Tuesday dataset.\n\nhttps://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-28\n\nThe `year` is extracted from the title, and may not be 100% accurate.\nThe `type` (Red/White) is manually constructed from the varity and is currently only available for the US."

    } else if(dataset_name == 'Gapminder') {

        loaded_dataset <- data.frame(gapminder::gapminder)
    }

    colnames(loaded_dataset) <- rt_pretty_text(colnames(loaded_dataset))
    return (list(dataset=loaded_dataset, description=data_description))
}

observeEvent__source_data__preloaded <- function(session, input, output, reactive__source_data, url_parameter_info) {
    observeEvent(input$preloaded_dataset, {

        req(!isolate(url_parameter_info$currently_updating))  # never update from input when updating from url

        withProgress(value=1/2, message='Loading Data',{

            log_message_block_start("Loading Dataset from Selection")

            results <- select_preloaded_dataset(dataset_name=input$preloaded_dataset)
            reactive__source_data$data <- results$dataset
            reactive__source_data$source <- "preloaded"

            if(is.null(results$description)) {

                shinyjs::hide('load_data__description')

            } else {

                output$load_data__description <- renderText({ results$description })
                shinyjs::show('load_data__description')
            }

        })

    }, suspended=TRUE)
}

apply_r_code_to_dataset <- function(dataset, r_code_string) {

    error_message <- NULL
    if(!is.null(r_code_string) && r_code_string != "") {

        # if successful, return NULL; otherwise an error/warning message will be returned
        error_message <- tryCatch(
            {
                eval(parse(text=r_code_string))

                # if successful, return NULL; otherwise an error/warning message will be returned
                NULL
            },
            error=function(cond) { return(cond) },
            warning=function(cond) { return(cond) })

        
        if(!is.null(error_message)) {

            error_message <- str_trim(as.character(error_message))
            error_message <- str_replace(error_message, fixed("Error in eval(parse(text = r_code_string)): "), "")
        }
    }

    return (list(dataset=dataset, error_message=error_message))
}

observeEvent__load_data__r_code_apply <- function(reactive__source_data, input, output) {
    # update dataset based on R code
    # the string `load_data__r_code_text` should contain code which manipulates `dataset` which is then
    # assigned into the master dataset
    observeEvent(input$load_data__r_code_apply, {

        dataset  <- reactive__source_data$data

        if(!is.null(input$load_data__r_code_text) && input$load_data__r_code_text != "") {

            withProgress(value=1/2, message="Running R Code", {

                log_message_variable("R Code", input$load_data__r_code_text)

                results <- dataset %>% apply_r_code_to_dataset(r_code_string = input$load_data__r_code_text)
                dataset <- results$dataset
                r_code_error_message <- results$error_message

                # if successful, function returns NULL error message;
                # otherwise an error/warning message will be returned
                if(is.null(r_code_error_message)) {
                    
                    shinyjs::hide('load_data__r_code_error')

                } else {

                    log_message_variable('R Code Error', r_code_error_message)
                    output$load_data__r_code_error <- renderText({ r_code_error_message })
                    shinyjs::show('load_data__r_code_error')
                }
            })
        }
        reactive__source_data$data <- dataset
        reactive__source_data$source <- "Custom R Code"
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__source_data__add_date_fields__UI <- function(dataset) {
    renderUI({
        req(dataset$data)
        selectInput(inputId='source_data__add_date_fields',
                    label = 'Add Date Fields based on Date Variable',
                    choices = c(global__select_variable_optional, colnames(dataset$data)),
                    selected = global__select_variable_optional,
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

    DT::renderDataTable({

        return (head(dataset$data, 500))
    })
}

renderDataTable__source_data__types <- function(dataset) {

    DT::renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            local_dataset <- dataset$data
            types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])
            return (data.frame(variable=colnames(local_dataset), type=types))
        })
    })
}
