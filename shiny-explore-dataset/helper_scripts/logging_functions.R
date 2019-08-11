log_message <- function(message) {

    # technically we don't need global__should_log_message (we could just set global__log_message_to teo 
    # empty vector; but it might speed things up on the server if we don't do multiple checks, not sure
    # if it's really a big deal, but...)
    if(global__should_log_message) {

        if('local_console' %in% global__log_message_to) {

            cat(paste0('\n', message))
        }
        if('browser_console' %in% global__log_message_to) {

            shinyjs::logjs(paste0('\n', message))

        }
    }
}

log_message_variable <- function(variable_name, variable_value) {

    log_message(paste0(variable_name, ': `', variable_value, '`'))
}

log_message_generic <- function(message, description) {

    log_message(paste0(message, ': ', description))
}

log_message_block_start <- function(message) {

    log_message(paste0('\n\n#############################################\n',
                       message,
                       '\n#############################################\n'))
}
