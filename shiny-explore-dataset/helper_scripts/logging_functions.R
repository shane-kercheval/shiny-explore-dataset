log_message <- function(message) {

    if(should_log_message) {

        cat(paste0('\n', message))
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
