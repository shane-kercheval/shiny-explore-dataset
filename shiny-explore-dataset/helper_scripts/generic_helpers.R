dataset_or_null <- function(file) {
    # loads the file if it exists, otherwise returns NULL.    

    if(file.exists(file)) {

        return (read.csv(file, header=TRUE))

    } else {

        return (NULL)
    }
}

is_date_type <- function(x) {
    return (is.Date(x) || is.POSIXct(x) || is.POSIXlt(x))
}

null_if_select_variable_optional <- function(value) {

    if(is.null(value) || value == select_variable_optional) {

        value <- NULL
    }

    return (value)
}

custom_filter <- function(dataset, factor_lump_number=NULL) {

    if(!is.na(factor_lump_number)) {

        dataset <- dataset %>%
            mutate_if(is.character, as.factor) %>%
            mutate_if(is.factor, ~fct_lump(.x, n=factor_lump_number))
    }

    return (dataset)
}

easy_regression <- function(dataset,
                            dependent_variable,
                            independent_variables,
                            interaction_variables=NULL,
                            polynomial=NULL) {

    if(is.null(interaction_variables)) {
        
        interaction_variables_formula <- ''
    
    } else {

        interaction_variables_formula <- paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')),
                                                   collapse = ' + ')
    }

    if(is.null(independent_variables) || length(independent_variables) == 0) {

        independent_variables_formula <- interaction_variables_formula

    } else if(is.null(interaction_variables) || length(interaction_variables) == 0) {

        independent_variables_formula <- paste(independent_variables, collapse =' + ')

    } else {
        
        independent_variables_formula <- paste(interaction_variables_formula,
                                               '+',
                                               paste(independent_variables, collapse =' + '))
    }

    formula <- paste(dependent_variable, '~', independent_variables_formula)
    
    if(is.numeric(dataset[, dependent_variable])) {

        type <- 'Linear Regression'
        result <- lm(formula, data=dataset, na.action = na.exclude)
        reference <- NULL
        
    } else {
        
        if(length(unique(dataset[, dependent_variable])) == 2) {
         
            type <- 'Logistic Regression'
            result <- glm(formula, data=dataset, na.action = na.exclude, family=binomial)
            reference <- rownames(contrasts(dataset[, dependent_variable]))[2]

        } else {
            
            return (NULL)
        }
    }

    return (
        list(rows_excluded=which(!complete.cases(dataset[, independent_variables])),
             type=type,
             formula=formula,
             results=result,
             reference=reference)
    )
}

# dataset <- read.csv("example_datasets/housing.csv", header=TRUE)
# dependent_variable <- 'median_house_value'
# independent_variables <- c('longitude', 'latitude', 'housing_median_age', 'total_rooms', 'total_bedrooms', 'population', 'households', 'median_income', 'ocean_proximity')

# dataset <- read.csv("example_datasets/credit.csv", header=TRUE)
# dependent_variable <- 'default'
# independent_variables <- colnames(dataset)[1:16]
# 
# results <- easy_regression(dataset, dependent_variable, independent_variables)
# summary(results$results)
# names(results$results)
# plot(results$results, which=c(1, 2, 3, 4, 5, 6))
# plot(results$results, which=c(1, 2, 3, 4))

# library(lattice)
# xyplot(dataset[, dependent_variable] ~ predict(results$results),
#        type=c('p', 'g'),
#        xlab='Predicted', ylab='Actual')
# 
# 
# library(lattice)
# xyplot(predict(results$results) ~ 1:nrow(dataset),
#        type=c('p', 'g'),
#        xlab='Observation Number', ylab='Predicted')
# 
# predict(results$results, type='response')
# contrasts(dataset[, dependent_variable])
# coefficients(results$results)
# summary(lm(median_house_value ~ housing_median_age * total_rooms + housing_median_age + total_rooms, data=dataset))
# # 
# #interaction_variables <- list(c('housing_median_age', 'total_rooms'), c('total_rooms', 'housing_median_age'))
# interaction_variables <- list(c('housing_median_age', 'total_rooms'))
# paste(' ', paste(map_chr(interaction_variables, ~ paste(., collapse =' * ')), collapse = ' + '), '+ ')


capture_messages_warnings <- function(func) {
    
    messages <- list()
    withCallingHandlers(
        warning = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        message = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        func()
    )
    return (paste0(messages, collapse = '\n'))
}
