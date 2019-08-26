library(scales)
library(purrr)
library(stringr)
##############################################################################################################
# FILTERS
##############################################################################################################

#' get all of the filter values from the dynamic filters without triggering refresh for the first time
#' @param columns the names of the columns/filters of interest
get_dynamic_filter_values <- function(input, columns) {

    selections_list <- purrr::map(columns, ~ isolate(input[[paste0('var_plots__dynamic_filter__', .)]]))
    names(selections_list) <- columns

    return (selections_list)
}

#' builds controls based on the type of variables in the dataset
reactive__filter_controls_list__creator <- function(input, dataset) {

    reactive({

        req(dataset$data)
        input$var_plots__filter_clear

        withProgress(value=1/2, message="Generating Filters", {

            log_message("Generating Filter Controls")
            
            ui_list <- imap(dataset$data, ~ {
                log_message_variable('filter control for', .y)
                input_id <- paste0('var_plots__dynamic_filter__', .y)
                filter_object <- NULL
                if(is_date_type(.x)) {
                    #'date'
                    min_index <- which.min(.x)
                    max_index <- which.max(.x)
                    min_value <- .x[min_index]
                    max_value <- .x[max_index]
                    
                    filter_object <- dateRangeInput(inputId=input_id,
                                                    label=.y,
                                                    start=min_value,
                                                    end=max_value,
                                                    width='100%')
                } else if(is.factor(.x)) {

                    if(length(levels(.x)) <= 1500) {
                        choices <- levels(.x)

                        if(any(is.na(.x))) {

                            choices <- c("<Missing Values (NA)>", choices)
                        }

                        filter_object <- selectInput(inputId=input_id,
                                                     label=.y,
                                                     choices=choices,
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     width='100%')
                    }
                } else if(is.character(.x)) {
                    
                    if(length(unique(.x)) <= 1500) {

                        choices <- as.character((as.data.frame(table(as.character(.x))) %>%
                                                     arrange(desc(Freq)))$Var1)

                        if(any(is.na(.x))) {

                            choices <- c("<Missing Values (NA)>", choices)
                        }

                        filter_object <- selectInput(inputId=input_id,
                                                     label=.y,
                                                     choices=choices,
                                                     selected = NULL,
                                                     multiple = TRUE,
                                                     width='100%')
                    }
                } else if(is.numeric(.x)) {

                    min_value <- min(.x, na.rm = TRUE)
                    max_value <- max(.x, na.rm = TRUE)
                    filter_object <- sliderInput(inputId=input_id,
                                                 label=.y,
                                                 min=min_value,
                                                 max=max_value,
                                                 value=c(min_value, max_value),
                                                 width='100%')
                } else if(is.logical(.x)) {

                    filter_object <- selectInput(inputId=input_id,
                                label=.y,
                                choices=c(TRUE, FALSE),
                                selected = NULL,
                                multiple = TRUE,
                                width='100%')

                } else if("hms" %in% class(.x)) {

                    # create slider options (hours in 15 min intervals, 24:00 means <= 23:59:59)
                    hours <- str_pad(seq(0, 23), 2, side='left', pad=0)
                    minutes <- str_pad(seq(0, 45, 15), 2, side='left', pad=0)
                    hours_minutes <- sort(apply(expand.grid(hours, minutes), 1, paste, collapse=":"))
                    hours_minutes <- c(hours_minutes, "24:00")

                    filter_object <- sliderTextInput(inputId=input_id,
                                                     label=.y,
                                                     choices=hours_minutes,
                                                     selected=c("00:00", "24:00"),
                                                     grid=FALSE,
                                                     width='100%')
                } else {
                    #class(.)[1]
                    stopifnot(FALSE)
                }

                if(!is.null(filter_object)) {

                    filter_object <- shinyjs::hidden(filter_object)
                }

                return (filter_object)
            })
        })
        ui_list
    })
}

observeEvent__var_plots__custom_labels_apply <- function(input, session) {

    observeEvent(input$var_plots__custom_labels_apply, ({

        log_message_block_start('Applying Custom Graph Labels')
        updateCollapse(session, "var_plots__bscollapse", style = list("Other Options" = "default"))
    }))
}

observeEvent__var_plots__custom_labels_clear <- function(input, session) {

    observeEvent(input$var_plots__custom_labels_clear, ({

        log_message_block_start('Clearing Custom Graph Labels')
        updateCollapse(session, "var_plots__bscollapse", style = list("Other Options" = "danger"))
        helper__restore_defaults_other_options(session)
    }))
}

observeEvent__var_plots__graph_options_apply <- function(input, session) {

    observeEvent(input$var_plots__graph_options_apply, {

        updateCollapse(session, "var_plots__bscollapse", style = list("Graph Options" = "default"))
    })
}

observeEvent__var_plots__graph_options_clear <- function(input, session) {

    observeEvent(input$var_plots__graph_options_clear, {

        log_message_block_start("Graph Options Dirty (Cleared Controls)")
        updateCollapse(session, "var_plots__bscollapse", style = list("Graph Options" = "danger"))
        helper__restore_defaults_graph_options(session)
    })
}

#' This gets the default value from var_plots__default_values
#' But if the default value is NULL it returns character(0) and if the default is NA it returns integer(0)
#' which the updateXXXInput controls rely on to reset
get_default_value_for_updating <- function(variable_name) {

    log_message_variable('getting default value', variable_name)

    stopifnot(variable_name %in% names(var_plots__default_values))

    value <- var_plots__default_values[[variable_name]]

    if(is.null(value)) {
        
        return (character(0))

    } else if (is.na(value)) {
        
        return (integer(0))

    } else {
        
        return (value)
    }
}

reset_hide_var_plot_option <- function(session, option_name, hide_option=TRUE) {

    log_message_variable('reseting', option_name)

    stopifnot(option_name %in% names(var_plots__default_values) && 
              option_name %in% names(var_plots__variable_types))

    if(hide_option) {

        shinyjs::hide(option_name)

        log_message_variable('hidding', option_name)
    }
    
    if(var_plots__variable_types[[option_name]] == 'updateSelectInput') {

        updateSelectInput(session, option_name, selected=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateCheckboxInput') {

        updateCheckboxInput(session, option_name, value=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateTextInput') {

        updateTextInput(session, option_name, value=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateSliderTextInput') {

        # perhaps its a bug, but it seems like for all the updateSliderTextInput controls I have to 
        # pass choices as well
        # I guess i have to hard-code?

        if(option_name == 'var_plots__filter_factor_lump_number') {

            variable_choices <- as.character(c("Off", seq(1, 10), seq(15, 50, 5)))

        } else if (option_name == 'var_plots__transparency') {

            variable_choices <- c(seq(0, 90, 10), 99)

        } else if (option_name == 'var_plots__base_size') {

            variable_choices <- seq(6, 20, 1)

        } else {
            stopifnot(FALSE)
        }

        updateSliderTextInput(session,
                              option_name,
                              choices=variable_choices,
                              selected=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateSliderInput') {

        updateSliderInput(session, option_name, value=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateNumericInput') {

        updateNumericInput(session, option_name, value=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateRadioButtons') {

        updateRadioButtons(session, option_name, selected=get_default_value_for_updating(option_name))

    } else if(var_plots__variable_types[[option_name]] == 'updateTextAreaInput') {

        updateTextAreaInput(session, option_name, value=get_default_value_for_updating(option_name))

    } else {
        stopifnot(FALSE)
    }
}

helper__restore_defaults_graph_options <- function(session) {
    # perhaps its a bug, but it seems like for all the updateSliderTextInput controls I have to 
    # pass choices as well

    reset_hide_var_plot_option(session, option_name='var_plots__filter_factor_lump_number', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__label_variables', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__annotate_points', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__show_points', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__year_over_year', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__include_zero_y_axis', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__numeric_graph_type', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__plot_type', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__snapshots__values', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__snapshots__units', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__snapshots__color_or_facet', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__last_n_cohorts', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__n_units_after_first_date', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__date_cr__separate_colors', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__categoric_view_type', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__order_by_variable', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__reverse_stack_order', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__show_variable_totals', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__show_comparison_totals', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__histogram_bins', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__transparency', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__jitter', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__numeric_aggregation_count_minimum', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__numeric_show_resampled_conf_int', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__trend_line', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__trend_extend_date', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__trend_line_se', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__ts_date_floor', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__ts_date_break_format', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__ts_breaks_width', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__scale_x_log_base_10', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__x_zoom_min', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__x_zoom_max', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__scale_y_log_base_10', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__y_zoom_min', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__y_zoom_max', hide_option=FALSE)
}

helper__restore_defaults_other_options <- function(session) {
    reset_hide_var_plot_option(session, option_name='var_plots__custom_title', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__custom_subtitle', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__custom_x_axis_label', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__custom_y_axis_label', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__custom_caption', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__custom_tag', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__pretty_text', hide_option=FALSE)
    # perhaps its a bug, but it seems like for all the updateSliderTextInput controls I have to 
    # pass choices as well
    reset_hide_var_plot_option(session, option_name='var_plots__base_size', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__vertical_annotations', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__horizontal_annotations', hide_option=FALSE)
    # even though I call updateTextInput before click, the values haven't been reset yet
    # click('var_plots__custom_labels_apply')
}

helper__restore_defaults_map_options <- function(session) {

    reset_hide_var_plot_option(session, option_name='var_plots__map_format', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__map_borders_database', hide_option=FALSE)
    reset_hide_var_plot_option(session, option_name='var_plots__map_borders_regions', hide_option=FALSE)
}

hide_graph_options <- function(session) {

    reset_hide_var_plot_option(session, 'var_plots__filter_factor_lump_number')
    reset_hide_var_plot_option(session, 'var_plots__label_variables')
    reset_hide_var_plot_option(session, 'var_plots__annotate_points')
    reset_hide_var_plot_option(session, 'var_plots__show_points')
    reset_hide_var_plot_option(session, 'var_plots__year_over_year')
    reset_hide_var_plot_option(session, 'var_plots__include_zero_y_axis')
    reset_hide_var_plot_option(session, 'var_plots__numeric_graph_type')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
    reset_hide_var_plot_option(session, 'var_plots__categoric_view_type')
    reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')
    reset_hide_var_plot_option(session, 'var_plots__show_variable_totals')
    reset_hide_var_plot_option(session, 'var_plots__show_comparison_totals')
    reset_hide_var_plot_option(session, 'var_plots__order_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__histogram_bins')
    reset_hide_var_plot_option(session, 'var_plots__transparency')
    reset_hide_var_plot_option(session, 'var_plots__jitter')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
    reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')
    reset_hide_var_plot_option(session, 'var_plots__trend_line')
    reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
    reset_hide_var_plot_option(session, 'var_plots__trend_line_se')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_floor')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_break_format')
    reset_hide_var_plot_option(session, 'var_plots__ts_breaks_width')
    reset_hide_var_plot_option(session, 'var_plots__scale_x_log_base_10')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_min')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_max')
    reset_hide_var_plot_option(session, 'var_plots__scale_y_log_base_10')
    reset_hide_var_plot_option(session, 'var_plots__y_zoom_min')
    reset_hide_var_plot_option(session, 'var_plots__y_zoom_max')
}

observeEvent__var_plots__graph_options__any_used__function <- function(input, session, url_parameter_info, var_plots_graph_options_can_dirty) {

    observeEvent(c(# any of these will trigger the graph options color change

                   # var_plots__mock_input is so that we can force this to run (and reset
                   # var_plots_graph_options_can_dirty) when the graph is created
                   input$var_plots__mock_input,  
                   input$var_plots__filter_factor_lump_number,
                   input$var_plots__label_variables,
                   input$var_plots__annotate_points,
                   input$var_plots__show_points,
                   input$var_plots__year_over_year,
                   input$var_plots__include_zero_y_axis,
                   input$var_plots__numeric_graph_type,
                   input$var_plots__date_cr__plot_type,
                   input$var_plots__date_cr__snapshots__values,
                   input$var_plots__date_cr__snapshots__units,
                   input$var_plots__date_cr__snapshots__color_or_facet,
                   input$var_plots__date_cr__last_n_cohorts,
                   input$var_plots__date_cr__n_units_after_first_date,
                   input$var_plots__date_cr__separate_colors,
                   input$var_plots__categoric_view_type,
                   input$var_plots__order_by_variable,
                   input$var_plots__reverse_stack_order,
                   input$var_plots__show_variable_totals,
                   input$var_plots__show_comparison_totals,
                   input$var_plots__histogram_bins,
                   input$var_plots__transparency,
                   input$var_plots__jitter,
                   input$var_plots__numeric_aggregation_count_minimum,
                   input$var_plots__numeric_show_resampled_conf_int,
                   input$var_plots__trend_line,
                   input$var_plots__trend_extend_date,
                   input$var_plots__trend_line_se,
                   input$var_plots__ts_date_floor,
                   input$var_plots__ts_date_break_format,
                   input$var_plots__ts_breaks_width,
                   input$var_plots__scale_x_log_base_10,
                   input$var_plots__x_zoom_min,
                   input$var_plots__x_zoom_max,
                   input$var_plots__scale_y_log_base_10,
                   input$var_plots__y_zoom_min,
                   input$var_plots__y_zoom_max
        ), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        if(var_plots_graph_options_can_dirty()) {

            if(isolate(input$var_plots__variable) != global__select_variable) {

                log_message_block_start('Graph Options Dirty (Control Used)')
                updateCollapse(session, "var_plots__bscollapse", style = list("Graph Options" = "danger"))
            }
        } else {
            var_plots_graph_options_can_dirty(TRUE)
        }

    }, ignoreNULL=TRUE, ignoreInit=TRUE)
}

observeEvent__var_plots__other_options__any_used__function <- function(input, session, url_parameter_info) {

    observeEvent(c(# any of these will trigger the graph options color change
                   input$var_plots__custom_title,
                   input$var_plots__custom_subtitle,
                   input$var_plots__custom_x_axis_label,
                   input$var_plots__custom_y_axis_label,
                   input$var_plots__custom_caption,
                   input$var_plots__custom_tag,
                   input$var_plots__base_size,
                   input$var_plots__vertical_annotations,
                   input$var_plots__horizontal_annotations,
                   input$var_plots__pretty_text
        ), {

        req(!isolate(url_parameter_info$currently_updating))  # should never update if we have params (until set to false)

        if(isolate(input$var_plots__variable) != global__select_variable) {

            log_message_block_start('Other Options Dirty (Control Used)')
            updateCollapse(session, "var_plots__bscollapse", style = list("Other Options" = "danger"))
        }

    }, ignoreNULL=TRUE, ignoreInit=TRUE)
}

##############################################################################################################
# FILTER BUTTONS
# Events that control the color of the Filter collapse panel, so that it turns red when the filters haven't
# been applied (i.e. changes that haven't been applied)
##############################################################################################################
observeEvent__var_plots__filter_clear <- function(input, session) {

    observeEvent(input$var_plots__filter_clear, ({

        if(isolate(input$var_plots__filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }
    }))
}

observeEvent__var_plots__filter_apply <- function(input, session) {

    observeEvent(input$var_plots__filter_apply, ({

        if(isolate(input$var_plots__filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'success'))
        }
    }))
}

observeEvent__var_plots__filter_use <- function(input, session) {

    observeEvent(input$var_plots__filter_use, {

        if(input$var_plots__filter_use) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'success'))

        } else {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'default'))
        }
    })
}

#' The filter objects (e.g. sliderInput, selectInput, etc.) are created based on the dataset column types;
#' all of these objects are added to the `var_plots__filter_bscollapse__UI`. 
#' When someone selects the variables to filter, all of the filter objects are shown/hidden according.
#' This observeEvent does the hiding/showing 
observeEvent__var_plots__show_hide_dynamic_filters <- function(input, session, dataset) {

    observeEvent(input$var_plots__filter_controls_selections, {
        
        if(input$var_plots__filter_use) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }

        dataset_columns <- colnames(dataset$data)
        filter_controls_selections <- input$var_plots__filter_controls_selections
        if('All Variables' %in% filter_controls_selections) {
            filter_controls_selections <- dataset_columns
        }

        unselected <- dataset_columns[!dataset_columns %in% filter_controls_selections]

        log_message_variable("Selected Filter Variables", paste(filter_controls_selections, collapse="; "))
        log_message_variable("Unselected Filter Variables", paste(unselected, collapse="; "))

        for(variable in filter_controls_selections) {

            shinyjs::show(paste0('var_plots__dynamic_filter__', variable))
        }

        for(variable in unselected) {

            shinyjs::hide(paste0('var_plots__dynamic_filter__', variable))
        }
    
    }, ignoreNULL = FALSE, ignoreInit=TRUE)  # ignoreNULL so that the observeEvent is triggered when the user removes all
                            # of the selections from the `var_plots__filter_controls_selections` inputSelect
}

observe__var_plots__bscollapse__dynamic_inputs <- function(input, session, dataset) {

    observe({

        req(dataset$data)

        log_message_block_start("Registering Dynamic Filters")

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(column_name in colnames(dataset$data)) {
            value <- input[[paste0('var_plots__dynamic_filter__', column_name)]]
            selections <- append(selections, value)
        }

        # only update if we are using the filter
        # also, if any of the selections are not null, that means they have been initialized and we can begin
        # to mark as being changed otherwise, the filter section hasn't even been opened
        if(isolate(input$var_plots__filter_use) && any(map_lgl(selections, ~ !is.null(.)))) {

            updateCollapse(session, "var_plots__bscollapse", style = list('Filters' = 'danger'))
        }
    })
}

##############################################################################################################
# FILTERED DATASET - Variable Plot's filtered dataset
# duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every
# time.
##############################################################################################################
reactive__var_plots__filtered_data__creator <- function(input, dataset, reactive_filter_message_list) {

    reactive({

        local_dataset <- dataset$data  # clear on new datasets

        # these are the columns we want to filter on; if the column is not in the selection, don't filter
        filter_controls_selections <- isolate(input$var_plots__filter_controls_selections)
        if('All Variables' %in% filter_controls_selections) {

            filter_controls_selections <- colnames(local_dataset)
        }

        if(!is.null(input$var_plots__filter_use) && input$var_plots__filter_use) {

            input$var_plots__filter_apply  # trigger for the "apply" button

            column_names <- colnames(local_dataset)
            num_columns <- length(column_names)
            withProgress(value=1 / 2, message='Applying Filters',{

                log_message_block_start('Filtering...')

                #### APPLY FILTERS
                # list with selections for each dynamic filter, and list names are the column names; includes
                # filter values for ALL controls, whether hidden or shown, so need to subset by the filters
                # that are actually selected
                progress_bar_callback <- function(index, num_columns, column_name) {

                    incProgress(index / num_columns, detail = paste("column:", column_name))
                }

                all_filter_values <- get_dynamic_filter_values(input, column_names)
                filter_results <- filter_data(dataset=local_dataset,
                                              filter_list=all_filter_values[filter_controls_selections],
                                              callback=progress_bar_callback)
                local_dataset <- filter_results[[1]]

                log_message(format_filtering_message(filter_results[[2]], local_dataset))
                reactive_filter_message_list$value <- filter_results[[2]]
            })
        } else {

            log_message_block_start('Not Filtering')
            reactive_filter_message_list$value <- NULL
        }

        return (local_dataset)
    })
}

hide_show_top_n_categories <- function(session, dataset, variable, comparison_variable, size_variable,
                                       color_variable, facet_variable, conversion_group_variable) {

    if(variable == global__select_variable || !(variable %in% colnames(dataset))) {

        reset_hide_var_plot_option(session, 'var_plots__filter_factor_lump_number')
        return (TRUE)
    }

    dataset <- dataset %>% 
        select(c(variable, comparison_variable, size_variable, color_variable, facet_variable, 
                 conversion_group_variable)) %>%
        select_if(is_categoric)

    if(ncol(dataset) > 0) {

        shinyjs::show('var_plots__filter_factor_lump_number')
        return (FALSE)

    } else {

        reset_hide_var_plot_option(session, 'var_plots__filter_factor_lump_number')
        return (TRUE)
    }
}

##############################################################################################################
# Dynamic Variable Logic
##############################################################################################################
var_plots__comparison__logic <- function(dataset, primary_variable, current_value) {

    log_message("Executing Logic for Comparison Variable")
    log_message_variable('var_plots__variable', primary_variable)
    log_message_variable('var_plots__comparison', current_value)

    column_names <- colnames(dataset)

    if(!is.null(primary_variable) &&
            primary_variable %in% column_names &&
            is_date_type(dataset[[primary_variable]])) {

        column_names <- colnames(dataset %>% select_if(is.numeric))
    }

    if(is.null(primary_variable) || primary_variable == global__select_variable) {

        current_value <- NULL
    }
    selected_variable <- default_if_null_or_empty_string(value=current_value,
                                                         # treat global__select_variable_optional as null
                                                         string_values_as_null=global__select_variable_optional,
                                                         default=global__select_variable_optional)

    log_message_variable('Final Value', selected_variable)
    return(list(choices=c(global__select_variable_optional, column_names),
                selected=selected_variable))
}

var_plots__date_conversion_variable__logic <- function(dataset, primary_variable, current_value) {

    log_message("Executing Logic for Date Conversion Variable")
    log_message_variable('var_plots__variable', primary_variable)
    log_message_variable('var_plots__date_conversion_variable', current_value)

    column_names <- colnames(dataset)

    if(!is.null(primary_variable) &&
            primary_variable %in% column_names &&
            is_date_type(dataset[[primary_variable]])) {

        date_column_names <- colnames(dataset %>% select_if(is_date_type)) %>% rt_remove_val(primary_variable)
        selected_variable <- default_if_null_or_empty_string(value=current_value,
                                                             # treat global__select_variable_optional as null
                                                             string_values_as_null=global__select_variable_optional,
                                                             default=global__select_variable_optional)

        return (list(choices=c(global__select_variable_optional, date_column_names),
                     selected=selected_variable))

    } else {

        return (list(choices=NULL,
                     selected=NULL))
    }
}

var_plots__color__logic <- function(dataset, primary_variable, comparison_variable, current_value) {

    log_message("Executing Logic for Color Variable")
    log_message_variable('var_plots__variable', primary_variable)
    log_message_variable('var_plots__comparison', comparison_variable)
    log_message_variable('var_plots__color_variable', current_value)

    column_names <- colnames(dataset)
    
    if(!is.null(primary_variable) &&
            primary_variable %in% column_names &&
            is_date_type(dataset[[primary_variable]])) {

        column_names <- colnames(dataset %>% select_if(is_categoric))

    } else if(xor(!is.null(primary_variable) &&
                      primary_variable %in% column_names &&
                      is.numeric(dataset[[primary_variable]]),
                  !is.null(comparison_variable) &&
                  comparison_variable %in% column_names &&
                  is.numeric(dataset[[comparison_variable]]))) {

        column_names <- colnames(dataset %>% select_if(is_categoric))

    } else {

        column_names <- colnames(dataset)
    }

    if(is.null(primary_variable) || primary_variable == global__select_variable) {

        current_value <- NULL
    }
    selected_variable <- default_if_null_or_empty_string(value=current_value,
                                                         # treat global__select_variable_optional as null
                                                         string_values_as_null=global__select_variable_optional,
                                                         default=global__select_variable_optional)

    log_message_variable('Final Value', selected_variable)
    return(list(choices=c(global__select_variable_optional, column_names),
                selected=selected_variable))
}

var_plots__categoric_view_type__logic <- function(dataset,
                                                  comparison_variable,
                                                  sum_by_variable,
                                                  count_distinct_variable,
                                                  current_value) {

    log_message("Executing Logic for Categoric View Type")
    log_message_variable('var_plots__comparison', comparison_variable)
    log_message_variable('var_plots__sum_by_variable', sum_by_variable)
    log_message_variable('var_plots__count_distinct_variable', count_distinct_variable)
    log_message_variable('var_plots__categoric_view_type', current_value)

    comparison_variable <- null_if_select_variable_optional(comparison_variable)
    sum_by_variable <- null_if_select_variable_optional(sum_by_variable)
    count_distinct_variable <- null_if_select_variable_optional(count_distinct_variable)

    view_type_options <- NULL
    if(!is.null(count_distinct_variable)) {
        if(is.null(comparison_variable)) {

            view_type_options <- c("Bar")

        } else {

            view_type_options <- c("Bar", "Facet by Comparison")
        }
    } else if(is.null(comparison_variable) && is.null(sum_by_variable)) {

        view_type_options <- c("Bar", "Confidence Interval")

    } else if(is.null(comparison_variable) && !is.null(sum_by_variable)) {

        view_type_options <- c("Bar")

    } else if(!is.null(comparison_variable) && is.null(sum_by_variable)) {

        view_type_options <- c("Bar",
                               "Confidence Interval",
                               "Stack",
                               "Stack Percent",
                               "Facet by Comparison",
                               "Confidence Interval - within Variable")

    } else { # both are not null
        
        view_type_options <- c("Bar", "Stack", "Stack Percent", "Facet by Comparison")
    }

    if(!is.null(current_value) && current_value %in% view_type_options) {
        
        selected_variable <- current_value

    } else {

        selected_variable <- "Bar"
    }

    log_message_variable('Final Value', selected_variable)
    return(list(choices=view_type_options,
                selected=selected_variable))
}

var_plots__trend_extend_date__logic <- function(dataset, primary_variable, current_value) {

    log_message("Executing Logic for Trend Extend To")
    log_message_variable('var_plots__variable', primary_variable)
    log_message_variable('var_plots__trend_extend_date', current_value)
    log_message_variable('class var_plots__trend_extend_date', class(current_value))

    column_names <- colnames(dataset)

    if(!is.null(primary_variable) &&
            primary_variable %in% column_names &&
            is_date_type(dataset[[primary_variable]])) {
        
        if(!is.null(current_value) && is.character(current_value)) {
            
            if(current_value == '') {
            
                current_value <- NULL    

            } else {
                
                current_value <- as.Date(current_value)
            }
        }

        selected <- current_value

        #if current value is beyond the max value of the primary variable, if yes, keep it, if not, get max of primary val
        max_date <- floor_date(as.POSIXct(max(dataset[[primary_variable]], na.rm=TRUE)), unit = 'day')
        if(is.null(current_value) || current_value < max_date) {

            selected <- floor_date(max_date %m+% months(6), unit='month')
        }
        selected <- as.character(selected)
        # default it to the max date of the primary_variable, extended 6 months
        log_message_variable('selected', selected)
        return (selected)

    } else {

        log_message_variable('returned default', '0000-01-01')
        return ('0000-01-01')  # value doesn't matter
    }
}

##############################################################################################################
# CREATE GGPLOT OBJECT
##############################################################################################################
custom_max_min <- function(x_vector, y_value) {
    max(min(x_vector, na.rm=TRUE), y_value, na.rm=TRUE)
}

#' @param local_primary_variable numeric
#' @param local_comparison_variable categoric
helper__plot_numeric_categoric <- function(dataset,
                                           numeric_graph_type,
                                           primary_variable,
                                           comparison_variable,
                                           color_variable,
                                           order_by_variable,
                                           filter_factor_lump_number,
                                           histogram_bins,
                                           horizontal_annotations,
                                           scale_x_log_base_10,
                                           x_zoom_min,
                                           x_zoom_max,
                                           scale_y_log_base_10,
                                           y_zoom_min,
                                           y_zoom_max,
                                           base_size
                                           ) {


    show_boxplot <- numeric_graph_type == 'Boxplot'

    if(show_boxplot) {

        if(order_by_variable %in% colnames(dataset)) {
            
            temp_order_by_variable <- order_by_variable

        } else {

            temp_order_by_variable <- NULL
        }

        annotation_x_location <- 0.5
        if(is.null(comparison_variable)) {
            # need this logic because the position changes depending on if it is a single boxplot or multiple
            annotation_x_location <- -0.9
        }

        ggplot_object <- dataset %>%
            select(primary_variable,
                   comparison_variable,
                   color_variable,
                   temp_order_by_variable) %>%
            mutate_factor_lump(factor_lump_number=filter_factor_lump_number) %>%
            mutate_factor_reorder(variable_to_order_by=order_by_variable,
                                  variable_to_order=comparison_variable) %>%
            rt_explore_plot_boxplot(variable=primary_variable,
                                    comparison_variable=comparison_variable,
                                    color_variable=color_variable,
                                    y_zoom_min=y_zoom_min,
                                    y_zoom_max=y_zoom_max,
                                    base_size=base_size) %>%
            scale_axes_log10(scale_x=FALSE,
                             scale_y=scale_y_log_base_10) %>%
            add_horizontal_annotations(horizontal_annotations,
                                       x_location=annotation_x_location)

    } else {

        ggplot_object <- dataset %>% select(primary_variable, comparison_variable) %>%
            mutate_factor_lump(factor_lump_number=filter_factor_lump_number) %>%
            rt_explore_plot_histogram(variable=primary_variable,
                                      comparison_variable=comparison_variable,
                                      num_bins=histogram_bins,
                                      x_zoom_min=x_zoom_min,
                                      x_zoom_max=x_zoom_max,
                                      base_size=base_size) %>%
            scale_axes_log10(scale_x=scale_x_log_base_10,
                             scale_y=FALSE)
    }

    return (ggplot_object)
}

reactive__var_plots__ggplot__creator <- function(input, session, dataset, url_parameter_info, var_plots_graph_options_can_dirty) {
    
    eventReactive(c(input$var_plots__graph_options_apply,
                    input$var_plots__custom_labels_apply,
                    url_parameter_info$can_plot,
                    dataset(),
                    input$var_plots__variable,
                    input$var_plots__comparison,
                    input$var_plots__numeric_aggregation,
                    input$var_plots__sum_by_variable,
                    input$var_plots__count_distinct_variable,
                    input$var_plots__size_variable,
                    input$var_plots__color_variable,
                    input$var_plots__facet_variable,
                    input$var_plots__date_conversion_variable,
                    input$var_plots__date_cr__snapshots__group_variable,
                    input$var_plots__multi_value_delimiter,
                    input$var_plots__numeric_group_comp_variable,
                    input$var_plots__numeric_aggregation_function,
                    input$var_plots__map_format,
                    input$var_plots__map_borders_database,
                    input$var_plots__map_borders_regions), {
        
        if(isolate(url_parameter_info$currently_updating)) {

            # if we are updating from url parameters, need to wait until we can_plot
            req(url_parameter_info$can_plot)
            req(input$var_plots__variable != global__select_variable)
        }

        req(input$var_plots__variable)
        req(dataset())

        ######################################################################################################
        ######################################################################################################
        dataset <- dataset()

        # set var_plots_graph_options_can_dirty to FALSE; then set var_plots__mock_input to a random value so
        # that it forces the observeEvent__var_plots__graph_options__any_used__function to run (which it might
        # not if no values get udpated.
        var_plots_graph_options_can_dirty(FALSE)
        updateTextInput(session, 'var_plots__mock_input', value=runif(1, 0, 1000))

        log_message_block_start('Preparing to Create ggplot Object')

        
        primary_variable <- input$var_plots__variable
        numeric_aggregation <- input$var_plots__numeric_aggregation

        # if there isn't a selection for these variables, then set them to NULL, because they will be
        # passed to rtools functions (and if they aren't null, rtools expects column names)
        comparison_variable <- null_if_select_variable_optional(input$var_plots__comparison)
        # these can actually be NULL (unlike comparison_variable which is req)
        # these can't be req because they aren't even shown initially
        sum_by_variable <- null_if_select_variable_optional(input$var_plots__sum_by_variable)
        count_distinct_variable <- null_if_select_variable_optional(input$var_plots__count_distinct_variable)
        size_variable <- null_if_select_variable_optional(input$var_plots__size_variable)
        label_variables <- null_if_select_variable_optional(isolate(input$var_plots__label_variables))
        color_variable <- null_if_select_variable_optional(input$var_plots__color_variable)
        facet_variable <- null_if_select_variable_optional(input$var_plots__facet_variable)
        date_conversion_variable <- null_if_select_variable_optional(input$var_plots__date_conversion_variable)
        date_cr__snapshots__group_variable <- null_if_select_variable_optional(input$var_plots__date_cr__snapshots__group_variable)
        year_over_year <- default_if_null_or_empty_string(isolate(input$var_plots__year_over_year),
                                                          default=FALSE)
        include_zero_y_axis <- default_if_null_or_empty_string(isolate(input$var_plots__include_zero_y_axis),
                                                               default=TRUE)
        multi_value_delimiter <- input$var_plots__multi_value_delimiter

        if(!is.null(comparison_variable) && is_date_type(dataset[[comparison_variable]])) {

            showModal(modalDialog(title = "Only the primary 'Variable' selection can be used with date types."))
            updateSelectInput(session, 'var_plots__comparison', selected=global__select_variable_optional)
            return (NULL)
        }

        if(isolate(input$var_plots__trend_line) == "Projection" && year_over_year) {
            showModal(modalDialog(title = "Cannot project year-over-year trend-line. Unselecting Year-over-Year. Click 'Apply Options' in 'Graph Options' section."))
            updateCheckboxInput(session, 'var_plots__year_over_year', value=FALSE)
            return (NULL)   
        }

        if(is_date_type(dataset[[primary_variable]]) && !is.null(color_variable) && year_over_year) {
            # we cannot use YOY and color (year will be the color)
            # So, if we aren't faceting, let's move color to the facet variable.
            # Otherwise, we will need to clear the color variable

            if(is.null(facet_variable)) {

                showModal(modalDialog(title = "Cannot select Color Variable & Year-over-Year simultaneously (the year will be used as the color). The Facet Variable will be set and the Color Variable will be cleared."))
                updateSelectInput(session, 'var_plots__facet_variable', selected=color_variable)
                updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
                log_message_block_start("Clearing Color Variable For Year-over-Year and setting Facet Variable")

            } else {

                showModal(modalDialog(title = "Cannot select Color Variable & Year-over-Year simultaneously (the year will be used as the color). The Color Variable will be cleared."))
                updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
                log_message_block_start("Clearing Color Variable For Year-over-Year")
            }

            return (NULL)            
        }

        numeric_group_comp_variable <- input$var_plots__numeric_group_comp_variable
        numeric_aggregation_function <- input$var_plots__numeric_aggregation_function
        numeric_aggregation_count_minimum <- isolate(input$var_plots__numeric_aggregation_count_minimum)
        numeric_show_resampled_confidence_interval <- isolate(input$var_plots__numeric_show_resampled_conf_int)

        vertical_annotations <- isolate(input$var_plots__vertical_annotations)
        horizontal_annotations <- isolate(input$var_plots__horizontal_annotations)

        transparency <- isolate(input$var_plots__transparency) / 100
        annotate_points <- isolate(input$var_plots__annotate_points)
        base_size <- isolate(input$var_plots__base_size)
        histogram_bins <- isolate(input$var_plots__histogram_bins)
        jitter <- isolate(input$var_plots__jitter)
        order_by_variable <- isolate(input$var_plots__order_by_variable)
        numeric_graph_type <- isolate(input$var_plots__numeric_graph_type)
        date_cr__plot_type <- isolate(input$var_plots__date_cr__plot_type)
        date_cr__snapshots__values <- isolate(input$var_plots__date_cr__snapshots__values)
        date_cr__snapshots__units <- isolate(input$var_plots__date_cr__snapshots__units)
        date_cr__snapshots__color_or_facet <- isolate(input$var_plots__date_cr__snapshots__color_or_facet)
        date_cr__last_n_cohorts <- isolate(input$var_plots__date_cr__last_n_cohorts)
        date_cr__n_units_after_first_date <- isolate(input$var_plots__date_cr__n_units_after_first_date)
        date_cr__separate_colors <- isolate(input$var_plots__date_cr__separate_colors)
        pretty_text <- isolate(input$var_plots__pretty_text)
        scale_x_log_base_10 <- isolate(input$var_plots__scale_x_log_base_10)
        scale_y_log_base_10 <- isolate(input$var_plots__scale_y_log_base_10)
        reverse_stack_order <- isolate(input$var_plots__reverse_stack_order)
        show_variable_totals <- isolate(input$var_plots__show_variable_totals)
        show_comparison_totals <- isolate(input$var_plots__show_comparison_totals)
        categoric_view_type <- default_if_null_or_empty_string(isolate(input$var_plots__categoric_view_type),
                                                               default="Bar")

        if(!is.null(count_distinct_variable)) {

            if((is.null(comparison_variable) && categoric_view_type != "Bar") || 
               (!is.null(comparison_variable) && !categoric_view_type %in% c("Bar", "Facet by Comparison"))) {

                showModal(modalDialog(title = "Invalid 'View Type' for counting distinct variables, setting the View Type to 'Bar'."))
                categoric_view_type <- "Bar"
            } 
        }
        
        trend_line <- isolate(input$var_plots__trend_line)
        trend_extend_date <- isolate(input$var_plots__trend_extend_date)
        trend_line_se <- isolate(input$var_plots__trend_line_se)
        x_zoom_min <- isolate(input$var_plots__x_zoom_min)
        x_zoom_max <- isolate(input$var_plots__x_zoom_max)
        y_zoom_min <- isolate(input$var_plots__y_zoom_min)
        y_zoom_max <- isolate(input$var_plots__y_zoom_max)

        # for time series plot
        show_points <- default_if_null_or_empty_string(isolate(input$var_plots__show_points),
                                                       default=FALSE)
        ts_date_floor <- default_if_null_or_empty_string(isolate(input$var_plots__ts_date_floor),
                                                         string_values_as_null='None')
        ts_date_break_format <- default_if_null_or_empty_string(isolate(input$var_plots__ts_date_break_format),
                                                                string_values_as_null='Auto')
        ts_date_breaks_width <- default_if_null_or_empty_string(isolate(input$var_plots__ts_breaks_width))

        filter_factor_lump_number <- isolate(input$var_plots__filter_factor_lump_number)

        top_n_is_hidden <- hide_show_top_n_categories(session,
                                                      dataset(),
                                                      primary_variable,
                                                      comparison_variable,
                                                      size_variable,
                                                      color_variable,
                                                      facet_variable,
                                                      date_cr__snapshots__group_variable)

        if(top_n_is_hidden ||
               is.null(filter_factor_lump_number) ||
               filter_factor_lump_number == "Off") {

            filter_factor_lump_number <- NULL

        } else {

            filter_factor_lump_number <- as.numeric(filter_factor_lump_number)
        }

        # NOTE MAP options don't have an apply button
        map_format <- input$var_plots__map_format
        map_borders_database <- input$var_plots__map_borders_database
        map_borders_regions <- input$var_plots__map_borders_regions

        custom_title <- isolate(input$var_plots__custom_title)
        custom_subtitle <- isolate(input$var_plots__custom_subtitle)
        custom_x_axis_label <- isolate(input$var_plots__custom_x_axis_label)
        custom_y_axis_label <- isolate(input$var_plots__custom_y_axis_label)
        custom_caption <- isolate(input$var_plots__custom_caption)
        custom_tag <- isolate(input$var_plots__custom_tag)

        if(primary_variable != global__select_variable &&
                primary_variable %in% colnames(dataset)) {

            if(is_date_type(dataset[[primary_variable]])) {

                hide_show_date(session, input)

            ##############################################################################################
            # Numeric Primary Variable
            ##############################################################################################
            } else if(is.numeric(dataset[[primary_variable]])) {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(comparison_variable) &&
                        is.numeric(dataset[[comparison_variable]])) {

                    hide_show_numeric_numeric(session, 
                                              numeric_group_comp_variable,
                                              numeric_aggregation_function == "Boxplot")

                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    show_boxplot <- numeric_graph_type == 'Boxplot'
                    hide_show_numeric_categoric(session=session,
                                showing_boxplot=show_boxplot,
                                has_comparison_variable=!is.null(comparison_variable))
                }

            ##############################################################################################
            # Categoric Primary Variable
            ##############################################################################################
            } else {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(comparison_variable) &&
                        is.numeric(dataset[[comparison_variable]])) {

                    show_boxplot <- numeric_graph_type == 'Boxplot'
                    hide_show_numeric_categoric(session=session,
                                showing_boxplot=show_boxplot,
                                has_comparison_variable=!is.null(comparison_variable))

                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    hide_show_categoric_categoric(session,
                                                  input,
                                                  has_comparison_variable=!is.null(comparison_variable))
                }
            }

        } else {

            hide_graph_options(session)
        }

        ggplot_object <- create_ggplot_object(dataset=dataset,
                                              primary_variable=primary_variable,
                                              comparison_variable=comparison_variable,
                                              sum_by_variable=sum_by_variable,
                                              count_distinct_variable=count_distinct_variable,
                                              size_variable=size_variable,
                                              label_variables=label_variables,
                                              color_variable=color_variable,
                                              facet_variable=facet_variable,
                                              date_conversion_variable=date_conversion_variable,
                                              date_cr__snapshots__group_variable=date_cr__snapshots__group_variable,

                                              numeric_aggregation=numeric_aggregation,
                                              numeric_group_comp_variable=numeric_group_comp_variable,
                                              numeric_aggregation_function=numeric_aggregation_function,
                                              numeric_aggregation_count_minimum=numeric_aggregation_count_minimum,
                                              numeric_show_resampled_confidence_interval=numeric_show_resampled_confidence_interval,

                                              transparency=transparency,
                                              annotate_points=annotate_points,
                                              reverse_stack_order=reverse_stack_order,
                                              show_variable_totals=show_variable_totals,
                                              show_comparison_totals=show_comparison_totals,
                                              histogram_bins=histogram_bins,
                                              jitter=jitter,
                                              order_by_variable=order_by_variable,
                                              filter_factor_lump_number=filter_factor_lump_number,
                                              numeric_graph_type=numeric_graph_type,
                                              date_cr__plot_type=date_cr__plot_type,
                                              date_cr__snapshots__values=date_cr__snapshots__values,
                                              date_cr__snapshots__units=date_cr__snapshots__units,
                                              date_cr__snapshots__color_or_facet=date_cr__snapshots__color_or_facet,
                                              date_cr__last_n_cohorts=date_cr__last_n_cohorts,
                                              date_cr__n_units_after_first_date=date_cr__n_units_after_first_date,
                                              date_cr__separate_colors=date_cr__separate_colors,
                                              categoric_view_type=categoric_view_type,
                                              multi_value_delimiter=multi_value_delimiter,
                                              trend_line=trend_line,
                                              trend_extend_date=trend_extend_date,
                                              trend_line_se=trend_line_se,
                                              x_zoom_min=x_zoom_min,
                                              x_zoom_max=x_zoom_max,
                                              y_zoom_min=y_zoom_min,
                                              y_zoom_max=y_zoom_max,
                                              scale_x_log_base_10=scale_x_log_base_10,
                                              scale_y_log_base_10=scale_y_log_base_10,

                                              # dates
                                              show_points=show_points,
                                              ts_date_floor=ts_date_floor,
                                              ts_date_break_format=ts_date_break_format,
                                              ts_date_breaks_width=ts_date_breaks_width,
                                              include_zero_y_axis=include_zero_y_axis,
                                              year_over_year=year_over_year,

                                              # mapping options
                                              map_format=map_format,
                                              map_borders_database=map_borders_database,
                                              map_borders_regions=map_borders_regions,

                                              # set graph label/etc. options
                                              pretty_text=pretty_text,
                                              base_size=base_size,
                                              custom_title=custom_title,
                                              custom_subtitle=custom_subtitle,
                                              custom_x_axis_label=custom_x_axis_label,
                                              custom_y_axis_label=custom_y_axis_label,
                                              custom_caption=custom_caption,
                                              custom_tag=custom_tag,
                                              vertical_annotations=vertical_annotations,
                                              horizontal_annotations=horizontal_annotations)

        if(is.null(ggplot_object)) {

            shinyjs::hide('var_plots__div__buttons_below_graphs')

        } else {

            shinyjs::show('var_plots__div__buttons_below_graphs')

            if(isolate(url_parameter_info$currently_updating)) {

                log_message("Detected that we're updating from URL parameters, setting `has_plotted` to TRUE")
                url_parameter_info$has_plotted <- TRUE
            }
        }

        # A) if these are dirty but we trigger the graph to update by selecting a variable or hitting Apply 
        # in an unrelated menu, the options get updated, so they are no longer dirty.
        # B) when we switch variable types and hide/show variables we also reset the values, which makes
        # the options "dirty" even though they are not being used
        updateCollapse(session, "var_plots__bscollapse", style = list("Graph Options" = "default"))
        updateCollapse(session, "var_plots__bscollapse", style = list("Other Options" = "default"))

        return (ggplot_object)
    
    })
}

create_ggplot_object <- function(dataset,
                                 primary_variable=NULL,
                                 comparison_variable=NULL,
                                 sum_by_variable=NULL,
                                 count_distinct_variable=NULL,
                                 size_variable=NULL,
                                 label_variables=NULL,
                                 color_variable=NULL,
                                 facet_variable=NULL,
                                 date_conversion_variable=NULL,
                                 date_cr__snapshots__group_variable=NULL,
                                 
                                 numeric_aggregation='Mean',
                                 numeric_group_comp_variable=FALSE,
                                 numeric_aggregation_function='Boxplot',
                                 numeric_aggregation_count_minimum=30,
                                 numeric_show_resampled_confidence_interval=FALSE,

                                 transparency=0.60,
                                 annotate_points=TRUE,
                                 reverse_stack_order=FALSE,
                                 show_variable_totals=TRUE,
                                 show_comparison_totals=TRUE,
                                 histogram_bins=30,
                                 jitter=FALSE,
                                 order_by_variable='Default',
                                 filter_factor_lump_number=10,
                                 numeric_graph_type='Boxplot',
                                 date_cr__plot_type="Snapshots",
                                 date_cr__snapshots__values="1, 7, 14",
                                 date_cr__snapshots__units="Days",
                                 date_cr__snapshots__color_or_facet="Color",
                                 date_cr__last_n_cohorts=10,
                                 date_cr__n_units_after_first_date=30,
                                 date_cr__separate_colors=TRUE,
                                 categoric_view_type='Bar',
                                 multi_value_delimiter=NULL,
                                 trend_line='None',
                                 trend_extend_date=NULL,
                                 trend_line_se='No',
                                 x_zoom_min=NA,
                                 x_zoom_max=NA,
                                 y_zoom_min=NA,
                                 y_zoom_max=NA,
                                 scale_x_log_base_10=FALSE,
                                 scale_y_log_base_10=FALSE,

                                 # dates
                                 show_points=TRUE,
                                 ts_date_floor='month',
                                 ts_date_break_format=NULL,
                                 ts_date_breaks_width=NULL,
                                 year_over_year=FALSE,
                                 include_zero_y_axis=TRUE,

                                 # mapping options
                                 map_format=FALSE,
                                 map_borders_database=NULL,
                                 map_borders_regions=NULL,

                                 # set graph label/etc. options
                                 pretty_text=FALSE,
                                 base_size=15,
                                 custom_title=NULL,
                                 custom_subtitle=NULL,
                                 custom_x_axis_label=NULL,
                                 custom_y_axis_label=NULL,
                                 custom_caption=NULL,
                                 custom_tag=NULL,
                                 vertical_annotations=NULL,
                                 horizontal_annotations=NULL

                                ) {

    ggplot_object <- NULL

    if(!is.null(primary_variable)  &&
            primary_variable != global__select_variable &&
            primary_variable %in% colnames(dataset)) {

        log_message_block_start('Creating ggplot object')

        log_message_variable('primary_variable', primary_variable)
        log_message_variable('comparison_variable', comparison_variable)
        log_message_variable('sum_by_variable', sum_by_variable)
        log_message_variable('count_distinct_variable', count_distinct_variable)
        log_message_variable('size_variable', size_variable)
        log_message_variable('label_variables', label_variables)
        log_message_variable('color_variable', color_variable)
        log_message_variable('facet_variable', facet_variable)
        log_message_variable('date_conversion_variable', date_conversion_variable)
        log_message_variable('date_cr__snapshots__group_variable', date_cr__snapshots__group_variable)

        log_message_variable('numeric_aggregation', numeric_aggregation)
        log_message_variable('numeric_group_comp_variable', numeric_group_comp_variable)
        log_message_variable('numeric_aggregation_function', numeric_aggregation_function)
        log_message_variable('numeric_aggregation_count_minimum', numeric_aggregation_count_minimum)
        log_message_variable('numeric_show_resampled_confidence_interval', numeric_show_resampled_confidence_interval)

        log_message_variable('transparency', transparency)
        log_message_variable('annotate_points', annotate_points)
        log_message_variable('show_variable_totals', show_variable_totals)
        log_message_variable('show_comparison_totals', show_comparison_totals)
        log_message_variable('histogram_bins', histogram_bins)
        log_message_variable('jitter', jitter)
        log_message_variable('order_by_variable', order_by_variable)
        log_message_variable('numeric_graph_type', numeric_graph_type)

        log_message_variable('date_cr__plot_type', date_cr__plot_type)
        log_message_variable('date_cr__snapshots__values', date_cr__snapshots__values)
        log_message_variable('date_cr__snapshots__units', date_cr__snapshots__units)
        log_message_variable('date_cr__snapshots__color_or_facet', date_cr__snapshots__color_or_facet)
        log_message_variable('date_cr__last_n_cohorts', date_cr__last_n_cohorts)
        log_message_variable('date_cr__n_units_after_first_date', date_cr__n_units_after_first_date)
        log_message_variable('date_cr__separate_colors', date_cr__separate_colors)

        log_message_variable('filter_factor_lump_number', filter_factor_lump_number)
        log_message_variable('categoric_view_type', categoric_view_type)
        log_message_variable('multi_value_delimiter', multi_value_delimiter)
        log_message_variable('trend_line', trend_line)
        log_message_variable('trend_extend_date', trend_extend_date)
        log_message_variable('trend_line_se', trend_line_se)
        log_message_variable('x_zoom_min', x_zoom_min)
        log_message_variable('x_zoom_max', x_zoom_max)
        log_message_variable('y_zoom_min', y_zoom_min)
        log_message_variable('y_zoom_max', y_zoom_max)
        log_message_variable('scale_x_log_base_10', scale_x_log_base_10)
        log_message_variable('scale_y_log_base_10', scale_y_log_base_10)

        # dates
        log_message_variable('show_points', show_points)
        log_message_variable('ts_date_floor', ts_date_floor)
        log_message_variable('ts_date_break_format', ts_date_break_format)
        log_message_variable('ts_date_breaks_width', ts_date_breaks_width)
        log_message_variable('year_over_year', year_over_year)
        log_message_variable('include_zero_y_axis', include_zero_y_axis)

        log_message_variable('map_format', map_format)
        log_message_variable('map_borders_database', map_borders_database)
        log_message_variable('map_borders_regions', map_borders_regions)

        # set graph label/etc. options
        log_message_variable('pretty_text', pretty_text)
        log_message_variable('base_size', base_size)
        log_message_variable('custom_title', custom_title)
        log_message_variable('custom_subtitle', custom_subtitle)
        log_message_variable('custom_x_axis_label', custom_x_axis_label)
        log_message_variable('custom_y_axis_label', custom_y_axis_label)
        log_message_variable('custom_caption', custom_caption)
        log_message_variable('custom_tag', custom_tag)
        log_message_variable('vertical_annotations', vertical_annotations)
        log_message_variable('horizontal_annotations', horizontal_annotations)

        if(!is.null(vertical_annotations)) {

            vertical_annotations <- str_split(vertical_annotations, "\n")[[1]]
            vertical_annotations <- purrr::map(vertical_annotations, ~ str_split(., ';')[[1]])
        }

        if(!is.null(horizontal_annotations)) {

            horizontal_annotations <- str_split(horizontal_annotations, "\n")[[1]]
            horizontal_annotations <- purrr::map(horizontal_annotations, ~ str_split(., ';')[[1]])
        }

        log_message_variable('var_plots__horizontal_annotations',
                             paste0(horizontal_annotations, collapse="..."))

        log_message_variable('var_plots__vertical_annotations',
                             paste0(vertical_annotations, collapse="..."))
        
        if(pretty_text) {
            # if we change to pretty text, it will update the columns and all values to be "pretty",
            # but that means we have to take the variables they selected and change them to be
            # "pretty" as well so subsetting by them finds the correct column
            required_variables <- c(default_if_null_or_empty_string(primary_variable),
                                    default_if_null_or_empty_string(comparison_variable),
                                    default_if_null_or_empty_string(sum_by_variable),
                                    default_if_null_or_empty_string(count_distinct_variable),
                                    default_if_null_or_empty_string(size_variable),
                                    default_if_null_or_empty_string(color_variable),
                                    default_if_null_or_empty_string(facet_variable),
                                    default_if_null_or_empty_string(date_conversion_variable),
                                    default_if_null_or_empty_string(date_cr__snapshots__group_variable))
            
            if(!is.null(label_variables) && length(label_variables) > 0) {
            
                required_variables <- c(required_variables, label_variables)
                
                label_variables <- rt_pretty_text(label_variables)
                log_message_variable('updated label_variables', paste0(label_variables, collapse = ', '))
            }
            
            dataset <- rt_pretty_dataset(dataset=dataset %>% select(required_variables))

            # R uses the "`My Variable`" syntax for variables with spaces which dplyr's xxx_() relies on
            primary_variable <- rt_pretty_text(primary_variable)
            if(!is_null_or_empty_string(comparison_variable)) {

                comparison_variable <- rt_pretty_text(comparison_variable)
            }
            if(!is_null_or_empty_string(sum_by_variable)) {

                sum_by_variable <- rt_pretty_text(sum_by_variable)
            }
            if(!is_null_or_empty_string(count_distinct_variable)) {

                count_distinct_variable <- rt_pretty_text(count_distinct_variable)
            }
            if(!is_null_or_empty_string(size_variable)) {

                size_variable <- rt_pretty_text(size_variable)
            }
            if(!is_null_or_empty_string(color_variable)) {

                color_variable <- rt_pretty_text(color_variable)
            }
            if(!is_null_or_empty_string(facet_variable)) {

                facet_variable <- rt_pretty_text(facet_variable)
            }
            if(!is_null_or_empty_string(date_conversion_variable)) {

                date_conversion_variable <- rt_pretty_text(date_conversion_variable)
            }
            if(!is_null_or_empty_string(date_cr__snapshots__group_variable)) {

                date_cr__snapshots__group_variable <- rt_pretty_text(date_cr__snapshots__group_variable)
            }

            log_message_variable('updated primary_variable', primary_variable)
            log_message_variable('updated comparison_variable', comparison_variable)
            log_message_variable('updated sum_by_variable', sum_by_variable)
            log_message_variable('updated count_distinct_variable', count_distinct_variable)
            log_message_variable('updated var_plots__size_variable', size_variable)
            log_message_variable('updated var_plots__color_variable', color_variable)
            log_message_variable('updated var_plots__facet_variable', facet_variable)
            log_message_variable('updated var_plots__date_conversion_variable', date_conversion_variable)
            log_message_variable('updated var_plots__date_cr__snapshots__group_variable', date_cr__snapshots__group_variable)
            log_message_generic('column names', paste0(colnames(dataset), collapse = '; '))
        }

        if(is_date_type(dataset[[primary_variable]])) {

            date_limits <- NULL
            if(!is.null(trend_line) && trend_line == "Projection") {
                # need to convert to Date in case it is POSIXct/etc.; in which case trend_extend_date gets
                # converted to something like 1970-01-01
                date_limits <- c(as.Date(min(dataset[[primary_variable]], na.rm=TRUE)), trend_extend_date)
            }

            add_confidence_interval <- !is.null(trend_line_se) && trend_line_se == 'Yes'

            ##########################################################################################
            # No Secondary Date, Plot Time-Series
            ##########################################################################################
            if(is.null(date_conversion_variable) || date_conversion_variable == global__select_variable_optional) {

                comparison_function <- NULL
                comparison_function_name <- NULL
                if(!is.null(comparison_variable)) {

                    comparison_function_name <- numeric_aggregation

                    if(numeric_aggregation == 'Mean') {

                        comparison_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                    } else if (numeric_aggregation == 'Geometric Mean') {

                        comparison_function <- rt_geometric_mean

                    } else if (numeric_aggregation == 'Median') {

                        comparison_function <- function(x) { return (median(x, na.rm=TRUE)) }

                    } else if (numeric_aggregation == 'Total') {

                        comparison_function_name = "Total"
                        comparison_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                    } else {

                        stopifnot(FALSE)
                    }
                }

                ggplot_object <- dataset %>%
                    select(primary_variable, comparison_variable, color_variable, facet_variable) %>%
                    mutate_factor_lump(factor_lump_number=filter_factor_lump_number) %>%
                    rt_explore_plot_time_series(variable=primary_variable,
                                                comparison_variable=comparison_variable,
                                                comparison_function=comparison_function,
                                                comparison_function_name=comparison_function_name,
                                                color_variable=color_variable,
                                                facet_variable=facet_variable,
                                                year_over_year=year_over_year,
                                                y_zoom_min=y_zoom_min,
                                                y_zoom_max=y_zoom_max,
                                                include_zero_y_axis=include_zero_y_axis,
                                                show_points=show_points,
                                                show_labels=annotate_points,
                                                date_floor=ts_date_floor,
                                                date_break_format=ts_date_break_format,
                                                date_breaks_width=ts_date_breaks_width,
                                                date_limits=date_limits,
                                                base_size=base_size) %>%
                    scale_axes_log10(scale_x=FALSE,
                                     scale_y=scale_y_log_base_10) %>%
                    add_trend_line(trend_line_type=trend_line,
                                   confidence_interval=add_confidence_interval,
                                   color_variable=color_variable) %>%
                    add_vertical_annotations(vertical_annotations,
                                             y_location=max(0, y_zoom_min, na.rm=TRUE),
                                             is_date=TRUE) %>%
                    add_horizontal_annotations(horizontal_annotations,
                                               x_location=min(dataset[[primary_variable]], na.rm=TRUE),
                                               x_location_is_date=TRUE)

            ##########################################################################################
            # Secondary Date, Plot Conversion Rates Or Adoption
            ##########################################################################################
            } else {

                if(date_cr__plot_type == global__date_cr_options[1]) {

                    ggplot_object <- dataset %>%
                        select(primary_variable, date_conversion_variable, date_cr__snapshots__group_variable) %>%
                        mutate_factor_lump(factor_lump_number=filter_factor_lump_number) %>%
                        rt_explore_plot_conversion_rates(first_date=primary_variable,
                                                         second_date=date_conversion_variable,
                                                         group_variable=date_cr__snapshots__group_variable,
                                                         reference_date=global__reference_date,
                                                         snapshots=as.numeric(str_split(string=date_cr__snapshots__values, pattern=', ', simplify = FALSE)[[1]]),
                                                         snapshot_units=date_cr__snapshots__units,
                                                         color_or_facet=date_cr__snapshots__color_or_facet,
                                                         year_over_year=year_over_year,
                                                         y_zoom_min=y_zoom_min,
                                                         y_zoom_max=y_zoom_max,
                                                         include_zero_y_axis=include_zero_y_axis,
                                                         show_points=show_points,
                                                         show_labels=annotate_points,
                                                         date_floor=ts_date_floor,
                                                         date_break_format=ts_date_break_format,
                                                         date_breaks_width=ts_date_breaks_width,
                                                         date_limits=date_limits,
                                                         base_size=base_size) %>%
                        add_trend_line(trend_line_type=trend_line,
                                       confidence_interval=add_confidence_interval,
                                       color_variable='Snapshot') %>%
                        add_vertical_annotations(vertical_annotations,
                                                 y_location=max(0, y_zoom_min, na.rm=TRUE),
                                                 is_date=TRUE) %>%
                        add_horizontal_annotations(horizontal_annotations,
                                                   x_location=min(dataset[[primary_variable]], na.rm=TRUE),
                                                   x_location_is_date=TRUE)
                } else {

                    ggplot_object <- dataset %>%
                        rt_explore_plot_cohorted_adoption(first_date=primary_variable,
                                                          second_date=date_conversion_variable,
                                                          reference_date=global__reference_date,
                                                          last_n_cohorts=date_cr__last_n_cohorts,
                                                          n_units_after_first_date=date_cr__n_units_after_first_date,
                                                          units=date_cr__snapshots__units,
                                                          separated_colors=date_cr__separate_colors,
                                                          date_floor=ts_date_floor,
                                                          y_zoom_min=y_zoom_min,
                                                          y_zoom_max=y_zoom_max,
                                                          include_zero_y_axis=include_zero_y_axis,
                                                          show_points=show_points,
                                                          show_labels=annotate_points,
                                                          date_break_format=ts_date_break_format,
                                                          base_size=base_size) %>%
                        add_vertical_annotations(vertical_annotations,
                                                 y_location=max(0, y_zoom_min, na.rm=TRUE),
                                                 is_date=TRUE) %>%
                        add_horizontal_annotations(horizontal_annotations,
                                                   x_location=min(dataset[[primary_variable]], na.rm=TRUE),
                                                   x_location_is_date=TRUE)
                }
            }

        ##############################################################################################
        # Numeric Primary Variable
        ##############################################################################################
        } else if(is.numeric(dataset[[primary_variable]])) {

            ##########################################################################################
            # Numeric Secondary Variable
            ##########################################################################################
            if(!is.null(comparison_variable) && is.numeric(dataset[[comparison_variable]])) {

                if(numeric_group_comp_variable) {

                    aggregation_function <- NULL
                    aggregation_function_name <- NULL

                    if(numeric_aggregation_function != 'Boxplot') {

                        aggregation_function_name <- numeric_aggregation_function

                        if(numeric_aggregation_function == 'Mean') {

                            aggregation_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                        } else if (numeric_aggregation_function == 'Geometric Mean') {

                            aggregation_function <- rt_geometric_mean

                        } else if (numeric_aggregation_function == 'Median') {

                            aggregation_function <- function(x) { return (median(x, na.rm=TRUE)) }

                        } else if (numeric_aggregation_function == 'Total') {

                            aggregation_function_name = 'Total'
                            aggregation_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                        } else {

                            stopifnot(FALSE)
                        }
                    }

                    ggplot_object <- dataset %>%
                        rt_explore_plot_aggregate_2_numerics(variable=primary_variable,
                                                             comparison_variable=comparison_variable,
                                                             aggregation_function=aggregation_function,
                                                             aggregation_function_name=aggregation_function_name,
                                                             aggregation_count_minimum=numeric_aggregation_count_minimum,
                                                             show_resampled_confidence_interval=numeric_show_resampled_confidence_interval,
                                                             show_points=show_points,
                                                             show_labels=annotate_points,
                                                             x_zoom_min=x_zoom_min,
                                                             x_zoom_max=x_zoom_max,
                                                             y_zoom_min=y_zoom_min,
                                                             y_zoom_max=y_zoom_max,
                                                             base_size=base_size) %>%
                        scale_axes_log10(scale_x=scale_x_log_base_10,
                                         scale_y=scale_y_log_base_10)
                } else {

                    add_confidence_interval <- !is.null(trend_line_se) && trend_line_se == 'Yes'

                    # Here, I don't want to lump the label variables, EXCEPT if one of the label variables
                    # happens to also be one of the color or size variables, which I do want to lump.
                    ignore_columns <- label_variables %>% rt_remove_val(c(size_variable, color_variable))
                    if(!is.null(ignore_columns) && 
                            (identical(ignore_columns, character(0)) || ignore_columns == '')) {
                        ignore_columns <- NULL
                    }
                    ggplot_object <- dataset %>% select(primary_variable,
                                                              comparison_variable,
                                                              color_variable,
                                                              size_variable,
                                                              label_variables) %>%
                        mutate_factor_lump(factor_lump_number=filter_factor_lump_number,
                                           ignore_columns=ignore_columns) %>%
                        rt_explore_plot_scatter(variable=primary_variable,
                                                comparison_variable=comparison_variable,
                                                color_variable=color_variable,
                                                size_variable=size_variable,
                                                label_variables=label_variables,
                                                # alpha is a measure of opacity which is the opposite of transparency, but transparency is more user-friendly
                                                alpha= 1 - transparency,
                                                jitter=jitter,
                                                x_zoom_min=x_zoom_min,
                                                x_zoom_max=x_zoom_max,
                                                y_zoom_min=y_zoom_min,
                                                y_zoom_max=y_zoom_max,
                                                base_size=base_size) %>%
                        scale_axes_log10(scale_x=scale_x_log_base_10,
                                         scale_y=scale_y_log_base_10) %>%
                        add_trend_line(trend_line_type=trend_line,
                                       confidence_interval=add_confidence_interval,
                                       color_variable=color_variable)

                    if(map_format) {

                        ggplot_object <- ggplot_object + coord_map()

                        if(!is_null_or_empty_string(map_borders_database)) {

                            regions <- str_split(map_borders_regions, pattern = ', ', simplify = TRUE)
                            regions <- as.character(regions)

                            ggplot_object <- ggplot_object +
                                borders(database=map_borders_database,
                                        regions=regions)
                        }
                    }
                }

                ggplot_object <- ggplot_object %>%
                        add_horizontal_annotations(horizontal_annotations,
                                                   x_location=custom_max_min(dataset[[comparison_variable]],
                                                                             x_zoom_min)) %>%
                        add_vertical_annotations(vertical_annotations,
                                                   y_location=custom_max_min(dataset[[primary_variable]],
                                                                             y_zoom_min))
            ##########################################################################################
            # NULL Or Categoric Secondary Variable
            ##########################################################################################
            } else {

                ggplot_object <- helper__plot_numeric_categoric(dataset=dataset,
                                                                numeric_graph_type=numeric_graph_type,
                                                                # same as above, except we need to swap the numeric/categoric variables
                                                                primary_variable=primary_variable,
                                                                comparison_variable=comparison_variable,
                                                                color_variable=color_variable,
                                                                order_by_variable=order_by_variable,
                                                                filter_factor_lump_number=filter_factor_lump_number,
                                                                histogram_bins=histogram_bins,
                                                                horizontal_annotations=horizontal_annotations,
                                                                scale_x_log_base_10=scale_x_log_base_10,
                                                                x_zoom_min=x_zoom_min,
                                                                x_zoom_max=x_zoom_max,
                                                                scale_y_log_base_10=scale_y_log_base_10,
                                                                y_zoom_min=y_zoom_min,
                                                                y_zoom_max=y_zoom_max,
                                                                base_siz=base_size)
            }

        ##############################################################################################
        # Categoric Primary Variable
        ##############################################################################################
        } else {

            ##########################################################################################
            # Numeric Secondary Variable
            ##########################################################################################
            if(!is.null(comparison_variable) && is.numeric(dataset[[comparison_variable]])) {

                ggplot_object <- helper__plot_numeric_categoric(dataset=dataset,
                                                                numeric_graph_type=numeric_graph_type,
                                                                # same as above, except we need to swap the numeric/categoric variables
                                                                primary_variable=comparison_variable,
                                                                comparison_variable=primary_variable,
                                                                color_variable=color_variable,
                                                                order_by_variable=order_by_variable,
                                                                filter_factor_lump_number=filter_factor_lump_number,
                                                                histogram_bins=histogram_bins,
                                                                horizontal_annotations=horizontal_annotations,
                                                                scale_x_log_base_10=scale_x_log_base_10,
                                                                x_zoom_min=x_zoom_min,
                                                                x_zoom_max=x_zoom_max,
                                                                scale_y_log_base_10=scale_y_log_base_10,
                                                                y_zoom_min=y_zoom_min,
                                                                y_zoom_max=y_zoom_max,
                                                                base_siz=base_size)

            ##########################################################################################
            # NULL Or Categoric Secondary Variable
            ##########################################################################################
            } else {

                if(!is.null(multi_value_delimiter) && multi_value_delimiter == '') {

                    multi_value_delimiter <- NULL
                }

                if(order_by_variable %in% colnames(dataset)) {
                        
                    temp_order_by_variable <- order_by_variable

                } else {

                    temp_order_by_variable <- NULL
                }

                ggplot_object <- dataset %>%
                    select(primary_variable,
                           comparison_variable,
                           sum_by_variable,
                           count_distinct_variable,
                           temp_order_by_variable) %>%
                    mutate_factor_lump(factor_lump_number=filter_factor_lump_number,
                                       ignore_columns=count_distinct_variable) %>%
                    mutate_factor_reorder(variable_to_order_by=order_by_variable,
                                          variable_to_order=primary_variable) %>%
                    rt_explore_plot_value_totals(variable=primary_variable,
                                                 comparison_variable=comparison_variable,
                                                 sum_by_variable=sum_by_variable,
                                                 count_distinct_variable=count_distinct_variable,
                                                 order_by_count=order_by_variable == "Frequency",
                                                 show_variable_totals=show_variable_totals,
                                                 show_comparison_totals=show_comparison_totals,
                                                 view_type=categoric_view_type,
                                                 multi_value_delimiter=multi_value_delimiter,
                                                 show_dual_axes=FALSE,
                                                 reverse_stack=reverse_stack_order,
                                                 base_size=base_size)
            }
        }

        if(!is_null_or_empty_string(custom_title)) {
            ggplot_object <- ggplot_object + labs(title=custom_title)
        }
        if(!is_null_or_empty_string(custom_subtitle)) {
            ggplot_object <- ggplot_object + labs(subtitle=custom_subtitle)
        }
        if(!is_null_or_empty_string(custom_x_axis_label)) {
            ggplot_object <- ggplot_object + labs(x=custom_x_axis_label)
        }
        if(!is_null_or_empty_string(custom_y_axis_label)) {
            ggplot_object <- ggplot_object + labs(y=custom_y_axis_label)
        }
        if(!is_null_or_empty_string(custom_caption)) {
            ggplot_object <- ggplot_object + labs(caption=custom_caption)
        }
        if(!is_null_or_empty_string(custom_tag)) {
            ggplot_object <- ggplot_object + labs(tag=custom_tag)
        }

    }

    return (ggplot_object)
}

##############################################################################################################
# INPUT
##############################################################################################################
renderUI__var_plots__filter_controls_selections__UI <- function(input, dataset, url_parameter_info) {
    renderUI({
	req(dataset$data)
        input$var_plots__filter_clear

        log_message_block_start('Creating Filter Controls Selections')

        choices <- list(
            "All Variables" = c("All Variables"),
            "Individual Variables" = colnames(dataset$data)
        )

        if(isolate(url_parameter_info$currently_updating)) {
            log_message('setting `has_created_filter_controls` to TRUE')

            url_parameter_info$has_created_filter_controls <- TRUE        
        }

        selectInput(inputId='var_plots__filter_controls_selections',
                    label = 'Filters',
                    choices = choices,
                    selected = NULL,
                    multiple = TRUE,
                    selectize = TRUE,
                    width='100%',
                    size = NULL)
    })
}

renderUI__var_plots__filter_bscollapse__UI <- function(input, dataset, filter_controls_list, url_parameter_info) {
 
    renderUI({ 

        log_message_block_start("Creating Filter Controls")

        filter_list <- tagList(list=filter_controls_list())

        if(isolate(url_parameter_info$currently_updating)) {
            log_message('setting `has_displayed_filter_controls` to TRUE')
            url_parameter_info$has_displayed_filter_controls <- TRUE

        }
        return (filter_list)
    })
}

clear_variables <- function(session, input, swap_primary_and_comparison=FALSE) {

    if(swap_primary_and_comparison) {

        current_variable_selected <- input$var_plots__variable
        current_comparison_selected <- input$var_plots__comparison

        updateSelectInput(session, 'var_plots__variable', selected=current_comparison_selected)
        updateSelectInput(session, 'var_plots__comparison', selected=current_variable_selected)

    } else {

        updateSelectInput(session, 'var_plots__variable', selected=global__select_variable)
        updateSelectInput(session, 'var_plots__comparison', selected=global__select_variable_optional)
    }

    updateSelectInput(session, 'var_plots__sum_by_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__count_distinct_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__facet_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__size_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__date_conversion_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__date_cr__snapshots__group_variable', selected=global__select_variable_optional)

    updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=FALSE)
    updateSelectInput(session,
                      'var_plots__numeric_aggregation_function',
                      selected=var_plots__default_values[['var_plots__numeric_aggregation_function']])
    updateSelectInput(session,
                      'var_plots__numeric_aggregation',
                      selected=var_plots__default_values[['var_plots__numeric_aggregation']])
    updateTextInput(session, 'var_plots__multi_value_delimiter', value="")
}

observeEvent__var_plots__variables_buttons_clear_swap <- function(session, input) {

    observeEvent(input$var_plots__variables_buttons_clear, {

        log_message_block_start('Clearing Variables via Button')
        clear_variables(session, input, swap_primary_and_comparison=FALSE)
    })

    observeEvent(input$var_plots__variables_buttons_swap, {

        log_message_block_start('Swapping Variables via Button')
        clear_variables(session, input, swap_primary_and_comparison=TRUE)
    })

    observeEvent(input$var_plots__color_facet_buttons_swap, {

        log_message_block_start('Swapping Color & Facet via Button')
        current_color_selected <- input$var_plots__color_variable
        current_facet_selected <- input$var_plots__facet_variable

        updateSelectInput(session, 'var_plots__color_variable', selected=current_facet_selected)
        updateSelectInput(session, 'var_plots__facet_variable', selected=current_color_selected)
    })
}

##############################################################################################################
# DYNAMICALLY SHOW/HIDE INPUT
##############################################################################################################
hide_show_date_cr_options <- function(session, input) {

    # NOTE other sections use reset_hide_var_plot_option rather than simply hiding, but I want to retain
    # values as I switch back and forth from Snapshots/Adoption

    if(!is.null(input$var_plots__date_conversion_variable) &&
            input$var_plots__date_conversion_variable != global__select_variable_optional) 
    {
        if(input$var_plots__date_cr__plot_type == global__date_cr_options[1]) {

            ###########
            # SNAPSHOTS
            ###########
            shinyjs::show('var_plots__date_cr__snapshots__group_variable')
            shinyjs::show('var_plots__date_cr__snapshots__values')
            shinyjs::show('var_plots__date_cr__snapshots__color_or_facet')

            shinyjs::hide('var_plots__date_cr__last_n_cohorts')
            shinyjs::hide('var_plots__date_cr__n_units_after_first_date')
            shinyjs::hide('var_plots__date_cr__separate_colors')

            shinyjs::show('var_plots__ts_date_break_format')
            shinyjs::show('var_plots__ts_breaks_width')

            # we cannot do YoY when grouping/segmenting, because the group is faceted, and snapshots use color,
            # whereas the YoY relies on color
            if(is.null(input$var_plots__date_cr__snapshots__group_variable) ||
                    input$var_plots__date_cr__snapshots__group_variable == global__select_variable_optional) {

                shinyjs::show('var_plots__year_over_year')

            } else {

                shinyjs::hide('var_plots__year_over_year')            
            }

        } else {

            ###########
            # ADOPTION
            ###########
            shinyjs::show('var_plots__date_cr__last_n_cohorts')
            shinyjs::show('var_plots__date_cr__n_units_after_first_date')
            shinyjs::show('var_plots__date_cr__separate_colors')

            shinyjs::hide('var_plots__date_cr__snapshots__group_variable')
            shinyjs::hide('var_plots__date_cr__snapshots__values')
            shinyjs::hide('var_plots__date_cr__snapshots__color_or_facet')
            helper__show_hide_trend_line(session, input, show=FALSE)
            shinyjs::hide('var_plots__year_over_year')
            shinyjs::hide('var_plots__ts_date_break_format')
            shinyjs::hide('var_plots__ts_breaks_width')
        }
    }
}

helper__show_hide_trend_line <- function(session, input, show=TRUE) {

    if(show) {

        shinyjs::show('var_plots__trend_line')
        if(input$var_plots__trend_line == "Projection") {

            shinyjs::show('var_plots__trend_extend_date')

        } else {

            reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
        }
        shinyjs::show('var_plots__trend_line_se')

    } else {

        reset_hide_var_plot_option(session, 'var_plots__trend_line')
        reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
        reset_hide_var_plot_option(session, 'var_plots__trend_line_se')
    }
}

hide_show_date <- function(session, input) {

    log_message('hide_show_date')
    
    updateSelectInput(session, 'var_plots__comparison', label="Numeric Aggregation")
    shinyjs::show('var_plots__comparison')
    shinyjs::show('var_plots__variables_buttons_clear')
    shinyjs::hide('var_plots__variables_buttons_swap')  # hide because we can't have date as comparison
    shinyjs::show('var_plots__color_facet_buttons_swap')

    shinyjs::show('var_plots__scale_y_log_base_10')
    shinyjs::show('var_plots__y_zoom_min')
    shinyjs::show('var_plots__y_zoom_max')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    shinyjs::show('var_plots__annotate_points')
    shinyjs::show('var_plots__show_points')
    shinyjs::show('var_plots__year_over_year')
    shinyjs::show('var_plots__include_zero_y_axis')
    helper__show_hide_trend_line(session, input)
    shinyjs::show('var_plots__ts_date_floor')
    shinyjs::show('var_plots__ts_date_break_format')
    shinyjs::show('var_plots__ts_breaks_width')

    has_date_conversion_variable <- !is.null(input$var_plots__date_conversion_variable) &&
            input$var_plots__date_conversion_variable != global__select_variable_optional

    has_comparison_variable <- !is.null(input$var_plots__comparison) &&
            input$var_plots__comparison != global__select_variable_optional

    has_color_variable <- !is.null(input$var_plots__color_variable) &&
            input$var_plots__color_variable != global__select_variable_optional

    has_facet_variable <- !is.null(input$var_plots__facet_variable) &&
            input$var_plots__facet_variable != global__select_variable_optional

    
    # if either comparison/color/facet is selected, date_conversion_variable isn't applicable
    if(has_comparison_variable || has_color_variable || has_facet_variable) {

        reset_hide_var_plot_option(session, 'var_plots__date_conversion_variable')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')

    } else {

        shinyjs::show('var_plots__date_conversion_variable')
        if(is.null(input$var_plots__date_conversion_variable) ||
                input$var_plots__date_conversion_variable == global__select_variable_optional) {

            reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')
        } else {

            shinyjs::show('var_plots__date_cr__snapshots__group_variable')
        }
    }

    # if secondary date, don't show color/facet
    if(has_date_conversion_variable) {

        reset_hide_var_plot_option(session, 'var_plots__comparison')
        reset_hide_var_plot_option(session, 'var_plots__color_variable')
        reset_hide_var_plot_option(session, 'var_plots__facet_variable')

        shinyjs::show('var_plots__date_cr__plot_type')
        shinyjs::show('var_plots__date_cr__snapshots__units')

        hide_show_date_cr_options(session, input)

    } else {

        shinyjs::show('var_plots__color_variable')
        shinyjs::show('var_plots__facet_variable')

        reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
        reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
    }

    if(has_comparison_variable) {

        shinyjs::show('var_plots__numeric_aggregation')

    } else {

        reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation')
    }

    reset_hide_var_plot_option(session, 'var_plots__size_variable')
    reset_hide_var_plot_option(session, 'var_plots__label_variables')
    reset_hide_var_plot_option(session, 'var_plots__numeric_group_comp_variable')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_function')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
    reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')
    reset_hide_var_plot_option(session, 'var_plots__transparency')
    reset_hide_var_plot_option(session, 'var_plots__jitter')
    reset_hide_var_plot_option(session, 'var_plots__scale_x_log_base_10')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_min')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_max')
    reset_hide_var_plot_option(session, 'var_plots__histogram_bins')
    reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')
    reset_hide_var_plot_option(session, 'var_plots__show_variable_totals')
    reset_hide_var_plot_option(session, 'var_plots__show_comparison_totals')
    reset_hide_var_plot_option(session, 'var_plots__order_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__categoric_view_type')
    reset_hide_var_plot_option(session, 'var_plots__numeric_graph_type')
    reset_hide_var_plot_option(session, 'var_plots__sum_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__count_distinct_variable')
    reset_hide_var_plot_option(session, 'var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    reset_hide_var_plot_option(session, 'var_plots__map_format')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_database')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_regions')
}

hide_show_numeric_numeric <- function(session,
                                      is_grouping_main_variable,
                                      grouping_is_boxplot,
                                      has_comparison_variable) {

    log_message('hide_show_numeric_numeric')

    updateSelectInput(session, 'var_plots__comparison', label="Secondary Variable")
    shinyjs::show('var_plots__comparison')
    shinyjs::show('var_plots__variables_buttons_clear')
    shinyjs::show('var_plots__variables_buttons_swap')
    shinyjs::hide('var_plots__color_facet_buttons_swap')
    
    # scatterplot; or if grouping the main variable, then boxplot or custom aggregation_function

    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation')
    shinyjs::show('var_plots__numeric_group_comp_variable')

    if(is_grouping_main_variable) {

        shinyjs::show('var_plots__numeric_aggregation_function')
        shinyjs::show('var_plots__numeric_aggregation_count_minimum')

        if(grouping_is_boxplot) {
            
            reset_hide_var_plot_option(session, 'var_plots__show_points')
            reset_hide_var_plot_option(session, 'var_plots__annotate_points')
            reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')

        } else {

            shinyjs::show('var_plots__show_points')
            shinyjs::show('var_plots__annotate_points')
            shinyjs::show('var_plots__numeric_show_resampled_conf_int')
        }
    
        reset_hide_var_plot_option(session, 'var_plots__size_variable')
        reset_hide_var_plot_option(session, 'var_plots__label_variables')
        reset_hide_var_plot_option(session, 'var_plots__color_variable')

        reset_hide_var_plot_option(session, 'var_plots__map_format')
        reset_hide_var_plot_option(session, 'var_plots__map_borders_database')
        reset_hide_var_plot_option(session, 'var_plots__map_borders_regions')

        reset_hide_var_plot_option(session, 'var_plots__transparency')
        reset_hide_var_plot_option(session, 'var_plots__jitter')

        reset_hide_var_plot_option(session, 'var_plots__trend_line')
        reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
        reset_hide_var_plot_option(session, 'var_plots__trend_line_se')

        updateCollapse(session, 'var_plots__bscollapse', close="Map Options")

    } else {

        reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_function')
        reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
        reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')
        reset_hide_var_plot_option(session, 'var_plots__annotate_points')

        shinyjs::show('var_plots__size_variable')
        shinyjs::show('var_plots__label_variables')
        shinyjs::show('var_plots__color_variable')
        shinyjs::show('var_plots__map_format')
        shinyjs::show('var_plots__map_borders_database')
        shinyjs::show('var_plots__map_borders_regions')
        updateCollapse(session, 'var_plots__bscollapse', open="Map Options")

        shinyjs::show('var_plots__transparency')
        shinyjs::show('var_plots__jitter')

        shinyjs::show('var_plots__trend_line')
        reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
        shinyjs::show('var_plots__trend_line_se')
        reset_hide_var_plot_option(session, 'var_plots__show_points')
    }

    shinyjs::show('var_plots__scale_x_log_base_10')
    shinyjs::show('var_plots__x_zoom_min')
    shinyjs::show('var_plots__x_zoom_max')
    shinyjs::show('var_plots__scale_y_log_base_10')
    shinyjs::show('var_plots__y_zoom_min')
    shinyjs::show('var_plots__y_zoom_max')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    
    reset_hide_var_plot_option(session, 'var_plots__facet_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_conversion_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
    reset_hide_var_plot_option(session, 'var_plots__year_over_year')
    reset_hide_var_plot_option(session, 'var_plots__include_zero_y_axis')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_floor')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_break_format')
    reset_hide_var_plot_option(session, 'var_plots__ts_breaks_width')
    reset_hide_var_plot_option(session, 'var_plots__histogram_bins')
    reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')
    reset_hide_var_plot_option(session, 'var_plots__show_variable_totals')
    reset_hide_var_plot_option(session, 'var_plots__show_comparison_totals')
    reset_hide_var_plot_option(session, 'var_plots__order_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__categoric_view_type')
    reset_hide_var_plot_option(session, 'var_plots__numeric_graph_type')
    reset_hide_var_plot_option(session, 'var_plots__sum_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__count_distinct_variable')
    reset_hide_var_plot_option(session, 'var_plots__multi_value_delimiter')
}

hide_show_numeric_categoric <- function(session, showing_boxplot, has_comparison_variable) {
    
    log_message('hide_show_numeric_categoric')

    updateSelectInput(session, 'var_plots__comparison', label="Secondary Variable")

    shinyjs::show('var_plots__comparison')
    shinyjs::show('var_plots__variables_buttons_clear')
    shinyjs::show('var_plots__variables_buttons_swap')
    shinyjs::hide('var_plots__color_facet_buttons_swap')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(showing_boxplot) {

        shinyjs::show('var_plots__scale_y_log_base_10')
        shinyjs::show('var_plots__y_zoom_min')
        shinyjs::show('var_plots__y_zoom_max')
        reset_hide_var_plot_option(session, 'var_plots__histogram_bins')
        reset_hide_var_plot_option(session, 'var_plots__scale_x_log_base_10')
        reset_hide_var_plot_option(session, 'var_plots__x_zoom_min')
        reset_hide_var_plot_option(session, 'var_plots__x_zoom_max')

        if(has_comparison_variable) {

            shinyjs::show('var_plots__color_variable')
            shinyjs::show('var_plots__order_by_variable')

        } else {

            reset_hide_var_plot_option(session, 'var_plots__color_variable')
            reset_hide_var_plot_option(session, 'var_plots__order_by_variable')
        }

    } else {

        shinyjs::show('var_plots__histogram_bins')
        shinyjs::show('var_plots__scale_x_log_base_10')
        shinyjs::show('var_plots__x_zoom_min')
        shinyjs::show('var_plots__x_zoom_max')
        reset_hide_var_plot_option(session, 'var_plots__scale_y_log_base_10')
        reset_hide_var_plot_option(session, 'var_plots__y_zoom_min')
        reset_hide_var_plot_option(session, 'var_plots__y_zoom_max')
        reset_hide_var_plot_option(session, 'var_plots__color_variable')
    }

    reset_hide_var_plot_option(session, 'var_plots__facet_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_conversion_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
    reset_hide_var_plot_option(session, 'var_plots__year_over_year')
    reset_hide_var_plot_option(session, 'var_plots__include_zero_y_axis')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation')
    reset_hide_var_plot_option(session, 'var_plots__size_variable')
    reset_hide_var_plot_option(session, 'var_plots__label_variables')
    reset_hide_var_plot_option(session, 'var_plots__numeric_group_comp_variable')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_function')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
    reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')

    shinyjs::show('var_plots__base_size')
    reset_hide_var_plot_option(session, 'var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    shinyjs::show('var_plots__numeric_graph_type')

    reset_hide_var_plot_option(session, 'var_plots__transparency')
    reset_hide_var_plot_option(session, 'var_plots__jitter')
    reset_hide_var_plot_option(session, 'var_plots__trend_line')
    reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
    reset_hide_var_plot_option(session, 'var_plots__trend_line_se')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_floor')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_break_format')
    reset_hide_var_plot_option(session, 'var_plots__ts_breaks_width')
    reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')
    reset_hide_var_plot_option(session, 'var_plots__show_variable_totals')
    reset_hide_var_plot_option(session, 'var_plots__show_comparison_totals')
    reset_hide_var_plot_option(session, 'var_plots__categoric_view_type')
    reset_hide_var_plot_option(session, 'var_plots__annotate_points')
    reset_hide_var_plot_option(session, 'var_plots__show_points')
    reset_hide_var_plot_option(session, 'var_plots__sum_by_variable')
    reset_hide_var_plot_option(session, 'var_plots__count_distinct_variable')
    reset_hide_var_plot_option(session, 'var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    reset_hide_var_plot_option(session, 'var_plots__map_format')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_database')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_regions')
}

hide_show_categoric_categoric <- function(session, input, has_comparison_variable) {

    log_message('hide_show_categoric_categoric')

    updateSelectInput(session, 'var_plots__comparison', label="Secondary Variable")
    shinyjs::show('var_plots__comparison')
    shinyjs::show('var_plots__variables_buttons_clear')
    shinyjs::show('var_plots__variables_buttons_swap')
    shinyjs::hide('var_plots__color_facet_buttons_swap')
    
    ####
    # Can only use SUM BY varaible or COUNT DISTINCT variable, so hide one of the other is not null
    ####
    if(is.null(input$var_plots__sum_by_variable) || input$var_plots__sum_by_variable == global__select_variable_optional) {

        shinyjs::show('var_plots__count_distinct_variable')

    } else {

        reset_hide_var_plot_option(session, 'var_plots__count_distinct_variable')

    }
    if(is.null(input$var_plots__count_distinct_variable) || input$var_plots__count_distinct_variable == global__select_variable_optional) {

        shinyjs::show('var_plots__sum_by_variable')

    } else {

        reset_hide_var_plot_option(session, 'var_plots__sum_by_variable')

    }

    shinyjs::show('var_plots__multi_value_delimiter')

    if(is.null(input$var_plots__comparison) || input$var_plots__comparison == global__select_variable_optional) {

        reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')

    } else {

        if(str_detect(string=input$var_plots__categoric_view_type, pattern='Stack')) {
            
            shinyjs::show('var_plots__reverse_stack_order')

        } else {

            reset_hide_var_plot_option(session, 'var_plots__reverse_stack_order')
        }
    }

    reset_hide_var_plot_option(session, 'var_plots__facet_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_conversion_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
    reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
    reset_hide_var_plot_option(session, 'var_plots__year_over_year')
    reset_hide_var_plot_option(session, 'var_plots__include_zero_y_axis')
    reset_hide_var_plot_option(session, 'var_plots__size_variable')
    reset_hide_var_plot_option(session, 'var_plots__label_variables')
    reset_hide_var_plot_option(session, 'var_plots__numeric_group_comp_variable')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_function')
    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
    reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')
    reset_hide_var_plot_option(session, 'var_plots__color_variable')

    shinyjs::show('var_plots__categoric_view_type')
    shinyjs::show('var_plots__show_variable_totals')
    shinyjs::show('var_plots__show_comparison_totals')
    shinyjs::show('var_plots__order_by_variable')
    shinyjs::show('var_plots__base_size')
    reset_hide_var_plot_option(session, 'var_plots__vertical_annotations')
    reset_hide_var_plot_option(session, 'var_plots__horizontal_annotations')

    reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation')
    reset_hide_var_plot_option(session, 'var_plots__scale_x_log_base_10')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_min')
    reset_hide_var_plot_option(session, 'var_plots__x_zoom_max')
    reset_hide_var_plot_option(session, 'var_plots__scale_y_log_base_10')
    reset_hide_var_plot_option(session, 'var_plots__y_zoom_min')
    reset_hide_var_plot_option(session, 'var_plots__y_zoom_max')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)

    reset_hide_var_plot_option(session, 'var_plots__transparency')
    reset_hide_var_plot_option(session, 'var_plots__jitter')
    reset_hide_var_plot_option(session, 'var_plots__trend_line')
    reset_hide_var_plot_option(session, 'var_plots__trend_extend_date')
    reset_hide_var_plot_option(session, 'var_plots__trend_line_se')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_floor')
    reset_hide_var_plot_option(session, 'var_plots__ts_date_break_format')
    reset_hide_var_plot_option(session, 'var_plots__ts_breaks_width')
    reset_hide_var_plot_option(session, 'var_plots__histogram_bins')
    reset_hide_var_plot_option(session, 'var_plots__numeric_graph_type')
    reset_hide_var_plot_option(session, 'var_plots__annotate_points')
    reset_hide_var_plot_option(session, 'var_plots__show_points')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    reset_hide_var_plot_option(session, 'var_plots__map_format')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_database')
    reset_hide_var_plot_option(session, 'var_plots__map_borders_regions')
}

observe__var_plots__hide_show_uncollapse_on_primary_vars <- function(session, input) {
    observeEvent(input$var_plots__variable, {

        req(input$var_plots__variable)

        local_primary_variable <- input$var_plots__variable

        if(local_primary_variable == global__select_variable) {

            log_message_block_start("Hiding Variables from default var_plots__variable")

            shinyjs::hide('var_plots__variables_buttons_clear')
            shinyjs::hide('var_plots__variables_buttons_swap') 
            shinyjs::hide('var_plots__color_facet_buttons_swap')

            reset_hide_var_plot_option(session, 'var_plots__comparison')
            reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation')
            reset_hide_var_plot_option(session, 'var_plots__sum_by_variable')
            reset_hide_var_plot_option(session, 'var_plots__count_distinct_variable')
            reset_hide_var_plot_option(session, 'var_plots__multi_value_delimiter')
            reset_hide_var_plot_option(session, 'var_plots__size_variable')
            reset_hide_var_plot_option(session, 'var_plots__numeric_group_comp_variable')
            reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_function')
            reset_hide_var_plot_option(session, 'var_plots__numeric_aggregation_count_minimum')
            reset_hide_var_plot_option(session, 'var_plots__numeric_show_resampled_conf_int')
            reset_hide_var_plot_option(session, 'var_plots__color_variable')
            reset_hide_var_plot_option(session, 'var_plots__facet_variable')
            reset_hide_var_plot_option(session, 'var_plots__date_conversion_variable')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__group_variable')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__plot_type')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__values')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__units')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__snapshots__color_or_facet')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__last_n_cohorts')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__n_units_after_first_date')
            reset_hide_var_plot_option(session, 'var_plots__date_cr__separate_colors')
            reset_hide_var_plot_option(session, 'var_plots__year_over_year')
            reset_hide_var_plot_option(session, 'var_plots__include_zero_y_axis')

        } else {

            updateCollapse(session, 'var_plots__bscollapse', open='Graph Options')
        }
    }, ignoreInit=TRUE)
}

##############################################################################################################
# OUTPUT
##############################################################################################################
renderPlot__variable_plot <- function(session, ggplot_object, messages) {

    renderPlot({

        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(ggplot_object()))
           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_var_plots_width * global__golden_ratio  # set height to % of width
    })
}

renderPrint__reactiveValues__vp__ggplot_message <- function(message) {

    renderPrint({
        cat(message$value)
    })
}

#' @param reactive_filter_message_list is a reactive object, whos' `value` is list object that contains a
#' string value for each variable being filtered
renderPrint__reactiveValues__vp_filtering_message <- function(reactive_filter_message_list, dataset) {

    renderPrint({
        cat(format_filtering_message(reactive_filter_message_list$value, dataset()))
    })
}

#' @param filter_message_list list object that contains a string value for each variable being filtered
format_filtering_message <- function(filter_message_list, dataset) {

    message <- NULL
    
    if(!is.null(filter_message_list) && length(filter_message_list) != 0) {

        message <- paste0("Filtering Variables:\n\n", paste0(filter_message_list, collapse="\n"))

        message <- paste0(message, "\n\n", my_number_format(nrow(dataset)), " Records Remaining")
    }

    return (message)
}


##############################################################################################################
# Functions for Updating from URL Parameters
##############################################################################################################
#' @param session session from the app server
#' @param params named list of url params with corresponding values; should only be var_plot__ params
update_var_plot_variables_from_url_params <- function(session, params, dataset, input) {

    # the variables that are dynamic based on the dataset will not have been loaded
    # and will need to be initialized with the list of choices
    # for some variables, the choices are based on other variables
    # so we need to manually build them up. 

    column_names <- colnames(dataset)
    numeric_column_names <- colnames(dataset %>% select_if(is.numeric))
    categoric_column_names <- colnames(dataset %>% select_if(is_categoric))

    #######################################################################
    # Update Primary Variable - cache selected for comparison/color logic
    #######################################################################
    selected_variable <- global__select_variable
    if (!is.null(params[['var_plots__variable']])) {

        selected_variable <- params[['var_plots__variable']]
        log_message_variable('updating variable', params[['var_plots__variable']])
    }
    updateSelectInput(session, 'var_plots__variable',
                      choices=c(global__select_variable, column_names),
                      selected=selected_variable)   
    
    #######################################################################
    # Update Comparison Variable - cache selected for color logic
    #######################################################################
    selected_comparison <- global__select_variable_optional
    if (!is.null(params[['var_plots__comparison']])) {

        selected_comparison <- params[['var_plots__comparison']]
        log_message_variable('updating comparison', params[['var_plots__comparison']])
    }
    results <- var_plots__comparison__logic(dataset=dataset,
                                            primary_variable=selected_variable,
                                            current_value=selected_comparison)
    updateSelectInput(session, 'var_plots__comparison',
                      choices=results$choices,
                      selected=results$selected)
    
    #######################################################################
    # Update Color Variable
    #######################################################################
    selected_color <- global__select_variable_optional
    if (!is.null(params[['var_plots__color_variable']])) {

        selected_color <- params[['var_plots__color_variable']]
        log_message_variable('updating color_variable', params[['var_plots__color_variable']])
    }
    results <- var_plots__color__logic(dataset=dataset,
                                            primary_variable=selected_variable,
                                            comparison_variable=selected_comparison,
                                            current_value=selected_color)
    updateSelectInput(session, 'var_plots__color_variable',
                      choices=results$choices,
                      selected=results$selected)


    #######################################################################
    # Update Date Conversion Variable
    #######################################################################
    selected_date_conversion_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__date_conversion_variable']])) {

        selected_date_conversion_variable <- params[['var_plots__date_conversion_variable']]
        log_message_variable('updating date_conversion_variable', params[['var_plots__date_conversion_variable']])
    }
    results <- var_plots__date_conversion_variable__logic(dataset=dataset,
                                                          primary_variable=selected_variable,
                                                          current_value=selected_date_conversion_variable)
    updateSelectInput(session, 'var_plots__date_conversion_variable',
                      choices=results$choices,
                      selected=results$selected)

    #######################################################################
    # Update Sum-By-Variable
    #######################################################################
    selected_sum_by_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__sum_by_variable']])) {

        selected_sum_by_variable <- params[['var_plots__sum_by_variable']]
        log_message_variable('updating sum_by_variable', params[['var_plots__sum_by_variable']])
    }
    updateSelectInput(session, 'var_plots__sum_by_variable',
                      choices=c(global__select_variable_optional, numeric_column_names),
                      selected=selected_sum_by_variable)

    selected_count_distinct_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__count_distinct_variable']])) {

        selected_count_distinct_variable <- params[['var_plots__count_distinct_variable']]
        log_message_variable('updating count_distinct_variable', params[['var_plots__count_distinct_variable']])
    }
    updateSelectInput(session, 'var_plots__count_distinct_variable',
                      choices=c(global__select_variable_optional, categoric_column_names),
                      selected=selected_count_distinct_variable)

    #######################################################################
    # Update Categoric View
    #######################################################################
    if (!is.null(params[['var_plots__categoric_view_type']])) {
        log_message_variable('updating categoric_view_type', params[['var_plots__categoric_view_type']])
        updateSelectInput(session, 'var_plots__categoric_view_type', selected=params[['var_plots__categoric_view_type']])
    }
    selected_value <- "Bar"
    if (!is.null(params[['var_plots__categoric_view_type']])) {

        selected_value <- params[['var_plots__categoric_view_type']]
        log_message_variable('updating categoric_view_type', params[['var_plots__categoric_view_type']])
    }
    results <- var_plots__categoric_view_type__logic(dataset=dataset,
                                                     comparison_variable=selected_comparison,
                                                     sum_by_variable=selected_sum_by_variable,
                                                     count_distinct_variable=selected_count_distinct_variable,
                                                     current_value=selected_value)
    updateSelectInput(session, 'var_plots__categoric_view_type',
                      choices=results$choices,
                      selected=results$selected)

    #######################################################################
    # Update Other Dynamic values that don't depend on other variables
    #######################################################################
    selected_facet_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__facet_variable']])) {

        selected_facet_variable <- params[['var_plots__facet_variable']]
        log_message_variable('updating facet_variable', params[['var_plots__facet_variable']])
    }
    updateSelectInput(session, 'var_plots__facet_variable',
                      choices=c(global__select_variable_optional, categoric_column_names),
                      selected=selected_facet_variable)

    selected_size_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__size_variable']])) {

        selected_size_variable <- params[['var_plots__size_variable']]
        log_message_variable('updating size_variable', params[['var_plots__size_variable']])
    }
    updateSelectInput(session, 'var_plots__size_variable',
                      choices=c(global__select_variable_optional, column_names),
                      selected=selected_size_variable)

    selected_cr_group_variable <- global__select_variable_optional
    if (!is.null(params[['var_plots__date_cr__snapshots__group_variable']])) {

        selected_cr_group_variable <- params[['var_plots__date_cr__snapshots__group_variable']]
        log_message_variable('updating cr_group_variable', params[['var_plots__date_cr__snapshots__group_variable']])
    }
    updateSelectInput(session, 'var_plots__date_cr__snapshots__group_variable',
                      choices=c(global__select_variable_optional, categoric_column_names),
                      selected=selected_cr_group_variable)

    selected_label_variables <- NULL
    if (!is.null(params[['var_plots__label_variables']])) {

        selected_label_variables <- params[['var_plots__label_variables']]
        log_message_variable('updating label_variables', paste0(params[['var_plots__label_variables']], collapse="; "))
    }
    updateSelectInput(session, 'var_plots__label_variables',
                      choices=column_names,
                      selected=selected_label_variables)

    selected_order_by_variable <- "Default"
    if (!is.null(params[['var_plots__order_by_variable']])) {

        selected_order_by_variable <- params[['var_plots__order_by_variable']]
        log_message_variable('updating order_by_variable', params[['var_plots__order_by_variable']])
    }
    updateSelectInput(session, 'var_plots__order_by_variable',
                      choices=c("Default", "Frequency", numeric_column_names),
                      selected=selected_order_by_variable)

    #######################################################################
    # Update Non-Dynamic
    # These should already have `choices` defined in UI
    #######################################################################
    if (!is.null(params[['var_plots__numeric_group_comp_variable']])) {

        log_message_variable('updating numeric_group_comp_variable', params[['var_plots__numeric_group_comp_variable']])
        updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=params[['var_plots__numeric_group_comp_variable']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation_function']])) {

        log_message_variable('updating numeric_aggregation_function', params[['var_plots__numeric_aggregation_function']])
        updateSelectInput(session, 'var_plots__numeric_aggregation_function', selected=params[['var_plots__numeric_aggregation_function']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation']])) {

        log_message_variable('updating numeric_aggregation', params[['var_plots__numeric_aggregation']])
        updateSelectInput(session, 'var_plots__numeric_aggregation', selected=params[['var_plots__numeric_aggregation']])
    }
    if (!is.null(params[['var_plots__multi_value_delimiter']])) {

        log_message_variable('updating multi_value_delimiter', params[['var_plots__multi_value_delimiter']])
        updateTextInput(session, 'var_plots__multi_value_delimiter', value=params[['var_plots__multi_value_delimiter']])
    }
    if (!is.null(params[['var_plots__filter_factor_lump_number']])) {

        # this object is actually a string, not a number, because "Off" can be chosen, and setting the
        # control as a number doesn't work.
        lump_number_string <- as.character(params[['var_plots__filter_factor_lump_number']])
        log_message_variable('updating filter_factor_lump_number', lump_number_string)
        # perhaps its a bug, but it seems like for all the updateSliderTextInput controls I have to 
        # pass choices as well
        updateSliderTextInput(session,
                              'var_plots__filter_factor_lump_number',
                              choices=as.character(c("Off", seq(1, 10), seq(15, 50, 5))),
                              selected=lump_number_string)
    }
    if (!is.null(params[['var_plots__annotate_points']])) {
        log_message_variable('updating annotate_points', params[['var_plots__annotate_points']])
        updateCheckboxInput(session, 'var_plots__annotate_points', value=params[['var_plots__annotate_points']])
    }
    if (!is.null(params[['var_plots__show_points']])) {
        log_message_variable('updating show_points', params[['var_plots__show_points']])
        updateCheckboxInput(session, 'var_plots__show_points', value=params[['var_plots__show_points']])
    }
    if (!is.null(params[['var_plots__date_cr__plot_type']])) {
        log_message_variable('updating date_cr__plot_type', params[['var_plots__date_cr__plot_type']])
        updateSelectInput(session, 'var_plots__date_cr__plot_type', selected=params[['var_plots__date_cr__plot_type']])
    }
    if (!is.null(params[['var_plots__date_cr__snapshots__values']])) {
        log_message_variable('updating date_cr__snapshots__values', params[['var_plots__date_cr__snapshots__values']])
        updateTextInput(session, 'var_plots__date_cr__snapshots__values', value=params[['var_plots__date_cr__snapshots__values']])
    }
    if (!is.null(params[['var_plots__date_cr__snapshots__units']])) {
        log_message_variable('updating date_cr__snapshots__units', params[['var_plots__date_cr__snapshots__units']])
        updateSelectInput(session, 'var_plots__date_cr__snapshots__units', selected=params[['var_plots__date_cr__snapshots__units']])
    }
    if (!is.null(params[['var_plots__date_cr__snapshots__color_or_facet']])) {
        log_message_variable('updating date_cr__snapshots__color_or_facet', params[['var_plots__date_cr__snapshots__color_or_facet']])
        updateRadioButtons(session, 'var_plots__date_cr__snapshots__color_or_facet', selected=params[['var_plots__date_cr__snapshots__color_or_facet']])
    }
    if (!is.null(params[['var_plots__date_cr__last_n_cohorts']])) {
        log_message_variable('updating date_cr__last_n_cohorts', params[['var_plots__date_cr__last_n_cohorts']])
        updateSliderInput(session, 'var_plots__date_cr__last_n_cohorts', value=params[['var_plots__date_cr__last_n_cohorts']])
    }
    if (!is.null(params[['var_plots__date_cr__n_units_after_first_date']])) {
        log_message_variable('updating date_cr__n_units_after_first_date', params[['var_plots__date_cr__n_units_after_first_date']])
        updateSliderInput(session, 'var_plots__date_cr__n_units_after_first_date', value=params[['var_plots__date_cr__n_units_after_first_date']])
    }
    if (!is.null(params[['var_plots__date_cr__separate_colors']])) {
        log_message_variable('updating date_cr__separate_colors', params[['var_plots__date_cr__separate_colors']])
        updateCheckboxInput(session, 'var_plots__date_cr__separate_colors', value=params[['var_plots__date_cr__separate_colors']])
    }
    if (!is.null(params[['var_plots__year_over_year']])) {
        log_message_variable('updating year_over_year', params[['var_plots__year_over_year']])
        updateCheckboxInput(session, 'var_plots__year_over_year', value=params[['var_plots__year_over_year']])
    }
    if (!is.null(params[['var_plots__include_zero_y_axis']])) {
        log_message_variable('updating include_zero_y_axis', params[['var_plots__include_zero_y_axis']])
        updateCheckboxInput(session, 'var_plots__include_zero_y_axis', value=params[['var_plots__include_zero_y_axis']])
    }
    if (!is.null(params[['var_plots__numeric_graph_type']])) {
        log_message_variable('updating numeric_graph_type', params[['var_plots__numeric_graph_type']])
        updateSelectInput(session, 'var_plots__numeric_graph_type', selected=params[['var_plots__numeric_graph_type']])
    }
    if (!is.null(params[['var_plots__reverse_stack_order']])) {
        log_message_variable('updating show_variable_totals', params[['var_plots__reverse_stack_order']])
        updateCheckboxInput(session, 'var_plots__reverse_stack_order', value=params[['var_plots__reverse_stack_order']])
    }
    if (!is.null(params[['var_plots__show_variable_totals']])) {
        log_message_variable('updating show_variable_totals', params[['var_plots__show_variable_totals']])
        updateCheckboxInput(session, 'var_plots__show_variable_totals', value=params[['var_plots__show_variable_totals']])
    }
    if (!is.null(params[['var_plots__show_comparison_totals']])) {
        log_message_variable('updating show_comparison_totals', params[['var_plots__show_comparison_totals']])
        updateCheckboxInput(session, 'var_plots__show_comparison_totals', value=params[['var_plots__show_comparison_totals']])
    }
    if (!is.null(params[['var_plots__histogram_bins']])) {
        log_message_variable('updating histogram_bins', params[['var_plots__histogram_bins']])
        updateNumericInput(session, 'var_plots__histogram_bins', value=params[['var_plots__histogram_bins']])
    }
    if (!is.null(params[['var_plots__transparency']])) {
        log_message_variable('updating transparency', params[['var_plots__transparency']])
        updateSliderTextInput(session, 'var_plots__transparency',
                                            choices=c(seq(0, 90, 10), 99),
                                            selected=params[['var_plots__transparency']])
    }
    if (!is.null(params[['var_plots__jitter']])) {
        log_message_variable('updating jitter', params[['var_plots__jitter']])
        updateCheckboxInput(session, 'var_plots__jitter', value=params[['var_plots__jitter']])
    }
    if (!is.null(params[['var_plots__numeric_aggregation_count_minimum']])) {
        log_message_variable('updating numeric_aggregation_count_minimum', params[['var_plots__numeric_aggregation_count_minimum']])
        updateNumericInput(session, 'var_plots__numeric_aggregation_count_minimum', value=params[['var_plots__numeric_aggregation_count_minimum']])
    }
    if (!is.null(params[['var_plots__numeric_show_resampled_conf_int']])) {
        log_message_variable('updating numeric_show_resampled_conf_int', params[['var_plots__numeric_show_resampled_conf_int']])
        updateCheckboxInput(session, 'var_plots__numeric_show_resampled_conf_int', value=params[['var_plots__numeric_show_resampled_conf_int']])
    }
    if (!is.null(params[['var_plots__trend_line']])) {
        log_message_variable('updating trend_line', params[['var_plots__trend_line']])
        updateRadioButtons(session, 'var_plots__trend_line', selected=params[['var_plots__trend_line']])
    }
    if (!is.null(params[['var_plots__trend_extend_date']])) {
        log_message_variable('updating trend_extend_date', params[['var_plots__trend_extend_date']])
        updateDateInput(session, inputId='var_plots__trend_extend_date', value = params[['var_plots__trend_extend_date']])
    }
    if (!is.null(params[['var_plots__trend_line_se']])) {
        log_message_variable('updating trend_line_se', params[['var_plots__trend_line_se']])
        updateRadioButtons(session, 'var_plots__trend_line_se', selected=params[['var_plots__trend_line_se']])
    }
    if (!is.null(params[['var_plots__ts_date_floor']])) {
        log_message_variable('updating ts_date_floor', params[['var_plots__ts_date_floor']])
        updateSelectInput(session, 'var_plots__ts_date_floor', selected=params[['var_plots__ts_date_floor']])
    }
    if (!is.null(params[['var_plots__ts_date_break_format']])) {
        log_message_variable('updating ts_date_break_format', params[['var_plots__ts_date_break_format']])
        updateSelectInput(session, 'var_plots__ts_date_break_format', selected=params[['var_plots__ts_date_break_format']])
    }
    if (!is.null(params[['var_plots__ts_breaks_width']])) {
        log_message_variable('updating ts_breaks_width', params[['var_plots__ts_breaks_width']])
        updateTextInput(session, 'var_plots__ts_breaks_width', value=params[['var_plots__ts_breaks_width']])
    }
    if (!is.null(params[['var_plots__scale_x_log_base_10']])) {
        log_message_variable('updating scale_x_log_base_10', params[['var_plots__scale_x_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=params[['var_plots__scale_x_log_base_10']])
    }
    if (!is.null(params[['var_plots__x_zoom_min']])) {
        log_message_variable('updating x_zoom_min', params[['var_plots__x_zoom_min']])
        updateNumericInput(session, 'var_plots__x_zoom_min', value=params[['var_plots__x_zoom_min']])
    }
    if (!is.null(params[['var_plots__x_zoom_max']])) {
        log_message_variable('updating x_zoom_max', params[['var_plots__x_zoom_max']])
        updateNumericInput(session, 'var_plots__x_zoom_max', value=params[['var_plots__x_zoom_max']])
    }
    if (!is.null(params[['var_plots__scale_y_log_base_10']])) {
        log_message_variable('updating scale_y_log_base_10', params[['var_plots__scale_y_log_base_10']])
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=params[['var_plots__scale_y_log_base_10']])
    }
    if (!is.null(params[['var_plots__y_zoom_min']])) {
        log_message_variable('updating y_zoom_min', params[['var_plots__y_zoom_min']])
        updateNumericInput(session, 'var_plots__y_zoom_min', value=params[['var_plots__y_zoom_min']])
    }
    if (!is.null(params[['var_plots__y_zoom_max']])) {
        log_message_variable('updating y_zoom_max', params[['var_plots__y_zoom_max']])
        updateNumericInput(session, 'var_plots__y_zoom_max', value=params[['var_plots__y_zoom_max']])
    }
    if (!is.null(params[['var_plots__custom_title']])) {
        log_message_variable('updating custom_title', params[['var_plots__custom_title']])
        updateTextInput(session, 'var_plots__custom_title', value=params[['var_plots__custom_title']])
    }
    if (!is.null(params[['var_plots__custom_subtitle']])) {
        log_message_variable('updating custom_subtitle', params[['var_plots__custom_subtitle']])
        updateTextInput(session, 'var_plots__custom_subtitle', value=params[['var_plots__custom_subtitle']])
    }
    if (!is.null(params[['var_plots__custom_x_axis_label']])) {
        log_message_variable('updating custom_x_axis_label', params[['var_plots__custom_x_axis_label']])
        updateTextInput(session, 'var_plots__custom_x_axis_label', value=params[['var_plots__custom_x_axis_label']])
    }
    if (!is.null(params[['var_plots__custom_y_axis_label']])) {
        log_message_variable('updating custom_y_axis_label', params[['var_plots__custom_y_axis_label']])
        updateTextInput(session, 'var_plots__custom_y_axis_label', value=params[['var_plots__custom_y_axis_label']])
    }
    if (!is.null(params[['var_plots__custom_caption']])) {
        log_message_variable('updating custom_caption', params[['var_plots__custom_caption']])
        updateTextInput(session, 'var_plots__custom_caption', value=params[['var_plots__custom_caption']])
    }
    if (!is.null(params[['var_plots__custom_tag']])) {
        log_message_variable('updating custom_tag', params[['var_plots__custom_tag']])
        updateTextInput(session, 'var_plots__custom_tag', value=params[['var_plots__custom_tag']])
    }
    if (!is.null(params[['var_plots__pretty_text']])) {
        log_message_variable('updating pretty_text', params[['var_plots__pretty_text']])
        updateCheckboxInput(session, 'var_plots__pretty_text', value=params[['var_plots__pretty_text']])
    }
    if (!is.null(params[['var_plots__base_size']])) {
        log_message_variable('updating base_size', params[['var_plots__base_size']])
        updateSliderTextInput(session,
                      'var_plots__base_size',
                      choices=seq(6, 20, 1),
                      selected=params[['var_plots__base_size']])
    }
    if (!is.null(params[['var_plots__vertical_annotations']])) {
        log_message_variable('updating vertical_annotations', params[['var_plots__vertical_annotations']])
        updateTextAreaInput(session, 'var_plots__vertical_annotations', value=params[['var_plots__vertical_annotations']])
    }
    if (!is.null(params[['var_plots__horizontal_annotations']])) {
        log_message_variable('updating horizontal_annotations', params[['var_plots__horizontal_annotations']])
        updateTextAreaInput(session, 'var_plots__horizontal_annotations', value=params[['var_plots__horizontal_annotations']])
    }
}
