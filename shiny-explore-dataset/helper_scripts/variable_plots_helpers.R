library(scales)
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

        withProgress(value=1/2, message='Generating Filters', {
            
            ui_list <- imap(dataset$data, ~ {

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

        updateTextInput(session, 'var_plots__custom_title', value='')
        updateTextInput(session, 'var_plots__custom_subtitle', value='')
        updateTextInput(session, 'var_plots__custom_x_axis_label', value='')
        updateTextInput(session, 'var_plots__custom_y_axis_label', value='')
        updateTextInput(session, 'var_plots__custom_caption', value='')
        updateTextInput(session, 'var_plots__custom_tag', value='')
        updateCheckboxInput(session, 'var_plots__pretty_text', value=FALSE)
        updateSliderTextInput(session, 'var_plots__base_size', selected=15)
        updateTextAreaInput(session, 'var_plots__vertical_annotations', value="")
        updateTextAreaInput(session, 'var_plots__horizontal_annotations', value="")

        # even though I call updateTextInput before click, the values haven't been reset yet
        # click('var_plots__custom_labels_apply')
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

        updateSliderTextInput(session,
                              'var_plots__filter_factor_lump_number',
                              choices=as.character(c("Off", seq(1, 10), seq(15, 50, 5))),
                              selected="10")
        updateSelectInput(session, 'var_plots__label_variables', selected=character(0))
        updateCheckboxInput(session, 'var_plots__annotate_points', value=FALSE)
        updateCheckboxInput(session, 'var_plots__show_points', value=FALSE)
        updateCheckboxInput(session, 'var_plots__year_over_year', value=FALSE)
        updateCheckboxInput(session, 'var_plots__include_zero_y_axis', value=TRUE)
        updateSelectInput(session, 'var_plots__numeric_graph_type', selected="Boxplot")
        updateSelectInput(session, 'var_plots__categoric_view_type', selected="Bar")
        updateSelectInput(session, 'var_plots__order_by_variable', selected="Default")
        updateCheckboxInput(session, 'var_plots__show_variable_totals', value=TRUE)
        updateCheckboxInput(session, 'var_plots__show_comparison_totals', value=TRUE)
        updateNumericInput(session, 'var_plots__histogram_bins', value=30)
        updateSliderTextInput(session, 'var_plots__transparency', selected=60)
        updateCheckboxInput(session, 'var_plots__jitter', value=FALSE)
        updateNumericInput(session, 'var_plots__numeric_aggregation_count_minimum', value=30)
        updateCheckboxInput(session, 'var_plots__numeric_show_resampled_conf_int', value=FALSE)
        updateRadioButtons(session, 'var_plots__trend_line', selected='None')
        updateRadioButtons(session, 'var_plots__trend_line_se', selected='Yes')
        updateSelectInput(session, 'var_plots__ts_date_floor', selected=names(global__date_part_vector)[1])
        updateSelectInput(session, 'var_plots__ts_date_break_format', selected=names(global__date_break_format_vector)[1])
        updateTextInput(session, 'var_plots__ts_breaks_width', value=integer(0))
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
        updateNumericInput(session, 'var_plots__x_zoom_min', value=integer(0))
        updateNumericInput(session, 'var_plots__x_zoom_max', value=integer(0))
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)
        updateNumericInput(session, 'var_plots__y_zoom_min', value=integer(0))
        updateNumericInput(session, 'var_plots__y_zoom_max', value=integer(0))
    })
}

hide_graph_options <- function(input) {

    shinyjs::hide('var_plots__filter_factor_lump_number')
    shinyjs::hide('var_plots__label_variables')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    shinyjs::hide('var_plots__year_over_year')
    shinyjs::hide('var_plots__include_zero_y_axis')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__categoric_view_type')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__order_by_variable')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_show_resampled_conf_int')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('div_var_plots__group_y_zoom_controls')
}

observeEvent__var_plots__graph_options__any_used <- function(input, session) {

    observeEvent(c(# any of these will trigger the graph options color change
                   input$var_plots__filter_factor_lump_number,
                   input$var_plots__label_variables,
                   input$var_plots__annotate_points,
                   input$var_plots__show_points,
                   input$var_plots__year_over_year,
                   input$var_plots__include_zero_y_axis,
                   input$var_plots__numeric_graph_type,
                   input$var_plots__categoric_view_type,
                   input$var_plots__order_by_variable,
                   input$var_plots__show_variable_totals,
                   input$var_plots__show_comparison_totals,
                   input$var_plots__histogram_bins,
                   input$var_plots__transparency,
                   input$var_plots__jitter,
                   input$var_plots__numeric_aggregation_count_minimum,
                   input$var_plots__numeric_show_resampled_conf_int,
                   input$var_plots__trend_line,
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

        if(isolate(input$var_plots__variable) != global__select_variable) {

            log_message_block_start('Graph Options Dirty (Control Used)')
            updateCollapse(session, "var_plots__bscollapse", style = list("Graph Options" = "danger"))
        }

    }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

observeEvent__var_plots__other_options__any_used <- function(input, session) {

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

        if(isolate(input$var_plots__variable) != global__select_variable) {

            log_message_block_start('Other Options Dirty (Control Used)')
            updateCollapse(session, "var_plots__bscollapse", style = list("Other Options" = "danger"))
        }

    }, ignoreNULL = TRUE, ignoreInit = TRUE)
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
    
    }, ignoreNULL = FALSE)  # ignoreNULL so that the observeEvent is triggered when the user removes all
                            # of the selections from the `var_plots__filter_controls_selections` inputSelect
}

observe__var_plots__bscollapse__dynamic_inputs <- function(input, session, dataset) {

    observe({

        req(dataset$data)

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

hide_show_top_n_categories <- function(dataset, variable, comparison_variable, size_variable, color_variable, facet_variable) {

    if(variable == global__select_variable || !(variable %in% colnames(dataset))) {

        shinyjs::hide('var_plots__filter_factor_lump_number')
        return (TRUE)
    }

    dataset <- dataset %>% 
        select(c(variable, comparison_variable, size_variable, color_variable, facet_variable)) %>%
        select_if(is_categoric)

    if(ncol(dataset) > 0) {

        shinyjs::show('var_plots__filter_factor_lump_number')
        return (FALSE)

    } else {

        shinyjs::hide('var_plots__filter_factor_lump_number')
        return (TRUE)
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
helper__plot_numeric_categoric <- function(session,
                                           local_dataset,
                                           local_numeric_graph_type,
                                           local_primary_variable,
                                           local_comparison_variable,
                                           local_color_variable,
                                           local_order_by_variable,
                                           local_var_plots__filter_factor_lump_number,
                                           local_histogram_bins,
                                           horizontal_annotations,
                                           local_scale_x_log_base_10,
                                           local_x_zoom_min,
                                           local_x_zoom_max,
                                           local_scale_y_log_base_10,
                                           local_y_zoom_min,
                                           local_y_zoom_max,
                                           local_base_size
                                           ) {


    show_boxplot <- local_numeric_graph_type == 'Boxplot'

    hide_show_numeric_categoric(session=session,
                                showing_boxplot=show_boxplot,
                                has_comparison_variable=!is.null(local_comparison_variable))
    if(show_boxplot) {

        log_message('**numeric null/categoric - boxplot**')

        log_message_variable('var_plots__order_by_variable', local_order_by_variable)
        log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
        log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
        log_message_variable('var_plots__scale_y_log_base_10', local_scale_y_log_base_10)

        if(local_order_by_variable %in% colnames(local_dataset)) {
            
            temp_order_by_variable <- local_order_by_variable

        } else {

            temp_order_by_variable <- NULL
        }

        annotation_x_location <- 0.5
        if(is.null(local_comparison_variable)) {
            # need this logic because the position changes depending on if it is a single boxplot or multiple
            annotation_x_location <- -0.9
        }

        ggplot_object <- local_dataset %>%
            select(local_primary_variable,
                   local_comparison_variable,
                   local_color_variable,
                   temp_order_by_variable) %>%
            mutate_factor_lump(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
            mutate_factor_reorder(variable_to_order_by=local_order_by_variable,
                                  variable_to_order=local_comparison_variable) %>%
            rt_explore_plot_boxplot(variable=local_primary_variable,
                                    comparison_variable=local_comparison_variable,
                                    color_variable=local_color_variable,
                                    y_zoom_min=local_y_zoom_min,
                                    y_zoom_max=local_y_zoom_max,
                                    base_size=local_base_size) %>%
            scale_axes_log10(scale_x=FALSE,
                             scale_y=local_scale_y_log_base_10) %>%
            add_horizontal_annotations(horizontal_annotations,
                                       x_location=annotation_x_location)

    } else {

        log_message('**numeric null/categoric - histogram**')

        log_message_variable('var_plots__histogram_bins', local_histogram_bins)
        log_message_variable('var_plots__x_zoom_min', local_x_zoom_min)
        log_message_variable('var_plots__x_zoom_max', local_x_zoom_max)
        log_message_variable('var_plots__scale_x_log_base_10', local_scale_x_log_base_10)
        
        ggplot_object <- local_dataset %>% select(local_primary_variable, local_comparison_variable) %>%
            mutate_factor_lump(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
            rt_explore_plot_histogram(variable=local_primary_variable,
                                      comparison_variable=local_comparison_variable,
                                      num_bins=local_histogram_bins,
                                      x_zoom_min=local_x_zoom_min,
                                      x_zoom_max=local_x_zoom_max,
                                      base_size=local_base_size) %>%
            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                             scale_y=FALSE)
    }

    return (ggplot_object)
}

reactive__var_plots__ggplot__creator <- function(input, session, dataset, query_parameters) {
    reactive({

        
        if(!is.null(query_parameters$step)) {

            if(query_parameters$step < create_url_param_step("Can Create Graph from URL Parameters") ||
                query_parameters$step >= create_url_param_step("Successfully Created Graph from URL Parameters")) {

                log_message_block_start("Ignoring plot creation")
                log_message_variable('query_parameters$step', isolate(query_parameters$step))

                return (NULL)

            } else {

                log_message_block_start("Can Plot From URL Parameters")
                #query_parameters$step <- NULL
            }
        }

        req(input$var_plots__variable)
        req(input$var_plots__comparison)
        req(dataset())

        input$var_plots__graph_options_apply  # trigger update if applying custom labels
        input$var_plots__custom_labels_apply  # trigger update if applying graph options

        # log_message_variable('is_null', is.null(isolate(input$var_plots__multi_value_delimiter)))
        # log_message_variable('is_na', is.na(isolate(input$var_plots__multi_value_delimiter)))
        # log_message_variable('is_empty', isolate(input$var_plots__multi_value_delimiter) == "")

        # log_message_variable('is_null', is.null(isolate(input$var_plots__vertical_annotations)))
        # log_message_variable('is_na', is.na(isolate(input$var_plots__vertical_annotations)))
        # log_message_variable('is_empty', isolate(input$var_plots__vertical_annotations) == "")

        # log_message_variable('is_null', is.null(isolate(input$var_plots__custom_title)))
        # log_message_variable('is_na', is.na(isolate(input$var_plots__custom_title)))
        # log_message_variable('is_empty', isolate(input$var_plots__custom_title) == "")


        # reactive data
        local_dataset <- dataset()
        local_primary_variable <- input$var_plots__variable
        local_numeric_aggregation <- input$var_plots__numeric_aggregation

        # if there isn't a selection for these variables, then set them to NULL, because they will be
        # passed to rtools functions (and if they aren't null, rtools expects column names)
        local_comparison_variable <- null_if_select_variable_optional(input$var_plots__comparison)
        # these can actually be NULL (unlike local_comparison_variable which is req)
        # these can't be req because they aren't even shown initially
        local_sum_by_variable <- null_if_select_variable_optional(input$var_plots__sum_by_variable)
        local_size_variable <- null_if_select_variable_optional(input$var_plots__size_variable)
        local_label_variables <- null_if_select_variable_optional(isolate(input$var_plots__label_variables))
        local_color_variable <- null_if_select_variable_optional(input$var_plots__color_variable)
        local_facet_variable <- null_if_select_variable_optional(input$var_plots__facet_variable)
        local_year_over_year <- default_if_null_or_empty_string(isolate(input$var_plots__year_over_year),
                                                             default=FALSE)
        local_include_zero_y_axis <- default_if_null_or_empty_string(isolate(input$var_plots__include_zero_y_axis),
                                                             default=TRUE)

        if(!is.null(local_comparison_variable) && is_date_type(local_dataset[[local_comparison_variable]])) {

            showModal(modalDialog(title = "Only the primary 'Variable' selection can be used with date types."))
            updateSelectInput(session, 'var_plots__comparison', selected=global__select_variable_optional)
            return (NULL)
        }

        if(is_date_type(local_dataset[[local_primary_variable]]) && !is.null(local_color_variable) && local_year_over_year) {
            # we cannot use YOY and color (year will be the color)
            # So, if we aren't faceting, let's move color to the facet variable.
            # Otherwise, we will need to clear the color variable

            if(is.null(local_facet_variable)) {

                showModal(modalDialog(title = "Cannot select Color Variable & Year-over-Year simultaneously (the year will be used as the color). The Facet Variable will be set and the Color Variable will be cleared."))
                updateSelectInput(session, 'var_plots__facet_variable', selected=local_color_variable)
                updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
                log_message_block_start("Clearing Color Variable For Year-over-Year and setting Facet Variable")

            } else {

                showModal(modalDialog(title = "Cannot select Color Variable & Year-over-Year simultaneously (the year will be used as the color). The Color Variable will be cleared."))
                updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
                log_message_block_start("Clearing Color Variable For Year-over-Year")
            }

            return (NULL)            
        }

        top_n_is_hidden <- hide_show_top_n_categories(dataset(),
                                                      local_primary_variable,
                                                      local_comparison_variable,
                                                      local_size_variable,
                                                      local_color_variable,
                                                      local_facet_variable)

        local_numeric_group_comp_variable <- input$var_plots__numeric_group_comp_variable
        local_numeric_aggregation_function <- input$var_plots__numeric_aggregation_function
        local_numeric_aggregation_count_minimum <- isolate(input$var_plots__numeric_aggregation_count_minimum)
        local_numeric_show_resampled_confidence_interval <- isolate(input$var_plots__numeric_show_resampled_conf_int)

        local_vertical_annotations <- isolate(input$var_plots__vertical_annotations)
        vertical_annotations <- str_split(local_vertical_annotations, "\n")[[1]]
        vertical_annotations <- purrr::map(vertical_annotations, ~ str_split(., ';')[[1]])

        local_horizontal_annotations <- isolate(input$var_plots__horizontal_annotations)
        horizontal_annotations <- str_split(local_horizontal_annotations, "\n")[[1]]
        horizontal_annotations <- purrr::map(horizontal_annotations, ~ str_split(., ';')[[1]])

        local_transparency <- isolate(input$var_plots__transparency) / 100
        local_annotate_points <- isolate(input$var_plots__annotate_points)
        local_base_size <- isolate(input$var_plots__base_size)
        local_histogram_bins <- isolate(input$var_plots__histogram_bins)
        local_jitter <- isolate(input$var_plots__jitter)
        local_order_by_variable <- isolate(input$var_plots__order_by_variable)
        local_numeric_graph_type <- isolate(input$var_plots__numeric_graph_type)
        local_pretty_text <- isolate(input$var_plots__pretty_text)
        local_scale_x_log_base_10 <- isolate(input$var_plots__scale_x_log_base_10)
        local_scale_y_log_base_10 <- isolate(input$var_plots__scale_y_log_base_10)
        local_show_variable_totals <- isolate(input$var_plots__show_variable_totals)
        local_show_comparison_totals <- isolate(input$var_plots__show_comparison_totals)
        local_categoric_view_type <- default_if_null_or_empty_string(isolate(input$var_plots__categoric_view_type),
                                                                     default="Bar")
        local_multi_value_delimiter <- isolate(input$var_plots__multi_value_delimiter)
        local_trend_line <- isolate(input$var_plots__trend_line)
        local_trend_line_se <- isolate(input$var_plots__trend_line_se)
        local_x_zoom_min <- isolate(input$var_plots__x_zoom_min)
        local_x_zoom_max <- isolate(input$var_plots__x_zoom_max)
        local_y_zoom_min <- isolate(input$var_plots__y_zoom_min)
        local_y_zoom_max <- isolate(input$var_plots__y_zoom_max)

        # for time series plot
        local_show_points <- default_if_null_or_empty_string(isolate(input$var_plots__show_points),
                                                             default=FALSE)
        local_ts_date_floor <- default_if_null_or_empty_string(isolate(input$var_plots__ts_date_floor),
                                                             string_values_as_null='None')
        local_ts_date_break_format <- default_if_null_or_empty_string(isolate(input$var_plots__ts_date_break_format),
                                                             string_values_as_null='Auto')
        local_ts_date_breaks_width <- default_if_null_or_empty_string(isolate(input$var_plots__ts_breaks_width))

        ggplot_object <- NULL
        if(local_primary_variable != global__select_variable &&
                local_primary_variable %in% colnames(local_dataset)) {

            log_message_block_start('Creating ggplot object')

            local_var_plots__filter_factor_lump_number <- isolate(input$var_plots__filter_factor_lump_number)
            if(top_n_is_hidden ||
                   is.null(local_var_plots__filter_factor_lump_number) ||
                   local_var_plots__filter_factor_lump_number == "Off") {

                local_var_plots__filter_factor_lump_number <- NULL

            } else {

                local_var_plots__filter_factor_lump_number <- as.numeric(local_var_plots__filter_factor_lump_number)
            }

            log_message_variable('primary_variable', local_primary_variable)
            log_message_variable('comparison_variable', local_comparison_variable)
            log_message_variable('var_plots__sum_by_variable', local_sum_by_variable)
            log_message_variable('var_plots__size_variable', local_size_variable)
            log_message_variable('var_plots__label_variables', paste0(local_label_variables, collapse=', '))
            log_message_variable('var_plots__color_variable', local_color_variable)
            log_message_variable('var_plots__facet_variable', local_facet_variable)
            log_message_variable('var_plots__base_size', local_base_size)
            log_message_variable('var_plots__pretty_text', local_pretty_text)
            log_message_variable('var_plots__annotate_points', local_annotate_points)
            log_message_variable('var_plots__year_over_year', local_year_over_year)
            log_message_variable('var_plots__include_zero_y_axis', local_include_zero_y_axis)
            log_message_variable('var_plots__filter_factor_lump_number',
                                 local_var_plots__filter_factor_lump_number)

            log_message_variable('var_plots__horizontal_annotations',
                             paste0(horizontal_annotations, collapse="..."))

            log_message_variable('var_plots__vertical_annotations',
                             paste0(vertical_annotations, collapse="..."))
            
            if(local_pretty_text) {
                # if we change to pretty text, it will update the columns and all values to be "pretty",
                # but that means we have to take the variables they selected and change them to be
                # "pretty" as well so subsetting by them finds the correct column
                local_dataset <- rt_pretty_dataset(dataset=local_dataset %>%
                                                                select(c(default_if_null_or_empty_string(local_primary_variable),
                                                                         default_if_null_or_empty_string(local_comparison_variable),
                                                                         default_if_null_or_empty_string(local_sum_by_variable),
                                                                         default_if_null_or_empty_string(local_size_variable),
                                                                         default_if_null_or_empty_string(local_color_variable),
                                                                         default_if_null_or_empty_string(local_facet_variable))))

                # R uses the "`My Variable`" syntax for variables with spaces which dplyr's xxx_() relies on
                local_primary_variable <- rt_pretty_text(local_primary_variable)
                if(!is_null_or_empty_string(local_comparison_variable)) {

                    local_comparison_variable <- rt_pretty_text(local_comparison_variable)
                }
                if(!is_null_or_empty_string(local_sum_by_variable)) {

                    local_sum_by_variable <- rt_pretty_text(local_sum_by_variable)
                }
                if(!is_null_or_empty_string(local_size_variable)) {

                    local_size_variable <- rt_pretty_text(local_size_variable)
                }
                if(!is_null_or_empty_string(local_color_variable)) {

                    local_color_variable <- rt_pretty_text(local_color_variable)
                }
                if(!is_null_or_empty_string(local_facet_variable)) {

                    local_facet_variable <- rt_pretty_text(local_facet_variable)
                }

                log_message_variable('updated primary_variable', local_primary_variable)
                log_message_variable('updated comparison_variable', local_comparison_variable)
                log_message_variable('updated sum_by_variable', local_sum_by_variable)
                log_message_variable('updated var_plots__size_variable', local_size_variable)
                log_message_variable('updated var_plots__color_variable', local_color_variable)
                log_message_variable('updated var_plots__facet_variable', local_facet_variable)
                log_message_generic('column names', paste0(colnames(local_dataset), collapse = '; '))
            }

            if(is_date_type(local_dataset[, local_primary_variable])) {

                hide_show_date(session, has_comparison_variable=!is.null(local_comparison_variable))

                log_message_variable('var_plots__numeric_aggregation', local_numeric_aggregation)

                log_message_variable('var_plots__show_points', local_show_points)
                log_message_variable('var_plots__ts_date_floor', local_ts_date_floor)
                log_message_variable('var_plots__ts_date_break_format', local_ts_date_break_format)
                log_message_variable('var_plots__ts_breaks_width', local_ts_date_breaks_width)

                comparison_function <- NULL
                comparison_function_name <- NULL
                if(!is.null(local_comparison_variable)) {

                    comparison_function_name <- local_numeric_aggregation

                    if(local_numeric_aggregation == 'Mean') {

                        comparison_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                    } else if (local_numeric_aggregation == 'Geometric Mean') {

                        comparison_function <- rt_geometric_mean

                    } else if (local_numeric_aggregation == 'Median') {

                        comparison_function <- function(x) { return (median(x, na.rm=TRUE)) }

                    } else if (local_numeric_aggregation == 'Sum') {

                        comparison_function_name = 'Sum of'
                        comparison_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                    } else {

                        stopifnot(FALSE)
                    }
                }

                add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'
                ggplot_object <- local_dataset %>%
                    select(local_primary_variable, local_comparison_variable, local_color_variable,
                           local_facet_variable) %>%
                    mutate_factor_lump(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                    rt_explore_plot_time_series(variable=local_primary_variable,
                                                comparison_variable=local_comparison_variable,
                                                comparison_function=comparison_function,
                                                comparison_function_name=comparison_function_name,
                                                color_variable=local_color_variable,
                                                facet_variable=local_facet_variable,
                                                year_over_year=local_year_over_year,
                                                y_zoom_min=local_y_zoom_min,
                                                y_zoom_max=local_y_zoom_max,
                                                include_zero_y_axis=local_include_zero_y_axis,
                                                show_points=local_show_points,
                                                show_labels=local_annotate_points,
                                                date_floor=local_ts_date_floor,
                                                date_break_format=local_ts_date_break_format,
                                                date_breaks_width=local_ts_date_breaks_width,
                                                base_size=local_base_size) %>%
                    scale_axes_log10(scale_x=FALSE,
                                     scale_y=local_scale_y_log_base_10) %>%
                    add_trend_line(trend_line_type=local_trend_line,
                                   confidence_interval=add_confidence_interval,
                                   color_variable=local_color_variable) %>%
                    add_vertical_annotations(vertical_annotations,
                                             y_location=max(0, local_y_zoom_min, na.rm=TRUE),
                                             is_date=TRUE) %>%
                    add_horizontal_annotations(horizontal_annotations,
                                               x_location=min(local_dataset[[local_primary_variable]], na.rm=TRUE),
                                               x_location_is_date=TRUE)

            ##############################################################################################
            # Numeric Primary Variable
            ##############################################################################################
            } else if(is.numeric(local_dataset[, local_primary_variable])) {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    hide_show_numeric_numeric(session, 
                                              local_numeric_group_comp_variable,
                                              local_numeric_aggregation_function == "Boxplot")

                    log_message('**numeric numeric**')

                    log_message_variable('var_plots__transparency', local_transparency)
                    log_message_variable('var_plots__jitter', local_jitter)
                    log_message_variable('var_plots__trend_line', local_trend_line)
                    log_message_variable('var_plots__trend_line_se', local_trend_line_se)

                    log_message_variable('var_plots__x_zoom_min', local_x_zoom_min)
                    log_message_variable('var_plots__x_zoom_max', local_x_zoom_max)
                    log_message_variable('var_plots__y_zoom_min', local_y_zoom_min)
                    log_message_variable('var_plots__y_zoom_max', local_y_zoom_max)
                    log_message_variable('var_plots__annotate_points', local_annotate_points)
                    log_message_variable('var_plots__show_points', local_show_points)
                    log_message_variable('var_plots__scale_x_log_base_10', local_scale_x_log_base_10)
                    log_message_variable('var_plots__scale_y_log_base_10', local_scale_y_log_base_10)

                    local_map_format <- isolate(input$var_plots__map_format)
                    local_map_borders_database <- isolate(input$var_plots___map_borders_database)
                    local_map_borders_regions <- isolate(input$var_plots___map_borders_regions)

                    log_message_variable('var_plots__map_format', local_map_format)
                    log_message_variable('var_plots__map_borders_database', local_map_borders_database)
                    log_message_variable('var_plots__map_borders_regions', local_map_borders_regions)

                    log_message_variable('var_plots__numeric_group_comp_variable',
                                         local_numeric_group_comp_variable)
                    log_message_variable('var_plots__numeric_aggregation_function',
                                         local_numeric_aggregation_function)
                    log_message_variable('var_plots__numeric_aggregation_count_minimum',
                                         local_numeric_aggregation_count_minimum)
                    log_message_variable('var_plots__numeric_show_resampled_conf_int',
                                         local_numeric_show_resampled_confidence_interval)

                    if(local_numeric_group_comp_variable) {

                        aggregation_function <- NULL
                        aggregation_function_name <- NULL
                        if(local_numeric_aggregation_function != 'Boxplot') {

                            aggregation_function_name <- local_numeric_aggregation_function

                            if(local_numeric_aggregation_function == 'Mean') {

                                aggregation_function <- function(x) { return (mean(x, na.rm=TRUE)) }

                            } else if (local_numeric_aggregation_function == 'Geometric Mean') {

                                aggregation_function <- rt_geometric_mean

                            } else if (local_numeric_aggregation_function == 'Median') {

                                aggregation_function <- function(x) { return (median(x, na.rm=TRUE)) }

                            } else if (local_numeric_aggregation_function == 'Sum') {

                                aggregation_function_name = 'Sum of'
                                aggregation_function <- function(x) { return (sum(x, na.rm=TRUE)) }

                            } else {

                                stopifnot(FALSE)
                            }
                        }

                        ggplot_object <- local_dataset %>%
                            rt_explore_plot_aggregate_2_numerics(variable=local_primary_variable,
                                                                 comparison_variable=local_comparison_variable,
                                                                 aggregation_function=aggregation_function,
                                                                 aggregation_function_name=aggregation_function_name,
                                                                 aggregation_count_minimum=local_numeric_aggregation_count_minimum,
                                                                 show_resampled_confidence_interval=local_numeric_show_resampled_confidence_interval,
                                                                 show_points=local_show_points,
                                                                 show_labels=local_annotate_points,
                                                                 x_zoom_min=local_x_zoom_min,
                                                                 x_zoom_max=local_x_zoom_max,
                                                                 y_zoom_min=local_y_zoom_min,
                                                                 y_zoom_max=local_y_zoom_max,
                                                                 base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=local_scale_y_log_base_10)
                    } else {

                        add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'

                        ggplot_object <- local_dataset %>% select(local_primary_variable,
                                                                  local_comparison_variable,
                                                                  local_color_variable,
                                                                  local_size_variable,
                                                                  local_label_variables) %>%
                            mutate_factor_lump(factor_lump_number=local_var_plots__filter_factor_lump_number,
                                               ignore_columns=local_label_variables) %>%
                            rt_explore_plot_scatter(variable=local_primary_variable,
                                                    comparison_variable=local_comparison_variable,
                                                    color_variable=local_color_variable,
                                                    size_variable=local_size_variable,
                                                    label_variables=local_label_variables,
                                                    # alpha is a measure of opacity which is the opposite of transparency, but transparency is more user-friendly
                                                    alpha= 1 - local_transparency,
                                                    jitter=local_jitter,
                                                    x_zoom_min=local_x_zoom_min,
                                                    x_zoom_max=local_x_zoom_max,
                                                    y_zoom_min=local_y_zoom_min,
                                                    y_zoom_max=local_y_zoom_max,
                                                    base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=local_scale_y_log_base_10) %>%
                            add_trend_line(trend_line_type=local_trend_line,
                                           confidence_interval=add_confidence_interval,
                                           color_variable=local_color_variable)

                        if(local_map_format) {

                            ggplot_object <- ggplot_object + coord_map()

                            if(!is_null_or_empty_string(local_map_borders_database)) {

                                regions <- str_split(local_map_borders_regions, pattern = ', ', simplify = TRUE)
                                regions <- as.character(regions)

                                ggplot_object <- ggplot_object +
                                    borders(database=local_map_borders_database,
                                            regions=regions)
                            }
                        }
                    }

                    ggplot_object <- ggplot_object %>%
                            add_horizontal_annotations(horizontal_annotations,
                                                       x_location=custom_max_min(local_dataset[[local_comparison_variable]],
                                                                                 local_x_zoom_min)) %>%
                            add_vertical_annotations(vertical_annotations,
                                                       y_location=custom_max_min(local_dataset[[local_primary_variable]],
                                                                                 local_y_zoom_min))

                            

                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    ggplot_object <- helper__plot_numeric_categoric(session,
                                                                    local_dataset,
                                                                    local_numeric_graph_type,
                                                                    local_primary_variable,
                                                                    local_comparison_variable,
                                                                    local_color_variable,
                                                                    local_order_by_variable,
                                                                    local_var_plots__filter_factor_lump_number,
                                                                    local_histogram_bins,
                                                                    horizontal_annotations,
                                                                    local_scale_x_log_base_10,
                                                                    local_x_zoom_min,
                                                                    local_x_zoom_max,
                                                                    local_scale_y_log_base_10,
                                                                    local_y_zoom_min,
                                                                    local_y_zoom_max,
                                                                    local_base_size)
                }

            ##############################################################################################
            # Categoric Primary Variable
            ##############################################################################################
            } else {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    ggplot_object <- helper__plot_numeric_categoric(session=session,
                                                                    local_dataset=local_dataset,
                                                                    local_numeric_graph_type=local_numeric_graph_type,
                                                                    # same as above, except we need to swap the numeric/categoric variables
                                                                    local_primary_variable=local_comparison_variable,
                                                                    local_comparison_variable=local_primary_variable,
                                                                    local_color_variable=local_color_variable,
                                                                    local_order_by_variable=local_order_by_variable,
                                                                    local_var_plots__filter_factor_lump_number=local_var_plots__filter_factor_lump_number,
                                                                    local_histogram_bins=local_histogram_bins,
                                                                    horizontal_annotations=horizontal_annotations,
                                                                    local_scale_x_log_base_10=local_scale_x_log_base_10,
                                                                    local_x_zoom_min=local_x_zoom_min,
                                                                    local_x_zoom_max=local_x_zoom_max,
                                                                    local_scale_y_log_base_10=local_scale_y_log_base_10,
                                                                    local_y_zoom_min=local_y_zoom_min,
                                                                    local_y_zoom_max=local_y_zoom_max,
                                                                    local_base_siz=local_base_size)

                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    if(local_multi_value_delimiter == '' || !is.null(local_comparison_variable)) {

                        local_multi_value_delimiter <- NULL
                    }

                    hide_show_categoric_categoric(session,
                                                  input,
                                                  has_comparison_variable=!is.null(local_comparison_variable))

                    log_message('**categoric null/categoric**')

                    log_message_variable('var_plots__order_by_variable', local_order_by_variable)
                    log_message_variable('var_plots__show_variable_totals', local_show_variable_totals)
                    log_message_variable('var_plots__show_comparison_totals', local_show_comparison_totals)
                    log_message_variable('var_plots__multi_value_delimiter', local_multi_value_delimiter)
                    log_message_variable('var_plots__categoric_view_type', local_categoric_view_type)

                    if(local_order_by_variable %in% colnames(local_dataset)) {
                            
                        temp_order_by_variable <- local_order_by_variable

                    } else {

                        temp_order_by_variable <- NULL
                    }

                    ggplot_object <- local_dataset %>%
                        select(local_primary_variable,
                               local_comparison_variable,
                               local_sum_by_variable,
                               temp_order_by_variable) %>%
                        mutate_factor_lump(factor_lump_number=local_var_plots__filter_factor_lump_number) %>%
                        mutate_factor_reorder(variable_to_order_by=local_order_by_variable,
                                              variable_to_order=local_primary_variable) %>%
                        rt_explore_plot_value_totals(variable=local_primary_variable,
                                                     comparison_variable=local_comparison_variable,
                                                     sum_by_variable=local_sum_by_variable,
                                                     order_by_count=local_order_by_variable == "Frequency",
                                                     show_variable_totals=local_show_variable_totals,
                                                     show_comparison_totals=local_show_comparison_totals,
                                                     view_type=local_categoric_view_type,
                                                     multi_value_delimiter=local_multi_value_delimiter,
                                                     show_dual_axes=FALSE,
                                                     base_size=local_base_size)
                }
            }

            local_var_plots__custom_title <- isolate(input$var_plots__custom_title)
            local_var_plots__custom_subtitle <- isolate(input$var_plots__custom_subtitle)
            local_var_plots__custom_x_axis_label <- isolate(input$var_plots__custom_x_axis_label)
            local_var_plots__custom_y_axis_label <- isolate(input$var_plots__custom_y_axis_label)
            local_var_plots__custom_caption <- isolate(input$var_plots__custom_caption)
            local_var_plots__custom_tag <- isolate(input$var_plots__custom_tag)

            if(!is_null_or_empty_string(local_var_plots__custom_title)) {
                ggplot_object <- ggplot_object + labs(title = local_var_plots__custom_title)
            }
            if(!is_null_or_empty_string(local_var_plots__custom_subtitle)) {
                ggplot_object <- ggplot_object + labs(subtitle = local_var_plots__custom_subtitle)
            }
            if(!is_null_or_empty_string(local_var_plots__custom_x_axis_label)) {
                ggplot_object <- ggplot_object + labs(x = local_var_plots__custom_x_axis_label)
            }
            if(!is_null_or_empty_string(local_var_plots__custom_y_axis_label)) {
                ggplot_object <- ggplot_object + labs(y = local_var_plots__custom_y_axis_label)
            }
            if(!is_null_or_empty_string(local_var_plots__custom_caption)) {
                ggplot_object <- ggplot_object + labs(caption = local_var_plots__custom_caption)
            }
            if(!is_null_or_empty_string(local_var_plots__custom_tag)) {
                ggplot_object <- ggplot_object + labs(tag = local_var_plots__custom_tag)
            }

        } else {

            hide_graph_options(input)
        }

        if(is.null(ggplot_object)) {

            shinyjs::hide('var_plots__generate_link')

        } else {

            shinyjs::show('var_plots__generate_link')

            # if(!is.null(query_parameters$step)) {

            #     query_parameters$step <- max(query_parameters$step,
            #                                create_url_param_step("Successfully Created Graph from URL Parameters"))
            # }
        }

        return (ggplot_object)
    })
}

##############################################################################################################
# INPUT
##############################################################################################################
get_selection_from_query_or_default <- function(query_parameters, variable_name, default_if_null_or_invalid, valid_options) {

    selection <- default_if_null_or_invalid

    if(!is.null(isolate(query_parameters$query)) &&
            length(isolate(query_parameters$query)) > 0 &&
            !is.null(isolate(query_parameters$query[[variable_name]]))) {

        query_value <- isolate(query_parameters$query[[variable_name]])

        if(is.null(valid_options) || query_value %in% valid_options) {

            selection <- query_value
        }

        log_message_variable(variable_name, query_value)
        log_message_variable(variable_name, selection)
        #query_parameters$query[[variable_name]] <- NULL  # clear from the list so it is not used again.
    }

    log_message_variable(variable_name, selection)
    return(selection)
}

# renderUI__var_plots__variable__UI <- function(dataset, query_parameters) {

#     renderUI({
#         req(dataset$data)

#         log_message_block_start("Creating Primary Variable")
#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'variable', global__select_variable, colnames(dataset$data))
#         selectInput(inputId='var_plots__variable',
#                     label = 'Variable',
#                     choices = c(global__select_variable, colnames(dataset$data)),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__comparison__UI <- function(input, dataset, query_parameters) {

#     renderUI({

#         # if we have a date type as the primary variable, the comparison should only be numeric

#         req(input$var_plots__variable)
#         req(dataset$data)
#         req(query_parameters$can_update)
#         log_message_block_start("Creating Comparison Variable")

#         current_selection <- input$var_plots__comparison
#         if(is.null(input$var_plots__variable) || input$var_plots__variable == global__select_variable) {

#             current_selection <- NULL
#         }
#         selected_variable <- default_if_null_or_empty_string(current_selection,
#                                                              global__select_variable_optional)

#         log_message_variable('selected_variable 1', selected_variable)

#         local_dataset <- dataset$data
#         local_primary_variable <- input$var_plots__variable

#         dataset_columns <- colnames(local_dataset)

#         variable_options <- NULL
#         # only show numeric variables for dates
#         if(local_primary_variable != global__select_variable &&
#                 local_primary_variable %in% dataset_columns &&  # in case datasets change
#                 is_date_type(local_dataset[, local_primary_variable])) {

#             variable_options <- colnames(local_dataset %>% select_if(is.numeric))

#         } else {

#             variable_options <- dataset_columns
#         }

#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'comparison', selected_variable, variable_options)



#         selectInput(inputId='var_plots__comparison',
#                     label = 'Comparison Variable',
#                     choices = c(global__select_variable_optional, variable_options),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__sum_by_variable <- function(dataset, query_parameters) {

#     renderUI({

#         req(dataset$data)

#         log_message_block_start("Creating Sum-By Variable")
#         variable_options <- colnames(dataset$data %>% select_if(is.numeric))
#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'sum_by_variable', global__select_variable_optional, variable_options)

#         selectInput(inputId='var_plots__sum_by_variable',
#                     label = 'Sum By Variable',
#                     choices = c(global__select_variable_optional, variable_options),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL) %>%
#             add_tooltip("Sums the selected variable associated with each instance.")
#     })
# }

# renderUI__var_plots__color_variable <- function(input, dataset, query_parameters) {

#     renderUI({
#         # if we have a date type as the primary variable, color should only be non-numeric

#         req(input$var_plots__variable)
#         req(dataset$data)
#         req(query_parameters$can_update)
#         local_dataset <- dataset$data

#         log_message_block_start("Creating Color Variable")
        
#         local_primary_variable <- input$var_plots__variable
#         local_comparison_variable <- null_if_select_variable_optional(input$var_plots__comparison)

#         dataset_columns <- colnames(local_dataset)

#         variable_options <- NULL
#         # only show categoric variables for dates
#         if(local_primary_variable != global__select_variable &&
#                 local_primary_variable %in% dataset_columns &&  # in case datasets change
#                 is_date_type(local_dataset[, local_primary_variable])) {

#            log_message_block_start('Creating Color Variable - Dates') 

#             variable_options <- colnames(local_dataset %>% select_if(purrr::negate(is.numeric)))

#         } else if(!is.null(local_primary_variable) &&
#                   !is.null(local_comparison_variable) &&
#                   local_primary_variable != global__select_variable &&
#                   local_primary_variable %in% dataset_columns &&  # in case datasets change
#                   xor(is.numeric(local_dataset[, local_primary_variable]),
#                       is.numeric(local_dataset[, local_comparison_variable]))) {
#             # if we have one numeric and one categoric variable (regardless which is which)
#             # then we are displaying a boxplot and we only want to display categoric variables
#             log_message_block_start('Creating Color Variable - XOR numeric/categoric')
#             variable_options <- colnames(local_dataset %>% select_if(purrr::negate(is.numeric)))

#         } else {

#             log_message_block_start('Creating Color Variable - other')
#             variable_options <- dataset_columns
#         }

#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'color_variable', global__select_variable_optional, variable_options)

#         selectInput(inputId='var_plots__color_variable',
#                     label = 'Color Variable',
#                     choices = c(global__select_variable_optional, variable_options),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__facet_variable <- function(dataset, query_parameters) {

#     renderUI({
#         # if we have a date type as the primary variable, color should only be non-numeric
#         req(dataset$data)
#         log_message_block_start("Creating Facet Variable")

#         variable_options <- colnames(dataset$data %>% select_if(purrr::negate(is.numeric)))
#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'facet_variable', global__select_variable_optional, variable_options)
#         selectInput(inputId='var_plots__facet_variable',
#                     label = 'Facet Variable',
#                     choices = c(global__select_variable_optional, variable_options),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__size_variable <- function(dataset, query_parameters) {

#     renderUI({

# 	    req(dataset$data)
#         log_message_block_start("Creating Size Variable")

#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'size_variable', global__select_variable_optional, colnames(dataset$data))
        
#         selectInput(inputId='var_plots__size_variable',
#                     label = 'Size Variable',
#                     choices = c(global__select_variable_optional, colnames(dataset$data)),
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__label_variables <- function(dataset, query_parameters) {

#     renderUI({
#         req(dataset$data)
#         log_message_block_start("Creating Label Variables")

#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'label_variables', NULL, colnames(dataset$data))

#         selectInput(inputId='var_plots__label_variables',
#                     label = 'Label Variables',
#                     choices = colnames(dataset$data),
#                     selected = selection,
#                     multiple = TRUE,
#                     selectize = TRUE,
#                     width='100%',
#                     size = NULL)
#     })
# }

# renderUI__var_plots__order_by_variable__UI <- function(dataset, query_parameters) {

#     renderUI({
#         req(dataset$data)
#         log_message_block_start("Creating Order By Variable")

#         valid_options <- c("Default", "Frequency", colnames(dataset$data %>% select_if(is.numeric)))
#         selection = get_selection_from_query_or_default(isolate(query_parameters), 'order_by_variable', "Default", valid_options)

#         selectInput(inputId='var_plots__order_by_variable',
#                     label = 'Order By',
#                     choices = valid_options,
#                     selected = selection,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width='100%') %>%
#             add_tooltip("If a numeric variable is selected, the categories are ordered by the categories\\' median value of the variable.")
#     })
# }

# observeEvent__var_plots__categoric_view_type <- function(input, session) {

#     observeEvent(c(#input$var_plots__categoric_view_type,
#                    input$var_plots__comparison,
#                    input$var_plots__sum_by_variable), {
        
#         # used for Categoric Primary and optionally Categoric Secondary variables
#         log_message_block_start("Creating Categoric View Type")

#         log_message_variable('input$var_plots__categoric_view_type', input$var_plots__categoric_view_type)
#         log_message_variable('input$var_plots__comparison', input$var_plots__comparison)
#         log_message_variable('input$var_plots__sum_by_variable', input$var_plots__sum_by_variable)

#         previous_selection <- input$var_plots__categoric_view_type
#         local_comparison_variable <- null_if_select_variable_optional(input$var_plots__comparison)
#         local_sum_by_variable <- null_if_select_variable_optional(input$var_plots__sum_by_variable)                    

#         view_type_options <- NULL
#         if(is.null(local_comparison_variable) && is.null(local_sum_by_variable)) {

#             view_type_options <- c("Bar", "Confidence Interval")

#         } else if(is.null(local_comparison_variable) && !is.null(local_sum_by_variable)) {

#             view_type_options <- c("Bar")

#         } else if(!is.null(local_comparison_variable) && is.null(local_sum_by_variable)) {

#             view_type_options <- c("Bar",
#                                    "Confidence Interval",
#                                    "Facet by Comparison",
#                                    "Confidence Interval - within Variable",
#                                    "Stack")

#         } else { # both are not null
            
#             view_type_options <- c("Bar", "Facet by Comparison", "Stack")
#         }

#         if(!is.null(previous_selection) && previous_selection %in% view_type_options) {
            
#             selected_option <- previous_selection

#         } else {

#             selected_option <- "Bar"
#         }

#         updateSelectInput(session,
#                           'var_plots__categoric_view_type',
#                           choices = view_type_options,
#                           selected = selected_option)
#     })
# }

renderUI__var_plots__filter_controls_selections__UI <- function(input, dataset) {
    renderUI({
	req(dataset$data)
        input$var_plots__filter_clear
        choices <- list(
            "All Variables" = c("All Variables"),
            "Individual Variables" = colnames(dataset$data)
        )

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

renderUI__var_plots__filter_bscollapse__UI <- function(input, dataset, filter_controls_list) {
 
    renderUI({ tagList(list=filter_controls_list()) })
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
    updateSelectInput(session, 'var_plots__color_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__facet_variable', selected=global__select_variable_optional)
    updateSelectInput(session, 'var_plots__size_variable', selected=global__select_variable_optional)

    updateCheckboxInput(session, 'var_plots__numeric_group_comp_variable', value=FALSE)
    updateSelectInput(session,
                      'var_plots__numeric_aggregation_function',
                      selected=global__num_num_aggregation_function_default)
    updateSelectInput(session,
                      'var_plots__numeric_aggregation',
                      selected=global__var_plots__numeric_aggregation_default)
    updateTextInput(session, 'var_plots__multi_value_delimiter', value="")
}

observeEvent__var_plots__variables_buttons_clear_swap <- function(session, input) {

    observeEvent(input$var_plots__variables_buttons_clear, {

        clear_variables(session, input, swap_primary_and_comparison=FALSE)
    })

    observeEvent(input$var_plots__variables_buttons_swap, {

        clear_variables(session, input, swap_primary_and_comparison=TRUE)
    })
}

##############################################################################################################
# DYNAMICALLY SHOW/HIDE INPUT
##############################################################################################################
hide_show_date <- function(session, has_comparison_variable) {

    log_message('hide_show_date')
    
    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    shinyjs::show('var_plots__annotate_points')
    shinyjs::show('var_plots__show_points')
    shinyjs::show('var_plots__year_over_year')
    shinyjs::show('var_plots__include_zero_y_axis')
    shinyjs::show('var_plots__color_variable')
    shinyjs::show('var_plots__facet_variable')
    shinyjs::show('div_var_plots__group_trend_controls')
    shinyjs::show('div_var_plots__group_time_series_controls')

    if(has_comparison_variable) {

        shinyjs::show('var_plots__numeric_aggregation')

    } else {

        shinyjs::hide('var_plots__numeric_aggregation')
    }

    shinyjs::hide('var_plots__size_variable')
    shinyjs::hide('var_plots__label_variables')
    shinyjs::hide('var_plots__numeric_group_comp_variable')
    shinyjs::hide('var_plots__numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_show_resampled_conf_int')
    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__order_by_variable')
    shinyjs::hide('var_plots__categoric_view_type')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__sum_by_variable')
    shinyjs::hide('var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')
}

hide_show_numeric_numeric <- function(session,
                                      is_grouping_main_variable,
                                      grouping_is_boxplot,
                                      has_comparison_variable) {

    log_message('hide_show_numeric_numeric')
    
    # scatterplot; or if grouping the main variable, then boxplot or custom aggregation_function

    shinyjs::hide('var_plots__numeric_aggregation')
    shinyjs::show('var_plots__numeric_group_comp_variable')

    if(is_grouping_main_variable) {

        shinyjs::show('var_plots__numeric_aggregation_function')
        shinyjs::show('var_plots__numeric_aggregation_count_minimum')

        if(grouping_is_boxplot) {
            
            shinyjs::hide('var_plots__show_points')
            shinyjs::hide('var_plots__annotate_points')
            shinyjs::hide('var_plots__numeric_show_resampled_conf_int')

        } else {

            shinyjs::show('var_plots__show_points')
            shinyjs::show('var_plots__annotate_points')
            shinyjs::show('var_plots__numeric_show_resampled_conf_int')
        }
    
        shinyjs::hide('var_plots__size_variable')
        shinyjs::hide('var_plots__label_variables')
        shinyjs::hide('var_plots__color_variable')

        shinyjs::hide('var_plots__map_format')
        shinyjs::hide('var_plots___map_borders_database')
        shinyjs::hide('var_plots___map_borders_regions')
        updateCollapse(session, 'var_plots__bscollapse', close="Map Options")

        shinyjs::hide('div_var_plots__group_scatter_controls')
        shinyjs::hide('div_var_plots__group_trend_controls')

    } else {

        shinyjs::hide('var_plots__numeric_aggregation_function')
        shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
        shinyjs::hide('var_plots__numeric_show_resampled_conf_int')

        shinyjs::show('var_plots__size_variable')
        shinyjs::show('var_plots__label_variables')
        shinyjs::hide('var_plots__annotate_points')
        shinyjs::show('var_plots__color_variable')
        shinyjs::show('var_plots__map_format')
        shinyjs::show('var_plots___map_borders_database')
        shinyjs::show('var_plots___map_borders_regions')
        updateCollapse(session, 'var_plots__bscollapse', open="Map Options")

        shinyjs::show('div_var_plots__group_scatter_controls')
        shinyjs::show('div_var_plots__group_trend_controls')
        shinyjs::hide('var_plots__show_points')
    }

    shinyjs::show('div_var_plots__group_x_zoom_controls')
    shinyjs::show('div_var_plots__group_y_zoom_controls')
    shinyjs::show('var_plots__base_size')
    shinyjs::show('var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    
    shinyjs::hide('var_plots__facet_variable')
    shinyjs::hide('var_plots__year_over_year')
    shinyjs::hide('var_plots__include_zero_y_axis')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__order_by_variable')
    shinyjs::hide('var_plots__categoric_view_type')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__sum_by_variable')
    shinyjs::hide('var_plots__multi_value_delimiter')
}

hide_show_numeric_categoric <- function(session, showing_boxplot, has_comparison_variable) {
    
    log_message('hide_show_numeric_categoric')
    
    # could be a boxplot or a histogram; if it is a boxplot, we want to show y-axis-controls, otherwise x-axis
    if(showing_boxplot) {

        shinyjs::hide('var_plots__histogram_bins')
        shinyjs::show('div_var_plots__group_y_zoom_controls')
        shinyjs::hide('div_var_plots__group_x_zoom_controls')

        if(has_comparison_variable) {

            shinyjs::show('var_plots__color_variable')
            shinyjs::show('var_plots__order_by_variable')

        } else {

            shinyjs::hide('var_plots__color_variable')
            shinyjs::hide('var_plots__order_by_variable')
        }

        # if we are hiding the x-controls, uncheck the scale_x_log10 option so it isn't carried over
        updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)

    } else {

        shinyjs::show('var_plots__histogram_bins')
        shinyjs::hide('div_var_plots__group_y_zoom_controls')
        shinyjs::show('div_var_plots__group_x_zoom_controls')
        shinyjs::hide('var_plots__color_variable')
        # if we are hiding the y-controls, uncheck the scale_y_log10 option so it isn't carried over
        updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)
    }

    shinyjs::hide('var_plots__facet_variable')
    shinyjs::hide('var_plots__year_over_year')
    shinyjs::hide('var_plots__include_zero_y_axis')
    shinyjs::hide('var_plots__numeric_aggregation')
    shinyjs::hide('var_plots__size_variable')
    shinyjs::hide('var_plots__label_variables')
    shinyjs::hide('var_plots__numeric_group_comp_variable')
    shinyjs::hide('var_plots__numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_show_resampled_conf_int')

    shinyjs::show('var_plots__base_size')
    shinyjs::hide('var_plots__vertical_annotations')
    shinyjs::show('var_plots__horizontal_annotations')
    shinyjs::show('var_plots__numeric_graph_type')

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('div_var_plots__group_barchar_controls')
    shinyjs::hide('var_plots__categoric_view_type')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    shinyjs::hide('var_plots__sum_by_variable')
    shinyjs::hide('var_plots__multi_value_delimiter')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')
}

hide_show_categoric_categoric <- function(session, input, has_comparison_variable) {

    log_message('hide_show_categoric_categoric')
    
    # grouped barchart
    shinyjs::show('var_plots__sum_by_variable') # categoric with categoric (or NULL) can select numeric sum_by_variable

    if(is.null(input$var_plots__comparison) || input$var_plots__comparison == global__select_variable_optional) {

        shinyjs::show('var_plots__multi_value_delimiter')

    } else {

        shinyjs::hide('var_plots__multi_value_delimiter')
    }

    shinyjs::hide('var_plots__facet_variable')
    shinyjs::hide('var_plots__year_over_year')
    shinyjs::hide('var_plots__include_zero_y_axis')
    shinyjs::hide('var_plots__size_variable')
    shinyjs::hide('var_plots__label_variables')
    shinyjs::hide('var_plots__numeric_group_comp_variable')
    shinyjs::hide('var_plots__numeric_aggregation_function')
    shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
    shinyjs::hide('var_plots__numeric_show_resampled_conf_int')
    shinyjs::hide('var_plots__color_variable')

    shinyjs::show('var_plots__categoric_view_type')
    shinyjs::show('div_var_plots__group_barchar_controls')
    shinyjs::show('var_plots__order_by_variable')
    shinyjs::show('var_plots__base_size')
    shinyjs::hide('var_plots__vertical_annotations')
    shinyjs::hide('var_plots__horizontal_annotations')

    shinyjs::hide('var_plots__numeric_aggregation')
    shinyjs::hide('div_var_plots__group_x_zoom_controls')
    shinyjs::hide('div_var_plots__group_y_zoom_controls')
    # if we are hiding the x/y-controls, uncheck the scale_x/y_log10 option so it isn't carried over
    updateCheckboxInput(session, 'var_plots__scale_x_log_base_10', value=FALSE)
    updateCheckboxInput(session, 'var_plots__scale_y_log_base_10', value=FALSE)

    shinyjs::hide('div_var_plots__group_scatter_controls')
    shinyjs::hide('div_var_plots__group_trend_controls')
    shinyjs::hide('div_var_plots__group_time_series_controls')
    shinyjs::hide('var_plots__histogram_bins')
    shinyjs::hide('var_plots__numeric_graph_type')
    shinyjs::hide('var_plots__annotate_points')
    shinyjs::hide('var_plots__show_points')
    updateCollapse(session, 'var_plots__bscollapse', close="Map Options")
    shinyjs::hide('var_plots__map_format')
    shinyjs::hide('var_plots___map_borders_database')
    shinyjs::hide('var_plots___map_borders_regions')
}

observe__var_plots__hide_show_uncollapse_on_primary_vars <- function(session, input) {
    observe({

        req(input$var_plots__variable)
        req(input$var_plots__comparison)

        local_primary_variable <- input$var_plots__variable
        local_comparison_variable <- input$var_plots__comparison

        if(local_primary_variable == global__select_variable || local_comparison_variable == global__select_variable_optional) {

            shinyjs::hide('var_plots__numeric_aggregation')
            shinyjs::hide('var_plots__sum_by_variable')
            shinyjs::hide('var_plots__multi_value_delimiter')
            shinyjs::hide('var_plots__size_variable')
            shinyjs::hide('var_plots__numeric_group_comp_variable')
            shinyjs::hide('var_plots__numeric_aggregation_function')
            shinyjs::hide('var_plots__numeric_aggregation_count_minimum')
            shinyjs::hide('var_plots__numeric_show_resampled_conf_int')
            shinyjs::hide('var_plots__color_variable')
            shinyjs::hide('var_plots__facet_variable')
            shinyjs::hide('var_plots__year_over_year')
            shinyjs::hide('var_plots__include_zero_y_axis')
        }

        if(local_primary_variable != global__select_variable || local_comparison_variable != global__select_variable_optional) {

            updateCollapse(session, 'var_plots__bscollapse', open='Graph Options')
        }
    })
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

var_plots__input_list_default_values <- list(

    'var_plots__variable' = global__select_variable,
    'var_plots__comparison' = global__select_variable_optional,
    'var_plots__sum_by_variable' = global__select_variable_optional,
    'var_plots__color_variable' = global__select_variable_optional,
    'var_plots__facet_variable' = global__select_variable_optional,
    'var_plots__size_variable' = global__select_variable_optional,
    'var_plots__numeric_group_comp_variable' = FALSE,
    'var_plots__numeric_aggregation_function' = global__num_num_aggregation_function_default,
    'var_plots__numeric_aggregation' = global__var_plots__numeric_aggregation_default,
    'var_plots__multi_value_delimiter' = "",
    'var_plots__label_variables' = NULL,
    'var_plots__annotate_points' = TRUE,
    'var_plots__show_points' = TRUE,
    'var_plots__year_over_year' = FALSE,
    'var_plots__include_zero_y_axis' = TRUE,
    'var_plots__numeric_graph_type' = "Boxplot",
    'var_plots__categoric_view_type' = "Bar",
    'var_plots__order_by_variable' = "Default",
    'var_plots__show_variable_totals' = TRUE,
    'var_plots__show_comparison_totals' = TRUE,
    'var_plots__histogram_bins' = 30,
    'var_plots__transparency' = 60,
    'var_plots__jitter' = FALSE,
    'var_plots__numeric_aggregation_count_minimum' = 30,
    'var_plots__numeric_show_resampled_conf_int' = FALSE,
    'var_plots__trend_line' = 'None',
    'var_plots__trend_line_se' = 'Yes',
    'var_plots__ts_date_floor' = 'None',
    'var_plots__ts_date_break_format' = 'Auto',
    'var_plots__ts_breaks_width' = '',
    'var_plots__scale_x_log_base_10' = FALSE,
    'var_plots__x_zoom_min' = NA,
    'var_plots__x_zoom_max' = NA,
    'var_plots__scale_y_log_base_10' = FALSE,
    'var_plots__y_zoom_min' = NA,
    'var_plots__y_zoom_max' = NA,
    'var_plots__custom_title' = "",
    'var_plots__custom_subtitle' = "",
    'var_plots__custom_x_axis_label' = "",
    'var_plots__custom_y_axis_label' = "",
    'var_plots__custom_caption' = "",
    'var_plots__custom_tag' = "",
    'var_plots__pretty_text' = FALSE,
    'var_plots__base_size' = 15,
    'var_plots__vertical_annotations' = "",
    'var_plots__horizontal_annotations' = ""
)
