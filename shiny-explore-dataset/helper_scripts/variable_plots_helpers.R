

renderUI__variable_plots_variable_UI <- function(dataset) {

    renderUI({
        selectInput(inputId='variable_plots_variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_comparison_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_comparison',
                    label = 'Comparison Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_sum_by_variable_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_sum_by_variable',
                    label = 'Sum By Variable',
                    choices = c(select_variable_optional, colnames(dataset() %>% select_if(is.numeric))),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_color_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_color',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_size_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_filter_bscollapse_UI <- function(filter_controls_list) {
 
    renderUI({
 
        tagList(list=filter_controls_list())
    })
}

# Events that control the color of the Filter collapse panel, so that it turns red when the filters haven't
# been applied (i.e. changes that haven't been applied)
observeEvent__variable_plots_filter_clear <- function(input, session) {

    observeEvent(input$variable_plots_filter_clear, ({

        if(isolate(input$variable_plots_filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
        }
    }))
}

observeEvent__variable_plots_filter_apply <- function(input, session) {

    observeEvent(input$variable_plots_filter_apply, ({

        if(isolate(input$variable_plots_filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'success'))
        }
    }))
}

observeEvent__variable_plots_filter_use <- function(input, session) {

    observeEvent(input$variable_plots_filter_use, {

        if(input$variable_plots_filter_use) {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'success'))

        } else {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'default'))
        }
    })
}

observe__variable_plots_bscollapse__dynamic_inputs <- function(input, session, dataset) {

    observe({

        req(dataset())

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(column_name in colnames(dataset())) {
            value <- input[[paste0('dynamic_filter_variable_plots_', column_name)]]
            selections <- append(selections, value)
        }

        # only update if we are using the filter
        # also, if any of the selections are not null, that means they have been initialized and we can begin
        # to mark as being changed otherwise, the filter section hasn't even been opened
        if(isolate(input$variable_plots_filter_use) && any(map_lgl(selections, ~ !is.null(.)))) {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
        }
    })
}







##############################################################################################################
# Variable Plot
# NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
# withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
# shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
# not affecting return of the plot from the function or it being displayed in shiny
##############################################################################################################
get_dynamic_filter_selections <- function(input, columns) {

    # get all of the selections from the dynamic filters without triggering refresh for the first time
    selections_list <- map(columns, ~ isolate(input[[paste0('dynamic_filter_variable_plots_', .)]]))
    names(selections_list) <- columns

    return (selections_list)

}

renderPlot__variable_plot <- function(input, output, session, reactive__variable_plots__ggplot, messages) {

    renderPlot({
        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(reactive__variable_plots__ggplot()))

           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_variable_plots_width * 0.66  # set height to % of width
    })
}