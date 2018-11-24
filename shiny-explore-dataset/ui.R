library(shiny)
library(shinyWidgets)
library(shinyjs)
source('definitions.R')

shinyUI(fluidPage(theme = "custom.css",
  
    useShinyjs(),

    titlePanel('Explore Dataset'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            fluidRow(
                column(5, fileInput(inputId='uploadFile', 'Upload data (.csv/.RDS)'))
            ),
            tags$br(),
            tags$h3('First 500 Records of Dataset:'),
            tags$div(class='results-table', dataTableOutput(outputId='dataset_head_table'))
        ),
        tabPanel(
            'Variable Types',
            tags$div(class='results-table', dataTableOutput(outputId='dataset_types_table'))
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('selected_numeric_summary_options_UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary_table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary_table'))
        ),
        tabPanel(
            'Correlations',
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderTextInput(inputId='selected_correlation_corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices = seq(0, 1, 0.05),
                                    selected = 0,
                                    grid = TRUE),
                    sliderTextInput(inputId='selected_correlation_p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices = seq(0, 1, 0.05),
                                    selected = 1,
                                    grid = TRUE),
                    sliderTextInput(inputId='selected_correlation_base_size',
                                    label='Text Size',
                                    choices = seq(6, 20, 1),
                                    selected = 15,
                                    grid = TRUE),
                    checkboxInput(inputId='selected_correlation_pretty_text',
                                       label='Pretty Text', value = FALSE, width = NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation_plot')
            )
        ),
        tabPanel(
            'Variable Plots',
            column(3,
                class='column-input-control-style; input-control-style',
                    uiOutput('selected_variable_plot_variable_UI'),
                    uiOutput('selected_variable_plot_comparison_UI'),
                    shinyjs::hidden(
                        selectInput(inputId='selected_variable_plot_numeric_graph_type',
                                label='Type',
                                choices=c('Boxplot', 'Histogram'),
                                selected='Boxplot')
                    ),
                    shinyjs::hidden(tags$div(id='div_variable_plots_group_barchar_controls',
                         checkboxInput(inputId='selected_variable_plots_order_by_count',
                                       label='Order By Totals', value = TRUE, width = NULL),
                         checkboxInput(inputId='selected_variable_plots_show_variable_totals',
                                       label='Show Variable Totals', value = TRUE, width = NULL),
                         checkboxInput(inputId='selected_variable_plots_show_comparison_totals',
                                       label='Show Comparison Totals', value = TRUE, width = NULL)
                    )),
                    shinyjs::hidden(tags$div(id='div_variable_plots_group_scatter_controls',
                        sliderTextInput(inputId='selected_variable_plots_alpha',
                                    label='Alpha',
                                    choices = seq(0.1, 1, 0.1),
                                    selected = 0.3,
                                    grid = TRUE),
                        checkboxInput(inputId='selected_variable_plots_jitter',
                                       label='Jitter', value = FALSE, width = NULL)
                    )),
                    shinyjs::hidden(tags$div(id='div_variable_plots_group_x_zoom_controls',
                         numericInput(inputId='selected_variable_plots_x_zoom_min',
                                      label='X-Axis Zoom Min',
                                      value=NULL),
                         numericInput(inputId='selected_variable_plots_x_zoom_max',
                                      label='X-Axis Zoom Max',
                                      value=NULL)                         
                    )),
                    shinyjs::hidden(tags$div(id='div_variable_plots_group_y_zoom_controls',
                         numericInput(inputId='selected_variable_plots_y_zoom_min',
                                      label='Y-Axis Zoom Min',
                                      value=NULL),
                         numericInput(inputId='selected_variable_plots_y_zoom_max',
                                      label='Y-Axis Zoom Max',
                                      value=NULL)                         
                    )),
                    shinyjs::hidden(
                         numericInput(inputId='selected_variable_plots_histogram_bins',
                                      label='Number of Bins',
                                      value=30)                 
                    ),
                    sliderTextInput(inputId='selected_variable_plots_base_size',
                                    label='Text Size',
                                    choices = seq(6, 20, 1),
                                    selected = 15,
                                    grid = TRUE),
                    checkboxInput(inputId='selected_variable_plots_pretty_text',
                                       label='Pretty Text', value = FALSE, width = NULL)
            ),
            column(9,
                plotOutput(outputId='variable_plot')
            )
        ),
        widths=c(2,10)
    )
))
