library(shiny)
library(shinyWidgets)
source('definitions.R')

shinyUI(fluidPage(theme = "custom.css",
  
    titlePanel('Explore Dataset'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            fluidRow(
                column(5, fileInput(inputId='uploadFile', 'Upload data (.csv/.RDS)')),
                column(7, uiOutput('selected_target_variable_UI'))
            ),
            tags$br(),
            tags$div(class='results-table', dataTableOutput(outputId='dataset_head_table'))
        ),
        tabPanel(
            'Variable Types',
            tags$div(class='results-table', dataTableOutput(outputId='dataset_types_table'))
        ),
        tabPanel(
            'Numeric Summary',
            column(2, tags$div(style=input_control_style, uiOutput('selected_numeric_summary_options_UI'))),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary_table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary_table'))
        ),
        tabPanel(
            'Correlations',
            tags$div(
                style=input_control_style,
                fluidRow(
                    column(6,
                           sliderTextInput(inputId='selected_correlation_corr_threshold',
                                           label='Min Correlation Threshold', ## percent increase
                                           choices = seq(0, 1, 0.05),
                                           selected = 0,
                                           grid = TRUE)
                    ),
                    column(6,
                           sliderTextInput(inputId='selected_correlation_p_value_threshold',
                                           label='Max P-Value Treshold',
                                           choices = seq(0, 1, 0.05),
                                           selected = 1,
                                           grid = TRUE)
                    )
                ),
                fluidRow(
                    column(6,
                           sliderTextInput(inputId='selected_correlation_base_size',
                                           label='Text Size',
                                           choices = seq(6, 20, 1),
                                           selected = 11,
                                           grid = TRUE)
                    )
                )
            ),
            plotOutput(outputId='correlation_plot')#, width = '900px', height = '700px')
                       # height='500px',
                       # width='850px')
        ),
        tabPanel(
            'Variable Plots',
            tags$div(
                style=input_control_style,
                fluidRow(
                    column(6, uiOutput('selected_variable_plot_variable_UI')),
                    column(6, uiOutput('selected_variable_plot_comparison_UI'))
                ),
                fluidRow(
                    column(2,
                           checkboxInput(inputId='selected_variable_plot_order_by_count',
                                         label='Order By Totals', value = TRUE, width = NULL)
                    ),
                    column(2,
                           checkboxInput(inputId='selected_variable_plot_show_variable_totals',
                                         label='Show Variable Totals', value = TRUE, width = NULL)
                    ),
                    column(2,
                           checkboxInput(inputId='selected_variable_plot_show_comparison_totals',
                                         label='Show Comparison Totals', value = TRUE, width = NULL)
                    ),
                    column(6,
                           sliderTextInput(inputId='selected_variable_plot_base_size',
                                           label='Text Size',
                                           choices = seq(6, 20, 1),
                                           selected = 11,
                                           grid = TRUE)
                    )
                )
            ),
            plotOutput(outputId='variable_plot')
        ),
        widths=c(3,9)
    )
))
