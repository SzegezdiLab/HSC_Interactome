library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)

int_df <- readRDS('www/int_df_h_d0.Rds')


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  titlePanel(tags$div(class='flex-container',
                      tags$div(class="flex-child", img(src ='hsc_interactome_logo.png', width = '20%')),
                      tags$div(class="flex-child", tags$h1("HSC Interactome"),
                               tags$p(tags$b('Explore cell-cell interactions between hematopoietic stem cells and other bone marrow cell types.', style = "font-size:18px;")),
                               tags$p(tags$b('Citation:'), 'Cell-cell interactome of the hematopoietic niche and its changes in acute myeloid leukemia.', tags$i("Ennis S et. al.,", 'iScience, 2023. DOI:', tags$a('10.1016/j.isci.2023.106943', href = 'https://doi.org/10.1016/j.isci.2023.106943')), style = "font-size:16px;"),
                               tags$p(tags$b('Created by: '), tags$a(' Sarah Ennis', href='mailto:s.ennis6@nuigalway.ie', .noWS='outside'), ', Micheál Ó Dálaigh and Jacopo Umberto Verga', style = "font-size:16px;", .noWS = c("after-begin", "before-end")))),
             windowTitle = 'HSC Interactome'),
  tabsetPanel(
    tabPanel('Table',
             div(class = 'mainDiv',
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     checkboxGroupInput("show_cols", "Columns to display",
                                        colnames(int_df), selected = c('source', 'target', 'ligand', 'receptor', 'timepoint'))
                   ),
                   mainPanel(
                     withSpinner(DT::DTOutput('interactome_table'), color="#9FDC93", size = 0.5)
                   )))
    ),
    tabPanel('Plots',
             div(class = 'mainDiv',
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     tabsetPanel(id="tabs",
                                 tabPanel('Interactions',
                                          br(),
                                          selectInput('interaction', label = 'Select interaction', choices = sort(unique(paste0(int_df$ligand, '|', int_df$receptor)))),
                                          hr(),
                                          radioButtons('plot_type_int', 'Select plot type', choices = c('Heatmap', 'Connections', 'Chord diagram', 'Violin plot'), selected = 'Heatmap')
                                 ),
                                 tabPanel('Cell types',
                                          br(),
                                          selectInput('celltype', label = 'Select cell type', choices = sort(unique(c(int_df$source, int_df$target)))),
                                          hr(),
                                          radioButtons('plot_type_cell', 'Select plot type', choices = c('Heatmap', 'Connections', 'Chord diagram', 'Network diagram'), selected = 'Heatmap'),
                                          hr(),
                                          sliderInput('n_ints_cell', 'Max no. of interactions to plot', min = 5, max = 40, value = 20, step = 1)
                                 ),
                                 tabPanel('Genes',
                                          br(),
                                          selectInput('gene', label = 'Select gene', choices = sort(unique(c(int_df$ligand, int_df$receptor))), multiple = T, selected = 'ADAM10'),
                                          hr(),
                                          radioButtons('plot_type_gene', 'Select plot type', choices = c('Heatmap', 'Connections', 'Chord diagram', 'Violin plot'), selected = 'Heatmap'),
                                          hr(),
                                          sliderInput('n_ints_gene', 'Max no. of interactions to plot', min = 5, max = 40, value = 20, step = 1)
                                 ))
                   ),
                   mainPanel(
                     conditionalPanel(condition="input.tabs == 'Interactions'",
                                      withSpinner(plotOutput('int_plot', width = '100%', height = "600px"), color="#9FDC93", size = 0.5)
                     ),
                     conditionalPanel(condition="input.tabs == 'Cell types'",
                                      withSpinner(plotOutput('cell_plot', width = '100%', height = "600px"), color="#9FDC93", size = 0.5)
                     ),
                     conditionalPanel(condition="input.tabs == 'Genes'",
                                      withSpinner(plotOutput('gene_plot', width = '100%', height = "600px"), color="#9FDC93", size = 0.5)
                     )
                   )))
    )
  )
))
