#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinycssloaders)

int_df <- readRDS('www/int_df_h_d0.Rds')

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel(tags$div(class='flex-container',
                      tags$div(class="flex-child", img(src ='hsc_interactome_logo.png', width = '20%')),
                      tags$div(class="flex-child", tags$h1("HSC Interactome"),
                               tags$p(tags$b('Explore cell-cell interactions between hematopoietic stem cells and other bone marrow cell types.', style = "font-size:18px;")),
                               tags$p(tags$b('Citation:'), 'Single-cell characterisation of the hematopoietic bone marrow interactome in health and myeloid leukemia.', tags$i("Ennis S et. al.,", 'bioRxiv, 2022. DOI:', tags$a('10.1101/2022.05.13.491790', href = 'https://doi.org/10.1101/2022.05.13.491790')), style = "font-size:16px;"),
                               tags$p(tags$b('Contact:'), tags$a('Sarah Ennis', href='mailto:s.ennis6@nuigalway.ie'), style = "font-size:16px;"))),
             windowTitle = 'HSC Interactome'),
  tabsetPanel(
    tabPanel('Table',
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 checkboxGroupInput("show_cols", "Columns to display",
                                    colnames(int_df), selected = c('source', 'target', 'ligand', 'receptor', 'timepoint'))
               ),
               mainPanel(
               withSpinner(DT::DTOutput('interactome_table'), color="#9FDC93", size = 0.5)
               ))),
    tabPanel('Plots',
             selectInput('interaction', label = 'Select interaction:', choices = sort(unique(paste0(int_df$ligand, '|', int_df$receptor)))),
             withSpinner(plotOutput('int_plot'), color="#9FDC93", size = 0.5),
             withSpinner(plotOutput('vln_plot'), color="#9FDC93", size = 0.5)
    )
    # tabPanel('UMAP',
    #          radioButtons("col_by",
    #                       label = "Colour by:",
    #                       choices = c("Cell type", "Gene"),
    #                       selected = "Cell type",
    #                       inline = TRUE),
    #          selectInput('col_by_gene', label = 'Select gene:', choices = unique(c(int_df$ligand, int_df$receptor))),
    #          plotOutput('umap')
    # )
  )
))
