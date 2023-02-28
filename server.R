#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(Matrix)
library(patchwork)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  int_df <- readRDS('www/int_df_h_d0.Rds')
  exp_mtr <- readRDS('www/ligand_receptor_mtx_h_d0.Rds')
  meta <- readRDS('www/meta.Rds')
  cols <- readRDS('www/colour_palettes.Rds')
  cell_cols <- cols$celltype
  output$interactome_table <- DT::renderDT(
    int_df[,input$show_cols], extensions = 'Buttons', server=FALSE,
    options = list(autoWidth = TRUE, scrollX = T, buttons = c('csv', 'excel'), dom = 'Bfrtip', pageLength = 20), filter = list(
      position = 'top', clear = FALSE
    ), class = "display"
  )
  output$int_plot <- renderPlot({
    lig <- str_extract(input$interaction, '[^|]+')
    rec <- str_extract(input$interaction, '[^|]+$')
    plot_df <- int_df %>% pivot_longer(cols = starts_with('agg'), names_to = 'tp', values_to = 'agg_rank', names_prefix = 'aggregate_rank_', values_drop_na = T) %>%
      mutate(tp = factor(tp, levels = c('healthy', 'diagnosis'), labels = c('Healthy', 'Diagnosis')))
    ggplot(plot_df %>% filter(ligand == lig, receptor == rec), 
           aes(y = source, x = target, fill = -log10(agg_rank), size = -log10(agg_rank))) +
      geom_point(pch=21) +
      scale_x_discrete(limits = names(cell_cols), name = 'To') +
      scale_y_discrete(limits = rev(names(cell_cols)), name = 'From') +
      scale_fill_viridis_c(option = 'G', direction = -1, limits = c(1,6), breaks = seq(2,6,2)) +
      scale_size(range = c(1, 5), limits = c(1,6), breaks = seq(2,6,2))+
      labs(title = input$interaction) +
      guides(fill= guide_legend(), size=guide_legend()) +
      facet_wrap(~tp, nrow = 1) +
      theme_minimal(base_size = 18) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text = element_text(colour = 'black'),
            axis.ticks = element_line(colour = 'grey20'),
            strip.background = element_rect(colour = 'black', fill = 'transparent'),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(colour = 'grey60'),
            legend.position = 'bottom')
  })
  output$vln_plot <- renderPlot({
    lig <- str_extract(input$interaction, '[^|]+')
    rec <- str_extract(input$interaction, '[^|]+$')
    exp_df <- cbind(meta, data.frame(lig = exp_mtr[, lig]), data.frame(rec = exp_mtr[,rec]))
    p1 <- ggplot(exp_df, aes(x = cell_type, y = lig, fill = timepoint)) +
      geom_violin(show.legend = F, scale = 'width', col = 'black', draw_quantiles = 0.5) +
      scale_fill_manual(values = cols$timepoint) +
      scale_x_discrete(limits = names(cell_cols)) +
      labs(y = lig) +
      theme_classic(base_size = 18) +
      theme(axis.text = element_text(colour = 'black'),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position = 'none')
    p2 <- ggplot(exp_df, aes(x = cell_type, y = rec)) +
      geom_violin(aes(fill = timepoint), scale = 'width', draw_quantiles = 0.5) +
      scale_fill_manual(values = cols$timepoint, name= 'Timepoint') +
      scale_x_discrete(limits = names(cell_cols)) +
      guides(colour = guide_legend(override.aes = list(size = 3))) +
      labs(y = rec) +
      theme_classic(base_size = 18) +
      theme(axis.text = element_text(colour = 'black'),
            axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.title.x = element_blank(),
            legend.position = 'bottom')
    p1/p2
  })
  # output$umap <- renderPlot({
  #   col_by <- ifelse(input$col_by == 'Cell type', 'cell_type', 'gene')
  #   exp_df <- cbind(umap_df, data.frame(gene = exp_mtr[, input$col_by_gene]))
  #   plot <- ggplot(exp_df, aes(x = umap1, y = umap2, col = exp_df[,col_by])) +
  #     geom_point() +
  #     theme_linedraw(base_size = 12) +
  #     theme(strip.text = element_text(size = 12, face = 'bold'))
  #   if(input$col_by == 'Cell type'){
  #     plot <- plot + scale_colour_manual(values = cell_cols, name = 'Cell type')}
  #   else{plot <- plot + scale_colour_viridis_c(option = 'A', name = input$col_by_gene)}
  #   plot
  # })
})
