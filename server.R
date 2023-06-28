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
library(CCPlotR)
library(BiocManager)
options(repos = BiocManager::repositories())

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
    plot_df <- int_df %>% pivot_longer(cols = starts_with('agg'), names_to = 'tp', values_to = 'score', names_prefix = 'aggregate_rank_', values_drop_na = T) %>%
      mutate(tp = factor(tp, levels = c('healthy', 'diagnosis'), labels = c('Healthy', 'Diagnosis')),
             from_hsc = ifelse(source == 'HSC.MPP', 'From HSC.MPP', 'To HSC.MPP'))
    if(input$plot_type == 'Heatmap'){
      print(cc_heatmap(plot_df %>% filter(ligand == lig, receptor == rec), option = 'B') + 
              facet_grid(from_hsc ~ tp, scales = 'free_x', switch = 'y', space = 'free_x') +
              theme(strip.placement = 'outside', legend.key.height = unit(4.5, 'lines')))
    }
    if(input$plot_type == 'Connections'){
      h_plot <- cc_sigmoid(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Healthy'), colours = cell_cols) + 
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      d_plot <- cc_sigmoid(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Diagnosis'), colours = cell_cols) + 
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      print(h_plot + d_plot)
    }
    if(input$plot_type == 'Chord diagram'){
      par(oma = c(4,1,1,1), mfrow = c(1, 2), mar = c(2, 2, 1, 1))
      if(nrow(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Healthy')) > 0){
        cc_circos(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Healthy'), cell_cols = cell_cols, option = 'B', cex = 1, show_legend = F, scale = T) 
        title('Healthy')}
      if(nrow(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Diagnosis')) > 0){
        cc_circos(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Diagnosis'), cell_cols = cell_cols, option = 'B', cex = 1, show_legend = F, scale = T) 
        title('Diagnosis')}
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      legend(x = "bottom", horiz = F,
        legend = unique(c((plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(source)), (plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(target)))),
        title = "Cell type",
        pch = 15,
        ncol = ceiling(length(unique(c((plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(source)), (plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(target)))))/2),
        text.width = max(sapply(unique(c((plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(source)), (plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(target)))), strwidth)),
        xpd = TRUE,
        col = cell_cols[unique(c((plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(source)), (plot_df %>% filter(ligand == lig, receptor == rec) %>% pull(target))))])
    }
    if(input$plot_type == 'Violin plot'){
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
      print(p1/p2)
    }
    
  })
  # output$vln_plot <- renderPlot({
  #   lig <- str_extract(input$interaction, '[^|]+')
  #   rec <- str_extract(input$interaction, '[^|]+$')
  #   
  # })
  # # output$umap <- renderPlot({
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
