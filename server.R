library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(Matrix)
library(patchwork)
library(CCPlotR)
library(BiocManager)
library(rvest)
options(repos = BiocManager::repositories())

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  int_df <- readRDS('www/int_df_h_d0.Rds')
  exp_mtr <- readRDS('www/ligand_receptor_mtx_h_d0.Rds')
  meta <- readRDS('www/meta.Rds')
  cols <- readRDS('www/colour_palettes.Rds')
  genes_ids <- readRDS('www/genes_ids')
  cell_cols <- cols$celltype
  
  ## scrap gene info from NCBI:
  scrape_gene_info <- function(gene_name) {
    gene_id = genes_ids[gene_name]
    url <- paste0('https://www.ncbi.nlm.nih.gov/gene/', gene_id)
    page <- rvest::read_html(url) %>% html_elements("dt:contains('Summary') + dd")
    summary <- html_text(page)
    return(c(summary,url))
  }
  
  output$interactome_table <- DT::renderDT(
    int_df[,input$show_cols], 
    extensions = 'Buttons', 
    server=FALSE,
    options = list(autoWidth = TRUE, scrollX = T, buttons = c('csv', 'excel'), dom = 'Bfrtip', pageLength = 20,
                   initComplete = JS(
                     "function(settings, json) {",
                     "  var table = this.api();",
                     "  table.on('click', 'td', function() {",
                     "    var colIdx = table.cell(this).index().column;",
                     "    if (table.column(colIdx).header().textContent === 'ligand' || table.column(colIdx).header().textContent === 'receptor') {",
                     "      var geneName = table.cell(this).data();",
                     "      Shiny.setInputValue('selected_gene', geneName);",
                     "    }",
                     "  });",
                     "}")),
    filter = list(
      position = 'top', clear = FALSE
    ),
    class = "display"
  )
  
  observeEvent(input$selected_gene, {
    gene_name <- input$selected_gene
    scraped_info <- scrape_gene_info(gene_name)
    gene_info <- scraped_info[1]
    title =  tags$a(href = scraped_info[2], target = "_blank", paste0("From NCBI: ", gene_name))
    showModal(modalDialog(
      title = title,
      gene_info,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$int_plot <- renderPlot({
    lig <- str_extract(input$interaction, '[^|]+')
    rec <- str_extract(input$interaction, '[^|]+$')
    plot_df <- int_df %>% pivot_longer(cols = starts_with('agg'), names_to = 'tp', values_to = 'score', names_prefix = 'aggregate_rank_', values_drop_na = T) %>%
      mutate(tp = factor(tp, levels = c('healthy', 'diagnosis'), labels = c('Healthy', 'Diagnosis')),
             from_hsc = ifelse(source == 'HSC.MPP', 'From HSC.MPP', 'To HSC.MPP'))
    if(input$plot_type_int == 'Heatmap'){
      print(cc_heatmap(plot_df %>% filter(ligand == lig, receptor == rec), option = 'B') + 
              facet_grid(from_hsc ~ tp, scales = 'free_x', switch = 'y', space = 'free_x') +
              theme(strip.placement = 'outside', legend.key.height = unit(4.5, 'lines')))
    }
    if(input$plot_type_int == 'Connections'){
      h_plot <- cc_sigmoid(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Healthy'), colours = cell_cols) + 
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      d_plot <- cc_sigmoid(plot_df %>% filter(ligand == lig, receptor == rec, tp == 'Diagnosis'), colours = cell_cols) + 
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      print(h_plot + d_plot)
    }
    if(input$plot_type_int == 'Chord diagram'){
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
    if(input$plot_type_int == 'Violin plot'){
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
  
  output$cell_plot <- renderPlot({
    int_cell <- input$celltype
    plot_df <- int_df %>% pivot_longer(cols = starts_with('agg'), names_to = 'tp', values_to = 'score', names_prefix = 'aggregate_rank_', values_drop_na = T) %>%
      mutate(tp = factor(tp, levels = c('healthy', 'diagnosis'), labels = c('Healthy', 'Diagnosis')),
             from_hsc = ifelse(source == 'HSC.MPP', 'From HSC.MPP', 'To HSC.MPP'))
    if(input$plot_type_cell == 'Heatmap'){
      p1 <- cc_heatmap(plot_df %>% filter(interacting_cell == int_cell, tp == 'Healthy'), option = 'B', n_top_ints = input$n_ints_cell) + 
        scale_fill_viridis_c(option = 'C', na.value = 'black', direction = 1, limits=plot_df %>% 
                               filter(interacting_cell == int_cell) %>% 
                               group_by(tp) %>% slice_max(order_by = score, n = input$n_ints_cell) %>% 
                               pull(score) %>% range()) +
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5),
              legend.key.height = unit(0.3, 'inches'))
      
      p2 <- cc_heatmap(plot_df %>% filter(interacting_cell == int_cell, tp != 'Healthy'), option = 'B', n_top_ints = input$n_ints_cell) + 
        scale_fill_viridis_c(option = 'C', na.value = 'black', direction = 1, limits=plot_df %>% 
                               filter(interacting_cell == int_cell) %>% 
                               group_by(tp) %>% slice_max(order_by = score, n = input$n_ints_cell) %>% 
                               pull(score) %>% range()) +
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5),
              legend.key.height = unit(0.3, 'inches'))
      print(p1+p2 + plot_layout(guides = 'collect'))
    }
    if(input$plot_type_cell == 'Connections'){
      h_plot <- cc_sigmoid(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Healthy'), colours = cell_cols, n_top_ints = input$n_ints_cell) +
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      d_plot <- cc_sigmoid(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Diagnosis'), colours = cell_cols, n_top_ints = input$n_ints_cell) +
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      print(h_plot + d_plot)
    }
    if(input$plot_type_cell == 'Chord diagram'){
      par(oma = c(4,1,1,1), mfrow = c(1, 2), mar = c(2, 2, 1, 1))
      if(nrow(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Healthy')) > 0){
        cc_circos(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Healthy'), cell_cols = cell_cols, option = 'B', cex = 0.8, show_legend = F, scale = T, n_top_ints = input$n_ints_cell)
        title('Healthy')}
      if(nrow(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Diagnosis')) > 0){
        cc_circos(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Diagnosis'), cell_cols = cell_cols, option = 'B', cex = 0.8, show_legend = F, scale = T, n_top_ints = input$n_ints_cell)
        title('Diagnosis')}
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      legend(x = "bottom", horiz = F,
             legend = unique(c((plot_df %>% filter(interacting_cell == input$celltype) %>% pull(source)), (plot_df %>% filter(interacting_cell == input$celltype) %>% pull(target)))),
             title = "Cell type",
             pch = 15,
             ncol = ceiling(length(unique(c((plot_df %>% filter(interacting_cell == input$celltype) %>% pull(source)), (plot_df %>% filter(interacting_cell == input$celltype) %>% pull(target)))))/2),
             text.width = max(sapply(unique(c((plot_df %>% filter(interacting_cell == input$celltype) %>% pull(source)), (plot_df %>% filter(interacting_cell == input$celltype) %>% pull(target)))), strwidth)),
             xpd = TRUE,
             col = cell_cols[unique(c((plot_df %>% filter(interacting_cell == input$celltype) %>% pull(source)), (plot_df %>% filter(interacting_cell == input$celltype) %>% pull(target))))])
    }
    if(input$plot_type_cell == 'Network diagram'){
      h_netplot <- cc_network(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Healthy'), colours = cell_cols, n_top_ints = input$n_ints_cell, option = 'B', node_size = 2.2) +
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      d_netplot <- cc_network(plot_df %>% filter(interacting_cell == input$celltype, tp == 'Diagnosis'), colours = cell_cols, n_top_ints = input$n_ints_cell, option = 'B', node_size = 2.2) +
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      print(h_netplot + d_netplot)
    }
  })
  
  output$gene_plot <- renderPlot({
    genes <- input$gene
    plot_df <- int_df %>% pivot_longer(cols = starts_with('agg'), names_to = 'tp', values_to = 'score', names_prefix = 'aggregate_rank_', values_drop_na = T) %>%
      mutate(tp = factor(tp, levels = c('healthy', 'diagnosis'), labels = c('Healthy', 'Diagnosis')),
             from_hsc = ifelse(source == 'HSC.MPP', 'From HSC.MPP', 'To HSC.MPP'))
    if(input$plot_type_gene == 'Heatmap'){
      p1 <- cc_heatmap(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp == 'Healthy'), option = 'B', n_top_ints = input$n_ints_gene) + 
        scale_fill_viridis_c(option = 'C', na.value = 'black', direction = 1, limits=plot_df %>% 
                               filter(ligand %in% genes | receptor %in% genes) %>% 
                               group_by(tp) %>% slice_max(order_by = score, n = input$n_ints_gene) %>% 
                               pull(score) %>% range()) +
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5),
              legend.key.height = unit(0.3, 'inches'))
      
      p2 <- cc_heatmap(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp != 'Healthy'), option = 'B', n_top_ints = input$n_ints_gene) + 
        scale_fill_viridis_c(option = 'C', na.value = 'black', direction = 1, limits=plot_df %>% 
                               filter(ligand %in% genes | receptor %in% genes) %>% 
                               group_by(tp) %>% slice_max(order_by = score, n = input$n_ints_gene) %>% 
                               pull(score) %>% range()) +
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5),
              legend.key.height = unit(0.3, 'inches'))
      print(p1+p2 + plot_layout(guides = 'collect'))
    }
    if(input$plot_type_gene == 'Connections'){
      h_plot <- cc_sigmoid(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp == 'Healthy'), colours = cell_cols, n_top_ints = input$n_ints_gene) +
        labs(title = 'Healthy') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      d_plot <- cc_sigmoid(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp != 'Healthy'), colours = cell_cols, n_top_ints = input$n_ints_gene) +
        labs(title = 'Diagnosis') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
      print(h_plot + d_plot)
    }
    if(input$plot_type_gene == 'Chord diagram'){
      par(oma = c(4,1,1,1), mfrow = c(1, 2), mar = c(2, 2, 1, 1))
      if(nrow(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp == 'Healthy')) > 0){
        cc_circos(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp == 'Healthy'), cell_cols = cell_cols, option = 'B', cex = 0.8, show_legend = F, scale = T, n_top_ints = input$n_ints_gene)
        title('Healthy')}
      if(nrow(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp != 'Healthy')) > 0){
        cc_circos(plot_df %>% filter((ligand %in% genes | receptor %in% genes) & tp != 'Healthy'), cell_cols = cell_cols, option = 'B', cex = 0.8, show_legend = F, scale = T, n_top_ints = input$n_ints_gene)
        title('Diagnosis')}
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      legend(x = "bottom", horiz = F,
             legend = unique(c((plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(source)), (plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(target)))),
             title = "Cell type",
             pch = 15,
             ncol = ceiling(length(unique(c((plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(source)), (plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(target)))))/2),
             text.width = max(sapply(unique(c((plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(source)), (plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(target)))), strwidth)),
             xpd = TRUE,
             col = cell_cols[unique(c((plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(source)), (plot_df %>% filter(ligand %in% genes | receptor %in% genes) %>% pull(target))))])
    }
    if(input$plot_type_gene == 'Violin plot'){
      exp_df <- cbind(meta, (as.data.frame(as.matrix(exp_mtr[, input$gene])) %>% setNames(input$gene))) %>% pivot_longer(input$gene, names_to = 'gene', values_to = 'value')
      ggplot(exp_df, aes(x = cell_type, y = value, fill = timepoint)) +
        geom_violin(show.legend = T, scale = 'width', col = 'black', draw_quantiles = 0.5) +
        scale_fill_manual(values = cols$timepoint, name = 'Timepoint') +
        scale_x_discrete(limits = names(cell_cols)) +
        labs(y = 'Normalised expression', x = NULL) +
        facet_grid(gene~., switch = 'y') +
        theme_classic(base_size = 18) +
        theme(axis.text = element_text(colour = 'black'),
              axis.text.x = element_text(hjust=1, angle = 90, vjust = 0.5),
              strip.placement = 'outside',
              legend.position = 'bottom')
    }
  })
  
})
