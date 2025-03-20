# SERVER
server = function(input, output, session) {
  
  

# Images ------------------------------------------------------------------
  output$scheme_cons <- renderImage({
    # Specify the path to the image you want to display
    list(src = "www/scheme_consensus.png", contentType = "image/png",
         alt = "consensus scheme",
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)
  
  output$scheme_multi <- renderImage({
    # Specify the path to the image you want to display
    list(src = "www/scheme_multicell.png", contentType = "image/png", alt = "multi",
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)

  output$scheme_bulk <- renderImage({
    # Specify the path to the image you want to display
    list(src = "www/scheme_bulk.png", contentType = "image/png", alt = "bulk",
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)

  output$scheme_sc <- renderImage({
    # Specify the path to the image you want to display
    list(src = "www/scheme_sc.png", contentType = "image/png", alt = "sc",
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)

  output$scheme_compmol <- renderImage({
    # Specify the path to the image you want to display
    list(src = "www/scheme_compmols.png", contentType = "image/png", alt = "sc",
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)
  
  observeEvent(input$reset_genes, {
    updatePickerInput(session, "select_gene", selected = character(0))
  })
  
#### Query genes ####
#test stuff
#input= list()
# input$select_gene <- c("NPPA", "POSTN")
#  output= list()

  output$gene_regulation_boxplot = renderPlotly({
  if (!is.null(input$select_gene)) {
    gene_regulation_boxplot = df.reheat1 %>%
      filter(ID %in% input$select_gene) %>%
      ggplot(aes(x= reorder(ID,logFC), y= logFC) )+
      geom_hline(yintercept = 0, color = "grey", linetype = 2)+
      geom_boxplot(outlier.colour = NA)+
      geom_jitter(aes(color= study), width=0.1, height=0.1, show.legend = F)+
      labs(x= "", y= "study specific logFC")+
      theme_cowplot()+
      theme(axis.text.x = element_text(angle= 45, hjust= 1),
            legend.position = "none")
    
    ggplotly(gene_regulation_boxplot, tooltip = c("study"))

  }
})

# add the consensus image  

# plot for rank positions

output$rank_position = renderPlot({
  if (!is.null(input$select_gene)) {
    new_data <- df.reheat2 %>% 
      filter(gene %in% input$select_gene)%>%
      mutate(label= paste0(gene, " (top ", rank_percentile,"%)"))
    
    rank_plot<- p.linefit+
      geom_vline(xintercept = 500, color="darkgrey")+
      geom_hline(yintercept = -log10(0.001), color="darkgrey")+
      geom_point(data = new_data, 
                 aes(x = rank, y = -log10(fisher_pvalue), color = label),
                 size = 4, 
                 show.legend = F)+
      geom_text_repel(data = new_data,
                      aes(label= label),
                      nudge_x = 1000, 
                      nudge_y= 30,
                      size= 7,
                      max.overlaps = 10000)+
      #theme_cowplot()+
      labs(x= "Gene ranking", y= "-log10(p-val)")+
      scale_color_brewer(type="qual", palette = 3)+
      theme(axis.text= element_text(size= 15, color="black"),
            axis.title= element_text(size= 19, color="black"),
            strip.text.x = element_text(size = 20))
    rank_plot
   
  }
})
 
output$ctype_marker = renderPlotly({
  if (!is.null(input$select_gene)) {
    new_data_comp<- df.reheat2 %>% 
      filter(gene %in% input$select_gene) %>% 
      select(gene, starts_with("mrkr") )%>%
      pivot_longer(cols= starts_with("mrkrLFC"), names_to="ctype",values_to="mean_LFC")%>%
      pivot_longer(cols= starts_with("mrkrP"), names_to="ctype2",values_to="mean_P")%>%
      mutate(ctype = str_replace_all(ctype, "mrkrLFC_" , ""),
             ctype2 = str_replace_all(ctype2, "mrkrP_" , ""), 
             ctype = factor(ctype))%>% 
      filter(ctype==ctype2)
     
     p.2.1_consmarker <- new_data_comp %>% 
       mutate(#ctype = factor(ctype, levels = unique(new_data_comp$ctype)), 
         star= ifelse(mean_P<.0001 & mean_LFC > 2, "*", ""),
         mean_LFC= round(mean_LFC, 1)       )%>%
       #drop_na()%>%
       ggplot(aes(x= gene, y= ctype, fill=mean_LFC ))+
       geom_tile(color="white")+
       geom_text(aes(label = star))+
       scale_fill_gradient(low="white", high ="red", na.value = "grey")+
       labs(x= "", y= "")+
       theme(axis.text.x =element_text(angle= 45, hjust= 1))+
       coord_equal()
    ggplotly(p.2.1_consmarker, tooltip = c("mean_LFC"))
  }
})

output$multi_cell = renderPlot({
  if (!is.null(input$select_gene)) {
    new_data_mcell<- df.reheat2 %>%
      #filter(gene %in% input$select_gene)%>%
      pivot_longer(cols= starts_with("mcell_HFscore_"), 
                   names_to="ctype",
                   values_to="value")%>%
      mutate(ctype = str_replace_all(ctype, "mcell_HFscore_" , ""))
    
    pls <- lapply(unique(new_data_mcell$ctype), function(cell){
      
      #this parameter tells us how many points we want to plot 
      #from the background (more points more comp time)
      reso =round(nrow(new_data_mcell)/ 3000, 0)
      df.plot<- new_data_mcell %>% 
        filter(ctype==cell)%>%
        mutate(rank= rank(value, ties.method = "random"))%>%
        mutate(label = ifelse(gene %in% input$select_gene, gene, ""), 
               label = factor(label , levels =c("", input$select_gene)), 
               size_fac= factor(label != ""))%>% 
        #filter(label !="" | (label =="" & rank %% reso == 0) | rank %in% c(seq(1:50), seq(max(rank)-50, max(rank))))%>% #this step filters half the points to save memory 
        filter(label !="" | (label =="" & rank %% reso == 0) | rank %in% c(1, max(rank)))%>% #this step filters half the points to save memory 
        distinct(gene, value, size_fac,label, ctype)%>%
        drop_na()
     
      if(sum(!input$select_gene %in% df.plot$gene)<0){return(NULL)}
      df.plot %>%
        ggplot(aes(x= reorder(gene,value), y= value ))+
        facet_grid(~ctype)+
        geom_hline(yintercept = 0.1)+
        geom_hline(yintercept = -0.1)+
        scale_size_manual(values= c(1,4))+
        geom_point( aes(color = label, size= size_fac), show.legend = F)+
        geom_text_repel(aes(label= label), size= 6, max.overlaps = 10000)+
        labs(x= "", y= "")+
        scale_color_brewer(type="qual", palette = 3)+
        theme_classic()+
        theme(axis.text.y= element_text(size= 15, color="black"),
              axis.title= element_text(size= 19, color="black"),
              strip.text.x = element_text(size = 20))+
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank())
      
    })
    p.3.1_loadings<- cowplot::plot_grid(plotlist = pls)
    p.3.1_loadings
   # ggplotly(p.2.1_consmarker, tooltip = c("mean_P"))
  }
})

output$text_display <- renderUI({
  # decide which significance thresh from reheat1
  thresh_fisherp=0.001
  
  df.talk <- df.reheat2 %>% 
    filter(gene %in% input$select_gene)%>%
    rowwise() %>%
    mutate(regulated = ifelse(fisher_pvalue< thresh_fisherp & mean_t>0, 
                              "upregulated", 
                              ifelse(fisher_pvalue< thresh_fisherp & mean_t<0, 
                                     "downregulated", 
                                     "not significantly dysregulated"
                              )
    )
    ) %>%   
    mutate(across(starts_with("mcell_"), ~ replace_na(., 0))) 
  
  text_outputs <- map(input$select_gene, function(geneX){
    ctypes <- df.talk%>% 
      filter(gene== geneX)%>%
      pivot_longer(cols= starts_with("mcell_"), values_to = "mcell_val", names_to="mcell_ctype")%>%
      group_by(gene)%>%
      filter(abs(mcell_val)> 0.1)%>% 
      mutate(mcell_ctype= str_replace_all(mcell_ctype, "mcell_HFscore_" , ""))%>%
      pull(mcell_ctype)
    
    ctypes = paste0(ctypes, collapse=", ")
    
    if(ctypes== ""){ctypes = "no cell type"}
    
    paste0("<p><strong>", geneX, "</strong> is ", 
            df.talk %>% filter(gene ==geneX) %>% pull(regulated), 
           " in HF (bulk) and is in the top ",
           df.talk %>% filter(gene ==geneX) %>% pull(rank_percentile),
           "% of genes (Fisher combined p-value = ",
           df.talk %>% filter(gene ==geneX)%>% pull(fisher_pvalue)%>% format(., scientific = TRUE) ,
           "). It is possibly regulated ", 
           df.talk %>% filter(gene ==geneX) %>% pull(scell_consistency), 
           " with deregulation in ", unlist(ctypes) , ".</p>")
    
  })
  
  # Combine all text outputs into one HTML content
  HTML(paste(text_outputs, collapse = ""))
})

#### Multicell network ####

# #for test runs 
# input= list()
# input$selected_edge= 3
# input$network_type = "NF"
# edge_info = list()
#  edge_info$from ="Fib"
#  edge_info$to = "Fib"
# Reactive storage for edges
edges_reactive <- reactive({
  df_filtered <- df.depend %>% 
    filter((!!rlang::sym(input$network_type)) > 0.2) %>% 
    select(predictor, target, 
           weight = !!rlang::sym(input$network_type),
           lr = !!rlang::sym(paste0("lr_", input$network_type)))
  
  g1 <- graph_from_data_frame(df_filtered, directed = TRUE)
  nodes <- data.frame(id = V(g1)$name, label = V(g1)$name)
  
  num_nodes <- nrow(nodes)
  if (num_nodes <= 12) {
    node_colors <- brewer.pal(num_nodes, "Set3")
  } else {
    node_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_nodes)
  }
  nodes$color <- node_colors
  
  edges <- as_data_frame(g1, what = "edges") %>%
    mutate(
      id = as.character(1:n()),
      title = paste(from, " -> ", to),
      value = weight * 2,  # Scale width
      color = "darkgrey"
    )
  
  list(nodes = nodes, edges = edges)  # Return both nodes & edges
})

# Render Network
output$network <- renderVisNetwork({
  data <- edges_reactive()  # Get reactive nodes & edges
  visNetwork(data$nodes, data$edges) %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visEvents(selectEdge = "function(data) {
                  Shiny.setInputValue('selected_edge', data.edges[0], {priority: 'event'});
                }") %>%
    visPhysics(
      solver = "barnesHut",
      barnesHut = list(
        gravitationalConstant = -2000,
        springLength = 300,
        damping = 0.3
      )) %>%
    visNodes(font = list(size = 20, face = "bold"))
})

# Display Edge Information
output$selected_edge_info <- renderPrint({
  req(input$selected_edge)  # Ensure selection exists
  
  data <- edges_reactive()  # Get latest edges
  edge_info <- data$edges %>% filter(id == input$selected_edge)
  
  if (nrow(edge_info) > 0) {
    cat("Selected Edge:\n")
    cat("Selected Network:", input$network_type, "\n")
    cat("From:", edge_info$from, "\n")
    cat("To:", edge_info$to, "\n")
    cat("Edge strength:", round(edge_info$value, 2), "\n")
    cat("Number of LR pairs:", edge_info$lr, "\n")
  } else {
    cat("No matching edge found.\n")
  }
})

# 
# output$network <- renderVisNetwork({
#  
#   df_filtered <- df.depend %>% 
#     filter((!!rlang::sym(input$network_type)) > 0.2) %>% 
#     # Rename the chosen column to "weight" so that the rest of the code can use it generically
#     select(predictor, target, 
#            weight = !!rlang::sym(input$network_type),
#            lr = !!rlang::sym(paste0("lr_", input$network_type)))
#   
#   # Create and return the igraph object using the filtered data
#   g1= graph_from_data_frame(df_filtered, directed = TRUE)
#   # Example: Convert igraph object to visNetwork format
#   nodes <- data.frame(id = V(g1)$name, label = V(g1)$name)  # Extract node names
# 
#   
#   # Generate a color palette based on the number of nodes
#   num_nodes <- nrow(nodes)
#   # Use Set3 palette (or any palette you prefer)
#   if(num_nodes <= 12) {
#     node_colors <- brewer.pal(num_nodes, "Set3")
#   } else {
#     node_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_nodes)
#   }
#   
#   # Assign the colors to the nodes data frame
#   nodes$color <- node_colors
#   
#   edges <- as_data_frame(g1, what = "edges") %>%
#     mutate(
#       id = as.character(1:n()),
#       #label = as.character(round(HF, 2)),      # Edge label based on HF
#       title = paste(from, " -> ", to),       # Tooltip on hover
#       value = weight * 2                           # Scale width based on HF (adjust factor as needed)
#     )
#   edges$color <- "darkgrey"
#   
#   #print(edges)
#   #get vis
#   
#   visNetwork(nodes, edges) %>%
#     visEdges(arrows = "to") %>%
#     visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
#     visEvents(selectEdge = "function(data) {
#                   Shiny.setInputValue('selected_edge', data.edges[0], {priority: 'event'});
#                 }")%>%
#     visPhysics(
#       solver = "barnesHut",
#       barnesHut = list(
#         gravitationalConstant = -2000,  # More negative = more repulsion
#         springLength = 300,             # Increase spring length to push nodes apart
#         damping = 0.3
#       ))%>%
#     visNodes(font = list(size = 20, face = "bold"))
# })
# 
# output$selected_edge_info <- renderPrint({
#   req(input$selected_edge)  # Ensure an edge is selected
#   
#   selected_edge <- input$selected_edge
#   edge_info <- edges %>% filter(id == selected_edge)  # Match the edge ID
#   
#   if (nrow(edge_info) > 0) {
#     cat("Selected Edge:\n")
#     cat("Selected Network:", input$network_type, "\n")
#     cat("From:", edge_info$from, "\n")
#     cat("To:", edge_info$to, "\n")
#     cat("Edge strength:", round(edge_info$value*2,2),  "\n")
#     cat("Number of LR pairs:", edge_info$lr, "\n")
#   } else {
#     cat("No matching edge found.\n")
#   }
# })


output$LR_sankey <- renderPlotly({
  req(input$selected_edge)
  
  edges <- edges_reactive()[[2]]
  selected_edge <- input$selected_edge
  edge_info <- edges %>% filter(id == selected_edge)
  
  if (length(edge_info) > 0) {
    
    df_s <- df.comm %>% 
      ungroup()%>% 
      filter(sender == edge_info$from, 
             receiver == edge_info$to,
             group == input$network_type) %>% 
             #abs(interact) > 0.1 ) %>% 
      arrange(aupr_corrected)
    
    # Step 1: Extract unique nodes (ligands & receptors)
    nodes_s <- data.frame(
      name = unique(c(df_s$source_genesymbol, df_s$target_genesymbol))
    )
    
    # Create a color vector: if a node appears in the ligand list, color blue; otherwise green.
    # Note: if a node appears in both, this simple logic will give blue.
    node_colors <- ifelse(nodes_s$name %in% df_s$source_genesymbol, "darkblue", "darkred")
    
    # Step 2: Create links using node indexes
    links_s <- df_s %>%
      mutate(
        source = match(source_genesymbol, nodes_s$name) - 1,  # Convert to zero-based index
        target = match(target_genesymbol, nodes_s$name) - 1,
        value= abs(interact)
      ) %>%
      select(source, target, value )
    
    # Step 3: Build Sankey diagram
    p <- plot_ly(
      type = "sankey",
      node = list(
        label = nodes_s$name,      # Node labels
        color = node_colors
      ),
      link = list(
        source = links_s$source,   # Source node indexes
        target = links_s$target,   # Target node indexes
        value = links_s$value      # Link values (strength of interaction)
      )
    )
    
    # Step 4: Show plot
    print(p)
    
  }
  })

output$LR <- renderPlotly({
  req(input$selected_edge)  # Ensure an edge is selected
  
  selected_edge <- input$selected_edge
  edges <- edges_reactive()[[2]]
  edge_info <- edges %>% filter(id == selected_edge)  # Match the edge ID
  if (length(edge_info) > 0) {
  #if (nrow(edge_info) > 0) {
    
    df <- df.comm %>% 
      filter(sender == edge_info$from, 
             receiver == edge_info$to,
             group == input$network_type) %>% 
           #  abs(interact) > 0.1 ) %>% 
      arrange(aupr_corrected)
    
    p <- df %>%
      distinct(aupr_corrected, med.interact, pearson, source_genesymbol, n.cell) %>%
      ggplot(aes(x = aupr_corrected, y = abs(med.interact), text = source_genesymbol)) +
      #ggrepel::geom_label_repel(col = "black") +
      #geom_text(col = "black", nudge_x = 0.2) +
      geom_point(size = 5.3, color = "black") +
      geom_point(aes(color = factor(n.cell)), size = 5) +
      theme_half_open() +
      scale_color_brewer(type = "seq", palette = 1) +
      labs(x = "Nichenet regulatory potential",
           y = "L-R score", 
           color = "n(cell types)\nexpressing\nligand")
      #ggtitle(paste0("Ligands from ", edge_info$from, "\ntargeting ", edge_info$to))
    
    p<- ggplotly(p, tooltip = c("text")) %>% 
      event_register("plotly_click")  # Register click events
    print(p)
  } else {
    return(NULL)
  }
})


output$NN <- renderPlotly({
  req(input$selected_edge)  # Ensure an edge is selected
  edges <- edges_reactive()[[2]]
  selected_edge <- input$selected_edge
  edge_info <- edges %>% filter(id == selected_edge)  # Match the edge ID
  
  if (nrow(edge_info) > 0) {
    
    df <- df.nn %>% 
      filter(sender == edge_info$from, 
             receiver == edge_info$to,
             group == input$network_type,
             abs(value) > 0.1) %>% 
      arrange(aupr_corrected)
    p = 
      df %>% 
    #filter(target %in% target.genes.oi)%>%
    ggplot(aes(x= reorder(target,weight), y= reorder(test_ligand,weight),  fill = weight))+
    geom_tile()+
    #facet_grid(~sender, scales = "free")+
    scale_fill_gradient(low = "grey", high= "red")+
    theme_cowplot()+
    theme(axis.text.x = element_text(size= 10,  angle=90, hjust= 1, vjust= 0.5),
          axis.text.y= element_text(size=10),
          axis.line= element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
          )+
    labs(x= paste0(edge_info$to, " target genes"), 
         y= paste0(edge_info$from, " ligands"), 
         fill ="Regulatory\npotential")
  
    p<- ggplotly(p, tooltip = c("weight")) 
    print(p)
  } else {
    return(NULL)
  }
})

}


