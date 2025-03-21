# UI
source("sub/global.R")
source("sub/helper.R")

ui = function(request) {
  fluidPage(
    #useShinyjs(),
    #tags$head(includeScript("google-analytics.js")),
    navbarPage(
      id = "menu", 
      theme = shinytheme("simplex"),
      title = div(img(src="logo_saezlab.png", width="25", height="25"),
                  "ReHeat2-App"),
      windowTitle = "ReHeat2",
      collapsible=T,
      
      #### Welcome ####
      tabPanel(
        title = "Welcome",
        icon = icon("home"),
        sidebarPanel(
          includeMarkdown("inst/landingpage_sidebar.md")
        ),
        mainPanel(
          includeMarkdown("inst/landingpage.md")
        )
      ),
      
      #### Query genes ####
      tabPanel(
        title = "Query genes",
        icon = icon("search"),
        sidebarPanel(
          includeMarkdown("inst/query_genes_sidebar.md"),
          pickerInput(inputId = "select_gene", label = "Select gene(s)",
                      choices = sort(unique(df.reheat2$gene)),
                      multiple = T,
                      selected = c("NPPA", "MXRA5", "NRG1", "INHBA", "POSTN"),
                      options = list(`live-search` = TRUE,
                                     size=6, `max-options` = 6)),
          actionButton("reset_genes", "Reset genes")
        ),
        mainPanel(
          h2("1. Bulk Transcriptomics"),
          br(), 
          h3("1.1 Regulation of Queried Genes Across Individual Studies"),
          # fluidRow(
          #   splitLayout(cellWidths = c("30%", "70%"), 
          #               imageOutput("scheme_bulk", width = "auto", height = "auto"),
          #               plotlyOutput("gene_regulation_boxplot", width = "100%", height = "300px") %>%
          #                  withSpinner()
          #   )
          #   # plotOutput("rank_position", width = "50%", height = "500px") %>%
          #   #   withSpinner())
          # ),
          plotlyOutput("gene_regulation_boxplot", width = "100%", height = "300px") %>%
                              withSpinner(),
          p("Comparison of the reported log2 fold change (y-axis) for queried genes (x-axis) across 20 individual bulk transcriptomic studies."),
         # hr(), 
          br(),
         h3("1.2 Position of Queried Genes in the Consensus Heart Failure Gene Ranking"),
         br(),
          fluidRow(
            splitLayout(cellWidths = c("22%", "78%"), 
                        imageOutput("scheme_cons", width= "auto", height= "auto"),
                        plotOutput("rank_position", width = "100%", height = "330px") %>%
                          withSpinner()
                        )
                        # plotOutput("rank_position", width = "50%", height = "500px") %>%
                        #   withSpinner())
          ),
          p("The consensus bulk signature of heart failure is a gene ranking derived from Fisher's combined p-value.
            Genes with lower ranks (toward the left) have a higher probability of being associated with heart failure.
            We split the ranking into down- and upregulated genes (panels left and right, respectively).
            The vertical line marks the top 500 genes, while the horizontal line represents a Fisher p-value threshold of 0.05."
            ), 
          br(),
          hr(),
          br(),
          h2("2. Gene Expression in Single-Nucleus Transcriptomics"),
          br(), 
          h3("2.1 Classification of Queried Genes as Cell Type Markers"),
          # fluidRow(
          #   splitLayout(cellWidths = c("30%", "70%"), 
          #               imageOutput("scheme_sc", width= "auto", height= "auto"),
          #               plotlyOutput("ctype_marker", width = "400", height = "350px") %>%
          #                 withSpinner(),
          #   )
         plotlyOutput("ctype_marker", width = "400", height = "350px") %>%
           withSpinner(),
         p("Comparison of log2 fold changes for testing expression of the queried gene
   between cell types (one vs. the rest) per single-nucleus data set. The mean log2 fold change is displayed, and an asterisk (*) indicates a Fisher combined p-value < 10⁻¹⁵."),
         br(), 
         h3("2.2 Dysregulation of Queried Genes in the Multicellular Program of Heart Failure"),
          
          imageOutput("scheme_multi", width = "40%", height = "auto"),
          p("In this example, Gene X is upregulated in HF in cell type A while it is not deregulated in cell type B. HF, heart failure; NF, non-failing"),
          br(),
          plotOutput("multi_cell", width = "100%", height = "650px") %>%
            withSpinner(),
         p("Visualization of the assigned gene loadings in the multicellular program of heart failure (MCP1). 
   Genes are sorted by loading (x-axis), with negative loadings associating with heart failure and positive loadings associating with non-failing patients. 
   Each panel represents the program for one cell type. Horizontal lines indicate loadings of 0.1 and -0.1."),
          br(),
          hr(),
          br(),
          h2("3. Conclusion"),
           br(),
         h4("3.1 Overview of Gene Regulation in Relation to Molecular and Compositional Changes"),
         br(), 
           fluidRow(
            splitLayout(cellWidths = c("90%", "10%"), 
                        imageOutput("scheme_compmol", width = "30%", height = "auto"),
                        
                        )
            ),
         p("Visualization of the different expression patterns underlying gene deregulation in disease. We categorize these groups based on the inferred expression patterns to better understand the organization of gene deregulation."),
         
         br(), 
         h4("3.2. Automated Summary of Observed Expression Patterns"), 
         br(), 
          htmlOutput("text_display"),  # Display the text outputs as HTML
          br(),  
          hr(),
          hr()
          # tabsetPanel(
          #   type = "tabs",
          #   tabPanel("Consensus", DT::dataTableOutput("summary_sub")),
          #   tabPanel("Individual", DT::dataTableOutput("individual_sub"))
          # )
        )
      ),
      
      tabPanel(
        title = "Multicellular network",
        icon = icon("project-diagram"),
        sidebarPanel(
          includeMarkdown("inst/network_sidebar.md"),
          radioButtons(
            inputId = "network_type",
            label = "Select network type:",
            choices = c("Heart failure" = "HF", "Non-failing" = "NF"),
            selected = "HF",
            inline = TRUE
          )
          #includeMarkdown("inst/query_genes_sidebar.md"),
          
        ),
        mainPanel(
          
          h2("1. Multicellular network"),
          visNetworkOutput("network"),
          p("Multicellular information network captured by multicellular program 1, 
          where each arrow describes how important the expression of a given cell-type is to predict the expression profile of another one. 
          Predictive importances come from linear mixed models of cell-type signatures of MCP1. 
          Importances below 0.2 are not included. Please see our mansuscript for details."),
          p("Cardiomyocytes (CM), fibroblasts (Fib), pericytes (PC), and endothelial (Endo), vascular smooth muscle (vSMCs) cells. Heart failure (HF), and non-failing (NF) hearts."),
          
          br(),
          h2("2. Edge exploration"),
          verbatimTextOutput("selected_edge_info"), 
          p("The selected edge can be explored for possible ligand receptor interactions. These were derived independently from the predictive importances. "),
          br(), 
          h3("2.1 Ligand-Receptors"),
          plotlyOutput("LR_sankey", height = "600px"),
          p("Sankey plot shows a network of ligand (left) - receptor (right) pairs, of a selected edge from the multicellular network. 
   The Ligand receptor pairs were derived by identifying known ligand-receptor pairs from a meta-database (", 
                                                        a("LIANA+", href = "https://www.nature.com/articles/s41556-024-01469-w", target = "_blank"), 
                                                        "), and filtered for high gene loadings in the multicellular program 1."),
          
          br(),
          h3("2.2 Ligand prioritization"),
          plotlyOutput("LR"),
          p("Scatterplot compares ligands for their possible importance based on two criteria. NicheNet derived a regulatory potential (corrected AUPR, x-axis) of a given ligand to deregulate a gene signature which here represents extreme gene loadings of the target cell type taken from the MCP1. 
            L-R score (y-axis) represent the mean gene loadings of the ligand and the receptor of a given cell type pair. If a ligand connected with multiple receptors, the median L-R score was calculated. Color represent the number of cell types that express this ligand."),
          br(),
          h3("2.3 Ligand - target genes"),
          plotlyOutput("NN", height= "800px"),
          p("Regulatory potential score, as estimated by ", 
            a("NicheNet", href = "https://www.nature.com/articles/s41596-024-01121-9", target = "_blank"), 
            ", represents the potential of ligands in contributing to the regulation of the target cell-type genes."),
          br(),
          br(),
          br()
        )
        ),
      

      
      #### Footer ####
      footer = column(12, align="center", "ReHeaT2-App 2024")
    ) # close navbarPage
  ) # close fluidPage
}
