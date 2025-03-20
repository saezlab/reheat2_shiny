library(shiny)
library(shinyWidgets)
library(shinyhelper)
library(shinythemes)
library(shinycssloaders)
library(forcats)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggrepel)
library(cowplot)


theme_set(theme_cowplot())

# options(repos = BiocManager::repositories())

p.linefit <- readRDS("data/plot_metaranking.rds")
df.reheat2<- readRDS("data/reheat2_stats_bulk2.rds")
df.reheat1<- readRDS("data/reheat1_tidy.rds")

#p.linefit <- ggplot2::ggplotGrob(ggplot2::ggsave("data/p_linefit_intermediate.png"))




