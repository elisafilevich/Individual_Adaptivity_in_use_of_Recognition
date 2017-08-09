#' ---
#' title: Get all results and figures for Filevich, Kühn and Horn 
#' author: Elisa Filevich
#' output: 
#'   html_document:
#'     code_folding: show       # code is hidden, but can be seen by clicking on "Code" buttons
#'     number_sections: true
#'     toc: true                # yes, a table of contents 
#'     toc_float:               # - floating (i.e. only remaining visible despite scrolling page)
#'       collapsed: false       
#'       smooth_scroll: false
#' 
#' ---


# --------------------------------------------------------------------------------------------------------------------------------
# Load all "preprocessed" data
load(file = paste(fullPath, '/data/modelresults.Rda', sep=""))
load(file = paste(fullPath, '/data/modelresults_long.Rda', sep=""))
load(file = paste(fullPath, '/data/congruencyIndex.Rda', sep=""))
load(file = paste(fullPath, '/data/resultsRH_perSubj.Rda', sep=""))
load(file = paste(fullPath, '/data/resultsRH_perTrial.Rda', sep=""))

# --------------------------------------------------------------------------------------------------------------------------------

# This does the actual analyses 
{{spin_child(paste(fullPath,'/scripts/FilevichEtAl_allMainResults.R', sep=""))}}

{{spin_child(paste(fullPath,'/scripts/plot_calculate_split_violins.R', sep=""))}}
{{spin_child(paste(fullPath,'/scripts/FilevichEtAl_plot_fig2.R', sep=""))}}
{{spin_child(paste(fullPath,'/scripts/FilevichEtAl_plot_fig3.R', sep=""))}}


# Run and knit this stript with 
# library(knitr)
# rmarkdown::render('/scripts/Filevichetal_1_AllSubjs.R')