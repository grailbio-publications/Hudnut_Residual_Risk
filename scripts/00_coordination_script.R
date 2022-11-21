#coordination script

library(tidyverse)
library(patchwork)
library(diagram)

date_code<-"20220308"

#execute code to generate tables/figures

source("scripts/01_setup_table.R")
source("scripts/02_compute_performance.R")
source("scripts/03_plot_paths_through_tree.R")
source("scripts/04_plot_abstract_decision_tree.R")
source("scripts/101_supplemental_ppv_by_incidence.R")