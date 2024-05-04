########################### ready ###########################
# Clear plots

if(!is.null(dev.list())) dev.off()

# Clear console

cat("\014") 

# Clean work space

rm(list=ls())

# set path

setwd("D:\\research\\cnt")

########################### preprocess ###########################
# call functions

source("src\\epi\\lib\\define_global_variables.R")
source("src\\epi\\lib\\upload_data.R")
source("src\\epi\\lib\\methods_report_2009.R")
source("src\\epi\\lib\\methods_prj_study.R")
source("src\\epi\\lib\\methods_SEIR.R")
source("src\\epi\\lib\\methods_visualization.R")

# contact data with proper form

cnt_data <- upload_cnt_data()

########################### 2009 report ###########################
# visualize NGM of contact data with the form of 2009 report

visualize_matrix_2009() # 2009 report
visualize_matrix_cnt_str_2009(cnt_data) # 2020 survey

# bootstrap result of comparison of R0s

bt_2009_result_R0s <- bootstrap_results_with2009_Rtimes(cnt_data, 10000) # bootstrap
draw_bootstrap_result(bt_2009_result_R0s) # draw histogram

# bootstrap results comparing the prevalence

bt_2009_result <- bootstrap_result_prvl_with2009_Rtimes(cnt_data, 1000)
draw_bootstrap_prvl_result(bt_2009_result, "2009")

########################### projecting study ###########################
# visualize NGM of contact data with the form of projecting study

visualize_matrix_prj() # projection study
visualize_matrix_cnt_str_prj(cnt_data) # 2020 survey (use gam() for smoothing)

# bootstrap result of comparison of R0s

bt_prj_result_R0s <- bootstrap_results_withPOLYMOD_Rtimes(cnt_data, 10000) # bootstrap
draw_bootstrap_result(bt_prj_result_R0s) # draw histogram

# bootstrap results comparing the prevalence

bt_prj_result <- bootstrap_result_prvl_withPOLYMOD_Rtimes(cnt_data, 1000)
draw_bootstrap_prvl_result(bt_prj_result, "prj")
