########## call library ##########
require(boot)
require(parallel)
require(cowplot)
require(ggpubr)
require(reshape)
require(xtable)

##################################
#                                #
#  Visualizing method of matrix  #
#                                #
##################################

visualize_matrix <- function(input_matrix, tick_vals=NaN, title_val=NaN){
  long_data <- melt(input_matrix)
  long_data <- long_data[long_data$value != 0,]
  
  ggplot(long_data, aes(x=X2, y=X1)) +
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient2(guide = guide_legend(label.theme = element_text(angle=0),
                                              reverse=TRUE)) +
    scale_x_continuous(breaks = c(1:length(tick_vals)), labels = tick_vals) +
    scale_y_continuous(breaks = c(1:length(tick_vals)), labels = tick_vals) +
    labs(x="Age participants", y="Age contacts") + 
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       legend.title = element_blank(),
                       legend.position = "right")
}

##################################
#                                #
#    See the NGM structured as   #
#           2009 report          #
#                                #
##################################

age_breaks_2009 <- c("0-1", "2-3", "4-10", "11-16", "17-20", "21-24", "25-34",
                "35-44", "45-64", "65+")

# ngm from the 2009 report
ngm_real_2009 <- get_ngm_from_report_2009()

visualize_matrix_2009 <- function(){
  visualize_matrix(ngm_real_2009, age_breaks_2009,
                   )
}

ngm_col_sum_real_2009 <- colSums(ngm_real_2009)
ngm_real_2009_summary <- rbind(ngm_real_2009, ngm_col_sum_real_2009)
colnames(ngm_real_2009_summary) <- age_breaks_2009
rownames(ngm_real_2009_summary) <- c(age_breaks_2009, "Sum")

latex_table_summary_2009 <- function(){
  xtable(ngm_real_2009_summary)
}

# ngm structured as 2009 report
visualize_matrix_cnt_str_2009 <- function(cnt_data){
  cnt_data_2009 <- subset(cnt_data, part_age < 70 & cnt_age_est_max < 70)
  ngm_str_2009 <- convert_data_to_ngm_str_2009(cnt_data_2009)
  
  visualize_matrix(ngm_str_2009, age_breaks_2009,
                   "Mean reciprocal contact number with the structure of 2009 report")
}

latex_table_summary_cnt_str_2009 <- function(cnt_data){
  cnt_data_2009 <- subset(cnt_data, part_age < 70 & cnt_age_est_max < 70)
  ngm_str_2009 <- data_to_mat_2009(cnt_data_2009)
  
  ngm_col_sum_str_2009 <- colSums(ngm_str_2009)
  ngm_str_2009_summary <- rbind(ngm_str_2009, ngm_col_sum_str_2009)
  colnames(ngm_str_2009_summary) <- age_breaks_2009
  rownames(ngm_str_2009_summary) <- c(age_breaks_2009, "Sum")
  
  xtable(ngm_str_2009_summary)
}

##################################
#                                #
#    See the NGM structured as   #
#            projection          #
#                                #
##################################

age_breaks_prj <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                "70-74", "75+")

# ngm from the projection study
ngm_real_prj <- get_ngm_from_polymod_prj()

visualize_matrix_prj <- function(){
  visualize_matrix(ngm_real_prj, age_breaks_prj,
                   "Mean reciprocal contact number from the projection study")
}

ngm_col_sum_real_prj <- colSums(ngm_real_prj)
ngm_real_prj_summary <- rbind(ngm_real_prj, ngm_col_sum_real_prj)
colnames(ngm_real_prj_summary) <- age_breaks_prj
rownames(ngm_real_prj_summary) <- c(age_breaks_prj, "Sum")

latex_table_summary_prj <- function(){
  xtable(ngm_real_prj_summary)
}

# ngm structured as projection study
visualize_matrix_cnt_str_prj <- function(cnt_data){
  cnt_data_prj <- subset(cnt_data, part_age < 80 & cnt_age_est_max < 80)
  ngm_str_prj <- convert_data_to_ngm_str_prj(cnt_data_prj)
  
  visualize_matrix(ngm_str_prj, age_breaks_prj,
                   "Mean reciprocal contact number with the structure of projection study")
}

latex_table_summary_cnt_str_prj <- function(cnt_data){
  cnt_data_prj <- subset(cnt_data, part_age < 80 & cnt_age_est_max < 80)
  ngm_str_prj <- data_to_mat_prj(cnt_data_prj)
  
  ngm_col_sum_str_prj <- colSums(ngm_str_prj)
  ngm_str_prj_summary <- rbind(ngm_str_prj, ngm_col_sum_str_prj)
  colnames(ngm_str_prj_summary) <- age_breaks_prj
  rownames(ngm_str_prj_summary) <- c(age_breaks_prj, "Sum")
  
  xtable(ngm_str_prj_summary)
}



draw_bootstrap_result <- function(bt_output){
  boot_output <- bt_output[[1]]
  result_R0s <- bt_output[[2]]
  
  xlim_axis <- c(min(c(boot_output$t, boot_output$t0))-0.001,
                 max(c(boot_output$t, boot_output$t0))+0.001)
  
  # make a density histogram from the results
  phist <- gghistogram(
    data = result_R0s, x = "R0.ratio", bins=100,
    add = "mean",
    fill = "#a1d76a", xlim = xlim_axis
  ) 
  
  pdensity <- ggdensity(
    data = result_R0s, x = "R0.ratio",
    color = "black", alpha = 0, xlim = xlim_axis
  ) + scale_y_continuous(
    position = "right"
  ) + theme_half_open(11, rel_small = 1) +
    rremove("x.axis") + rremove("xlab") + 
    rremove("x.text") + rremove("x.ticks") + rremove("legend") +  
    geom_vline(xintercept = boot_output$t0, 
               linetype = "solid", color = "#e9a3c9", size = 1)
  
  aligned_plots <- align_plots(phist, pdensity, align = "hv", axis = "tblr")
  ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
}

##################################
#                                #
#   See the bootstrapped result  #
#          of prevalence         #
#                                #
##################################

# Default palette of ggplot2
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

draw_prvl_result_age_groups <- function(usual_case, cnt_data, age_group){
  if(usual_case=="2009"){
    cnt_mat <- get_contact_rate_report_2009()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_2009(cnt_data)
    transmissibility <- trans_2009_report
    
    pop_vec <- get_population_2007()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- get_contact_rate_polymod_prj()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_prj(cnt_data)
    transmissibility <- trans_polymod_prj
    
    pop_vec <- get_population_2015()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- transmissibility * cnt_mat
  waifw_mat_2020 <- transmissibility * cnt_mat_2020
  num_groups <- length(pop_vec)
  
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  out_2020 <- solve_SEIR(waifw_mat_2020, pop_vec, delta_t)
  time_stamp <- out[,1]
  
  prvl <- out[,c(1,2*num_groups + 1 + (1:num_groups))]
  colnames(prvl) <- c("time_stamp", age_string_vec)
  prvl_2020 <- out_2020[,c(1,2*num_groups + 1 + (1:num_groups))]
  colnames(prvl_2020) <- c("time_stamp", age_string_vec)
  
  prvl <- melt(prvl[,-1])
  prvl[,1] <- time_stamp[(prvl[,1]-1)%%num_grids+1]
  colnames(prvl) <- c("time_stamp", "age_groups", "value")
  prvl_2020 <- melt(prvl_2020[,-1])
  prvl_2020[,1] <- time_stamp[(prvl_2020[,1]-1)%%num_grids+1]
  colnames(prvl_2020) <- c("time_stamp", "age_groups", "value")
  
  prvl$age_groups <- factor(
    prvl$age_groups,
    levels = age_string_vec
  )
  prvl_2020$age_groups <- factor(
    prvl_2020$age_groups,
    levels = age_string_vec
  )
  
  prvl <- subset(prvl, age_groups == age_string_vec[age_group])
  prvl <- cbind(prvl, contact = "usual")
  prvl_2020 <- subset(prvl_2020, age_groups == age_string_vec[age_group])
  prvl_2020 <- cbind(prvl_2020, contact = "2020")

  # prvl_data <- data.frame(time_stamp = prvl$time_stamp,
  #                         prvl_usual = prvl$value,
  #                         prvl_2020 = prvl_2020$value)
  prvl_data <- rbind(prvl, prvl_2020)
  
  # make a subplots for age-specific prevalence
  ggplot(prvl_data, aes(x=time_stamp, y=value, col = contact)) + 
    geom_line(
      size=1.5
    ) + 
    theme(
      legend.position = "right"
    ) + 
    labs(title=paste("Prevalence of age ", age_string_vec[age_group]),
         x="Time [days]", y="Prevalence")
}


draw_prvl_result_cmp <- function(usual_case, cnt_data, init_group){
  if(usual_case=="2009"){
    cnt_mat <- get_contact_rate_report_2009()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_2009(cnt_data)
    transmissibility <- trans_2009_report
    
    pop_vec <- get_population_2007()
    pop_vec_2020 <- get_population_2020_str_2009()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- get_contact_rate_polymod_prj()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_prj(cnt_data)
    transmissibility <- trans_polymod_prj
    
    pop_vec <- get_population_2015()
    pop_vec_2020 <- get_population_2020_str_prj()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- transmissibility * cnt_mat
  waifw_mat_2020 <- transmissibility * cnt_mat_2020
  num_groups <- length(pop_vec)
  
  out <- solve_SEIR_concentrated_initial(waifw_mat, pop_vec, 
                                         delta_t, init_group)
  out_2020 <- solve_SEIR_concentrated_initial(waifw_mat_2020, pop_vec_2020, 
                                              delta_t, init_group)
  
  time_stamp <- out[,1]
  prvl <- cbind(time_stamp, 
                rowSums(out[,c(2*num_groups + 1 + (1:num_groups))]))
  colnames(prvl) <- c("time_stamp", "prevalence")
  prvl_2020 <- cbind(time_stamp, 
                     rowSums(out_2020[,c(2*num_groups + 1 + (1:num_groups))]))
  colnames(prvl_2020) <- c("time_stamp", "prevalence")
  
  prvl <- data.frame(prvl, contact = "usual")
  prvl_2020 <- data.frame(prvl_2020, contact = "2020")
  prvl_data <- rbind(prvl, prvl_2020)
  
  # make a subplots for age-specific prevalence
  ggplot(prvl_data, aes(x=time_stamp, y=prevalence, col = contact)) + 
    geom_line(
      size=1.5
    ) + 
    theme(
      legend.position = "right"
    ) + 
    labs(title=paste("Prevalence with initial infectious at the age group",
                     age_string_vec[init_group]),
         x="Time [days]", y="Prevalence")
  
  ggsave(file = paste("prevalence_comparison_with_initial_at_", 
                      age_string_vec[init_group],
                      "_within_",
                      usual_case,"str.png", sep=""))
}

draw_prvl_result <- function(usual_case){
  if(usual_case=="2009"){
    cnt_mat <- get_contact_rate_report_2009()
    transmissibility <- trans_2009_report
    
    pop_vec <- get_population_2007()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- get_contact_rate_polymod_prj()
    transmissibility <- trans_polymod_prj
    
    pop_vec <- get_population_2015()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- transmissibility * cnt_mat
  num_groups <- length(pop_vec)
  
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  time_stamp <- out[,1]
  
  prvl <- out[,c(1,2*num_groups + 1 +(1:num_groups))]
  colnames(prvl) <- c("time_stamp", age_string_vec)
  
  prvl <- melt(prvl[,-1])
  prvl[,1] <- time_stamp[(prvl[,1]-1)%%num_grids+1]
  colnames(prvl) <- c("time_stamp", "age_groups", "value")
  
  prvl$age_groups <- factor(
    prvl$age_groups,
    levels = age_string_vec
  )
  
  # make a subplots for age-specific prevalence
  ggplot(prvl, aes(x=time_stamp, y=value)) + 
    facet_wrap(~age_groups, scales = "free", ncol=3) + 
    geom_line(
      data=prvl, 
      aes(y=value, color=age_groups),
      size=1.5
    ) + 
    theme(legend.position = "none") +
    labs(x="Time [days]", y="Prevalence")
}

draw_prvl_result_2020 <- function(usual_case, cnt_data){
  if(usual_case=="2009"){
    cnt_mat <- convert_data_to_contact_rate_str_2009(cnt_data)
    transmissibility <- trans_2009_report
    
    pop_vec <- get_population_2020_str_2009()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- convert_data_to_contact_rate_str_prj(cnt_data)
    transmissibility <- trans_polymod_prj
    
    pop_vec <- get_population_2020_str_prj()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- transmissibility * cnt_mat
  num_groups <- length(pop_vec)
  
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  time_stamp <- out[,1]
  
  prvl <- out[,c(1,2*num_groups + 1 +(1:num_groups))]
  colnames(prvl) <- c("time_stamp", age_string_vec)
  
  prvl <- melt(prvl[,-1])
  prvl[,1] <- time_stamp[(prvl[,1]-1)%%num_grids+1]
  colnames(prvl) <- c("time_stamp", "age_groups", "value")
  
  prvl$age_groups <- factor(
    prvl$age_groups,
    levels = age_string_vec
  )
  
  # make a subplots for age-specific prevalence
  ggplot(prvl, aes(x=time_stamp, y=value)) + 
    facet_wrap(~age_groups, scales = "free", ncol=3) + 
    geom_line(
      data=prvl, 
      aes(y=value, color=age_groups),
      size=1.5
    ) + 
    theme(legend.position = "none") +
    labs(x="Time [days]", y="Prevalence")
}

draw_prvl_result_R0_proportionals <- function(usual_case, cnt_data){
  if(usual_case=="2009"){
    cnt_mat <- get_contact_rate_report_2009()
    transmissibility <- trans_2009_report
    rat <- ratio_R0s_2020_of_2009(cnt_data)
    
    pop_vec <- get_population_2007()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- get_contact_rate_polymod_prj()
    transmissibility <- trans_polymod_prj
    rat <- ratio_R0s_2020_of_prj(cnt_data)
    
    pop_vec <- get_population_2015()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- rat * transmissibility * cnt_mat
  num_groups <- length(pop_vec)
  
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  time_stamp <- out[,1]
  
  prvl <- out[,c(1,2*num_groups + 1 +(1:num_groups))]
  colnames(prvl) <- c("time_stamp", age_string_vec)
  
  prvl <- melt(prvl[,-1])
  prvl[,1] <- time_stamp[(prvl[,1]-1)%%num_grids+1]
  colnames(prvl) <- c("time_stamp", "age_groups", "value")
  
  prvl$age_groups <- factor(
    prvl$age_groups,
    levels = age_string_vec
  )
  
  # make a subplots for age-specific prevalence
  ggplot(prvl, aes(x=time_stamp, y=value)) + 
    facet_wrap(~age_groups, scales = "fixed", ncol=3) + 
    geom_line(
      data=prvl, 
      aes(y=value, color=age_groups),
      size=1.5
    ) + 
    theme(legend.position = "none") +
    labs(x="Time [days]", y="Prevalence")
}

draw_bootstrap_prvl_result <- function(bt_output, usual_case){
  bt_output_t0_with_boot0 <- cbind(boot_cases=0, bt_output$t0)
  bt_result_combined <- rbind(bt_output_t0_with_boot0, bt_output$t)
  
  bt_result_ci_95 <- aggregate(
    value~time_stamp+age_groups, 
    bt_result_combined, 
    FUN=function(x) {quantile(x,c(0.025, 0.975))}
  )
  bt_result_ci_95[,c(3,4)] <- bt_result_ci_95$value
  colnames(bt_result_ci_95) <- 
    c("time_stamp", "age_groups", 
      "limit_low", "limit_high")
  
  drawing_data <- bt_output$t0
  drawing_data <- merge(
    drawing_data,
    bt_result_ci_95,
    by = c("time_stamp", "age_groups")
    )
  drawing_data$age_groups <- factor(
    drawing_data$age_groups,
    levels = if(usual_case=="2009") {getstring_age_breaks_str_2009()}
    else {getstring_age_breaks_str_prj()}
  )
  
  # make a subplots for age-specific prevalence
  ggplot(drawing_data, aes(x=time_stamp, y=value)) + 
    facet_wrap(~age_groups, scales = "free", ncol=3) + 
    geom_line(
      data=drawing_data, 
      aes(y=value, color=age_groups),
      size=1.5
    ) + 
    geom_ribbon(
      data=drawing_data, 
      aes(y=value, ymin=limit_low, ymax=limit_high, fill=age_groups), 
      alpha = 0.5
    ) + 
    theme(legend.position = "none") +
    labs(x="Time [days]", y="Prevalence")
}

get_peak_times_solution <- function(waifw_mat, pop_vec, delta_t){
  num_groups <- length(pop_vec)
  peak_times <- matrix(0, nrow=(num_groups+1), ncol=(num_groups+1))
  
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  time_stamp <- out[,1]
  
  prvl <- out[,c(2*num_groups + 1 +(1:num_groups))]
  prvl <- data.frame(prvl, rowSums(prvl))
  peak_times[1,] <- time_stamp[max.col(t(prvl))]
  
  for (i in 1:num_groups)
  {
    out <- solve_SEIR_concentrated_initial(waifw_mat, pop_vec, delta_t, i)
    time_stamp <- out[,1]
    
    prvl <- out[,c(2*num_groups + 1 +(1:num_groups))]
    prvl <- data.frame(prvl, rowSums(prvl))
    peak_times[(i+1),] <- time_stamp[max.col(t(prvl))]
  }
  
  return(peak_times)
}

get_peak_times_usual_case_cnt_data <- function(usual_case, cnt_data){
  if(usual_case=="2009"){
    cnt_mat <- get_contact_rate_report_2009()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_2009(cnt_data)
    transmissibility <- trans_2009_report
    
    pop_vec <- get_population_2007()
    pop_vec_2020 <- get_population_2020_str_2009()
    age_string_vec <- getstring_age_breaks_str_2009()
  } else {
    cnt_mat <- get_contact_rate_polymod_prj()
    cnt_mat_2020 <- convert_data_to_contact_rate_str_prj(cnt_data)
    transmissibility <- trans_polymod_prj
    
    pop_vec <- get_population_2015()
    pop_vec_2020 <- get_population_2020_str_prj()
    age_string_vec <- getstring_age_breaks_str_prj()
  }
  
  waifw_mat <- transmissibility * cnt_mat
  waifw_mat_2020 <- transmissibility * cnt_mat_2020
  num_groups <- length(pop_vec)
  
  peak_time_tables <- list()
  peak_time_tables$old <- get_peak_times_solution(waifw_mat, 
                                                 pop_vec, 
                                                 delta_t)
  peak_time_tables$now <- get_peak_times_solution(waifw_mat_2020, 
                                                 pop_vec_2020, 
                                                 delta_t)
  
  colnames(peak_time_tables$old) <- c(paste("at", age_string_vec, sep="_"), 
                                      "total")
  rownames(peak_time_tables$old) <- c("uniform", 
                                      paste("in", age_string_vec, sep="_"))
  colnames(peak_time_tables$now) <- c(paste("at", age_string_vec, sep="_"), 
                                      "total")
  rownames(peak_time_tables$now) <- c("uniform", 
                                      paste("in", age_string_vec, sep="_"))
  
  return(peak_time_tables)
}
