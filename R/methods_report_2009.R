##################################
#                                #
#    provide a contact matrix    #
#      formed 2009 report        #
#                                #
##################################

require(tidyr)
require(dplyr)

########################### Generals ###########################

get_age_breaks_str_2009 <- function(){
  age_group_cut <- c(0, 2, 4, 11, 17, 21, 25, 35, 45, 65, 70)
}

getstring_age_breaks_str_2009 <- function(){
  age_group_cut <- c("0-1", "2-3", "4-10", "11-16", "17-20",
                     "21-24", "25-34", "35-44", "45-64", "65+")
}

get_population_2020_str_2009 <- function(){
  # population is from the KOSIS data 
  # https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1BPA001&conn_path=I3
  # Caution! originally 2009 report set the population of 2007 is under or 
  #   equal to 69. Therefor, the following 2020 population is cut also.
  population_group <- c(628612, 725624, 3139938, 2710477, 2210421, 2707304, 
                        6956867, 7774935, 16800969, 2659790)
}

convert_data_to_contact_rate_str_2009 <- function(cnt_data){
  age_group_cut <- get_age_breaks_str_2009()
  age_group_number <- length(age_group_cut)-1
  
  population_group <- get_population_2020_str_2009()
  
  part_group <- cut(cnt_data$part_age, breaks = age_group_cut, right = FALSE,
                    labels = c(1:age_group_number))
  cnt_group <- cut(cnt_data$cnt_age, breaks = age_group_cut, right = FALSE,
                   labels = c(1:age_group_number))
  
  # data matrix: the matrix elements for total number of contacts have value of
  # from j-th-grouped participants to i-th-grouped people
  data_mat <- table(cnt_group, part_group) # row: cnt_group, col: part_group
  
  cnt_matrix <- matrix(0, nrow = age_group_number, ncol = age_group_number)
  exist_row <- as.numeric(rownames(data_mat))
  exist_col <- as.numeric(colnames(data_mat))
  cnt_matrix[exist_row, exist_col] <- data_mat
  
  # average number of contacts for each age group
  part_id <- cnt_data$part_id
  part_info <- unique(data.frame(part_id, part_group))
  
  number_of_part <- matrix(0, nrow = 1, ncol = age_group_number)
  number_of_part[exist_col] <- table(part_info$part_group)
  number_of_part <- as.numeric(number_of_part)
  
  avg_matrix <- cnt_matrix %*% diag(1/number_of_part)
  rat_matrix <- avg_matrix / population_group[row(avg_matrix)]
  R0_matrix <- (rat_matrix + t(rat_matrix))/2
  
  return(R0_matrix)
}

convert_data_to_ngm_str_2009 <- function(cnt_data) {
  population_group <- get_population_2020_str_2009()
  R0_matrix <- convert_data_to_contact_rate_str_2009(cnt_data)
  
  R0_matrix <- R0_matrix * population_group[row(R0_matrix)]
  
  return(R0_matrix)
}

get_contact_rate_report_2009 <- function(){
  orig_matrix <- matrix(
    c(4.05e-7, 7.96e-8, 5.75e-8, 4.18e-8, 0.00e+0, 2.10e-8, 1.37e-7, 7.60e-8, 3.71e-8, 7.04e-8,
      7.96e-8, 8.24e-7, 1.84e-7, 1.97e-8, 4.78e-8, 4.28e-8, 1.89e-7, 1.85e-7, 4.76e-8, 4.43e-8,
      5.75e-8, 1.84e-7, 2.04e-6, 1.52e-7, 2.06e-8, 3.29e-8, 1.53e-7, 2.49e-7, 6.50e-8, 9.24e-8,
      4.18e-8, 1.97e-8, 1.52e-7, 2.52e-6, 1.81e-7, 5.16e-8, 7.03e-8, 2.25e-7, 1.36e-7, 1.11e-7,
      0.00e-0, 4.78e-8, 2.06e-8, 1.81e-7, 2.44e-6, 3.40e-7, 1.65e-7, 2.10e-7, 1.94e-7, 7.31e-8,
      2.10e-8, 4.28e-8, 3.29e-8, 5.16e-8, 3.40e-7, 1.29e-6, 3.25e-7, 1.85e-7, 2.17e-7, 7.52e-8,
      1.37e-7, 1.89e-7, 1.53e-7, 7.03e-8, 1.65e-7, 3.25e-7, 4.74e-7, 2.78e-7, 2.11e-7, 8.64e-8,
      7.60e-8, 1.85e-7, 2.49e-7, 2.25e-7, 2.10e-7, 1.85e-7, 2.78e-7, 5.02e-7, 3.05e-7, 1.99e-7,
      3.71e-8, 4.76e-8, 6.50e-8, 1.36e-7, 1.94e-7, 2.17e-7, 2.11e-7, 3.05e-7, 4.83e-7, 3.15e-7,
      7.04e-8, 4.43e-8, 9.24e-8, 1.11e-7, 7.31e-8, 7.52e-8, 8.64e-8, 1.99e-7, 3.15e-7, 1.66e-6),
    nrow = 10, ncol = 10
  )
  
  return(orig_matrix)
}

get_population_2007 <- function(){
  # population is from the KOSIS data
  # https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1BPA001&conn_path=I3
  # Caution! originally 2009 report set the population of 2007 is under or 
  #   equal to 69.
  population_group_2007 <- c(897322, 922728, 4116852, 4117758, 2457634, 
                             2766647, 8097282, 8605797, 11940942, 1804867)
  
  return(population_group_2007)
}

get_ngm_from_report_2009 <- function(){
  population_group_2007 <- get_population_2007()
  orig_matrix <- get_contact_rate_report_2009()
  
  ngm_matrix <- orig_matrix * population_group_2007[row(orig_matrix)]
}

########################### R0 ratio ###########################

get_dom_eig_from_report_2009 <- function(){
  ngm_matrix <- get_ngm_from_report_2009()
  
  orig_eigens <- eigen(ngm_matrix, only.values = TRUE)
  orig_eigens <- sort(abs(orig_eigens$values), decreasing = TRUE)
  orig_dom_eigen <- orig_eigens[1]
  
  return(orig_dom_eigen)
}

ratio_R0s_2020_of_2009 <- function(cnt_data){
  orig_dom_eigen <- get_dom_eig_from_report_2009()
  R0_matrix <- convert_data_to_ngm_str_2009(cnt_data)
  
  R0_eigens <- eigen(R0_matrix, only.values = TRUE)
  R0_eigens <- sort(abs(R0_eigens$values), decreasing = TRUE)
  R0_dom_eigen <- R0_eigens[1]
  
  return(R0_dom_eigen/orig_dom_eigen)
}

bootstrap_R0s_2009 <- function(num_data, indices){
  bootstrapped_num_data <- num_data[indices,]
  bootstrapped_data <- 
    bootstrapped_num_data[rep(seq_len(nrow(bootstrapped_num_data)),
                              bootstrapped_num_data$n),]
  
  bootstrapped_result <- ratio_R0s_2020_of_2009(bootstrapped_data)
  return(bootstrapped_result)
}

bootstrap_results_with2009_Rtimes <- function(cnt_data, num_R){
  NUM_CPU <- 16
  cl <- makePSOCKcluster(NUM_CPU)
  clusterExport(cl = cl, "unnest")
  
  clusterExport(cl = cl, "get_age_breaks_str_2009")
  clusterExport(cl = cl, "get_population_2020_str_2009")
  clusterExport(cl = cl, "convert_data_to_contact_rate_str_2009")
  clusterExport(cl = cl, "convert_data_to_ngm_str_2009")
  
  clusterExport(cl = cl, "get_contact_rate_report_2009")
  clusterExport(cl = cl, "get_population_2007")
  clusterExport(cl = cl, "get_ngm_from_report_2009")
  clusterExport(cl = cl, "get_dom_eig_from_report_2009")
  clusterExport(cl = cl, "ratio_R0s_2020_of_2009")
  
  cnt_data_2009 <- subset(cnt_data, part_age < 70 & cnt_age_est_max < 70)
  
  cnt_data_2009_bt <- cnt_data_2009 %>%
    nest(data = c(part_id, part_region, part_region_detail, 
                  cnt_age_est_min, cnt_age_est_max, weight, weight2))
  cnt_data_2009_bt$age_part_cnt_id <- c(1:nrow(cnt_data_2009_bt))
  cnt_data_2009_bt <- unnest(data = cnt_data_2009_bt, cols = data)
  
  num_of_cnt <- merge(
    cnt_data_2009_bt, 
    cnt_data_2009_bt %>% 
      group_by(part_id) %>% 
      count(age_part_cnt_id),
    by=c("part_id", "age_part_cnt_id")
  )
  num_of_cnt <- num_of_cnt[, -which(names(num_of_cnt) %in% 
                                       c("cnt_age_est_min","cnt_age_est_max"))]
  num_of_cnt <- unique(num_of_cnt)
  
  num_of_cnt <- num_of_cnt %>%
    mutate(strata_id = group_indices(., part_age, part_region_detail))
  
  start_time <- proc.time()
  boot_output_2009 <- boot(data = num_of_cnt,
                           statistic = bootstrap_R0s_2009, 
                           stype = "i", strata = num_of_cnt$strata_id,
                           weights = num_of_cnt$weight, 
                           R = num_R, parallel="snow", ncpus = NUM_CPU, cl = cl)
  print(proc.time()-start_time)
  boot_output_2009$t0 <- ratio_R0s_2020_of_2009(cnt_data_2009)
  
  stopCluster(cl)
  
  result_R0s_2009 <- boot_output_2009$t
  colnames(result_R0s_2009) <- "R0.ratio"
  result_R0s_2009 <- data.frame(result_R0s_2009)
  
  return(list(boot_output_2009, result_R0s_2009))
}

########################### Prevalence ###########################

get_prvl_2009 <- function(cnt_data){
  # convert contact data to contact matrix
  cnt_mat <- convert_data_to_contact_rate_str_2009(cnt_data)
  waifw_mat <- trans_2009_report*cnt_mat
  
  # get the population structure with age breaks of 2009 report
  pop_vec <- get_population_2020_str_2009()
  num_groups <- length(pop_vec)
  
  # solve SEIR
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  
  prvl <- out[,c(1,2*num_groups + 1 +(1:num_groups))]
  return(prvl)
}

bootstrap_prvl_2009 <- function(cnt_data, indices){
  bootstrapped_data <- cnt_data[indices,]
  data_nrow <- nrow(bootstrapped_data)
  
  bootstrapped_data$cnt_age <-
    round(runif(
      data_nrow,  # number of valid data (needs to be set as the
      # length of min and max vector)
      min = bootstrapped_data$cnt_age_est_min,  # minimum
      max = bootstrapped_data$cnt_age_est_max   # maximum
    ))
  
  bootstrapped_result <- get_prvl_2009(bootstrapped_data)
  return(bootstrapped_result)
}

require("deSolve")

bootstrap_result_prvl_with2009_Rtimes <- function(cnt_data, num_R){
  NUM_CPU <- 16
  cl <- makePSOCKcluster(NUM_CPU)
  clusterExport(cl = cl, "MODEL_f")
  clusterExport(cl = cl, "MODEL_r")
  clusterExport(cl = cl, "MODEL_R0")
  clusterExport(cl = cl, "delta_t")
  clusterExport(cl = cl, "num_grids")
  clusterExport(cl = cl, "solve_SEIR")
  clusterExport(cl = cl, "ode")
  
  clusterExport(cl = cl, "trans_2009_report")
  clusterExport(cl = cl, "get_prvl_2009")
  clusterExport(cl = cl, "convert_data_to_contact_rate_str_2009")
  clusterExport(cl = cl, "get_age_breaks_str_2009")
  clusterExport(cl = cl, "get_population_2020_str_2009")
  
  cnt_data_2009 <- subset(cnt_data, part_age < 70 & cnt_age_est_max < 70)
  
  start_time <- proc.time()
  boot_output_2009 <- boot(data = cnt_data_2009, 
                           statistic = bootstrap_prvl_2009, 
                           stype = "i", strata=cnt_data_2009$part_id,
                           R = num_R, parallel="snow", ncpus = NUM_CPU, cl = cl)
  print(proc.time()-start_time)
  boot_output_2009$t0 <- bootstrap_prvl_2009(cnt_data_2009)
  
  stopCluster(cl)
  
  time_step <- boot_output_2009$t0[,1]
  age_strings <- getstring_age_breaks_str_2009()
  
  boot_output_2009$t0 <- boot_output_2009$t0[,-1]
  dimnames(boot_output_2009$t0) <- list(time_stamp = factor(time_step),
                                        age_groups = age_strings)
  boot_output_2009$t0 <- melt(boot_output_2009$t0)
  
  boot_output_2009$t <- boot_output_2009$t[,-c(1:length(time_step))]
  boot_output_2009$t <- melt(boot_output_2009$t)
  boot_output_2009$t[,2] <- time_step[(boot_output_2009$t[,2]-1)%%num_grids+1]
  boot_output_2009$t <- cbind(boot_output_2009$t, 
                              rep(age_strings, each=num_R*num_grids))
  colnames(boot_output_2009$t) <- 
    c("boot_cases", "time_stamp", "value", "age_groups")
  
  return(boot_output_2009)
}