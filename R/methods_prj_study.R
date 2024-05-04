########## call library ##########
library(mgcv)
require(tidyr)
require(dplyr)

##################################
#                                #
#    provide a contact matrix    #
#     formed POLYMOD study       #
#                                #
##################################

########################### Generals ###########################

# provide a contact matrix (formed as the previous research projecting POLYMOD
# onto the Korean traits, https://doi.org/10.1371/journal.pcbi.1005697)

get_age_breaks_str_prj <- function(){
  age_group_cut <- seq(from=0, to=80, by=5)
}

getstring_age_breaks_str_prj <- function(){
  age_group_cut <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                     "25-29", "30-34", "35-39", "40-44", "45-49",
                     "50-54", "55-59", "60-64", "65-69", "70-74", 
                     "75-79")
}

get_population_2020_str_prj <- function(){
  # population is from the KOSIS data
  # Caution! originally projection study set the population of 2015 is under or 
  #   equal to 80. Therefor, the following 2020 population is cut also.
  population_group <- c(1781372, 2274368, 2241098, 2491002, 3334536, 3637249,
                        3319618, 3883932, 3891003, 4400725, 4343767, 4243280,
                        3813197, 2659790, 1990720, 1600813)
}

convert_data_to_contact_rate_str_prj <- function(cnt_data){
  cnt_data_by5 <- cnt_data
  
  age_group_cut <- get_age_breaks_str_prj()
  age_group_number <- length(age_group_cut)-1
  
  population_group <- get_population_2020_str_prj()
  
  participant_ages <- rep(c(0:(age_group_number-1)), 
                          each = age_group_number) # grouped by 5 years old.
  contactee_ages <- rep(c(0:(age_group_number-1)), 
                        times = age_group_number) # grouped by 5 years old.
  
  part_age_by5 <- cnt_data$part_age %/% 5
  part_age_by5[which(part_age_by5>15)] = 15
  cnt_data_by5$part_age <- part_age_by5
  
  cnt_age_by5 <- cnt_data$cnt_age %/% 5
  cnt_age_by5[which(cnt_age_by5>15)] = 15
  cnt_data_by5$cnt_age <- cnt_age_by5
  
  cnt_data_by5$count <- 1
  
  zero_data_by5 <- unique(data.frame(part_id = rep(cnt_data_by5$part_id, 
                                                   each = age_group_number),
                                     part_age = rep(cnt_data_by5$part_age,
                                                    each = age_group_number),
                                     weight2 = rep(cnt_data_by5$weight2,
                                                   each = age_group_number),
                                     cnt_age = c(0:(age_group_number-1)),
                                     count = 0
  )
  )
  num_data_by5 <- aggregate(count ~ part_id + part_age + cnt_age + weight2, 
                            data = cnt_data_by5,
                            FUN = sum)
  
  data_by5 <- rbind(num_data_by5, zero_data_by5)
  data_by5 <- aggregate(count ~ part_id + part_age + cnt_age + weight2,
                        data = data_by5,
                        FUN = sum)
  
  gam_by5 <- gam(count ~ te(cnt_age, part_age, k=15, bs="tp"), negbin(c(1:10)), 
                 optimizer="perf", data=data_by5, 
                 weights=weight2)
  gam_prediction_by5 <- predict(gam_by5, type = "response", 
                                newdata = data.frame(part_age = participant_ages,
                                                     cnt_age = contactee_ages)
  )
  
  avg_matrix <- matrix(gam_prediction_by5, nrow = age_group_number)
  rat_matrix <- avg_matrix / population_group[row(avg_matrix)]
  R0_matrix <- (rat_matrix + t(rat_matrix))/2
  
  return(R0_matrix)
}

convert_data_to_ngm_str_prj <- function(cnt_data){
  population_group <- get_population_2020_str_prj()
  R0_matrix <- convert_data_to_contact_rate_str_prj(cnt_data)
  
  R0_matrix <- R0_matrix * population_group[row(R0_matrix)]
  
  return(R0_matrix)
}

get_contact_rate_polymod_prj <- function(){
  orig_matrix <- matrix(
    c(5.7772E-07,2.0394E-07,9.1668E-08,3.7976E-08,4.8715E-08,1.3057E-07,1.8109E-07,1.8345E-07,9.5040E-08,5.0018E-08,5.7719E-08,
      9.9973E-08,1.1983E-07,1.0630E-07,6.3350E-08,1.4926E-07,2.4614E-07,2.0181E-06,6.8908E-07,1.0692E-07,5.1167E-08,8.2850E-08,
      2.4114E-07,2.8544E-07,2.0712E-07,1.3694E-07,1.6332E-07,1.8126E-07,1.2480E-07,1.6814E-07,1.7935E-07,1.9438E-07,1.2742E-07,
      3.8972E-07,3.7699E-06,1.0542E-06,7.7980E-08,4.9485E-08,1.7639E-07,2.3253E-07,2.6305E-07,1.7478E-07,2.7441E-07,1.7719E-07,
      9.2483E-08,1.3900E-07,1.9399E-07,2.8996E-07,7.6681E-08,1.0592E-07,3.6653E-07,3.8694E-06,6.6185E-07,2.3898E-07,1.2940E-07,
      1.6611E-07,2.9188E-07,4.0971E-07,3.8089E-07,2.4640E-07,1.6514E-07,8.2874E-08,1.9369E-07,2.4342E-07,1.0685E-07,6.2762E-08,
      1.2162E-07,4.2308E-07,1.1129E-06,5.7991E-07,2.5578E-07,1.8082E-07,2.0394E-07,2.1021E-07,2.7204E-07,1.9428E-07,1.4813E-07,
      1.0999E-07,9.2563E-08,9.7110E-08,1.9314E-07,1.5198E-07,1.1363E-07,2.0788E-07,4.6069E-07,1.0167E-06,4.4047E-07,3.3455E-07,
      2.6680E-07,2.1969E-07,3.2074E-07,3.2127E-07,2.3626E-07,1.6142E-07,1.5805E-07,1.1615E-07,3.2443E-07,2.8415E-07,1.8581E-07,
      1.7666E-07,3.2000E-07,5.3667E-07,8.1314E-07,4.6009E-07,3.6993E-07,2.7940E-07,2.9416E-07,3.4990E-07,3.1149E-07,2.8356E-07,
      1.8400E-07,2.0833E-07,3.2852E-07,3.4767E-07,3.1863E-07,2.4532E-07,2.8626E-07,4.1015E-07,4.9320E-07,8.1963E-07,4.3986E-07,
      3.2815E-07,2.9445E-07,2.7460E-07,3.2552E-07,2.7659E-07,2.9938E-07,2.5132E-07,2.1376E-07,3.5818E-07,4.5755E-07,3.3812E-07,
      2.5958E-07,3.5715E-07,3.7599E-07,5.4473E-07,7.1204E-07,3.6844E-07,4.0458E-07,3.2151E-07,2.7762E-07,2.6017E-07,3.7241E-07,
      2.9130E-07,9.9936E-08,1.4502E-07,2.5094E-07,3.5505E-07,3.3388E-07,3.0529E-07,2.8047E-07,3.3370E-07,3.8153E-07,5.2234E-07,
      4.4424E-07,2.6843E-07,2.3499E-07,1.5532E-07,2.7055E-07,3.4249E-07,1.1047E-07,9.3967E-08,1.5044E-07,2.0211E-07,2.4725E-07,
      3.1867E-07,2.3645E-07,2.4225E-07,2.7044E-07,2.8081E-07,5.2377E-07,3.3681E-07,2.2161E-07,1.7527E-07,2.2602E-07,3.3805E-07,
      8.3472E-08,6.9442E-08,6.6706E-08,7.2482E-08,1.2970E-07,1.8834E-07,1.7084E-07,1.3520E-07,1.0257E-07,1.2531E-07,2.2957E-07,
      4.2300E-07,2.5822E-07,2.0548E-07,1.7795E-07,1.9781E-07,5.2323E-08,5.0024E-08,3.3836E-08,2.7646E-08,4.1403E-08,7.3559E-08,
      8.6748E-08,8.0991E-08,6.0912E-08,4.8224E-08,7.9434E-08,1.3544E-07,3.3604E-07,1.9291E-07,2.7390E-07,1.3066E-07,4.2086E-08,
      3.6832E-08,3.4026E-08,1.8121E-08,1.3784E-08,2.0622E-08,3.1113E-08,4.7369E-08,3.2763E-08,2.2284E-08,3.2106E-08,5.5481E-08,
      1.1687E-07,3.8585E-07,2.7108E-07,1.7755E-07,2.6493E-08,1.8352E-08,2.8591E-08,1.1435E-08,1.6076E-08,1.0285E-08,1.6512E-08,
      2.7678E-08,2.4013E-08,2.1336E-08,2.4207E-08,2.8019E-08,6.6871E-08,1.0349E-07,4.3146E-07,1.6588E-07,1.5430E-08,1.4956E-08,
      2.3372E-08,8.9264E-09,1.3078E-08,9.1931E-09,1.4447E-08,1.0912E-08,1.1594E-08,2.1468E-08,2.5520E-08,2.2839E-08,2.6799E-08,
      4.3797E-08,1.2858E-07,2.7395E-07
    ),
    nrow = 16, ncol = 16
  )
  
  return(orig_matrix)
}

get_population_2015 <- function(){
  # population is from the KOSIS data
  population_group_2015 <- c(2290097,2251100,2488686,3222268,3511968,3269454,
                             3871602,3892100,4398381,4354036,4289039,3884217,
                             2750831,2105631,1779544,1350503 
  )
  
  return(population_group_2015)
}

get_ngm_from_polymod_prj <- function(){
  population_group_2015 <- get_population_2015()
  orig_matrix <- get_contact_rate_polymod_prj()
  
  ngm_matrix <- orig_matrix * population_group_2015[row(orig_matrix)]
}

########################### R0 ratio ###########################

get_dom_eig_from_polymod_prj <- function(){
  ngm_matrix <- get_ngm_from_polymod_prj()
  
  orig_eigens <- eigen(ngm_matrix, only.values = TRUE)
  orig_eigens <- sort(abs(orig_eigens$values), decreasing = TRUE)
  orig_dom_eigen <- orig_eigens[1]
  
  return(orig_dom_eigen)
}

ratio_R0s_2020_of_prj <- function(cnt_data){
  orig_dom_eigen <- get_dom_eig_from_polymod_prj()
  R0_matrix <- convert_data_to_ngm_str_prj(cnt_data)
  
  R0_eigens <- eigen(R0_matrix, only.values = TRUE)
  R0_eigens <- sort(abs(R0_eigens$values), decreasing = TRUE)
  R0_dom_eigen <- R0_eigens[1]
  
  return(R0_dom_eigen/orig_dom_eigen)
}

bootstrap_R0s_prj <- function(num_data, indices) {
  bootstrapped_num_data <- num_data[indices,]
  bootstrapped_data <- 
    bootstrapped_num_data[rep(seq_len(nrow(bootstrapped_num_data)),
                              bootstrapped_num_data$n),]
  
  bootstrapped_result <- ratio_R0s_2020_of_prj(bootstrapped_data)
  return(bootstrapped_result)
}

bootstrap_results_withPOLYMOD_Rtimes <- function(cnt_data, num_R){
  NUM_CPU <- 16
  cl <- makePSOCKcluster(NUM_CPU)
  clusterExport(cl = cl, "gam")
  clusterExport(cl = cl, "negbin")
  clusterExport(cl = cl, "unnest")
  
  clusterExport(cl = cl, "get_age_breaks_str_prj")
  clusterExport(cl = cl, "get_population_2020_str_prj")
  clusterExport(cl = cl, "convert_data_to_contact_rate_str_prj")
  clusterExport(cl = cl, "convert_data_to_ngm_str_prj")
  
  clusterExport(cl = cl, "get_contact_rate_polymod_prj")
  clusterExport(cl = cl, "get_population_2015")
  clusterExport(cl = cl, "get_ngm_from_polymod_prj")
  clusterExport(cl = cl, "get_dom_eig_from_polymod_prj")
  clusterExport(cl = cl, "ratio_R0s_2020_of_prj")
  
  cnt_data_prj <- subset(cnt_data, part_age < 80 & cnt_age_est_max < 80)
  
  cnt_data_prj_bt <- cnt_data_prj %>%
    nest(data = c(part_id, part_region, part_region_detail,
                  cnt_age_est_min, cnt_age_est_max, weight, weight2))
  cnt_data_prj_bt$age_part_cnt_id <- c(1:nrow(cnt_data_prj_bt))
  cnt_data_prj_bt <- unnest(data = cnt_data_prj_bt, cols = data)

  num_of_cnt <- merge(
    cnt_data_prj_bt,
    cnt_data_prj_bt %>%
      group_by(part_id) %>%
      count(age_part_cnt_id),
    by=c("part_id", "age_part_cnt_id")
  )
  num_of_cnt <- num_of_cnt[, -which(names(num_of_cnt) %in%
                                      c("cnt_age_est_min","cnt_age_est_max"))]
  num_of_cnt <- unique(num_of_cnt)

  num_of_cnt <- num_of_cnt %>%
    mutate(strata_id = group_indices(., part_age, part_region_detail))
  
  # cnt_data_prj <- cnt_data_prj %>%
  #   mutate(strata_id = group_indices(., part_age, cnt_age, part_region))
  
  start_time <- proc.time()
  boot_output_prj <- boot(data = num_of_cnt, 
                          statistic = bootstrap_R0s_prj, 
                          stype = "i", strata=num_of_cnt$strata_id,
                          weights = num_of_cnt$weight2, 
                          R = num_R, parallel="snow", ncpus = NUM_CPU, cl = cl)
  print(proc.time()-start_time)
  boot_output_prj$t0 <- ratio_R0s_2020_of_prj(cnt_data_prj)
  
  stopCluster(cl)
  
  result_R0s_prj <- boot_output_prj$t
  colnames(result_R0s_prj) <- "R0.ratio"
  result_R0s_prj <- data.frame(result_R0s_prj)
  
  return(list(boot_output_prj, result_R0s_prj))
}

########################### Prevalence ###########################

get_prvl_prj <- function(cnt_data){
  # convert contact data to contact matrix
  cnt_mat <- convert_data_to_contact_rate_str_prj(cnt_data)
  waifw_mat <- trans_polymod_prj*cnt_mat
  
  # get the population structure with age breaks of 2009 report
  pop_vec <- get_population_2020_str_prj()
  num_groups <- length(pop_vec)
  
  # solve SEIR
  out <- solve_SEIR(waifw_mat, pop_vec, delta_t)
  
  prvl <- out[,c(1,2*num_groups + 1 +(1:num_groups))]
  return(prvl)
}

bootstrap_prvl_prj <- function(cnt_data, indices){
  bootstrapped_data <- cnt_data[indices,]
  data_nrow <- nrow(bootstrapped_data)
  
  bootstrapped_data$cnt_age <-
    round(runif(
      data_nrow,  # number of valid data (needs to be set as the
      # length of min and max vector)
      min = bootstrapped_data$cnt_age_est_min,  # minimum
      max = bootstrapped_data$cnt_age_est_max   # maximum
    ))
  
  bootstrapped_result <- get_prvl_prj(bootstrapped_data)
  return(bootstrapped_result)
}

require("deSolve")

bootstrap_result_prvl_withPOLYMOD_Rtimes <- function(cnt_data, num_R){
  NUM_CPU <- 16
  cl <- makePSOCKcluster(NUM_CPU)
  clusterExport(cl = cl, "MODEL_f")
  clusterExport(cl = cl, "MODEL_r")
  clusterExport(cl = cl, "MODEL_R0")
  clusterExport(cl = cl, "delta_t")
  clusterExport(cl = cl, "num_grids")
  clusterExport(cl = cl, "solve_SEIR")
  clusterExport(cl = cl, "ode")
  
  clusterExport(cl = cl, "gam")
  clusterExport(cl = cl, "negbin")
  
  clusterExport(cl = cl, "trans_polymod_prj")
  clusterExport(cl = cl, "get_prvl_prj")
  clusterExport(cl = cl, "convert_data_to_contact_rate_str_prj")
  clusterExport(cl = cl, "get_age_breaks_str_prj")
  clusterExport(cl = cl, "get_population_2020_str_prj")
  
  cnt_data_prj <- subset(cnt_data, part_age < 80 & cnt_age_est_max < 80)
  
  start_time <- proc.time()
  boot_output_prj <- boot(data = cnt_data_prj, 
                          statistic = bootstrap_prvl_prj, 
                          stype = "i", strata=cnt_data_prj$part_id,
                          R = num_R, parallel="snow", ncpus = NUM_CPU, cl = cl)
  print(proc.time()-start_time)
  boot_output_prj$t0 <- bootstrap_prvl_prj(cnt_data_prj)
  
  stopCluster(cl)
  
  time_step <- boot_output_prj$t0[,1]
  age_strings <- getstring_age_breaks_str_prj()
  
  boot_output_prj$t0 <- boot_output_prj$t0[,-1]
  dimnames(boot_output_prj$t0) <- list(time_stamp = factor(time_step),
                                       age_groups = age_strings)
  boot_output_prj$t0 <- melt(boot_output_prj$t0)
  
  boot_output_prj$t <- boot_output_prj$t[,-c(1:length(time_step))]
  boot_output_prj$t <- melt(boot_output_prj$t)
  boot_output_prj$t[,2] <- time_step[(boot_output_prj$t[,2]-1)%%num_grids+1]
  boot_output_prj$t <- cbind(boot_output_prj$t, 
                             rep(age_strings, each=num_R*num_grids))
  colnames(boot_output_prj$t) <- 
    c("boot_cases", "time_stamp", "value", "age_groups")
  
  return(boot_output_prj)
}