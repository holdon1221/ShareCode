##################################
#                                #
#  upload data file with proper  #
#             format             #
#                                #
##################################
require(magrittr)


upload_cnt_data <- function(){
  survey_data <- read.csv("data\\epi\\survey_merge.csv")
  survey_data <- subset(survey_data, contact_type == 1)
  
  # gather the necessary data
  
  cnt_data <- survey_data %>% 
    dplyr::select(part_id, part_age, 
                  cnt_age, cnt_age_est_min, cnt_age_est_max, 
                  part_region, part_region_detail,
                  weight, weight2)
  valid_min <- !is.na(cnt_data$cnt_age_est_min)
  valid_max <- !is.na(cnt_data$cnt_age_est_max)
  valid_range <- (valid_min & valid_max)
  cnt_data <- cnt_data[valid_range,]
  
  # reversion error
  
  reversion_error_indices <- (cnt_data$cnt_age_est_min > cnt_data$cnt_age_est_max)
  cnt_data[which(reversion_error_indices),c(3,4)] <- 
    cnt_data[which(reversion_error_indices),c(4,3)]
  
  return(cnt_data)
}
