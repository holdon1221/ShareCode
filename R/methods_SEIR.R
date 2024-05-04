########### call library ###########
require(deSolve)

####################################
#                                  #
# make an ODE solver for comparing #
#             contact              #
#                                  #
####################################

solve_SEIR <- function(waifw_matrix, pop_vector, delta_t){
  if(nrow(waifw_matrix) != ncol(waifw_matrix)) errorCondition(
    "solveSIR: Use square matrix contact")
  
  num_groups = nrow(waifw_matrix)
  
  y0 = c(
    pop_vector-1, # S0
    rep(0, num_groups), #E0
    rep(1, num_groups), # I0
    rep(0, num_groups) # R0
  )
  
  parameter_set = list(
    waifw_matrix = waifw_matrix,
    f = MODEL_f,
    r = MODEL_r
  )
  
  sir_model <- function(Time, State, Pars){
    with(as.list(c(State, Pars)), {
      S <- State[1:num_groups]
      E <- State[num_groups + (1:num_groups)]
      I <- State[2*num_groups + (1:num_groups)]
      R <- State[3*num_groups + (1:num_groups)]
      
      waifw <- Pars$waifw_matrix
      f <- Pars$f
      r <- Pars$r
      
      foi <- waifw_matrix %*% I
      transmission <- foi*S
      being_infectious <- f*E
      recovered <- r*I
      
      dS <- -transmission
      dE <- transmission - being_infectious
      dI <- being_infectious - recovered
      dR <- recovered
      
      return(list(c(dS, dE, dI, dR)))
    })
  }
  
  out <- ode(y = y0, 
             func = sir_model, 
             times = seq(from = 0, to = MODEL_Tf, by = delta_t),
             parms = parameter_set,
             method = "ode45"
  )
  
  return(out)
}


solve_SEIR_concentrated_initial <- function(
  waifw_matrix, 
  pop_vector, 
  delta_t, 
  initial_group_index
  )
  {
  if(nrow(waifw_matrix) != ncol(waifw_matrix)) errorCondition(
    "solveSIR: Use square matrix contact")
  
  gi = initial_group_index
  num_groups = nrow(waifw_matrix)
  
  
  S0s <- if (gi == num_groups) c(pop_vector[0:(gi-1)], pop_vector[gi]) else
    c(pop_vector[0:(gi-1)], pop_vector[gi], pop_vector[(gi+1):num_groups])
  E0s <- rep(0, num_groups)
  I0s <- c(rep(0, gi-1), num_groups, rep(0, num_groups-gi))
  R0s <- rep(0, num_groups)
  
  y0 = c(
    S0s, E0s, I0s, R0s
  )
  
  parameter_set = list(
    waifw_matrix = waifw_matrix,
    f = MODEL_f,
    r = MODEL_r
  )
  
  sir_model <- function(Time, State, Pars){
    with(as.list(c(State, Pars)), {
      S <- State[1:num_groups]
      E <- State[num_groups + (1:num_groups)]
      I <- State[2*num_groups + (1:num_groups)]
      R <- State[3*num_groups + (1:num_groups)]
      
      waifw <- Pars$waifw_matrix
      f <- Pars$f
      r <- Pars$r
      
      foi <- waifw_matrix %*% I
      transmission <- foi*S
      being_infectious <- f*E
      recovered <- r*I
      
      dS <- -transmission
      dE <- transmission - being_infectious
      dI <- being_infectious - recovered
      dR <- recovered
      
      return(list(c(dS, dE, dI, dR)))
    })
  }
  
  out <- ode(y = y0, 
             func = sir_model, 
             times = seq(from = 0, to = MODEL_Tf, by = delta_t),
             parms = parameter_set,
             method = "ode45"
  )
  
  return(out)
}

get_peak_time_from_ode_solution <- function(ode_out, usual_case){
  if(usual_case == "2009"){
    number_of_group = 10
  } else {
    number_of_group = 16
  }
  time_stamp = ode_out[,1]
  infectious_indices = c((2*number_of_group+1):(3*number_of_group))+1
  infectious_sol = ode_out[,infectious_indices]
  
  peak_times = rep(0, number_of_group)
  for(i in 1:number_of_group)
    peak_times[i] = time_stamp[which.max(infectious_sol[,i])]
  
  return(peak_times)
}

get_transmissibility_from_usual_contact <- function(ngm){
  # get the dominant eigenvalue
  orig_eigens <- eigen(ngm, only.values = TRUE)
  orig_eigens <- sort(abs(orig_eigens$values), decreasing = TRUE)
  orig_dom_eigen <- orig_eigens[1]
  
  # R0 = \beta\rho(C\circ N)/\gamma
  # \beta = R0\gamma / (\rho(C\circ N))
  return(MODEL_R0*MODEL_r/orig_dom_eigen)
}

ngm_polymod_prj <- get_ngm_from_polymod_prj()
trans_polymod_prj <- get_transmissibility_from_usual_contact(ngm_polymod_prj)

ngm_2009_report <- get_ngm_from_report_2009()
trans_2009_report <- get_transmissibility_from_usual_contact(ngm_2009_report)

# @TODO draw infectious people from the usual contacts (black colored)
# @TODO draw infectious people from the 2020 contacts with bootstrapping (red)
# @TODO draw the whole people as one axis, for each age.

require(ggplot2)
require(dplyr)

draw_age_group <- function(ode_out, nth_age){
  last_column <- dim(ode_out)
  last_column <- last_column[2]
  num_groups <- (last_column-1)/4
  
  time_steps <- ode_out[,1]
  compartments <- ode_out[,seq(from=nth_age+1, to=last_column, by=num_groups)]
  
  states <- data.frame(
    S = compartments[,1],
    E = compartments[,2],
    I = compartments[,3],
    R = compartments[,4]
    )
  
  states <- cbind(Time=rep(time_steps, 4), melt(states))
  colnames(states) <- c("Time", "State", "Number")
  
  states %>%
    ggplot(aes(Time, Number, color=State)) + 
    geom_line(size=1) + 
    ggtitle(
      paste("Number of compartments the ", nth_age,"th-age group", 
            sep="")
      ) +
    theme(
      # panel.background = element_blank(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      legend.position = "bottom"
      )+
    guides(color=guide_legend(location = "top", ncol=4))
}

draw_bootstrapped_results <- function(cnt_data){
  last_column <- dim(out)
  last_column <- last_column[2]
  num_groups <- (last_column-1)/4
  
  time_steps <- ode_out[,1]
  compartments <- ode_out[,seq(from=nth_age+1, to=last_column, by=num_groups)]
  
  states <- data.frame(
    S = compartments[,1],
    E = compartments[,2],
    I = compartments[,3],
    R = compartments[,4]
  )
  
  states <- cbind(Time=rep(time_steps, 4), melt(states))
  colnames(states) <- c("Time", "State", "Number")
  
  states %>%
    ggplot(aes(Time, Number, color=State)) + 
    geom_line(size=1) + 
    ggtitle(
      paste("Number of compartments the ", nth_age,"th-age group", 
            sep="")
    ) +
    theme(
      # panel.background = element_blank(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      legend.position = "bottom"
    )+
    guides(color=guide_legend(location = "top", ncol=4))
}