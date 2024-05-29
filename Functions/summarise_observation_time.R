# dyadic observation times
dyadic_ot <- function(sender, receiver, ot_frame){
  
  # dataset
  dataset <- 
    data.frame(sender, receiver) %>% 
    left_join(ot_frame, by = c("sender" = "Focal")) %>%
    left_join(ot_frame, by = c("receiver" = "Focal")) %>%
    mutate(obs_time = Observation_time.x + Observation_time.y) %>% 
    select(sender, receiver, obs_time)
  
  return(dataset)
}

# individual observation times
individual_ot <- function(focal, ot_frame){
  
  # dataset
  dataset <- 
    data.frame(focal) %>% 
    left_join(ot_frame, by = c("focal" = "Focal")) %>%
    mutate(obs_time = Observation_time) %>%
    select(focal, obs_time)
  
  return(dataset)
}

# create a switch statement that will run the function that is passed to it
observation_time <- function(data, ot_frame, func){
  
  # switch statement
  switch(
    func,
    dyadic = dyadic_ot(data$sender, data$receiver, ot_frame),
    individual = individual_ot(data$focal, ot_frame)
  )
}


