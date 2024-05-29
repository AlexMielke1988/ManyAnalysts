# binary response
response_binary <- function(grooming_variable) {
  return(ifelse(grooming_variable == 0, 0, 1))
}

# raw counts
response_raw <- function(grooming_variable) {
  return(grooming_variable)
}

# proportion of grooming interactions by all sender grooming interaction
response_proportion_sender <- 
  function(grooming_variable, sender) {
    
    # create dataset
    dataset <- 
      data.frame(grooming_variable, sender)
    
    # calculate proportion
    response <-
      dataset %>%
      # group by sender
      group_by(sender) %>%
      # summarise
      summarise(grooming_variable = sum(grooming_variable)) %>%
      ungroup() %>%
      # left join to dataset again 
      left_join(dataset, by = c("sender" = "sender")) %>%
      # add proportion
      mutate(proportion = grooming_variable.y / grooming_variable.x) %>% 
      # select proportion
      pull(proportion)
    
    return(response)
  }

# rate of grooming compared to observation time in seconds/seconds
response_rate_seconds <- 
  function(grooming_variable, observation_time) {
    
    # calculate rate
    response <- grooming_variable / (observation_time * 3600)
    
    return(response)
  }

# rate of grooming compared to observation time in seconds/hours
response_rate_hours <- 
  function(grooming_variable, observation_time) {
    
    # calculate rate
    response <- grooming_variable / observation_time
    
    return(response)
  }

# create a vector with all the function names in this file
