# directional grooming counts
grooming_count_directional <- function(sender, receiver) {
  
  # create all possible dyads
  all_pairs <- 
    expand.grid(sender = unique(sender), receiver = unique(receiver), stringsAsFactors = FALSE) %>% 
    filter(sender != receiver)
  
  # add 'dyad' column that contains the sorted pair of sender and receiver separated by '_'
  all_pairs <- 
    all_pairs %>%
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup()
    
  # create dataset
  dataset <- 
    data.frame(sender, receiver) %>%
    # remove those where winner is same as loser
    filter(sender != receiver)
  
  # sum up the grooming given
  dataset <- 
    dataset %>%
    group_by(sender, receiver) %>%
    summarise(grooming_given = n()) %>%
    ungroup()
  
  # left_join the dataset with all_pairs
  all_pairs <- 
    left_join(all_pairs, dataset, by = c("sender" = "sender", "receiver" = "receiver")) %>% 
    replace_na(list(grooming_given = 0)) %>% 
    arrange(sender, receiver)
  
  # return dataset
  return(all_pairs)
}

# undirectional grooming counts
grooming_count_undirectional <- function(sender, receiver) {
  
  # create all possible dyads
  all_pairs <- 
    expand.grid(sender = unique(sender), receiver = unique(receiver), stringsAsFactors = FALSE) %>% 
    filter(sender != receiver)
  
  # add 'dyad' column that contains the sorted pair of sender and receiver separated by '_', remove duplicates
  all_pairs <- 
    all_pairs %>%
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup() %>% 
    distinct(dyad, .keep_all = TRUE)
  
  # create dataset
  dataset <- 
    data.frame(sender, receiver) %>%
    # remove those where winner is same as loser
    filter(sender != receiver) %>% 
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup()
  
  # sum up the grooming given
  dataset <- 
    dataset %>%
    group_by(dyad) %>%
    summarise(grooming_given = n()) %>%
    ungroup()
  
  # left_join the dataset with all_pairs
  all_pairs <- 
    left_join(all_pairs, dataset, by = c("dyad" = "dyad")) %>% 
    replace_na(list(grooming_given = 0)) %>% 
    arrange(sender, receiver)
  
  # return dataset
  return(all_pairs)
}

# directional grooming durations
grooming_duration_directional <- function(sender, receiver, duration) {
  
  # create all possible dyads
  all_pairs <- 
    expand.grid(sender = unique(sender), receiver = unique(receiver), stringsAsFactors = FALSE) %>% 
    filter(sender != receiver)
  
  # add 'dyad' column that contains the sorted pair of sender and receiver separated by '_'
  all_pairs <- 
    all_pairs %>%
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup()
  
  # create dataset
  dataset <- 
    data.frame(sender, receiver, duration) %>%
    # remove those where winner is same as loser
    filter(sender != receiver)
  
  # sum up the grooming given
  dataset <- 
    dataset %>%
    group_by(sender, receiver) %>%
    summarise(grooming_given = sum(duration)) %>%
    ungroup()
  
  # left_join the dataset with all_pairs
  all_pairs <- 
    left_join(all_pairs, dataset, by = c("sender" = "sender", "receiver" = "receiver")) %>% 
    replace_na(list(grooming_given = 0)) %>% 
    arrange(sender, receiver)
  
  # return dataset
  return(all_pairs)
}

# undirectional grooming counts
grooming_duration_undirectional <- function(sender, receiver, duration) {
  
  # create all possible dyads
  all_pairs <- 
    expand.grid(sender = unique(sender), receiver = unique(receiver), stringsAsFactors = FALSE) %>% 
    filter(sender != receiver)
  
  # add 'dyad' column that contains the sorted pair of sender and receiver separated by '_', remove duplicates
  all_pairs <- 
    all_pairs %>%
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup() %>% 
    distinct(dyad, .keep_all = TRUE)
  
  # create dataset
  dataset <- 
    data.frame(sender, receiver) %>%
    # remove those where winner is same as loser
    filter(sender != receiver) %>% 
    rowwise() %>%
    mutate(dyad = str_c(sort(c(sender, receiver)), collapse = "_")) %>% 
    ungroup()
  
  # sum up the grooming given
  dataset <- 
    dataset %>%
    group_by(dyad) %>%
    summarise(grooming_given = n()) %>%
    ungroup()
  
  # left_join the dataset with all_pairs
  all_pairs <- 
    left_join(all_pairs, dataset, by = c("dyad" = "dyad")) %>% 
    replace_na(list(grooming_given = 0)) %>% 
    arrange(sender, receiver)
  
  # return dataset
  return(all_pairs)
}

