library(tidyverse)

# Elo rating in AniDom
elo_anidom <- function(winner, loser, dates){
  #load library
  library(aniDom)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  elos <- elo_scores(winners = dataset$winner, 
                    losers = dataset$loser)
  rank_data = data.frame(elos) %>% 
    rownames_to_column(var = "ID") %>% 
    rename(rank = "elos")
  
  return(rank_data)
}
 

library(tidyverse)

# Elo rating in EloRating with 0-1 standardisation and final values
elo_elorating_standard <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  elos <- elo.seq(winner=dataset$winner, loser=dataset$loser, Date=dataset$dates, runcheck=FALSE)
  
  rank_data = 
    data.frame(rank = extract_elo(elos, standardize = TRUE, IDs = NULL, NA.interpolate = TRUE)) %>% 
    rownames_to_column(var = "ID")
  
  return(rank_data)
} 

# Elo rating in EloRating with raw values and final values
elo_elorating_raw <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  elos <- elo.seq(winner=dataset$winner, loser=dataset$loser, Date=dataset$dates, runcheck=FALSE)
  
  rank_data = 
    data.frame(rank = extract_elo(elos, standardize = FALSE, IDs = NULL, NA.interpolate = TRUE)) %>% 
    rownames_to_column(var = "ID")
  
  return(rank_data)
} 

# Elo rating in EloRating with raw values and median values
elo_elorating_median <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  elos <- elo.seq(winner=dataset$winner, loser=dataset$loser, Date=dataset$dates, runcheck=FALSE)
  
  # extract elo ratings
  elo <- cbind(elos$truedates, as.data.frame(elos$cmat)) %>% # elo ratings with NA's replaced by little steps until next value 
    rename(Date = 'elos$truedates')
  
  # calculate median elo rating across study period, leaving out 1 month burn-in period
  rank_data <- elo %>% 
    gather(ID, dom_elo_rating, names(elo)[-1]) %>% 
    group_by(ID) %>% 
    summarize(rank = median(as.numeric(dom_elo_rating)))
  
  return(rank_data)
} 

# Elo rating in EloRating with optimised k values and final values
elo_elorating_optimised <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  elos <- elo.seq(winner=dataset$winner, loser=dataset$loser, Date=dataset$dates, runcheck=FALSE)
  
  # optimise k value
  optimise_k <- optimizek(elos)
  
  # create matrix with optimised k value
  elos <- elo.seq(winner=dataset$winner, loser=dataset$loser, Date=dataset$dates, k= optimise_k$best[1,1], runcheck=FALSE)
  
  # create output
  rank_data = 
    data.frame(rank = extract_elo(elos, standardize = FALSE, IDs = NULL, NA.interpolate = TRUE)) %>% 
    rownames_to_column(var = "ID")
  
  return(rank_data)
} 

# Davids Scores in EloRating with raw values
ds_elorating_raw <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  disp_matrix <- creatematrix(winners=dataset$winner, losers=dataset$loser)
  
  #create David score from matrix using the DS function from the EloRating package
  rank_data = 
    data.frame(DS(disp_matrix)) %>% 
    rename(rank = "DS") %>% 
    select(ID, rank)
  
  return(rank_data)
} 

# Davids Scores in EloRating with raw values
ds_elorating_norm <- function(winner, loser, dates){
  #load library
  library(EloRating)
  
  # create dataset
  dataset <- 
    data.frame(winner, loser, dates) %>%
    # arrange by date
    arrange(dates) %>% 
    # remove those where winner is same as loser
    filter(winner != loser)
  
  # create matrix
  disp_matrix <- creatematrix(winners=dataset$winner, losers=dataset$loser)
  
  #create David score from matrix using the DS function from the EloRating package
  rank_data = 
    data.frame(DS(disp_matrix)) %>% 
    rename(rank = "normDS") %>% 
    select(ID, rank)
  
  return(rank_data)
} 
