# libraries
library(tidyverse)
library(lme4)
library(glmmTMB)
library(brms)
library(doParallel)
library(parallel)

# set parameters

rank_calculation <-
  c('elo_anidom',
    'elo_elorating_raw',
    'elo_elorating_standard',
    'elo_elorating_optimised',
    'elo_elorating_median',
    'ds_elorating_raw',
    'ds_elorating_norm'
  )

standardisation_functions <-
  c("z_standardise",
    "minmax_standardise")

grooming_functions <-
  c("grooming_count_directional",
    "grooming_count_undirectional",
    "grooming_duration_directional",
    "grooming_duration_undirectional")

ot_functions <-
  c("dyadic_ot")

response_functions <-
  c("response_binary",
    "response_raw",
    "response_rate_hours")

removal_functions <-
  c('all', 'only_positive')


# possible model combinations
sex_model_functions <- c(
  'main_effects',
  'interaction',
  'combination'
)
age_model_functions <- c(
  'main_effects',
  'interaction',
  'difference',
  'distance'
)
rank_model_functions <- c(
  'main_effects',
  'interaction',
  'squared_interaction',
  'difference',
  'distance'
)
random_effects_functions <- c(
  'none',
  'individuals',
  'dyad',
  'individuals_dyad'
)
offset_functions <- c(
  'no',
  'yes'
)
model_engine_functions <- c(
  'glmmTMB'
)
distribution_functions <- c(
  'binary',
  'poisson',
  'zi_poisson',
  'negbin',
  'zi_negbin',
  'gaussian',
  'beta',
  'lognormal',
  'gamma')


# create a parameter table
parameter_table <- expand.grid(
  rank_calculation = rank_calculation,
  grooming_functions = grooming_functions,
  standardisation_functions = standardisation_functions,
  ot_functions = ot_functions,
  response_functions = response_functions,
  removal_functions = removal_functions,
  sex_model_functions = sex_model_functions,
  age_model_functions = age_model_functions,
  rank_model_functions = rank_model_functions,
  random_effects_functions = random_effects_functions,
  offset_functions = offset_functions,
  model_engine_functions = model_engine_functions,
  distribution_functions = distribution_functions,
  stringsAsFactors = FALSE
  )

# only include combinations that lead to results later on

parameter_table <- parameter_table %>%
  filter(
    (response_functions == 'response_raw' & distribution_functions %in% c('poisson', 'zi_poisson', 'negbin', 'zi_negbin')) |
      (response_functions == 'response_binary' & distribution_functions %in% c('binary'))|
      (response_functions != 'response_raw' & response_functions != 'response_binary' & distribution_functions %in% c('gaussian', 'beta', 'lognormal'))
    ) %>%
    filter((grooming_functions %in% c("grooming_count_undirectional", "grooming_duration_undirectional") & random_effects_functions %in% c('none', 'dyad'))|
             (grooming_functions %in% c("grooming_count_directional", "grooming_duration_directional"))) %>%
  filter(
    (offset_functions == 'no' & response_functions == 'response_raw') |
      (offset_functions == 'no' & response_functions == 'response_binary') |
      (response_functions != 'response_raw' & response_functions != 'response_binary')
  )

parameter_table <-
  parameter_table %>%
  slice_sample(n = 100000, replace = FALSE)

# turn off tidyverse summarise messages

options(dplyr.summarise.inform = FALSE)

# Open or Read the four files
setwd("C:/Users/usr/Documents/GitHub/ManyAnalysts/Data")
set.seed(2807)

demo=read.csv(file="demographics.csv",header=T)
dis=read.csv(file="displacements.csv",header=T)
gr=read.csv(file="grooming.csv",header=T)
ob=read.csv(file="observation_times.csv",header=T)


# parallelise
mycluster <- makeCluster(24, type = "PSOCK")
# export the relevant information to each core
clusterExport(
  cl = mycluster,
  c(
    "demo",
    "dis",
    "gr",
    "ob",
    "parameter_table",
    paste(colnames(parameter_table))),
  envir = environment()
)
registerDoParallel(mycluster)

model_values <-
  parLapply(
    mycluster,
    X = 1:nrow(parameter_table),
    function(k){
      print(k)
      # libraries
      library(tidyverse)
      library(lme4)
      library(glmmTMB)
      library(brms)
      library(EloRating)
      library(aniDom)

      # source all functions in the folder
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/prepare_response.R")
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/model_specs.R")
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/rank_calculation.R")
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/standardisation_functions.R")
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/summarise_grooming.R")
      source("C:/Users/usr/Documents/GitHub/ManyAnalysts/Functions/summarise_observation_time.R")

              # select one function from each category
  rank_index <- parameter_table$rank_calculation[k]
  grooming_summary_index <- parameter_table$grooming_functions[k]
  standardisation_index <- parameter_table$standardisation_functions[k]
  ot_index <- parameter_table$ot_functions[k]
  response_index <- parameter_table$response_functions[k]
  removal_index <- parameter_table$removal_functions[k]

  sex_model_index <- parameter_table$sex_model_functions[k]
  age_model_index <- parameter_table$age_model_functions[k]
  rank_model_index <- parameter_table$rank_model_functions[k]
  random_effects_index <- parameter_table$random_effects_functions[k]
  offset_index <- parameter_table$offset_functions[k]
  model_engine_index <- parameter_table$model_engine_functions[k]
  distribution_index <- parameter_table$distribution_functions[k]

  # select function for rank calculation based on the rank_index parameter

  rank_variable  <- switch(rank_index,
                           "elo_anidom" = elo_anidom(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "elo_elorating_raw" = elo_elorating_raw(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "elo_elorating_standard" = elo_elorating_standard(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "elo_elorating_optimised" = elo_elorating_optimised(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "elo_elorating_median" = elo_elorating_median(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "ds_elorating_raw" =  ds_elorating_raw(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date),
                           "ds_elorating_norm" = ds_elorating_norm(winner = dis$Sender, loser = dis$Receiver, dates = dis$Date)
  ) %>%
    arrange(desc(rank))

  # select function for grooming based on the grooming_summary_index parameter

  grooming_summary <- switch(grooming_summary_index,
                             "grooming_count_directional" = grooming_count_directional(gr$Sender, gr$Receiver),
                             "grooming_count_undirectional" = grooming_count_undirectional(gr$Sender, gr$Receiver),
                             "grooming_duration_directional" = grooming_duration_directional(gr$Sender, gr$Receiver, gr$Duration_seconds),
                             "grooming_duration_undirectional" = grooming_duration_undirectional(gr$Sender, gr$Receiver, gr$Duration_seconds),
                             stop("Invalid type")
  )


  # select function for observation time based on the ot_index parameter

  ot_summary <- switch(
    ot_index,
    'dyadic_ot' = dyadic_ot(grooming_summary$sender, grooming_summary$receiver, ot_frame = ob),
    'individual_ot' = individual_ot(grooming_summary$sender, ot_frame = ob)
  )


  # add observation times to grooming summary

  grooming_summary <-
    grooming_summary %>%
    mutate(observation_time = ot_summary$obs_time)

  # if removal variable is set to 'only_positive', remove all 0 values from grooming_summary

  if(removal_index == 'only_positive') {
    grooming_summary <- grooming_summary %>% filter(grooming_given > 0)
  }

  # create response variable based on the response_index parameter

  grooming_summary$response <- switch(response_index,
                                      "response_binary" = response_binary(grooming_summary$grooming_given),
                                      "response_raw" = response_raw(grooming_summary$grooming_given),
                                      "response_proportion_sender" = response_proportion_sender(grooming_summary$grooming_given, grooming_summary$observation_time),
                                      "response_rate_seconds" = response_rate_seconds(grooming_summary$grooming_given, grooming_summary$observation_time),
                                      "response_rate_hours" = response_rate_hours(grooming_summary$grooming_given, grooming_summary$observation_time))


  # left_join age, sex, and rank for sender and receiver to the grooming_summary

  grooming_summary <-
    grooming_summary %>%
    left_join(demo, by = c("sender" = "ID")) %>%
    left_join(demo, by = c("receiver" = "ID"), suffix = c("_sender", "_receiver")) %>%
    left_join(rank_variable, by = c("sender" = "ID")) %>%
    left_join(rank_variable, by = c("receiver" = "ID"), suffix = c("_sender", "_receiver")) %>%
    mutate(rank_difference = rank_sender - rank_receiver,
           age_difference = Age_sender - Age_receiver) %>%
    mutate(rank_distance = abs(rank_difference),
           age_distance = abs(age_difference))


  # select rank standardisation based on the standarisation_index parameter

  grooming_summary$rank_sender <- switch(standardisation_index,
                                         "z_standardise" = z_standardise(grooming_summary$rank_sender),
                                         "minmax_standardise" = minmax_standardise(grooming_summary$rank_sender),
                                         "centre" = centre(grooming_summary$rank_sender),
                                         'proportional' = proportional(grooming_summary$rank_sender))

  grooming_summary$rank_receiver <- switch(standardisation_index,
                                           "z_standardise" = z_standardise(grooming_summary$rank_receiver),
                                           "minmax_standardise" = minmax_standardise(grooming_summary$rank_receiver),
                                           "centre" = centre(grooming_summary$rank_receiver),
                                           'proportional' = proportional(grooming_summary$rank_receiver))

  grooming_summary$rank_difference <- switch(standardisation_index,
                                             "z_standardise" = z_standardise(grooming_summary$rank_difference),
                                             "minmax_standardise" = minmax_standardise(grooming_summary$rank_difference),
                                             "centre" = centre(grooming_summary$rank_difference),
                                             'proportional' = proportional(grooming_summary$rank_difference))

  grooming_summary$rank_distance <- switch(standardisation_index,
                                           "z_standardise" = z_standardise(grooming_summary$rank_distance),
                                           "minmax_standardise" = minmax_standardise(grooming_summary$rank_distance),
                                           "centre" = centre(grooming_summary$rank_distance),
                                           'proportional' = proportional(grooming_summary$rank_distance))

  # select age standardisation based on the standarisation_index parameter

  grooming_summary$Age_sender <- switch(standardisation_index,
                                        "z_standardise" = z_standardise(grooming_summary$Age_sender),
                                        "minmax_standardise" = minmax_standardise(grooming_summary$Age_sender),
                                        "centre" = centre(grooming_summary$Age_sender),
                                        'proportional' = proportional(grooming_summary$Age_sender))

  grooming_summary$Age_receiver <- switch(standardisation_index,
                                          "z_standardise" = z_standardise(grooming_summary$Age_receiver),
                                          "minmax_standardise" = minmax_standardise(grooming_summary$Age_receiver),
                                          "centre" = centre(grooming_summary$Age_receiver),
                                          'proportional' = proportional(grooming_summary$Age_receiver))

  grooming_summary$age_difference <- switch(standardisation_index,
                                            "z_standardise" = z_standardise(grooming_summary$age_difference),
                                            "minmax_standardise" = minmax_standardise(grooming_summary$age_difference),
                                            "centre" = centre(grooming_summary$age_difference),
                                            'proportional' = proportional(grooming_summary$age_difference))

  grooming_summary$age_distance <- switch(standardisation_index,
                                          "z_standardise" = z_standardise(grooming_summary$age_distance),
                                          "minmax_standardise" = minmax_standardise(grooming_summary$age_distance),
                                          "centre" = centre(grooming_summary$age_distance),
                                          'proportional' = proportional(grooming_summary$age_distance))

  # create sex_combination variable

  grooming_summary$sex_combination <-
    paste(grooming_summary$Sex_sender, grooming_summary$Sex_receiver, sep = "-")


  # prepare the different models based on the model index parameters
  sex_formula <- switch(sex_model_index,
                        'main_effects' = 'Sex_sender + Sex_receiver',
                        'interaction' = 'Sex_sender * Sex_receiver',
                        'combination' = 'sex_combination')

  age_formula <- switch(age_model_index,
                        'main_effects' = 'Age_sender + Age_receiver',
                        'interaction' = 'Age_sender * Age_receiver',
                        'difference' = 'age_difference',
                        'distance' = 'age_distance')

  rank_formula <- switch(rank_model_index,
                         'main_effects' = 'rank_sender + rank_receiver',
                         'interaction' = 'rank_sender * rank_receiver',
                         'squared_interaction' = 'rank_sender * rank_receiver + I(rank_sender^2) + I(rank_receiver^2)',
                         'difference' = 'rank_difference',
                         'distance' = 'rank_distance')

  random_effects_formula <- switch(random_effects_index,
                                   'none' = '',
                                   'individuals' = '(1|sender) + (1|receiver)',
                                   'dyad' = '(1|dyad)',
                                   'individuals_dyad' = '(1|sender) + (1|receiver) + (1|dyad)')

  offset_formula <- switch(offset_index,
                           'no' = '',
                           'yes' = 'offset(log(observation_time))')

  # create the formula for the model

  if(response_index != 'response_raw') {
    model_formula <- paste0(sex_formula, '+', age_formula, '+', rank_formula, '+', random_effects_formula) %>%
      str_replace_all('\\++', replacement =  '+') %>%
      # remove '+' from last position in string
      str_remove('\\+$') %>%
      # remove + from first position in string
      str_remove('^\\+')

    model_formula <- as.formula(paste('response ~', model_formula))
  } else {
    model_formula <- paste0(sex_formula, '+', age_formula, '+', rank_formula, '+', random_effects_formula, '+', offset_formula) %>%
      str_replace_all('\\++', replacement =  '+') %>%
      # remove '+' from last position in string
      str_remove('\\+$') %>%
      # remove + from first position in string
      str_remove('^\\+')

    model_formula <- as.formula(paste('response ~', model_formula))
  }


  # prepare models for glmmTMB
  if(model_engine_index == 'glmmTMB') {
    if(distribution_index == 'binary' &
       response_index == 'response_binary') {
      model <- glmmTMB(model_formula, data = grooming_summary, family = binomial)
    } else if(distribution_index == 'poisson' &
              response_index == 'response_raw') {
      model <- glmmTMB(model_formula, data = grooming_summary, family = poisson)
    } else if(distribution_index == 'zi_poisson' &
              response_index == 'response_raw') {
      model <- glmmTMB(model_formula, data = grooming_summary, family = poisson, ziformula = ~1)
    } else if(distribution_index == 'negbin' &
              response_index == 'response_raw') {
      model <- glmmTMB(model_formula, data = grooming_summary, family = nbinom2)
    } else if(distribution_index == 'zi_negbin' &
              response_index == 'response_raw') {
      model <- glmmTMB(model_formula, data = grooming_summary,
                       zi = ~1,
                       family = nbinom2)
    } else if(distribution_index == 'gaussian' &
              response_index != 'response_binary') {
      model <- glmmTMB(model_formula, data = grooming_summary,
                       family = gaussian())
    } else if(distribution_index == 'lognormal' &
              response_index != 'response_binary') {
      grooming_summary$response <- log(grooming_summary$response + 0.0001)
      model <- glmmTMB(model_formula, data = grooming_summary,
                       family = gaussian)
    } else if(distribution_index == 'gamma' &
              response_index != 'response_binary') {
      model <- glmmTMB(model_formula, data = grooming_summary,
                       family = Gamma(link = 'log'))
    } else if(distribution_index == 'beta' &
              response_index != 'response_binary' &
              response_index != 'response_raw' &
              max(grooming_summary$response) <= 1) {
      grooming_summary$response <- grooming_summary$response + 0.0001
      model <- glmmTMB(model_formula, data = grooming_summary,
                       family = beta_family())
    } else {
      model <- NA
    }
  }

  if(length(is.na(model)) > 1) {
    sig <- names(which(summary(model)$coefficients$cond[,4] < 0.05))
    sig <- sig[sig != '(Intercept)']
    return(list(
      model_summary = summary(model),
      sig_parameters = sig,
      model_AIC = AIC(model),
      model_estimates = confint(model),
      parameter_table = parameter_table[k,]
    ))} else {
      return(list(
        model_summary = NA,
        sig_parameters = NA,
        model_performance = NA,
        model_estimates = NA,
        parameter_table = parameter_table[k,]
      ))
    }
})
stopCluster(mycluster)

model_values_transposed <- purrr::transpose(model_values)

model_terms <- sapply(model_values_transposed$model_estimates, function(x){
  rownames(x)
}) %>%
  unlist() %>%
  unique()

sig_matrix <- matrix(NA, nrow = nrow(parameter_table), ncol = length(model_terms))
colnames(sig_matrix) <- model_terms

for(i in seq_along(model_values_transposed$model_estimates)){
  m_terms <- model_values_transposed$model_estimates[[i]] %>% rownames()
  sig_matrix[i, m_terms] <- 0
  sig_matrix[i, model_values_transposed$sig_parameters[[i]]] <- 1
}

# focus the sig_matrix on the important variables

sig_matrix <- sig_matrix %>%
  data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%
  select(colnames(sig_matrix)[!(str_detect(colnames(sig_matrix), 'cond')) &
                                !(str_detect(colnames(sig_matrix), 'Std.Dev'))  &
                                !(str_detect(colnames(sig_matrix), 'Intercept'))])

establish_patterns <- lapply(1:ncol(sig_matrix), function(y){
  print(y)
  test_data <-
    cbind(
      outcome = sig_matrix[,y],
      parameter_table[,sapply(1:ncol(parameter_table), function(x) length(unique(parameter_table[,x])) > 1)]) %>%
    filter(!is.na(outcome)) %>%
    droplevels()
  test_data <- test_data[,sapply(apply(test_data, 2, unique), length) > 1]

  if('outcome' %in% colnames(test_data)){
    model_res <- glm(outcome ~ ., data = test_data)
    return(
      list(
        variable = colnames(sig_matrix)[y],
        summary = model_res %>% summary,
        drop1 = model_res %>% drop1(test = 'Chisq')
      )
    )
  }
  if(!('outcome' %in% colnames(test_data))){
    return(
      list(
        variable = colnames(sig_matrix)[y],
        summary = NA,
        drop1 = NA
      )
    )
  }
})


parameter_table$mean_correct <- sapply(1:nrow(sig_matrix), function(x){
  mean(c(
    sig_matrix$Sex_senderm[x] == 1,
    sig_matrix$Sex_receiverm[x] == 1,
    sig_matrix$`Sex_senderm:Sex_receiverm`[x] == 1,
    sig_matrix$`sex_combinationf-m`[x] == 1,
    sig_matrix$`sex_combinationm-f`[x] == 1,
    sig_matrix$`sex_combinationm-m`[x] == 1,
    sig_matrix$Age_sender[x] == 0,
    sig_matrix$Age_receiver[x] == 0,
    sig_matrix$age_difference[x] == 0,
    sig_matrix$age_distance[x] == 0,
    sig_matrix$`Age_sender:Age_receiver`[x] == 0,
    sig_matrix$rank_sender[x] == 0,
    sig_matrix$rank_receiver[x] == 1,
    sig_matrix$`I(rank_receiver^2)`[x] == 1,
    sig_matrix$`I(rank_sender^2)`[x] == 0,
    sig_matrix$rank_difference[x] == 1,
    sig_matrix$rank_distance[x] == 1,
    sig_matrix$`rank_sender:rank_receiver`[x] == 1
  ), na.rm = T)
})

parameter_table %>% group_by(distribution_functions) %>% summarise(aa = mean(mean_correct, na.rm = T)) %>% ungroup() %>% arrange(aa)