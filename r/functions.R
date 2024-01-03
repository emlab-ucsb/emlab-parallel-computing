# Define an arbitrary function that takes some amount of time to run
# Input is the number of seconds
# Output is that value again
long_function <- function(how_many_seconds){
  
  Sys.sleep(how_many_seconds)
  
  how_many_seconds
}

# Let's test the function and time it
# tictoc::tic()
# long_function(2)
# tictoc::toc()

# Create a wrapper for long_function that runs sequentially over a vector of seconds
long_function_sequential <- function(how_many_seconds_vector){
  
  how_many_seconds_vector |>
    purrr::map(long_function, 
               # Include a progress bar
               .progress = TRUE)
  
}

# Let's test the function and time it
# tictoc::tic()
# long_function_sequential(rep(2,5))
# tictoc::toc()

# Create a version for long_function that runs in parallel over a vector of seconds
long_function_parallel <- function(how_many_seconds_vector){
  
  # Run the function in parallel using furrr::future_map
  how_many_seconds_vector |>
    furrr::future_map(long_function, 
                      # Include a progress bar
                      .progress = TRUE)
  
}

# Set number of workers for parallel processing
# One way to do this would be to use all available cores on the machine
# num_workers <- future::availableCores()
# num_workers

# If working locally on your personal computer, you may wish to leave 1 core free for other tasks
# num_workers <- future::availableCores(omit = 1)
# num_workers

# Alternatively, you can set the number of workers manually
# number_workers <- 5

# Initialize workers for parallel processing using future::plan
# Make fork cluster, which should work on Macs, Linux, and GRIT servers
# An alternative is to use future::plan(future::multisession, workers = number_of_workers)
# future::plan(future::cluster,
#              workers = parallel::makeForkCluster(number_workers))

# # Let's test the function and time it
# tictoc::tic()
# long_function_parallel(rep(2,5))
# tictoc::toc()
# 
# # Always shut down the workers when you're done
# future::plan(future::sequential)

# Create a wrapper function for long_function_parallel that allows us to specify the number of workers,
# and automatically starts and shuts down the workers
long_function_parallel_wrapper <- function(how_many_seconds_vector,
                                           number_of_workers = 1){
  
  # Initialize workers for parallel processing
  # Make fork cluster, which should work on Macs, Linux, and GRIT servers
  # An alternative is to use future::plan(future::multisession, workers = number_of_workers)
  future::plan(future::cluster,
               workers = parallel::makeForkCluster(number_of_workers))
  
  # Run the function in parallel
  result <- how_many_seconds_vector |>
    long_function_parallel()
  
  # Close the workers
  future::plan(future::sequential)
  
  # Return the result
  result
  
}

# Let's test the function and time it
# Since it takes time to initialize and shut down the workers, we can see this takes a little longer to run
# tictoc::tic()
# long_function_parallel_wrapper(rep(2,5),
#                                number_of_workers = number_workers)
# tictoc::toc()

# Create a function to train and test machine learning model
# This will use the penguins dataset to build a model that predicts penguin body weight
run_ml_models <- function(dataset,
                          # Across how many parallel workers should we run cross-validation?
                          number_of_workers_for_cv = 1,
                          # Random forest can internally run in parallel
                          # How many parallel workers should we use?
                          number_of_workers_for_rf = 1){
  
  # Set random seed for consistency
  set.seed(101)
  
  # Split the data into training and testing sets
  split <- rsample::initial_split(dataset, prop = 0.75)
  train <- rsample::training(split)
  test <- rsample::testing(split)
  
  # Create splits from training data using 5-fold cross-validation
  train_cv_splits <- rsample::vfold_cv(train, v = 5)
  
  # Create a data preprocessing recipe
  # Predict body_mass_g using everything else
  recipe <- recipes::recipe(body_mass_g ~ ., data = train)  |>
    # Remove rows with missing outcome variable
    recipes::step_naomit(recipes::all_outcomes())  |>
    # Impute missing categorical predictors with mode
    recipes::step_impute_mode(recipes::all_nominal_predictors())  |>
    # Impute missing numeric predictors with median
    recipes::step_impute_median(recipes::all_numeric_predictors())
  
  # Create a random forest model specification
  rf_spec <- 
    parsnip::rand_forest(trees = 1000,
                         mtry = tune::tune(),
                         min_n = tune::tune()) |>
    parsnip::set_engine("ranger", 
                        importance = "none",
                        seed = 101,
                        # Random forest can internally run in parallel
                        num.threads = number_of_workers_for_rf) |>
    parsnip::set_mode("regression")
  
  # Create a workflow
  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(rf_spec)
  
  # Specify performance metric for tuning hyperparaemetrs
  performance_metrics <- yardstick::metric_set(yardstick::rsq)
  
  # Initialize workers for cross-validation parallel processing
  # This will run each of our CV folds on a different worker
  # Need to register backend for tune::tune_grid to pick it up
  if(number_of_workers_for_cv > 1) doFuture::registerDoFuture()
  # Make fork cluster, which should work on Macs, Linux, and GRIT servers
  # An alternative is to use future::plan(future::multisession, workers = number_of_workers)
  if(number_of_workers_for_cv > 1) future::plan(future::cluster,
                                                workers = parallel::makeForkCluster(number_of_workers_for_cv))
  
  # Run cross-validation using our CV splits
  # Over a grid size of 10 hyperparameter combinations
  cv_results <-
    workflow |>
    tune::tune_grid(
      resamples = train_cv_splits,
      metrics = performance_metrics,
      grid = 10,
      control = tune::control_grid(verbose = TRUE,
                                   # Allow this to run in parallel if number_of_workers_for_cv > 1 
                                   # Otherwise, don't run this in parallel
                                   allow_par = number_of_workers_for_cv > 1,
                                   parallel_over = "resamples"))
  
  # Select best hyperparameter set
  best_hyperparameters <- cv_results |>
    tune::select_best("rsq")
  
  # Close the workers, if necessary
  if(number_of_workers_for_cv > 1) future::plan(future::sequential)
  
  # Fit the model on the training dataset
  # Using optimized hyperparameter set
  fitted_model <- workflow |>
    tune::finalize_workflow(best_hyperparameters) |>
    parsnip::fit(data = train)
  
  # Make predictions for testing dataset
  predictions <- fitted_model |>
    predict(test)
  
  # Add predictions to testing dataset
  test <- test |>
    dplyr::mutate(.pred = predictions$.pred)
  
  # Calculate performance metrics
  model_performance <- performance_metrics(test, 
                                           truth = body_mass_g,
                                           estimate = .pred)
  
  model_performance
  
}

# Let's test the function and time it - first running RF sequentially and cross-validation sequentially 
# tictoc::tic()
# run_ml_models(penguins,
#               number_of_workers_for_cv = 1,
#               number_of_workers_for_rf = 1)
# tictoc::toc()

# Let's test the function and time it - next running RF in parallel and cross-validation sequentially
# tictoc::tic()
# run_ml_models(penguins,
#               number_of_workers_for_cv = 1,
#               number_of_workers_for_rf = 10)
# tictoc::toc()

# Let's test the function and time it - now running RF sequentially and cross-validation in parallel
# This is much faster because cross-validation is the more outer loop than the internal random forest model, 
# and is the longest running operation
# tictoc::tic()
# run_ml_models(penguins, 
#               number_of_workers_for_cv = 10,
#               number_of_workers_for_rf = 1)
# tictoc::toc()

# Let's create a wrapper function that runs the ML function above by group
run_ml_models_by_group <- function(dataset,
                                   group_name,
                                   # This specifies the number of workers across which we want to do group-wise model building
                                   number_of_workers_for_groups = 1,
                                   # This specifies the number of workers across which we want to do cross-validation
                                   # I only include this for demonstration purposes
                                   # To maximize efficiencies and minimize overhead loss,
                                   # We typically want to parallelize the longest running operation (e.g., the outermost loop)
                                   number_of_workers_for_cv = 1,
                                   # Random forest can internally run in parallel
                                   # How many parallel workers should we use?
                                   number_of_workers_for_rf = 1){
  
  # Make fork cluster, which should work on Macs, Linux, and GRIT servers
  # An alternative is to use future::plan(future::multisession, workers = number_of_workers)
  if(number_of_workers_for_groups > 1) future::plan(future::cluster,
                                                    workers = parallel::makeForkCluster(number_of_workers_for_groups))
  
  # Take our dataset, group by group_name, then use that to nest so that each row corresponds to the data from each group
  # This is a nice way to keep everything organized, and ensure model results correspond to group names
  # As an example to see what this looks like before building the models, you can try the following
  # penguins |> 
  #   dplyr::group_by(species) |> 
  #   tidyr::nest() |> 
  #   dplyr::ungroup()
  
  results <- dataset |>
    dplyr::group_by(across(all_of(group_name))) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    # Use furrr to run the run_ml_models function in parallel across each row (e.g., run it in parallel across groups)
    dplyr::mutate(model_performance_results = furrr::future_map(data, 
                                                                ~run_ml_models(.,
                                                                               number_of_workers_for_cv = number_of_workers_for_cv,
                                                                               number_of_workers_for_rf = number_of_workers_for_rf),
                                                                # Set this option to ensure proper random seeds each time it's run, even in parallel
                                                                .options = furrr::furrr_options(seed = 101),
                                                                # Include progress bar
                                                                .progress = TRUE)) |>
    # We no longer need the data column
    dplyr::select(-data) |>
    # Unnest the model_performance_results column to get a regular looking tibble
    # Where each row is a group and its corresponding model performance
    tidyr::unnest(model_performance_results)
  
  # Close the workers, if necessary
  if(number_of_workers_for_groups > 1) future::plan(future::sequential)
  
  results
}

# Do parallelization over groups
# tictoc::tic()
# run_ml_models_by_group(penguins,
#                        group_name = "species",
#                        number_of_workers_for_groups = 3,
#                        number_of_workers_for_cv = 1,
#                        number_of_workers_for_rf = 1)
# tictoc::toc()

# Do parallelization over groups and random forest
# This is a bit faster
# tictoc::tic()
# run_ml_models_by_group(penguins,
#                        group_name = "species",
#                        number_of_workers_for_groups = 3,
#                        number_of_workers_for_cv = 1,
#                        number_of_workers_for_rf = 10)
# tictoc::toc()

# Do parallelization over cross-validation - this takes longer
# This is because we typically want to parallelize across the most outer loop possible
# (e.g., the unit of analysis that will take the longest to run)
# cross-validation is just part of the analysis, so it's less efficient to only parallelize this portion
# it's more efficient to parallelize over groups, if we're doin gthat

# tictoc::tic()
# run_ml_models_by_group(penguins,
#                        group_name = "species",
#                        number_of_workers_for_groups = 1,
#                        number_of_workers_for_cv = 5,
#                        number_of_workers_for_rf = 1)
# tictoc::toc()

# Do parallelization over cross-validation and random forest - this is a bit faster, but still slower than doing over groups and RF
# tictoc::tic()
# run_ml_models_by_group(penguins,
#                        group_name = "species",
#                        number_of_workers_for_groups = 1,
#                        number_of_workers_for_cv = 5,
#                        number_of_workers_for_rf = 10)
# tictoc::toc()