# Define an arbitray function that takes some amount of time to run
# Input is the number of seconds
# Output is a tibble with one column, how_many_seconds
long_function <- function(how_many_seconds){
  
  Sys.sleep(how_many_seconds)
  
  tibble::tibble(how_many_seconds = how_many_seconds)
  
}

# Create a wrapper for long_function that runs sequentially over a vector of seconds
long_function_sequential <- function(how_many_seconds_vector){
  
  how_many_seconds_vector |>
    purrr::map_dfr(long_function)
  
}

# Create a wrapper for long_function that runs in parallel over a vector of seconds
# The number of cores is set by the user
long_function_parallel <- function(how_many_seconds_vector,
                                   number_of_workers = 1){
  
  # Initialize workers for parallel processing
  future::plan(future::multisession,
               workers = number_of_workers)
  
  # Run the function in parallel
  result <- how_many_seconds_vector |>
    furrr::future_map_dfr(long_function)
  
  # Close the workers
  future::plan(future::sequential)
  
  # Return the result
  result
  
}

# Create a function to train and test machine learning model
# This will use the penguins dataset to build a model that predicts penguin body weight
run_ml_models <- function(dataset,
                          number_of_workers = 1){
  
  # Set random seed for consistency
  set.seed(101)
  
  # Split the data into training and testing sets
  split <- rsample::initial_split(dataset, prop = 0.75)
  train <- rsample::training(split)
  test <- rsample::testing(split)
  
  # Create splits from training data using 10-fold cross-validation
  train_cv_splits <- rsample::vfold_cv(train, v = 10)
  
  # Create a data preprocessing recipe
  # Predict body_mass_g using everything else
  recipe <- recipes::recipe(body_mass_g ~ ., data = train)  |>
    # Remove rows with missing outcome variable
    recipes::step_naomit(recipes::all_outcomes())  |>
    # Impute missing predictors using bagged tree model
    recipes::step_impute_bag(recipes::all_predictors(), trees = 100)
  
  # Create a random forest model specification
  rf_spec <- 
    parsnip::rand_forest(trees = 1000,
                         mtry = tune::tune(),
                         min_n = tune::tune()) |>
    parsnip::set_engine("ranger", 
                        importance = "none",
                        seed = 101,
                        # Random forest can internally run in parallel
                        # But set this to 1, so that only parallel computation happens during CV
                        # and we don't end up with nested parallelism
                        num.threads = 1) |>
    parsnip::set_mode("regression")
  
  # Create a workflow
  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(rf_spec)
  
  # Specify performance metric for tuning hyperparaemetrs
  performance_metrics <- yardstick::metric_set(yardstick::rsq)
  
  # Initialize workers for parallel processing

  doFuture::registerDoFuture()
  
  future::plan(future::multisession, 
               workers = number_of_workers)
  
  # Run cross-validation using our CV splits
  # Over a grid size of 10 hyperparameter combinations
  cv_results <-
    workflow |>
    tune::tune_grid(
      resamples = train_cv_splits,
      metrics = performance_metrics,
      grid = 25,
      control = tune::control_grid(verbose = TRUE,
                                   allow_par = TRUE,
                                   parallel_over = "resamples"))
  
  # Select best hyperparameter set
  best_hyperparameters <- cv_results |>
    tune::select_best("rsq")
  
  # Close the workers
  future::plan(future::sequential)
  
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
