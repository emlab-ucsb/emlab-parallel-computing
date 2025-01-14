# Load penguins dataset, which we'll use in ML modeling example
data(penguins, package = "modeldata")

# Inspect the dataset
penguins

# Create a function to train and test machine learning model
# This will use the penguins dataset to build a model that predicts penguin body weight
run_ml_models <- function(dataset,
                          # Random forest can internally run in parallel
                          # How many parallel workers should random forest use?
                          number_of_workers_for_rf = 1,
                          # Across how many parallel workers should we run cross-validation?
                          number_of_workers_for_cv = 1){
  
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
  # Over a grid size of 25 hyperparameter combinations
  cv_results <-
    workflow |>
    tune::tune_grid(
      resamples = train_cv_splits,
      metrics = performance_metrics,
      grid = 25,
      control = tune::control_grid(verbose = TRUE,
                                   # Only allow parallelization across folds if doing CV in parallel
                                   allow_par = number_of_workers_for_cv > 1))
  
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
tictoc::tic()
run_ml_models(penguins,
              number_of_workers_for_rf = 1,
              number_of_workers_for_cv = 1)
tictoc::toc()

# Let's test the function and time it - next running RF in parallel and cross-validation sequentially
tictoc::tic()
run_ml_models(penguins,
              number_of_workers_for_rf = 10,
              number_of_workers_for_cv = 1)
tictoc::toc()

# Let's test the function and time it - now running RF sequentially and cross-validation in parallel
# This will typically be faster because cross-validation is the more outer loop than the internal random forest model, 
# and is the longest running operation
tictoc::tic()
run_ml_models(penguins,
              number_of_workers_for_rf = 1,
              number_of_workers_for_cv = 10)
tictoc::toc()


# Now let's train separate models for each of the 3 species groups
# To do this, we will nest our dataset by group, then run run_ml_models for each group,
# then unnest the results
# First, we can do this sequentially using purrr::map

tictoc::tic()

penguins |>
  dplyr::group_by(species) |>
  tidyr::nest() |>
  dplyr::ungroup() |>
  # Use furrr to run the run_ml_models function in parallel across each row (e.g., run it in parallel across groups)
  dplyr::mutate(model_performance_results = purrr::map(data, 
                                                       ~run_ml_models(.,
                                                                      number_of_workers_for_cv = 1,
                                                                      number_of_workers_for_rf = 1),
                                                       # Include progress bar
                                                       .progress = TRUE)) |>
  # We no longer need the data column
  dplyr::select(-data) |>
  # Unnest the model_performance_results column to get a regular looking tibble
  # Where each row is a group and its corresponding model performance
  tidyr::unnest(model_performance_results)

tictoc::toc()


# Now use parallelization over groups
# We have 3 groups, so use 3 workers

future::plan(future::cluster,
             workers = parallel::makeForkCluster(3))

tictoc::tic()

penguins |>
  dplyr::group_by(species) |>
  tidyr::nest() |>
  dplyr::ungroup() |>
  # Use furrr to run the run_ml_models function in parallel across each row (e.g., run it in parallel across groups)
  dplyr::mutate(model_performance_results = furrr::future_map(data, 
                                                              ~run_ml_models(.,
                                                                             number_of_workers_for_cv = 1,
                                                                             number_of_workers_for_rf = 1),
                                                              # Set this option to ensure proper random seeds each time it's run, even in parallel
                                                              .options = furrr::furrr_options(seed = 101),
                                                              # Include progress bar
                                                              .progress = TRUE)) |>
  # We no longer need the data column
  dplyr::select(-data) |>
  # Unnest the model_performance_results column to get a regular looking tibble
  # Where each row is a group and its corresponding model performance
  tidyr::unnest(model_performance_results)

tictoc::toc()

future::plan(future::sequential)
