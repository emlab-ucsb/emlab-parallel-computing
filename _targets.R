# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# Load penguins dataset, which we'll use in ML modeling example
data(penguins, package = "modeldata")

# Set number of works to use for parallel processing
# If doing this on your personal machine, you can use something like this:
#num_workers <- future::availableCores()

# If using the emLab server, you can use something like this:
num_workers <- 10

# Set target options:
tar_option_set(
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #    controller = crew::crew_controller_local(workers = num_workers)
  #
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/functions.R")

# Here is our list of targets
list(
  # What values should we run in our arbitrary function, long_function?
  tar_target(
    name = values_to_run,
    command = rep(2,5)
  ),
  # Let's run long_function sequentially, using values_to_run
  tar_target(
    name = results_sequential,
    command = long_function_sequential(values_to_run)
  ),
  # Let's run long_function sequentially again, using values_to_run
  tar_target(
    name = results_sequential_2,
    command = long_function_sequential(values_to_run)
  )
  # I include these other targets so you see how we could include them in the pipeline, if desired
  # But for the demo, we will just focus on the above
  # # Now let's run long_function in parallel, using values_to_run
  # tar_target(
  #   name = results_parallel,
  #   command = long_function_parallel(values_to_run,
  #                                    number_of_workers = num_workers)
  # ),
  # # Run ML model, with cross-validation being done sequentially
  # tar_target(
  #   name = ml_results_sequential_cv,
  #   command = run_ml_models(penguins,
  #                           number_of_workers = 1)
  # ),
  # # Run ML model, with cross-validation being done in parallel
  # tar_target(
  #   name = ml_results_parallel_cv,
  #   command = run_ml_models(penguins,
  #                           number_of_workers_for_cv = num_workers)
  # ),
  # # Run ML model by species, where parallelization happens across species, and not within cross-validation
  # tar_target(
  #   name = ml_results_parallel_by_group,
  #   command = run_ml_models_by_group(penguins,
  #                          group_name = "species",
  #                          number_of_workers_for_groups = num_workers,
  #                          number_of_workers_for_cv = 1)    
  # )
)