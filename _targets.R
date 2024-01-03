# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with workers which will run as local R processes:
  #
  #    controller = crew::crew_controller_local(workers = 2)
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
  )#,
  # # Now let's run long_function in parallel, using values_to_run
  # tar_target(
  #   name = results_parallel,
  #   command = long_function_parallel_wrapper(values_to_run,
  #                                    number_of_workers = 5)
  # )
)