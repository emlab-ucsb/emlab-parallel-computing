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