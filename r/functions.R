# Define an arbitrary function that takes some amount of time to run
# Input is the number of seconds
# Output is that value again
long_function <- function(how_many_seconds){
  
  Sys.sleep(how_many_seconds)
  
  how_many_seconds
}

# Create a wrapper for long_function that runs sequentially over a vector of seconds
long_function_sequential <- function(how_many_seconds_vector){
  
  how_many_seconds_vector |>
    purrr::map(long_function, 
               # Include a progress bar
               .progress = TRUE)
  
}

# Create a wrapper function for long_function that allows us to specify the number of workers,
# and automatically starts and shuts down the workers
long_function_parallel <- function(how_many_seconds_vector,
                                           number_of_workers = 1){
  
  # Initialize workers for parallel processing
  # Make fork cluster, which should work on Macs, Linux, and GRIT servers
  # An alternative is to use future::plan(future::multisession, workers = number_workers), but this may not be as fast in all cases
  future::plan(future::cluster,
               workers = parallel::makeForkCluster(number_of_workers))
  
  # Run the function in parallel
  result <- how_many_seconds_vector |>
    furrr::future_map(long_function, 
                      # Include a progress bar
                      .progress = TRUE)
  
  # Close the workers
  future::plan(future::sequential)
  
  # Return the result
  result
  
}