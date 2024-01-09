# Define an arbitrary function that takes some amount of time to run
# Input is the number of seconds
# Output is that value again
long_function <- function(how_many_seconds){
  
  Sys.sleep(how_many_seconds)
  
  how_many_seconds
}

# Let's test long_function and time it
tictoc::tic()
long_function(2)
tictoc::toc()

# Let's run this function repeatedly over a vector of values using purrr::map
# We will run this 5 times using a value of 2

tictoc::tic()
rep(2,5) |>
  purrr::map(long_function, 
             # Include a progress bar
             .progress = TRUE)
tictoc::toc()

# This looks like an example of something that could be pleasingly parallel!
# Let's now run this in parallel using furrr::future_map

# First, we need to set how many workers we want to use
# One way to do this would be to use all available cores on the machine
num_workers <- future::availableCores()
num_workers

# If working locally on your personal computer, you may wish to leave 1 core free for other tasks
# Using future::availableCores(omit = 1) instead of future::availableCores() -1 means
# this will always be at least 1
num_workers <- future::availableCores(omit = 1)
num_workers

# Alternatively, you can set the number of workers manually
number_workers <- 5

# Initialize workers for parallel processing using future::plan
# Make fork cluster, which should work on Macs, Linux, and GRIT servers
# An alternative is to use future::plan(future::multisession, workers = number_workers), but this may not be as fast in all cases
 future::plan(future::cluster,
              workers = parallel::makeForkCluster(number_workers))
 
 # Check to see how many workers are now available for parallel processing
 future::nbrOfWorkers()

 # Now we can run it in parallel! We simply change purrr::map to furrr::future_map
 tictoc::tic()
 rep(2,5) |>
   furrr::future_map(long_function, 
              # Include a progress bar
              .progress = TRUE)
 tictoc::toc()
 
 # Always shut down the workers when you're done
 # Do this by simply changing the future::plan settings to evaluate things sequentially
future::plan(future::sequential)

# Check to see how many workers are now available for parallel processing
future::nbrOfWorkers()