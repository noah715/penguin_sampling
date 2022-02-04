library("tidyverse")

setwd("~/Desktop/penguin_sampling/")
samples <- read.csv("N_matrix.csv")

# Only sample 1 set of sites
option_1 <- function(n, samples){
  # Samples all of the sites to see which ones will be picked
  sites <- sample(1:nrow(samples), nrow(samples))
  
  # Creates a new dataframe with the reordered sites 
  reordered_samples <- samples[sites, ]
  
  # Creates a new dataframe that will have NA values in it, and then the actual
  # site information will be copied over
  new_samples <- reordered_samples
  new_samples[-1] <- NA
  
  # This is only sampling the first n sites
  new_samples[1:n,] <- reordered_samples[1:n,]
  
  return(new_samples)
}

# New sites every year
option_2 <- function(n, samples){
  sites <- sample(1:nrow(samples), nrow(samples))
  
  reordered_samples <- samples[sites, ]
  new_samples <- reordered_samples
  new_samples[-1] <- NA

  # This method samples a new n sites every year
  for (i in 1:(ncol(samples)-1)){
    new_samples[1:n + (i-1)*n,i+1] <- as.numeric(reordered_samples[1:n + (i-1)*n, i+1])
  }
  
  return(new_samples)
}

# Rotating panel of sites
option_3 <- function(n, samples, group_size){
  sites <- sample(1:nrow(samples), nrow(samples))
  
  reordered_samples <- samples[sites, ]
  new_samples <- reordered_samples
  new_samples[-1] <- NA
  
  i <- 1
  
  # Tacks on an extra site to be sampled while removing the first site in the sampling screen
  while (i + group_size - 1 <= ncol(samples)){
    new_samples[i:(i+group_size-1), i+1] <- as.numeric(reordered_samples[i:(i+group_size-1), i+1])
    
    i <- i + 1
  }
  
  return(new_samples)
}

# Augmented serially alternating
option_4 <- function(n, samples, steps, common){
  sites <- sample(1:nrow(samples), nrow(samples))
  
  reordered_samples <- samples[sites, ]
  new_samples <- reordered_samples
  new_samples[-1] <- NA
  
  # Common sites that are sampled
  new_samples[1:common,-1] <- reordered_samples[1:common,-1]
  
  # Applies the same logic and in option 2, but uses a modulus operator to have it repeat
  for (i in 0:(length(samples) - 2)){
    new_samples[(common+1):(common+n) + n*(i %% steps), i+2] <- as.numeric(reordered_samples[(common+1):(common+n) + n*(i %% steps), i+2])
  }

  return(new_samples)
}

# Partially augmented serially alternating
option_5 <- function(n, samples, steps, common){
  sites <- sample(1:nrow(samples), nrow(samples))
  
  reordered_samples <- samples[sites, ]
  new_samples <- reordered_samples
  new_samples[-1] <- NA
  
  # Same as in option 4
  new_samples[1:common,-1] <- reordered_samples[1:common,-1]
  
  for (i in 0:(length(samples) - 2)){
    new_samples[(common+1):(common+n) + n*(i %% steps), i+2] <- as.numeric(reordered_samples[(common+1):(common+n) + n*(i %% steps), i+2])
    
    # Picks a site from the previous year to be sampled in the next year
    # Always chooses the first site from each group first, then moves on to the 
    # second site from each group, then the third and so on.  It will loop back to
    # the first site if all sites have been resampled
    if (i > 0){
      resample_site <- ((i-1) %/% steps) %% n + common + 1 + n*((i - 1) %% steps)
      new_samples[resample_site, i+2] <- as.numeric(reordered_samples[resample_site, i+2])
    }
  }
  
  return(new_samples)
}

# Random sampling
option_6 <- function(n, samples){
  
  new_samples <- samples
  new_samples[-1] <- NA
  
  # Generates a new list of random samples for each year
  for (i in 1:(length(samples)-1)){
    sites <- sample(1:nrow(samples), n)
    new_samples[sites,i+1] <- samples[sites, i+1]
  }
  return(new_samples)
}



option_3(100, samples, 2)
