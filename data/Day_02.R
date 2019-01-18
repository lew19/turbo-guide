rm(list = ls())
dat <- read.csv(file = "data/inflammation-01.csv" , 
                header = FALSE)

# this new functio to get summary statistics from data
analyze <- function(file_name) {
  
  #read in datat at file_name
  dat <-read.csv(file =file_name, header = FALSE)
  
  #get average, minimum, maximum along colunns
  column_max <- apply(dat, 2, max)
  column_min <- apply(dat, 2, min)
  column_avg <- apply(dat, 2, mean)
  
  # output
  out <- rbind(column_max, column_min, column_avg)
  return(out)
}

analyze(file_name = "data/inflammation-01.csv")

# get files names of inflammation CSV files
inflammation_files <- list.files(path = "data",
                                 pattern = "inflammation",
                                 full.names = TRUE)

#for loops for iteration
inflammation_results <- vector("list"),
                               length(inflammation_files))

# for loops interating over files
for (file_index in seq_along(inflammation_results)) {
  file <- inflammation_results[file_index]
  print(paste("Analyzing", file))
  out <- analyze(file_name = file)
  inflammation_results[[file_index]] <- out
}

inflammation_results2 <-
  lapply(X = seq_along(inflammation_results),
       FUN = function(file_index) {
         file <- inflammation_files[file_index]
         out <- analyze(file_name = file)
         return(out)
       })

all.equal(inflammation_results,
          inflammation_results2)

