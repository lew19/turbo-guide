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

# visualization with ggplot2
library(ggplot2)
dat_for_plotting <- dat[, 1:5]
hist(dat_for_plotting[,2])

p_1 <- ggplot(dat_for_plotting, aes(x =V2)) +
  geom_histogram()
p_1

# exercise: create a scatterplot for V2 against V3 
#           x-axis = V2, y-axis = V3
p_2 <- ggplot(dat_for_plotting, aes(x = V3, y = V4)) +
  geom_point() +
  xlab("Day 3 inflammation data") +
  ylab("Day 4 inflammation data") +
  ggtitle("Scatterplot of Day 3 vs 4 inflammation data") +
  theme_minimal()
p_2

# data subsetting with dplyr
library(dplyr) 
dat_subset <- filter(dat, V2 > 0)

dat_subset_piped <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4"))
  
# exercise: create an object dat_subset_piped2 that
#           is exactly the same as dat_subset piped 
#           but does not use the pipe to create a 
#           sequence of function calls
dat_subset_piped2<- select(filter(dat, V2 > 0),
                            c("V3", "V4"))
all.equal(dat_subset_piped,dat_subset_piped2)


# more dplyr fun: mutate, summarize
? mutate
dat_transmuted <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  transmute (
    sum_V4_and_V5 = V4 + V5,
    prod_V4_and_V5 = V4 * V5,
    mean_all_columns = (V3 + V4 + V5) / 3
  )

?summarise
dat_summarised <- dat %>%
  summarise(
    mean_V4 = mean(V4),
    sd_V4 = sd(V4),
    min_V4 = min(V4)
  )
# wrapping up piping, dplyr, and ggplot2
p_dat_transmuted <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  transmute (
    sum_V4_and_V5 = V4 + V5,
    prod_V4_and_V5 = V4 * V5,
    mean_all_columns = (V3 + V4 + V5) / 3
  ) %>% 
  ggplot(aes(x = sum_V4_and_V5, y = mean_all_columns, colour = prod_V4_and_V5)) +
  geom_point() +
  xlab("sum of Day 4 and Day 5") +
  ylab("Mean of days 3, 4, and 5") +
  ggtitle("Scatterplot of derived values") +
  theme_minimal()
ggsave("final_plot.pdf" , plot = p_dat_transmuted)
