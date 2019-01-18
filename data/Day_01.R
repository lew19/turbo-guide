head(dat)
head(dat, n = 10)
class(dat)
dim(dat)
dat{1,1]
dat[1,1]
view(dat)
View(dat)
# functions for summarizing data
patient_10 <- dat[10, ]
min(patient_10)
max(patient_10)
mean(patient_10)
class(patient_10)
patient_10
mean(as.numeric(patient_10))
?apply
apply(dat, 1 , mean)
apply(dat, 1, mean)
column means <- apply(dat, 2, mean)
column_means <- apply(dat, 2, mean)
(column_means <-apply(dat, 2, mean))
class(column_means)
row_min <- apply(dat, 1, min)
column_min <- apply(dat, 2, min)
row_max <- apply(dat, 1, max)
column_max <- apply(dat, 2, max)
row_SD <- apply(dat, 1, sd)
column_SD <- apply(dat, 2, sd)
?summary
summary(dat)
summary(dat[4:6, 1])
# plotting
plot(row_max)
row_max
plot(row_max, type = "l")
 # exercise: make a plot of meam value across 
# patients 1 - 10 for all columns

column_mean <- apply(dat[1:10, ] , 2, mean)
plot(column_mean, type = "l", ylab = "mean", xlab = "day", main = "mean inflammation across days")

# writing functions
fahrenheit_to_kelvin <- function(temp_F) {
temp_K <- (temp_F - 32) * (5/9) + 273.15 
return(temp_K)
}
fahrenheit_to_kelvin(51)


fahrenheit_to_celcius <- function (temp_F) {
  temp_C <- (temp_F - 32) * (5/9)
  return(temp_C)
}
(temp_C <- fahrenheit_to_celcius(51))

fahrenheit_to_celcius_v2 <- function(temp_F) {
  temp_K <- fahrenheit_to_kelvin(temp_F) 
  temp_C <- temp_K - 273.15
  return(temp_C)
}


# exercise: write a function the computes the minimum and
#           maximum for a numeric vector input, and
#           the returns of the sum of the two
#           (HINT: sum() computes the sum pf the two values)
column_min <-apply(dat, 2, min)
column_min                                            
column_max                                                                                            
sum_of_min_and_max <- function(numeric_vector) {
  min_vector <- min(numeric_vector) 
  max_vector <- max(numeric_vector)
  sum_of_both <- sum(min_vector, max_vector)
  return(sum_of_both)
}
(example_of_exercise <- sum_of_min_and_max(column_min))

# functions with mulitple argumnents and default values
fahrenheit_conversion <- function(temp_F, 
                                  to = "celsius") {
  # check class of temp_F
  stopifnot(class(temp_F) == "numeric")
  stopifnot(class(to) == "character")                                  
                                    
  # what to do if asking for celcius conversion
  if (to == "celsius") {
    temp_out <- fahrenheit_to_celcius(temp_F)
    plot(temp_out, 
         xlab = "Temperature in Fahrenheit",
         ylab = "Converted temperture")
  # what do to othewise
  } else if (to == "kelvin") {
    temp_out <- fahrenheit_to_kelvin(temp_F)
    plot(temp_out, 
         xlab = "Temperature in Fahrenheit",
         ylab = "Converted temperture")
  } else {
    message ("unexpected input for argument 'to'")
  }
}
converted_temperature <- 
  fahrenheit_conversion(temp_F = 51, to = "fahrenheit")
converted_temperature

fahrenheit_to_celcius_v2 <- function(temp_F) {
  temp_K <- fahrenheit_to_kelvin(temp_F) 
  temp_C <- temp_K - 273.15
}


# exercise: in the case that our new function
#           converted_temperature() has a sensible
#           input to argument 'to' , create a plot 
#           of the converted temperature as output
input_temperature <- as.numeric(50:90)

fahrenheit_conversion(input_temperature)
class(input_temperature)

