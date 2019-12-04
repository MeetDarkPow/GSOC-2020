#################### Easy Test ####################

# Load these libraries

library(microbenchmark)
library(PeakSegDP)
library(PeakSegOptimal)
library(ggplot2)

# Taking input to create user friendly data set 

N <- as.integer(readline(prompt="Enter size of Data set: "))
test_size_value <- c(N, 10*N, 100*N, 1000*N)

# Two empty variables to hold computation time

cdpa <- integer(length(test_size_value))
pdpa <- integer(length(test_size_value))

# Function to calculate computation time of cDPA() and PeakSegPDPA() Function
# computation_time function 2 parameters.
# First  -> ms which corresponds to number of max segements
# Second -> lambda which corresponds to vector of means

computation_time <- function(ms, lambda) {
  # for() loop used to compute each set computation time
  for (i in 1:length(test_size_value)) {
    mbm <- microbenchmark(cDPA(rpois(test_size_value[i], lambda), maxSegments = as.integer(ms)),
                          PeakSegPDPA(rpois(test_size_value[i], lambda), max.segments = as.integer(ms)))
    execution_time <- summary(mbm)$mean   # Extracting computation time
    cdpa[i] <<- execution_time[1]         # Storing extracted computation time 
    pdpa[i] <<- execution_time[2]         # Storing extracted computation time 
  }
}

# Function for plotting graph of PeakSegDP::cDPA versus PeakSegOptimal::PeakSegPDPA
# computation_graph has 1 parameter
# First -> sets which corresponds to number of data sets we want to display

computation_graph <- function(sets) {
  coord <- data.frame(cdpa, pdpa, test_size_value)  
  ggplot(coord, aes(test_size_value)) + 
    geom_line(aes(y = cdpa, colour = "cDPA")) + 
    geom_line(aes(y = pdpa, colour = "PeakSegPDPA")) +
    ggtitle("PeakSegDP::cDPA versus PeakSegOptimal::PeakSegPDPA") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_log10() + scale_y_log10() +
    xlab("N") + labs(colour = "Function Comparison") + 
    ylab("Execution Time") + 
    theme_bw(base_size = 15)
}
