#################################################################
#  1 STEP TO SUCCESS
#################################################################

N <- readline(prompt="Enter size of Data set: ")
N <- as.integer(N)
S <- readline(prompt="Enter number of Data set: ")
S <- as.integer(S)
df <- data.frame(matrix(ncol = S, nrow = N))
creating_dataset <- function(N, S) {
  for (i in 1:S) {
    df[, i] <<- rpois(N,50)
  }
}

#################################################################
# 2 STEP OF SUCCESS
#################################################################

library(microbenchmark)
library(PeakSegDP)
library(PeakSegOptimal)

cdpa <- c()
pdpa <- c()
computational_time <- function(N, S) {
  for (i in 1:S) {
    f <- cDPA(df[, i], maxSegments = 2L)
    g <- PeakSegPDPA(df[, i], max.segments = 2L)
    mbm <- microbenchmark(f, g, times = 1000L)
    execution_time <- summary(mbm)$mean 
    cdpa <<- append(cdpa, execution_time[1])
    pdpa <<- append(pdpa, execution_time[2])
  }
}

#################################################################
# 3 STEP OF SUCCESS
#################################################################

library(ggplot2)

coord <- data.frame(cdpa, pdpa)
data_set_num <- c(1:S)
ggplot(coord, aes(data_set_num)) + 
  geom_line(aes(y = cdpa, colour = "cDPA")) + 
  geom_line(aes(y = pdpa, colour = "PeakSegPDPA")) +
  scale_x_log10(breaks = seq(0, S, by = 10)) + 
  scale_y_log10(breaks = seq(0, 1000, by = 2)) + 
  xlab("Data Set Number") + labs(colour = "Function Comparison") + 
  ylab("Execution Time (in Nanoseconds)") + 
  theme_bw(base_size = 15)

