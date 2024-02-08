
### Run once to install necessary Julia packages (takes some time, but only required once per machine)

#source("install_julia_packages.R")
#install_julia_packages()

### Install needed packages

library(tidyverse)
library(data.table)
source("functions.R")

### Call of Julia is done through the system() with arguments defining the number of repetitions and the parameters to change
### For each parameters, the call must include the name of the parameter, the starting value, the increment and the stop value
### The number of repetitions must be given as final argument
###
### Example : I want to change P_diamMin from 0.7 to 0.8 by increments of 0.01, and do 10 repetitions of each set of parameters
### The call will be 'system("Julia batch_MT.jl P_diamMin 0.7 0.01 0.8 10")'
### To specify a fixed value for a particular parameter, put the same value for starting and stop value, the increment doesn't matter (but is
### needed)

### This is the multiprocessing version of Julia. Workers will be spawned on the first specified parameter to share the load.
### If you have multiple parameters you want to change, be sure to put the one with the more steps as the first parameter
### to optimize the parallelization.

### If you want to specify the number of workers, put the optional parameter "-p n" after "Julia" in the call
### where n is the number of workers. If unspecified, the number of workers will be equal to the number of
### logical CPUs of the computer.

### /!\ Due to the way ArchiSimple is constructed, this is a multiprocess version, not a multithread one. As such, the
### parallelization is achieved by spawning an independent Julia process for each worker with its own memory space.
### It can quickly become very hungry memory-wise. If the memory is full the execution will slow down and eventually crash.
### If this is the case, two possibilities : either diminish the number of workers (via the "-p n" optional argument) or the
### number of values by parameter (divide the execution in several ones).


rep <- 1

distRamif <- c(4, 6)
diamMin <- c(0.1, 0.3)
diamMax <- c(3, 6)
penteVitDiam <- c(27, 30)
propDiamRamif <- c(0.3, 0.6)

request <- paste0("Julia batch_MT.jl ",
                  
                  "P_distRamif ",
                  distRamif[1]," ",
                  get_inc(distRamif)," ",
                  distRamif[2]," ",
                  # 
                  "P_diamMin ",
                  diamMin[1]," ",
                  get_inc(diamMin)," ",
                  diamMin[2]," ",
                  # 
                  "P_diamMax ",
                  diamMax[1]," ",
                  get_inc(diamMax)," ",
                  diamMax[2]," ",
                  
                  # "P_penteVitDiam ",
                  # penteVitDiam[1]," ",
                  # get_inc(penteVitDiam)," ",
                  # penteVitDiam[2]," ",
                  # 
                  # "P_propDiamRamif ",
                  # propDiamRamif[1]," ",
                  # get_inc(propDiamRamif)," ",
                  # propDiamRamif[2]," ",
                  
                  rep,"")

setwd("~/Desktop/ArchiJulia-main//")
system(request)


### Import result file






setwd("~/Desktop/ArchiJulia-main/output/")
library(tidyverse)
library(data.table)
library(dplyr)

files <- list.files()
length(files)
all_data <- NULL

cols <- c("NumAxe", "Jour", "Diam", 
          "X1", "Y1", "Z1", "X2", "Y2", "Z2", 
          "countSR", 
          "IPD", 
          "DiamMin",
          "DiamMax"#,
          # "EL",
          # "propDiam"
          )
i <- 0
k <- 0
all_data <- NULL

for(f in files){
  i <- i+1
  k <- k+1
  if(k == 100){
    print(i)
    k <- 0
  }
  rs <- fread(f, header = T, col.names = cols) %>% 
    mutate(length = sqrt((X2-X1)^2 + (Y2-Y1)^2 + (Z2-Z1)^2) )
  temp <- tibble(file = f, 
                 axes = max(rs$NumAxe), 
                 length = sum(rs$length),
                 depth = max(rs$Z1),
                IPD = rs$IPD[1], 
                 EL = rs$EL[1],
                 DiamMax = rs$DiamMax[1],
                 propDiam = rs$propDiam[1],
                 DiamMin = rs$DiamMin[1]
  )
  all_data <- rbind(all_data, temp)
}


hist(all_data$axes, breaks = 100)
hist(all_data$length, breaks = 100)
hist(all_data$depth, breaks = 100)
hist(all_data$IPD, breaks = 100)
hist(all_data$EL, breaks = 100)
hist(all_data$DiamMin, breaks = 100)
hist(all_data$DiamMax, breaks = 100)
hist(all_data$, breaks = 100)

plot(all_data$axes, all_data$length)

plot(all_data$IPD*all_data$DiamMin*all_data$EL, all_data$length)
plot(all_data$EL, all_data$length)
plot(all_data$DiamMin, all_data$length)





fread(files[sample(1:length(files), 1)], header = T, col.names = cols) %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = X1, y = -Z1, xend = X2, yend = -Z2
                   #,size = Diam
                   ), alpha=0.9) +
  coord_fixed()


fread(files[length(files)], header = T, col.names = cols) %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = X1, y = -Z1, xend = X2, yend = -Z2
                   #,size = Diam
  ), alpha=0.9) +
  coord_fixed()




ind  <- which(all_data$length == max(all_data$length))

fread(files[ind], header = T, col.names = cols) %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = X1, y = -Z1, xend = X2, yend = -Z2
                   #, size = Diam
                   ), alpha=0.9) +
  coord_fixed()
