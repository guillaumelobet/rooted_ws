
library(tidyverse)
library(data.table)

setwd("c:/users/benja/Onedrive/Bureau/Archisimple/ArchiJulia_MT/")

### Run once to install necessary Julia packages (takes some time, but only required once per machine)

source("install_julia_packages.R")
install_julia_packages()

### Call of Julia is done through the system() with arguments defining the number of repetitions and the parameters to change
### For each parameters, the call must include the name of the parameter, the starting value, the increment and the stop value
### The number of repetitions must be given as final argument

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

system("Julia batch_MT.jl P_diamMin 0.7 0.01 0.8 10")


### Import result file

all_sims <- fread("myroot.txt", header = T)

### Plot all the RS (/!\ can be dramatically slow if there are a lot)

for(i in unique(all_sims$countSR)){
  rs = subset(all_sims, countSR == i)
  print(rs %>%
    ggplot() +
    theme_classic() +
    geom_segment(aes(x = X1, y = -Z1, xend = X2, yend = -Z2), alpha=0.9) +
    coord_fixed())
}
