
### Run once to install necessary Julia packages (takes some time, but only required once per machine)

#source("install_julia_packages.R")
#install_julia_packages()

### Install needed packages

library(tidyverse)
library(data.table)
library(dplyr)
source("functions.R")


# Go to the main repository
setwd("~/Desktop/ArchiJulia-main//")


# run the model
explore_model_space(repetitions = 10,
            steps = 15,
            distRamif = c(3, 8),
            diamMax = c(2, 6)
  )


## Get the summary dta from the outputs
all_data <- get_summary_data(path = "output")

# plot an histogram of output metrics
hist(all_data$length, breaks = 50)

# Plot variables against each others

all_data %>% 
  ggplot(aes(diamMax, distRamif, z=length)) + 
  geom_contour_filled(bins = 20) + 
  theme_classic() 

all_data %>% 
  ggplot(aes(diamMax / distRamif,length, color = diamMax)) + 
  geom_point() + 
  theme_classic() 


all_data %>% 
  ggplot(aes(diamMax, distRamif, z=depth)) + 
  geom_contour_filled(bins = 20) + 
  theme_classic() 


all_data %>% 
  ggplot(aes(diamMax ,depth, color = distRamif)) + 
  geom_point() + 
  theme_classic()

all_data %>% 
  ggplot(aes(diamMax, distRamif, z=axes)) + 
  geom_contour_filled(bins = 20) + 
  theme_classic() 


# Plot random root systems from the outputs
plot_roots(all_data, path = "output", n = 3)




# Plot the smallest / largest root system

files <- list.files("output")
ind  <- which(all_data$length == min(all_data$length))

fread(paste0("output/", files[ind]), header = T, col.names = cols) %>%
  ggplot() +
  theme_classic() +
  geom_segment(aes(x = X1, y = -Z1, xend = X2, yend = -Z2
                   #, size = Diam
  ), alpha=0.9) +
  coord_fixed()

