
# calculate the increments

get_inc <- function(vect, steps = 5){
  return((vect[2]-vect[1])/steps)
}



### Run the model

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

explore_model_space <- 
  function(repetitions = 3,
           steps = 5,
           distRamif = c(5,5),
           diamMax = c(4, 4)
  ){
    
    #empty output results
    if(dir.exists("output")){
      files <- list.files("output/")
      for(f in files){
        file.remove(paste0("output/",f))
      }
    }
    
    
    dR <- get_inc(distRamif, steps)
    if(dR == 0) dR <- 1
    
    dM <- get_inc(diamMax, steps)
    if(dM == 0) dM <- 1
    
    request <- paste0("Julia batch_MT.jl ",
                      
                      "P_distRamif ",
                      distRamif[1]," ",
                      dR," ",
                      distRamif[2]," ",
                      # 
                      "P_diamMax ",
                      diamMax[1]," ",
                      dM," ",
                      diamMax[2]," ",
                      
                      repetitions,"")
      
      system(request)
  }


get_summary_data <- function(path = "output"){
  
  
  files <- list.files(path)
  length(files)
  
  cols <- c("NumAxe", "Jour", "Diam", 
            "X1", "Y1", "Z1", "X2", "Y2", "Z2", 
            "countSR", 
            "distRamif", 
            "diamMax"
  )
  
  i <- 0
  all_data <- NULL
  
  for(f in files){
    i <- i+1
    
    rs <- fread(paste0(path,"/",f), header = T, col.names = cols) %>% 
      mutate(length = sqrt((X2-X1)^2 + (Y2-Y1)^2 + (Z2-Z1)^2) )
    
    temp <- tibble(file = f, 
                   axes = max(rs$NumAxe), 
                   length = sum(rs$length),
                   depth = max(rs$Z1),
                   distRamif = rs$distRamif[1], 
                   diamMax = rs$diamMax[1]
    )
    all_data <- rbind(all_data, temp)
  }
  
  print(paste0(i, " root system data compiled"))
  return(all_data)
}






get_all_roots <- function(path = "output"){
  
  files <- list.files(path)
  length(files)
  
  cols <- c("NumAxe", "Jour", "Diam", 
            "X1", "Y1", "Z1", "X2", "Y2", "Z2", 
            "countSR", 
            "distRamif", 
            "diamMax"
  )
  
  i <- 0
  all_data <- NULL
  
  for(f in files){
    i <- i+1
    
    rs <- fread(paste0(path,"/",f), header = T, col.names = cols) %>% 
      mutate(sim = i)
    all_data <- rbind(all_data, rs)
  }
  
  print(paste0(i, " root system data compiled"))
  return(all_data)
}





### Plot root systems
plot_roots <- function(all_data, 
                       path = "output", 
                       n_roots = 3){
  
  dR <- unique(all_data$distRamif)
  ind_dR <- sample(1:length(dR), n_roots)
  dR <- dR[ind_dR]
  
  dM <- unique(all_data$diamMax)
  ind_dM <- sample(1:length(dM), n_roots)
  dM <- dM[ind_dM]
  
  temp <- all_data %>% 
    filter(distRamif %in% dR & diamMax %in% dM) %>% 
    distinct(distRamif, diamMax, .keep_all = T)
  
  
  files <- unique(temp$file)
  cols <- c("NumAxe", "Jour", "Diam", 
            "X1", "Y1", "Z1", "X2", "Y2", "Z2", 
            "countSR", 
            "distRamif", 
            "diamMax"
  )
  
  i <- 0
  temp <- NULL
  
  for(f in files){
    i <- i+1
    
    rs <- fread(paste0(path,"/",f), header = T, col.names = cols) %>% 
      mutate(sim = i)
    temp <- rbind(temp, rs)
  }
  
  temp %>%
    ggplot() +
    theme_classic() +
    geom_segment(aes(x = X1, y = -Z1, 
                     xend = X2, yend = -Z2,
                     color = Jour
                     #,size = Diam
    ), alpha=0.9) +
    coord_fixed() + 
    facet_grid(distRamif~diamMax) + 
    scale_color_viridis_c()
  
}
  




