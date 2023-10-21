# Load required libraries
require(stringr)
require(ggplot2)
require(data.table)

# Define function to list all subfolders in a folder
list_subfolders <- function(folder_path) {
  subfolders <- list.dirs(folder_path, recursive = FALSE, full.names = FALSE)
  return(subfolders)
}

# Folder list to iterate over
folder=list("mar","out","sep")

# Loop over each folder
for(a in 1:length(folder)){
  month=folder[[a]]
  
  # Set directory path
  dir_path <- paste0("Groundbasedlidar_k34/",month)
  
  # Get list of image files
  image_files <- list.files(path = dir_path, pattern = "\\.(jpg|jpeg|png|gif|bmp)$", recursive = TRUE, full.names = TRUE)
  
  # Delete the files
  lapply(image_files, file.remove)
  
  # Set month for the next steps
  monthss=month
  
  # List all subfolders within the month's folder
  folders=list_subfolders(paste0("Groundbasedlidar_k34/",monthss))
  
  # Loop through each subfolder
  for(j in 1:length(folders)){
    # Set working directory
    setwd(paste0("Groundbasedlidar_k34//",monthss,"/",folders[j]))
    
    # List all .asc files
    files=list.files(pattern=".asc")
    
    # Define a function to read the files
    fread2 = function(x){read.table(x, sep = ";")}
    files=lapply(files, fread2)
    
    # Function to process each file
    fun1 = function(x){
      h1 <- substr(x[seq(1, nrow(x), 1), 1], 2, 1000)
      h1 <- data.frame(altura = as.numeric(as.character(h1)))
      
      # Modify h1 based on specific conditions
      if(h1[nrow(h1),1]<100 & h1[nrow(h1),1] != 0) {h1 <- data.frame(h1[-nrow(h1),]);names(h1) <- "altura"}
      if(h1[1,1]>100){h1 <- data.frame(h1[-1,]);names(h1) <- "altura"}
      if (nrow(h1) %% 2 != 0) {h1 <- data.frame(h1[-1,]);names(h1) <- "altura"}
      
      cam1 <- data.frame(cam=1, h1)
      return(cam1)
    }
    
    # Apply the function to each file
    parcela = lapply(files,fun1)
    
 
    dist1 <- 0 + (rep(1:(nrow(parcela[[1]])/2),each=2))*(15/(nrow(parcela[[1]])/2))
    dist2 <- 15 + (rep(1:(nrow(parcela[[2]])/2),each=2))*(15/(nrow(parcela[[2]])/2))
    dist3 <- 30 + (rep(1:(nrow(parcela[[3]])/2),each=2))*(15/(nrow(parcela[[3]])/2))
    dist4 <- 45 + (rep(1:(nrow(parcela[[4]])/2),each=2))*(15/(nrow(parcela[[4]])/2))
    dist5 <- 60 + (rep(1:(nrow(parcela[[5]])/2),each=2))*(15/(nrow(parcela[[5]])/2))
    dist6 <- 75 + (rep(1:(nrow(parcela[[6]])/2),each=2))*(15/(nrow(parcela[[6]])/2))
    dist7 <- 90 + (rep(1:(nrow(parcela[[7]])/2),each=2))*(15/(nrow(parcela[[7]])/2))
    dist8 <- 105 + (rep(1:(nrow(parcela[[8]])/2),each=2))*(15/(nrow(parcela[[8]])/2))
    dist9 <- 120 + (rep(1:(nrow(parcela[[9]])/2),each=2))*(15/(nrow(parcela[[9]])/2))
    dist10 <- 135 + (rep(1:(nrow(parcela[[10]])/2),each=2))*(15/(nrow(parcela[[10]])/2))


    cam1 <- data.frame(cam=1, distancia = dist1, parcela[[1]])
    cam2 <- data.frame(cam=2, distancia = dist2, parcela[[2]])
    cam3 <- data.frame(cam=3, distancia = dist3, parcela[[3]])
    cam4 <- data.frame(cam=4, distancia = dist4, parcela[[4]])
    cam5 <- data.frame(cam=5, distancia = dist5, parcela[[5]])
    cam6 <- data.frame(cam=6, distancia = dist6, parcela[[6]])
    cam7 <- data.frame(cam=7, distancia = dist7, parcela[[7]])
    cam8 <- data.frame(cam=8, distancia = dist8, parcela[[8]])
    cam9 <- data.frame(cam=9, distancia = dist9, parcela[[9]])
    cam10 <- data.frame(cam=10, distancia = dist10, parcela[[10]])

    parcela <- rbind(cam1, cam2, cam3, cam4, cam5, cam6, cam7, cam8, cam9, cam10)
    tipo <- rep(1:2, nrow(parcela)/2)
    # Adjust height
    h <- data.frame(parcela$altura)  
    h <- apply(h, c(1, 2), function(x) ifelse (x > 100, x - 100, x))
    h <- apply(h, c(1, 2), function(x) ifelse(x > 0, x + 1, x))
    parcela$altura <- data.frame (altura = as.numeric(as.character(h))) 
    
    # Create the final table
    parc=paste0(str_sub(getwd(),78,80))
    parcela <- data.frame(trat = paste0(str_sub(getwd(),78,80)), parc = "p1", cam = parcela$cam, tipo, distancia = parcela$distancia, altura = parcela$altura)
    parcela <- parcela[parcela$altura < 60,]
    
    interval <- 5

    # Loop over the distance intervals and generate a plot for each interval
    for (i in seq(from = 0, to = max(parcela$distancia)-interval , by = interval)) {
      
      # Subset the data for plotting
      subset_data <- subset(parcela, distancia >= i & distancia <= i+interval)
      subset_data <- subset_data[!is.na(subset_data$distancia) & !is.na(subset_data$altura),]
      subset_data=subset_data[subset_data$tipo==2,]
      
      # Create the plot
      plot <- ggplot(na.omit(subset_data), aes(x=distancia, y=altura)) + ylim(2, 36) +
        geom_bin2d(bins = 90) +
        scale_fill_continuous(type = "viridis") +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +  guides(fill = FALSE)
      
      # Print and save the plot
      print(plot)
      ggsave(paste0("plot_",monthss,"_",paste0(parc),"_",round(i,2),"_to_",round(i+interval,2),".jpeg"), plot, width = 7, height = 7, dpi = 78)
    }
  }
}
