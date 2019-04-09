
#rm(i)

#lapply(dbListConnections(MySQL()),dbDisconnect)

#con=dbConnect(MySQL(),user='root',password='',host='localhost',dbname='minor')
library(ggplot2)
library(RMySQL)
library(htmlTable)
library(magrittr)
library(R2HTML)
library(yaml)


kvalues <- c(5,7,9,11,13,15,17)

percentage_correctness <- c()

measure_accuracy <- function(k){
  
  correct <- 0
  
  incorrect <- 0
  
  j=1
  while (j < 21)
  {
    Image <- readImage(paste(paste("C:\\xampp\\htdocs\\minor\\Minor_dataset\\Alternaria Alternata\\",j,sep=""),".jpg",sep=""))
    ##Removing noise
    #Obtaining dimension
    imgDm<- dim(Image)
    # Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(Image[,,1]),
      G = as.vector(Image[,,2]),
      B = as.vector(Image[,,3])
    )
    
    kClusters <-k
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
    kColours <- rgb(kMeans$centers[kMeans$cluster,])
    
    kcolors = data.frame(
      label = 1:nrow(kMeans$centers), 
      R = kMeans$centers[,1],
      G = kMeans$centers[,2],
      B = kMeans$centers[,3]
    )
    #finding avgRGB
    avgR=sum(kcolors$R)/kClusters
    avgG=sum(kcolors$G)/kClusters
    avgB=sum(kcolors$B)/kClusters
    
    
    vector1=c()
    vector2=c()
    vector3=c()
    vector4=c()
    vector5=c()
    
    for (i in 1:20)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector1[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a1=min(vector1)
    
    for (i in 1:26)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector2[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a2=min(vector2)
    
    for (i in 1:13)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector3[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a3=min(vector3)
    
    for (i in 1:23)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector4[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a4=min(vector4)
    
    for (i in 1:24)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector5[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a5=min(vector5)
    
    min_distance=min(a1,a2,a3,a4,a5)
    
    if(min_distance == a1){
      
      correct <- correct + 1
      
    } else incorrect = incorrect + 1
    
    j=j+1
    
  }
  
  j=1
  while (j <= 26)
  {
    Image <- readImage(paste(paste("C:\\xampp\\htdocs\\minor\\Minor_dataset\\Anthracnose\\",j,sep=""),".jpg",sep=""))
    ##Removing noise
    #Obtaining dimension
    imgDm<- dim(Image)
    # Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(Image[,,1]),
      G = as.vector(Image[,,2]),
      B = as.vector(Image[,,3])
    )
    
    kClusters <-k
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
    kColours <- rgb(kMeans$centers[kMeans$cluster,])
    
    kcolors = data.frame(
      label = 1:nrow(kMeans$centers), 
      R = kMeans$centers[,1],
      G = kMeans$centers[,2],
      B = kMeans$centers[,3]
    )
    #finding avgRGB
    avgR=sum(kcolors$R)/kClusters
    avgG=sum(kcolors$G)/kClusters
    avgB=sum(kcolors$B)/kClusters
    
    
    vector1=c()
    vector2=c()
    vector3=c()
    vector4=c()
    vector5=c()
    
    for (i in 1:20)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector1[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a1=min(vector1)
    
    for (i in 1:26)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector2[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a2=min(vector2)
    
    for (i in 1:13)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector3[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a3=min(vector3)
    
    for (i in 1:23)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector4[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a4=min(vector4)
    
    for (i in 1:24)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector5[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a5=min(vector5)
    
    min_distance=min(a1,a2,a3,a4,a5)
    
    if(min_distance == a2){
      
      correct <- correct + 1
      
    } else incorrect = incorrect + 1
    
    j=j+1
    
  }
  
  j=1
  while (j <= 13)
  {
    Image <- readImage(paste(paste("C:\\xampp\\htdocs\\minor\\Minor_dataset\\Bacterial Blight\\",j,sep=""),".jpg",sep=""))
    ##Removing noise
    #Obtaining dimension
    imgDm<- dim(Image)
    # Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(Image[,,1]),
      G = as.vector(Image[,,2]),
      B = as.vector(Image[,,3])
    )
    
    kClusters <-k
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
    kColours <- rgb(kMeans$centers[kMeans$cluster,])
    
    kcolors = data.frame(
      label = 1:nrow(kMeans$centers), 
      R = kMeans$centers[,1],
      G = kMeans$centers[,2],
      B = kMeans$centers[,3]
    )
    #finding avgRGB
    avgR=sum(kcolors$R)/kClusters
    avgG=sum(kcolors$G)/kClusters
    avgB=sum(kcolors$B)/kClusters
    
    
    vector1=c()
    vector2=c()
    vector3=c()
    vector4=c()
    vector5=c()
    
    for (i in 1:20)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector1[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a1=min(vector1)
    
    for (i in 1:26)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector2[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a2=min(vector2)
    
    for (i in 1:13)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector3[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a3=min(vector3)
    
    for (i in 1:23)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector4[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a4=min(vector4)
    
    for (i in 1:24)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector5[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a5=min(vector5)
    
    min_distance=min(a1,a2,a3,a4,a5)
    
    if(min_distance == a3){
      
      correct <- correct + 1
      
    } else incorrect = incorrect + 1
    
    j=j+1
    
  }
  
  j=1
  while (j <= 23)
  {
    Image <- readImage(paste(paste("C:\\xampp\\htdocs\\minor\\Minor_dataset\\Cercospora Leaf Spot\\",j,sep=""),".jpg",sep=""))
    ##Removing noise
    #Obtaining dimension
    imgDm<- dim(Image)
    # Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(Image[,,1]),
      G = as.vector(Image[,,2]),
      B = as.vector(Image[,,3])
    )
    
    kClusters <-k
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
    kColours <- rgb(kMeans$centers[kMeans$cluster,])
    
    kcolors = data.frame(
      label = 1:nrow(kMeans$centers), 
      R = kMeans$centers[,1],
      G = kMeans$centers[,2],
      B = kMeans$centers[,3]
    )
    #finding avgRGB
    avgR=sum(kcolors$R)/kClusters
    avgG=sum(kcolors$G)/kClusters
    avgB=sum(kcolors$B)/kClusters
    
    
    vector1=c()
    vector2=c()
    vector3=c()
    vector4=c()
    vector5=c()
    
    for (i in 1:20)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector1[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a1=min(vector1)
    
    for (i in 1:26)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector2[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a2=min(vector2)
    
    for (i in 1:13)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector3[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a3=min(vector3)
    
    for (i in 1:23)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector4[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a4=min(vector4)
    
    for (i in 1:24)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector5[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a5=min(vector5)
    
    min_distance=min(a1,a2,a3,a4,a5)
    
    if(min_distance == a4){
      
      correct <- correct + 1
      
    } else incorrect = incorrect + 1
    
    j=j+1
    
  }
  
  j=1
  while (j <= 24)
  {
    Image <- readImage(paste(paste("C:\\xampp\\htdocs\\minor\\Minor_dataset\\Healthy Leaves\\",j,sep=""),".jpg",sep=""))
    ##Removing noise
    #Obtaining dimension
    imgDm<- dim(Image)
    # Assign RGB channels to data frame
    imgRGB <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(Image[,,1]),
      G = as.vector(Image[,,2]),
      B = as.vector(Image[,,3])
    )
    
    kClusters <-k
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
    kColours <- rgb(kMeans$centers[kMeans$cluster,])
    
    kcolors = data.frame(
      label = 1:nrow(kMeans$centers), 
      R = kMeans$centers[,1],
      G = kMeans$centers[,2],
      B = kMeans$centers[,3]
    )
    #finding avgRGB
    avgR=sum(kcolors$R)/kClusters
    avgG=sum(kcolors$G)/kClusters
    avgB=sum(kcolors$B)/kClusters
    
    
    vector1=c()
    vector2=c()
    vector3=c()
    vector4=c()
    vector5=c()
    
    for (i in 1:20)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from alternaria_alternata where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector1[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a1=min(vector1)
    
    for (i in 1:26)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from anthracnose where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector2[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a2=min(vector2)
    
    for (i in 1:13)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from bacterial_blight where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector3[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a3=min(vector3)
    
    for (i in 1:23)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from cercospora_leaf_spot where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector4[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a4=min(vector4)
    
    for (i in 1:24)
    {
      avgR_SQL <- dbGetQuery(con,(paste(paste("select avgR from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgG_SQL <- dbGetQuery(con,(paste(paste("select avgG from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      avgB_SQL <- dbGetQuery(con,(paste(paste("select avgB from healthy_leaves where image_name = '",i,sep=""),".jpg'",sep=""))) 
      vector5[i]=sqrt(((avgR-avgR_SQL)^2)+((avgG-avgB_SQL)^2)+((avgB-avgB_SQL)^2))
    }
    a5=min(vector5)
    
    min_distance=min(a1,a2,a3,a4,a5)
    
    if(min_distance == a5){
      
      correct <- correct + 1
      
    } else incorrect = incorrect + 1
    
    j=j+1
    
  }
  
  percentage = (correct / incorrect) * 100
  
  percentage <- round(percentage,4)
  
  print(paste(paste("Accuracy is:",percentage),"%",sep=""))
  
  return(percentage)
  
}
 
# accuracy <- function(){
#   
#    percentage_correctness <- c()
#    
#    accuracy_value <- c()
#  
#    for(k in kvalues){
#      
#      for(count in 1:1){
#        
#        measure <- measure_accuracy(k)
#        
#        accuracy_value <- c(accuracy_value,measure)
#        
#      }
#      
#      avg_accuracy_value <- sum(accuracy_value)/count
#        
#      percentage_correctness <- c(percentage_correctness,avg_accuracy_value)
#      
#    }
#    
#    print(percentage_correctness)
#    
# }
# 
# accuracy()

accuracy <- function(){
  
  for(k in kvalues){
  
      avg_accuracy_value <- measure_accuracy(k)
    
      percentage_correctness <- c(percentage_correctness,avg_accuracy_value)
  
  }
  
  print(percentage_correctness)
  
  #plot(kvalues,percentage_correctness,type = "o");
  
}

accuracy()

#  percentagek5 = sum(c(51.4286, 51.6238, 29.2683, 47.222, 32.5))/4
# # 
#  percentagek9 = sum(c(39.8974, 41.3333, 35.8974, 39.4737, 47.2222))/4
# # 
#  percentagek11 = sum(c(58.209, 37.6622, 49.2958, 35.8974, 45.2055))/4
# # 
#  percentagek13 = sum(c(43.2432, 30.8644, 34.1772, 37.6623, 49.2958))/4
# # 
#  percentage_correctness <- c(percentagek5,percentagek9,percentagek11,percentagek13)
# # 
#  plot(kvalues,percentage_correctness,type = "o")
# # 
#  title("Plot for accuracy","Generating correctness",col.main = 490,col.sub = 300, col.lab = 250, col.axis = 200)

  percentagek5 = sum(c(37.6623, 32.5, 27.7108, 32.5, 27.7108))/4
# # 
  percentagek7 = sum(c(30.8642, 35.8974, 35.8974, 39.4737, 35.8974))/4
 # 
  percentagek9 = sum(c(45.2055, 47.2222, 39.4737, 34.1772, 53.6232))/4
# # 
  percentagek11 = sum(c(43.2432, 53.6232, 47.2222, 45.2055, 43.2432))/4
  
  percentagek13 = sum(c(32.5, 43.2432, 45.2055, 34.1772, 47.2222))/4
  
  percentagek15 = sum(c(43.2432, 39.4737, 35.8974, 45.2055, 39.4737))/4
  
  percentagek17 = sum(c(37.6623, 34.1772, 34.1772, 47.2222, 45.2055))/4
# # 
  percentage_correctness <- c(percentagek5,percentagek7,percentagek9,percentagek11,percentagek13,percentagek15,percentagek17)
# # 
  plot(kvalues,percentage_correctness,type = "o")
# # 
  title("Plot for accuracy","Generating correctness",col.main = 490,col.sub = 300, col.lab = 250, col.axis = 200)

