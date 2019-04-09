#TRAINING 

# run the following 4 lines to install EBImage library
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library(EBImage)
library(ggplot2)
library(RMySQL)
library(htmlTable)
library(magrittr)
library(R2HTML)
library(yaml)

rm(i)

lapply(dbListConnections(MySQL()),dbDisconnect)

con=dbConnect(MySQL(),user='root',password='',host='localhost',dbname='nic')

i=1
while (i < 21)
{
  Image <- readImage(paste(paste("C:\\xampp\\htdocs\\nic\\nic_dataset\\Alternaria Alternata\\",i,sep=""),".jpg",sep=""))
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
  
  kClusters <- 10
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong" ,centers = kClusters,iter.max = 10)
  kColours <- rgb(kMeans$centers[kMeans$cluster,])
  
  kcolors = data.frame(
    label = 1:nrow(kMeans$centers), 
    R = kMeans$centers[,1],
    G = kMeans$centers[,2],
    B = kMeans$centers[,3]
  )
  #finding avgRGB of each image 
  avgR=sum(kcolors$R)/kClusters
  avgG=sum(kcolors$G)/kClusters
  avgB=sum(kcolors$B)/kClusters
  
  dbSendQuery(con,(paste(paste(paste(paste(paste(paste(paste("insert into Alternaria_Alternata values('", i, sep = ""),"jpg'",sep="."),"'Alternaria Alternata'",sep=","),avgR,sep=","),avgG,sep=","),avgB,sep=","),")",sep="")))
  
  i=i+1
}

i=1
while (i <= 26)
{
  Image <- readImage(paste(paste("C:\\xampp\\htdocs\\nic\\nic_dataset\\Anthracnose\\",i,sep=""),".jpg",sep=""))
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
  kClusters <- 10
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong", centers = kClusters,iter.max = 10)
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
  
  dbSendQuery(con,(paste(paste(paste(paste(paste(paste(paste("insert into Anthracnose values('", i, sep = ""),"jpg'",sep="."),"'Anthracnose'",sep=","),avgR,sep=","),avgG,sep=","),avgB,sep=","),")",sep="")))
  
  i=i+1

}
  
i=1
while (i <= 13)
{
  Image <- readImage(paste(paste("C:\\xampp\\htdocs\\nic\\nic_dataset\\Bacterial Blight\\",i,sep=""),".jpg",sep=""))
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
  kClusters <- 10
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], algorithm = "Hartigan-Wong", centers = kClusters,iter.max = 10)
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
  
  dbSendQuery(con, (paste(paste(paste(paste(paste(paste(paste("insert into Bacterial_Blight values('", i, sep = ""),"jpg'",sep="."),"'Bacterial Blight'",sep=","),avgR,sep=","),avgG,sep=","),avgB,sep=","),")",sep="")))
  
  i=i+1

}

i=1
while (i <= 23)
{
  Image <- readImage(paste(paste("C:\\xampp\\htdocs\\nic\\nic_dataset\\Cercospora Leaf Spot\\",i,sep=""),".jpg",sep=""))
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
  kClusters <- 10
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")],  algorithm = "Hartigan-Wong", centers = kClusters,iter.max = 10)
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
  
  dbSendQuery(con, (paste(paste(paste(paste(paste(paste(paste("insert into Cercospora_Leaf_Spot values('", i, sep = ""),"jpg'",sep="."),"'Cercospora Leaf Spot'",sep=","),avgR,sep=","),avgG,sep=","),avgB,sep=","),")",sep="")))
  
  i=i+1

}

i=1
while (i <= 24)
{
  Image <- readImage(paste(paste("C:\\xampp\\htdocs\\nic\\nic_dataset\\Healthy Leaves\\",i,sep=""),".jpg",sep=""))
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
  kClusters <- 10
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")],  algorithm = "Hartigan-Wong", centers = kClusters,iter.max = 10)
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
  
  dbSendQuery(con, (paste(paste(paste(paste(paste(paste(paste("insert into Healthy_Leaves values('", i, sep = ""),"jpg'",sep="."),"'Healthy Leaves'",sep=","),avgR,sep=","),avgG,sep=","),avgB,sep=","),")",sep="")))

  i=i+1

}


##Classification and disease detection
#The input image is calculated upto the feature extraction and averaging part and then the 
#shortest distance between the avg values present in the database is compared with that of
#the uploaded image and thus diesease can be detected by checking its column title. 
