#TEST

# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")
# library(EBImage)
# 
#  lapply(dbListConnections(MySQL()),dbDisconnect)
# #
library(ggplot2)
library(RMySQL)
library(htmlTable)
library(magrittr)
library(R2HTML)
library(yaml)

  con=dbConnect(MySQL(), user='root', password='', dbname='nic', host='localhost')

Image <- readImage("C:\\xampp\\htdocs\\nic\\wamp\\2.jpg")
display(Image)


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


##Plotting
#library(ggplot2)
# ggplot theme to be used

 plotTheme <- function() {
   theme(
     panel.background = element_rect(
       size = 3,
       colour = "black",
       fill = "white"),
     axis.ticks = element_line(
     size = 2),
     panel.grid.major = element_line(
       colour = "black",
       linetype = "dotted"),
     panel.grid.minor = element_line(
       colour = "red",
       linetype = "dashed"),
     axis.title.x = element_text(
       size = rel(1.2),
       face = "bold"),
     axis.title.y = element_text(
       size = rel(1.2),
       face = "bold"),
     plot.title = element_text(
       size = 20,
       face = "bold",
       vjust = 1.5)
   )
 }
# Plot the image
 ggplot(data = imgRGB, aes(x = x, y = y)) +
   geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
   labs(title = "Image uploaded by user from image processing") +
   xlab("x") +
   ylab("y") +
   plotTheme()
#Applying K means clusttering to this plot
kClusters <- 11
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

 ggplot(data = imgRGB, aes(x = x, y = y)) +
   geom_point(colour = kColours) +
   labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
   xlab("x") +
   ylab("y") +
   plotTheme()

##Feature Extraction
#Feature extraction will take place on the basis of the colors of the cluster 
#I have to find the average value of the rgb that is avg of all the red values,
#green values and blue values will form average rgb value
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

if ( min_distance == a1) {
  
  matrix(nrow = 5,
         ncol=1,
         dimnames = list(c("<strong>Causing Agent</strong>","<strong>Prevention</strong>","<strong>Disease Causing Agent</strong>","","<strong>Link</strong>")),
         c("Alternaria Alternata", 
           "Alternaria is also called black rot and causes damage to the fruit 
           in the form of wounds and rot on the interior of the fruit.
           It occurs after heavy rains just when fruit is beginning to form.
           Use a copper fungicide to this fungal diseaseand promote good 
           circulation by pruning in the dormant season to open the canopy",
           "<img src='https://upload.wikimedia.org/wikipedia/commons/3/38/Chain_of_conidia_of_a_Alternaria_sp._fungus_PHIL_3963_lores.jpg' style='width:30%' />",
           "<figcaption>Figure 1. - Alternaria </figcaption>",
           "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=alternaria+alternata'>For more information about Alternaria Alternata Disease</a>")) %>%
    htmlTable
  
} else if ( min_distance == a2) {
  matrix(nrow = 5,
         ncol=1,
         dimnames = list(c("<strong>Causing Agent</strong>","<strong>Prevention</strong>","<strong>Disease Causing Agent</strong>","","<strong>Link</strong>")),
         c("Anthracnose", 
           "Carbendazim/ Thiophanate methyl at 0.25ml/lit sprays at
           fort-nightly intervals have been found effective.Spraying of Difenconazole 
           25 EC at 1.0 ml/lit or Prochloraz 45 EC at 0.75ml/lit were effective against
           anthracnose disease.","<img src='https://upload.wikimedia.org/wikipedia/commons/d/db/Budding.png' style='width:30%' />",
           "<figcaption>Figure 2. - Ascomycota </figcaption>",
           "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=anthracnose'>For more information about Anthracnose Disease</a>")) %>%
    htmlTable
  
} else if ( min_distance == a3) {
  matrix(nrow = 5,
         ncol=1,
         dimnames = list(c("<strong>Causing Agent</strong>","<strong>Prevention</strong>","<strong>Disease Causing Agent</strong>","","<strong>Link</strong>")),
         c("Bacterial Blight", 
           "Wide row spacing.Selection of disease free seedlings for fresh planting.
           Give minimum four month rest after harvesting the fruits",
           "<img src='https://www.popsci.com/sites/popsci.com/files/styles/1000_1x_/public/feature-defenses-stoma.jpg?itok=hDAXNWa7&fc=50,50' style='width:30%' />",
           "<figcaption>Figure 3. - Pseudomonas Syringae </figcaption>",
           "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=bacterial+blight'>For more information about Bacterial Blight disease</a>")) %>%
    htmlTable
} else if ( min_distance == a4) {
  matrix(nrow = 5,
         ncol=1,
         dimnames = list(c("<strong>Causing Agent</strong>","<strong>Prevention</strong>","<strong>Disease Causing Agent</strong>","","<strong>Link</strong>")),
         c("Cercospora Leaf Spot", 
           "The diseased fruits should be collected and destroyed.
           Pruning and destruction of diseased twigs",
           "<img src='https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQP6hZQ6Rwxn8qx9G9ZvLsFksKzSF0ascmGjZyjIMzDHCHUE2Hw-g' style='width:30%' />",
           "<figcaption>Figure 4. - Cercospora </figcaption>",
           "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=cercospora'>For more information about Cercospora disease</a>")) %>%
    htmlTable
} else if ( min_distance == a5) {
  matrix(nrow = 2,
         ncol=1,
         dimnames = list(c("<strong>Disease Detected</strong>","<strong>Prevention</strong>")),
         c("Healthy leaves", 
           "This leaf is perfect!!!!")) %>%
    htmlTable
} else print ("You have inputed a wrong image")

#of this image and store it in a database
#then apply same thing for every image of the dataset(150 images) and put it in the database

##Classification and disease detection
#The input image is calculated upto the feature extraction and averaging part and then the 
#shortest distance between the avg values present in the database is compared with that of
#the uploaded image and thus diesease can be detected by checking its column title. 