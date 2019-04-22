# Prediction Model for Automated Leaf Disease Detection and Analysis

Repository for Nikita Goel, Dhruv Jain, Adwitiya Sinha ["Prediction Model for Automated Leaf Disease Detection & Analysis"](https://ieeexplore.ieee.org/document/8692116), 8th IEEE International Advance Computing  Conference (IACC), 2018


Our proposed model provides an automatic method to determine disease in a plant leaf using a trained dataset of pomegranate leaf images. 
The test set is used to check whether an image entered into the system contains disease or not. 
If not, it is considered to be healthy, otherwise the disease of that leaf is predicted and the prevention of that disease is proposed instantly. 
Further, the rodent causing disease is also identified with image analysis performed on the image certified by biologists and scientists. 
This model provides an accuracy of the results generated using different cluster sizes, optimized experimentally, with image segmentation. 
Our model provides useful estimation and prediction of disease causing agent with necessary precautions.
The basic methodology adopted for implementing the 5 main stages of the project is as follows:
1.	**Image Acquisition:**
The images are collected and compiled to a data set from a government sponsored web based portal called IPM images which was launched by University of Georgia, to help provide high definition images of living organisms which can be used for developing educational applications. These images were collected and stored in a database of files. Training and Test set were separated according to the images which are needed to be trained and which are to be tested. Then a web based application was prepared to upload an image from the test set and was loaded into the R application.

2.	**Image Enhancement:**
It is used to enhance the contrast of the images belonging to both the training and test sets. The image is now stored in the form of a matrix where each cell contains the X coordinate, Y coordinate and their corresponding R(red) , G(green), B(blue) values. This enhancement is used to obtain dimensions of the image also.

3.	**Image Segmentation:**
It subdivides the image region into small regions. In the proposed method k-means clustering algorithm has been used for the segmentation.

       I.	Input images are first converted into greyscale

       II. Applying enhancement by obtaining dimensions and removing noise

       III.	Finnaly, applying K means algorithm on the processed image

4.	**Feature Extraction:**
Feature extraction is very important and essential step to extract region of interest. In the proposed model the feature of colour was incorporated and used as the cluster points to extract a particular disease type. The average value of each image pixel was estimated by calculating their separate R G B values and finding the mean of the entire image.

5.	**Image Classification:**
The SVM classifier is used to identify the classes, which are closely connected to the known classes. The Support vector machine creates the optimal separating hyper plane between the classes using the training data.
