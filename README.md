# Basic ML dashboard with Rshiny
In this dashboard, I implement several simple machine learning models (Linear Regression, Logistic Regression, Poisson Regression, 
Kmeans, Kmeans++, KNN) as well as a data visualization platform. 
I consider these ML models because they do not need to tune too 
many hyper-parameters.  

The url of online basic machine learning platform is listed below:  
<center>https://anakinchen.shinyapps.io/BasicML</center>

Please put all files in same path and run 'main.R'.  
The 'ui.R' is user interface, 'server.R' defines server logic 
while 'diy.R' and 'kmeans++.R' give some functions that cannot be found
in R and R packages (from my view this is true at least).  
I provide some data to test this online platform. 
The 'pimatr(e).csv' can be used for classification, 
Bostontr(e).csv' is for regression task 
while 'iris.csv' is for clustering and 'poisson.csv' is for 
GLM with poisson response only.



 
