##### Read in the Skulls.csv dataset
skulls1 <- read.csv( file.choose(), header=TRUE )
head( skulls1 )

#### The outcome variables of interest are B BH BL NH
#### The group variable is Year.
#### We need Year to be a category
skulls1.manova <- manova( cbind( B, BH, BL, NH ) ~ as.factor( Year ),
                          data = skulls1)
summary( skulls1.manova )

#### We can use summary to get other tests as well.
summary( skulls1.manova, test="Hotelling-Lawley")

#### Look to see which ones differ
summary.aov( skulls1.manova )

#### Do pairwise comparisons 4000BC to 200BC
skulls2.manova <- manova( cbind( B, BH, BL, NH ) ~ as.factor( Year ),
                          data = skulls1,
                          subset = as.factor( Year ) %in% c("-4000","-200") )
summary( skulls2.manova )

#### Iris data for PCA
# write.csv( iris, "Iris.csv", row.names=FALSE )
iris1 <- read.csv( file.choose(), header = TRUE )
head( iris1 )

#### Plot the data in a pairs plot
  pairs(iris1[,1:4])

####  Run a PCA on the "continuous" data columns
iris1.pca <- princomp( iris1[ , 1:4 ] , cor = TRUE )
plot(iris1.pca)

#### Generate a summary
summary(iris1.pca)

#### Get the compressed data.
head( iris1.pca$scores )

####  Subset the compressed data.
head( iris1.pca$scores[ , 1:2 ] )



####  Exploratory Factor Analysis
####  Read in the data Student.csv
student1 <- read.csv( file.choose(), header = TRUE )
head(student1)

#### Obtain a pairs plot for the student data.
pairs(student1)

####  PCA to determine number of dimensions
student1.pca <- princomp( student1, cor = TRUE )
plot( student1.pca )

#### Obtain the summaries from the PCA to determine the number of components.
summary( student1.pca )

####  Run a factor analysis with 2 components.
student1.fa <- factanal( student1, factors = 2 )
student1.fa

####  Run a factor analysis with 2 components.
####  Get the scores ... the data transformed into the factors.
student2.fa <- factanal( student1, factors = 2, scores="Bartlett" )
head( student2.fa$scores)

#### Linear Discriminant Analysis
### Load the MASS library
library(MASS)

### Read in the data  Reagent1.csv
data1 <- read.csv( file.choose(), header = TRUE )
head(data1)

### Plot data1
plot( data1[ , c(2,3) ], 
      col=data1[ ,1 ] )  

### Perform the LDA on data1
data1.lda <- lda( Group ~ Reagent1 + Reagent2, 
                  data = data1)
data1.lda

### See how well it classified the data.
data1.lda.p <- predict(data1.lda, 
                       newdata=data1[,c(2,3)]
)$class

### Determine how well the model fits.
table(data1.lda.p, data1[,1])

#### Read in the dataset
k.data1 <- read.csv( file.choose(), header = TRUE )
head( k.data1 )

#### Plot the data
plot( k.data1,
      xlab=expression(x[1]),
      ylab=expression(x[2])
)


#### Do k-means clustering with three clusters
k.data1.3means <- kmeans( k.data1, centers = 3 )

#### Show the centers
k.data1.3means$centers

#### Show the clusters
k.data1.3means$cluster


#### Plot the groups
plot( k.data1[ k.data1.3means$cluster == 1, ], 
      col = "red",
      xlim=c( min( k.data1[,1] ), max( k.data1[,1] ) ),
      ylim=c( min( k.data1[,2] ), max( k.data1[,2] ) ),
      xlab=expression(x[1]),
      ylab=expression(x[2])
)

points( k.data1[ k.data1.3means$cluster == 2,  ], 
        col = "blue" )

points( k.data1[ k.data1.3means$cluster == 3,  ], 
        col = "seagreen" )

#### Plot the centers on the plot
points( k.data1.3means$centers, pch=17, col="black")

