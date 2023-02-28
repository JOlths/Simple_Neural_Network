
#load packages
list.of.packages <- c('ggplot2', 'caret')
lapply(list.of.packages, require, character.only=T)

##define spiral dataset with 4 classes and 200 examples each ####
N <- 100 #number of points per class
D <- 2 #number of dimensions
K <- 4 #number of classes
X <- data.frame() #data matrix
y <- data.frame() #class labels

set.seed(308) #set random seed for testing purposes

##creating dataset ####
for (j in (1:K)){
  r <- seq(0.05, 1, length.out=N) #radius
  t <- seq((j-1)*4.7,j*4.7, length.out=N) + rnorm(N,sd=0.3) #theta ??
  Xtemp <- data.frame(x = r*sin(t), y= r*cos(t))
  ytemp <- data.frame(matrix(j,N,1))
  X <- rbind(X, Xtemp)
  y <- rbind(y, ytemp)
}

data <- cbind(X, y)
colnames(data) <- c(colnames(X), 'label')
data

ggplot() +
  geom_point(data = data, aes(x=x, y=y, color=as.character(label)), size=2) +
  scale_color_discrete(name = 'Label') + coord_fixed(ratio = 0.6) +
  theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_blank(),legend.position = 'none')


# convert X in matrix for later manipulation
X <- as.matrix(X)

# for the labels in Y a new matrix (400 by 4) is created so that for each example (each row in Y) the entry with index == label is equal to 1, otherwise 0
Y <- matrix(0, N*K, K)
for (i in 1:(N*K)){
  Y[i, y[i,]] <- 1
}
## creating neural network ####
# two-layered neural network, using ReLU as activation function and as loss function softmax
# %*% dot product, * element wise product

neuralnetwork <- function(X, Y, step_size = 0.5, reg = 0.001, h = 10, niteration){
  #get dimensions of the input
  N <- nrow(X) #number of examples
  K <- ncol(Y) #number of classes
  D <- ncol(X) #dimensionality of examples
  
  #initialize parameters randomly
  W <- 0.01 * matrix(rnorm(D*h), nrow = D)
  b <- matrix(0, nrow = 1, ncol = h)
  W2 <- 0.01 * matrix(rnorm(h*K), nrow = h)
  b <- matrix(0, nrow = 1, ncol = K)
  
}