
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

#relu activation function
Activation_ReLU <- function(inputs){
  return(pmax(inputs,0))
}

#softmax activation function
Activation_Softmax <- function(inputs){
  exp_values <- exp(matrix(mapply("-", inputs, apply(inputs, 1, max)), ncol = ncol(inputs)))
  probabilities <- matrix(mapply("/", exp_values, rowSums(exp_values)), ncol = ncol(exp_values)) 
  return(probabilities)
}

#define class or dense layer
setClass("DenseLayer", slots = c(n_inputs = 'numeric', n_neurons = 'numeric'))

#set init (constructor)
setGeneric('init', function(layer) standardGeneric('init'))

#set method for init
setMethod('init', 'DenseLayer',
          function(layer) {
            n_weights <- layer@n_inputs * layer@n_neurons
            weights <- matrix(rnorm(n_weights),
                              nrow = layer@n_inputs,
                              ncol = layer@n_neurons
            )
            attr(layer, 'weights') <- 0.10 * weights
            attr(layer, 'biases') <- rep(0, layer@n_neurons)
            layer
          })

#set method for forward function
setGeneric('forward', function(layer, inputs) standardGeneric('forward'))
setMethod('forward', 'DenseLayer',
          function(layer, inputs){
            attr(layer, 'outputs') <- inputs %*% layer@weights + layer@biases
            layer
          })

#create wrapper for initializing layer object
LayerDense <- function(n_inputs, n_neurons){
  init(new('DenseLayer', n_inputs=n_inputs, n_neurons=n_neurons))
}

#make 'prediction' (no feedback yet, just testing softmax function)
#create first layer
#input is 2, since we have 2 variables (x,y)
layer1 <- LayerDense(n_inputs = 2, n_neurons = 3)
layer1 <- forward(layer1, as.matrix(X))
layer1@outputs

#input is 3, since output of layer 1 is 3 neurons. n_neurons is 3 since we have three classes.
#ReLU activation is initialized here
layer2 <- LayerDense(n_inputs = 3, n_neurons = 3)
layer2 <- forward(layer2, Activation_ReLU(layer1@outputs))

#view first 5 output with softmax activation
head(Activation_Softmax(layer2@outputs))