#' sspf function
#'
#' This function splits the independent variable and computes the weighted GINI index by selecting the lowest.
#'
#' @param y Factor vector
#'
#' @param x vector: it can be numeric, character or factor
#'
#' @return data.frame
#'
#' @examples
#' sspf(iris$Species,iris$Sepal.Width)
#'
#' @export
sspf <- function(y,x){
  # make a data.frame with both variables
  data <- data.frame(y = (y), x = (x))
  
  # To ensure the same results NA values are omited in the data.frame
  data <- na.omit(data)
  
  # After the database is clean is sorted by the independent variable
  data <- data[order(data$x),]
  
  # Then the next is to decide if the predictor is numeric or not
  if(is.numeric(x) == F ){
    
    # if the data is not numeric it will select the unique elements of the factor vector
    x <- unique(data$x)
    
    # The next is a for loop to make the binary splits
    # it will loop over the length of the unique vectors
    giniG <- data.frame()

    for (i in 1:length(x)) {
      # this will separates the data into two: the diferent and the equal to the factor
      left <- data[data[,2] ==  x[i],] 
      right <- data[data[,2] !=  x[i],]
      giniG[i,1] <- if(length(x)<3){ # this assigns the value of 1 if there is less than tree factors to avoid overfitting 
        1
      }else{
        # here is computed the global gini index from the value of the right and the left side
        sum((gini(left[,1])*length(left[,1])/length(data[,1])),
            (gini(right[,1])*length(right[,1])/length(data[,1])), na.rm = T)
      }

      giniG[i,2] <- x[i] # adds a countes to the data.frame
      # !Experimental: This both adds a counter and a name and for now is not showed by the print 
      # giniG[i,3] <- paste(names(table(data[,1])[1]),"=",table(data[,1])[1])
      # giniG[i,4] <- paste(names(table(data[,1])[2]),"=",table(data[,1])[2])

    }

    giniG <- giniG[order(giniG[,1]),]
    
    # This makes the data.frame with the name of the best split and the gini index
    giniG <- data.frame(bSplit = giniG[1,2], G = giniG[1,1])

    return(giniG)

  }else{
    
    # If the data is numeric then the next step is to select the unique values of the predictor variable
    x <- unique(data$x)
    
    # This is for making the numeric vector for the splits 
    # i.e. for a initial vector of 'x <- c(1,2,3)' the resulting one will be 'xx <- c(1.5,2.5)'
    xx <- 0

    for (i in 1:length(x)) {
      xx[i] <- sum(x[i], x[i+1], na.rm = F ) /2
    }

    xx <- na.omit(xx) # to ensure no NAs are in the vector
    
    # This is the loop for making the partitions based on the xx vector
    giniG <- data.frame()
    for (i in 1:length(xx)) {
      # Likewise for the factor vector, this makes the partition in left and right sides 
      # based on the numeric vector xx, but in this case the partition is based on < or >=
      left <- data[data[,2] <  xx[i],] 
      right <- data[data[,2] >=  xx[i],]
      # From here the other coputations are the same as the factor part
      giniG[i,1] <- if(length(x)<3){
        1
      }else{
        sum((gini(left[,1])*length(left[,1])/length(data[,1])),
            (gini(right[,1])*length(right[,1])/length(data[,1])), na.rm = T)
      }

      giniG[i,2] <- xx[i]
      giniG[i,3] <- paste(names(table(data[,1])[1]),"=",table(data[,1])[1]) # experimental
      giniG[i,4] <- paste(names(table(data[,1])[2]),"=",table(data[,1])[2]) # experimental
      giniG[i,5] <- length(left[,1])/length(data[,1])
      giniG[i,6] <- length(right[,1])/length(data[,1])
      giniG[i,7] <- if(length(giniG[i,5]>0)){
        if(giniG[i,5]<0.2|giniG[i,6]<0.2){
          1
        }else{
          giniG[i,1]
        }

      }else{
        giniG[i,1]
      }

    }

    giniG <- giniG[order(giniG[,7]),]

    giniG <- data.frame(bSplit = giniG[1,2], G = giniG[1,7])

    return(giniG)

  }

}
