#' sspf function
#'
#' This function splits the independent variable and computes the weighted Gini index by selecting the lowest one
#'
#' @param y Factor vector
#'
#' @param x vector: it can be numeric, a character or a factor
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
  
  # To ensure the same results, 'NA' values are omitted in the data.frame
  data <- na.omit(data)
  
  # After the database is cleaned, it is sorted by the independent variable
  data <- data[order(data$x),]
  
  # Then the next step is to check if the predictor is numeric or not
  if(is.numeric(x) == F ){
    
    # if the data is not numerical, it will select unique elements of the factor vector
    x <- unique(data$x)
    
    # The next step is to run a for-loop to make binary splits
    # it will loop over the length of the new vector 'x'
    giniG <- data.frame()

    for (i in 1:length(x)) {
      # this separates the data in two parts: the one that is different from and the one that is equal to the subject factor 'i'
      left <- data[data[,2] ==  x[i],] 
      right <- data[data[,2] !=  x[i],]
      giniG[i,1] <- if(length(x)<3){ # this assigns the value of 1, if there are less than tree factors (to avoid overfitting) 
        1
      }else{
        # here the Gini index for left and right sides is computed
        sum((gini(left[,1])*length(left[,1])/length(data[,1])),
            (gini(right[,1])*length(right[,1])/length(data[,1])), na.rm = T)
      }

      giniG[i,2] <- x[i] # adds a counter to the data.frame
      # !Experimental: These both add a counter and a name, and currently it is not shown by the print 
      # giniG[i,3] <- paste(names(table(data[,1])[1]),"=",table(data[,1])[1])
      # giniG[i,4] <- paste(names(table(data[,1])[2]),"=",table(data[,1])[2])

    }

    giniG <- giniG[order(giniG[,1]),]
    
    # This makes the data.frame with the name of the best split and the respective Gini index
    giniG <- data.frame(bSplit = giniG[1,2], G = giniG[1,1])

    return(giniG)

  }else{
    
    # If the data is numeric, then the next step is to select unique values of the predictor variable
    x <- unique(data$x)
    
    # This is for making the numeric vector for splits 
    # e.g. for the initial vector 'x <- c(1,2,3)', the resulting one will be 'xx <- c(1.5,2.5)'
    xx <- 0

    for (i in 1:length(x)) {
      xx[i] <- sum(x[i], x[i+1], na.rm = F ) /2
    }

    xx <- na.omit(xx) # to ensure no NAs are in the vector
    
    # This is the loop for making partitions based on the xx vector
    giniG <- data.frame()
    for (i in 1:length(xx)) {
      # Likewise for the factor vector, this splits in left and right sides 
      # based on the numeric vector xx, but in this case the partition is based on < or >=
      left <- data[data[,2] <  xx[i],] 
      right <- data[data[,2] >=  xx[i],]
      # From here, other copmutations are same as those of the factor part
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
