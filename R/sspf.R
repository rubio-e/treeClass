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

  data <- data.frame(y=(y),x=(x))

  data <- na.omit(data)

  data <- data[order(data$x),]

  if(is.numeric(x) == F ){

    x <- unique(data$x)

    giniG <- data.frame()

    for (i in 1:length(x)) {
      left <- data[data[,2] ==  x[i],]
      right <- data[data[,2] !=  x[i],]
      giniG[i,1] <- if(length(x)<3){
        1
      }else{
        sum((gini(left[,1])*length(left[,1])/length(data[,1])),
            (gini(right[,1])*length(right[,1])/length(data[,1])), na.rm = T)
      }

      giniG[i,2] <- x[i]
      giniG[i,3] <- paste(names(table(data[,1])[1]),"=",table(data[,1])[1])
      giniG[i,4] <- paste(names(table(data[,1])[2]),"=",table(data[,1])[2])

    }

    giniG <- giniG[order(giniG[,1]),]

    giniG <- data.frame(bSplit = giniG[1,2], G = giniG[1,1], n1 = giniG[1,3], n2= giniG[1,4])

    return(giniG)

  }else{

    x <- unique(data$x)

    xx <- 0

    for (i in 1:length(x)) {
      xx[i] <- sum(x[i],x[i+1],na.rm = F)/2
    }

    xx <- na.omit(xx)

    giniG <- data.frame()

    for (i in 1:length(xx)) {
      left <- data[data[,2] <  xx[i],]
      right <- data[data[,2] >=  xx[i],]
      giniG[i,1] <- if(length(x)<3){
        1
      }else{
        sum((gini(left[,1])*length(left[,1])/length(data[,1])),
            (gini(right[,1])*length(right[,1])/length(data[,1])), na.rm = T)
      }

      giniG[i,2] <- xx[i]
      giniG[i,3] <- paste(names(table(data[,1])[1]),"=",table(data[,1])[1])
      giniG[i,4] <- paste(names(table(data[,1])[2]),"=",table(data[,1])[2])
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
