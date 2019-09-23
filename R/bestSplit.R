#' bestSplit function
#'
#' This function makes splits among columns, computes the Gini index for a specific split and selects the column with the lowest GINI Index.
#'
#' @param z database, where the last column is the dependent variable
#'
#' @return list with a data.frame called base, the best (i.e. lowest) Gini Index, the value at which the split was made and the variable corresponding to the best split
#'
#' @examples
#' bestSplit(iris)
#'
#' @export
bestSplit <- function(z){
  
  # This function depends on 'sspf' to select the best split of the data.base

  y <- z[,ncol(z)] # Select only the column with the dependent variable

  z1 <- z[,-ncol(z)] # Select all the columns except the dependent variable
  
  # The next "if - else" condition is to loop over the columns only when the data.frame has more than two columns
  # Otherwise a loop is not necessary
  if(ncol(z) > 2){
    
    # The next step is to create a data.frame and fill it with the sppf function
    bbs <- data.frame()

    for (i in 1:length(z1)) {
      bbs[1,i] <- sspf(y,z1[,i])[2] # only extract the second value of the sspf result that equals to the Gini value
    }

    colnames(bbs) <- names(z1) # new dataframe gets same names as the original database

    bbs <- bbs[,order(bbs[1,])] # sorting the data.frame by the lowest Gini value

    name <- as.character(colnames(bbs)[1]) # Making a vector with the name of the lowest Gini value column 

    bbsa <- sspf(y,z1[,name])[1] # applying the sspf function to the column to get the split value for the specific column

    yz <- data.frame(cbind(y,z1)) # structuring the data.frame with the dependent variable as the first column

    yz <- na.omit(yz) # To ensure consistent results, this line could be modified later to treat NAs differently
    
    # The next 'if else' condition is to determine if numeric or not
    if(is.numeric(bbsa[1,1])){
      
      # This 'if else' procedure creates a new column called 'splitF' that will paste the name of the best split column,
      # then the condition is '< or >=' and the split value
      # This will later serve to make the split in the same way for both numeric and vector variables
      yz$splitF <- ifelse(yz[,name] >= bbsa[1,1],
                         paste(name,">=",bbsa[1,1]),
                         paste(name,"<",bbsa[1,1]))

      colnames(yz)[1] <- colnames(z)[ncol(z)]

      yz <- yz[,c(2:ncol(yz), 1)] # Re-ordering the database
      
      # This makes the list with:
      # The data.frame including the new column
      # The Gini value
      # The value at which the split was made
      # The name of the best split column
      yz <- list(base = yz, G = bbs[1,1], splitVal = bbsa[1,1], variable = name)

    }else{
      # If the best split variable is not numerical, it will repeat the proccess using the factors
      # If it is numerical, it will add the new column to the data.frame by using the factors way
      allFactors <- unique(yz[,name])

      yz$splitF <- ifelse(yz[,name] == bbsa[1,1],
                         as.character(bbsa[1,1]),
                         paste(allFactors[allFactors!=bbsa[1,1]],collapse = ","))

      colnames(yz)[1] <- colnames(z)[ncol(z)]

      yz <- yz[,c(2:ncol(yz), 1)]

      yz <- list(base = yz,G = bbs[1,1],splitVal = bbsa[1,1], variable = name)

    }


  }else{
    # This is the condition when the data frame has only one independent variable
    # It repeats the proccess above, but without the 'for loop'

    splitOne <- sspf(y,z1)

    if(is.numeric(splitOne[1,1]) == F){

      allFactors <- unique(z1)

      yz <- z

      yz$splitF <- ifelse(z[,1]==splitOne[1,1],
                         as.character(splitOne[1,1]),
                         paste(allFactors[allFactors!=splitOne[1,1]],collapse = ","))

      yz <- as.data.frame(cbind(yz[,-c(ncol(yz)-1)],z[,ncol(z)]))
      colnames(yz)[ncol(yz)]  <- colnames(z)[2]

      yz <- list(base = yz,G = splitOne[1,2],splitVal = splitOne[1,1], variable = colnames(z)[1])


    }else{

      yz <- z

      yz$splitF <- ifelse(z[,1]>=splitOne[1,1],
                         paste(colnames(z)[1],">=",splitOne[1,1]),
                         paste(colnames(z)[1],"<",splitOne[1,1]))

      yz <- as.data.frame(cbind(yz[,-c(ncol(yz)-1)],z[,ncol(z)]))
      colnames(yz)[ncol(yz)] <- colnames(z)[2]

      yz <- list(base = yz,G = splitOne[1,2],splitVal = splitOne[1,1], colnames(z)[1])


    }

  }

  return(yz)

}
