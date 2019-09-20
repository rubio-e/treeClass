#' treeCl function
#'
#' This function uses several functions to build a classification tree based on data
#'
#' @param formula Formula to define the dependent and independent varibles
#'
#' @param data database
#'
#' @param depth Numeric value to indicate the depth of the tree
#'
#' @return tree object
#'
#' @examples
#'
#' treeCl(Species~Petal.Width+Petal.Length+Sepal.Width,data = iris, depth = 3)
#'
#' myTree <- treeCl(Species~Petal.Width+Petal.Length+Sepal.Width,data = iris, depth = 3)
#'
#' plot(myTree)
#'
#' @export
treeCl <- function(formula, data, depth) {
  
  # This function is adapted to the data.tree package to show the results in a better way
  # The next function will be used as a loop to build the tree
  # This is made by recalling the function until the rules are met or until it reaches the
  # desired depth
  
  loopColsE <- function(node, formula, data, counter = 0) {
    
    # the node element belongs to the data.tree object
    
    node$cuenta <- counter + 1 # adds a counter to the loop

    cuenta <- counter + 1 # this is later used to match the tree depth 

    datax <- model.frame(formula, data = data) # this makes a model.frame based on the formula

    datax <- data.frame(datax[,c(2:ncol(datax),1)]) # this will gives the data.frame structure to match the bestSplit function

    dataz <- bestSplit(datax) # Here initiates the spliting steps by selectin the best split column

    dataa <- dataz[[1]] # This the first element of the list dataz is choosen, which corresponds to the data.frame of the list

    node$nElements <- nrow(dataa) # This will count the elements of the new data.frame and added to the tree node and output

    x <- ncol(dataa) # this is to look for number of columns which in turn is corresponds to the localization of the dependent variable

    if (purity(dataa[,x]) == 'pure') { # applies the purity function to the splitted data
      
      # if it is pure it will assing a child to the tree and make a leaf node

      hijo <- node$AddChild(unique(dataa[, ncol(dataa)])) # add the child to the tree object

      hijo$nElements <- nrow(dataa) # counts the elements

      tabla <- table(dataa[,ncol(dataa)]) #!experimental: this counts the elements of the dependent variable

    } else { # if the split is not pure then
      
      tabla <- table(dataa[,ncol(dataa)]) #!experimental: this counts the elements of the dependent variable

      nData <- split(dataa, dataa[,c("splitF")], drop = T) # split the data based on the factor column added

      for (i in 1:length(nData)) {

        if(length(nData) < 1){ # this controls the loop to crash and add the number of elements
          
          hijo <- node$AddChild(names(nData)[i]) # add the name of the node in case of leaf
          
          hijo <- node$AddChild(names(which.max(tabla))) #!experimental not used in print
          
          hijo$nElements <- nrow(nData[[i]])
          
          break
        }

        if(nrow(nData[[i]]) <= 3){ # if the row is less than 3 the loop stops
          
          hijo <- node$AddChild(names(nData)[i])
          
          hijo <- node$AddChild(names(which.max(tabla))) #!experimental not used in print
          
          hijo$nElements <- nrow(nData[[i]])
          
          break
        }
        
        # This is a conditional to deals with higly umbalanced datasets
        # if one side of the data has more than 80 % the next split with the lowest gini is choosen
        # it is almost experimental but works fine 
        if((tabla)[1]/sum(tabla) > 0.80 || (tabla)[2]/sum(tabla) > 0.80){
          
          hijo <- node$AddChild(names(which.max(tabla)))
          
          hijo$nElements <- nrow(nData[[i]])
          
          break
        }


        if(cuenta == depth){ # this breaks the loop when it reaches the tree depth required
          
          hijo <- node$AddChild(names(which.max(tabla)))
          
          hijo$nElements <- nrow(nData[[i]])
          
          break
        }

        hijo <- node$AddChild(names(nData)[i]) # add the name in case of leaf

        cuent <- cuenta # takes the loop sequence number

        Recall(hijo, formula, nData[[i]], cuent) # restart the general loop based on the split

      }
    }
  }
  
  # The next lines are used only to match the data.tree package requirements and print 
  # the output in a more informative way.

  tree <- Node$new("Root Node") # This makes an element 

  loopColsE(tree, formula, data) # This will call the funtion 

  print(tree,"nElements") # This shows the output in the data.tree format

  return(tree)

}
