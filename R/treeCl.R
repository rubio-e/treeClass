#' treeCl function
#'
#' This function uses several functions to build a tree based on data
#'
#' @param formula Formula to define the dependent and independent varibles
#'
#' @param data data base
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

  loopColsE = function(node, formula, data, counter = 0) {

    node$N <- nrow(data)

    node$cuenta = counter + 1

    cuenta = counter + 1

    datax = model.frame(formula, data = data)

    datax = data.frame(datax[,c(2:ncol(datax),1)])

    dataz = bestSplit(datax)

    dataa = dataz[[1]]

    node$nElements <- nrow(dataa)

    node$Elements <- paste("(",
                           names(table(dataa[,ncol(dataa)])[1]),
                           ": ", format(table(dataa[,ncol(dataa)])[1]),"; ",
                           names(table(dataa[,ncol(dataa)])[2]),
                           ": ", format(table(dataa[,ncol(dataa)])[2]),")", sep = ""
    )

    node$ElementsProp <- paste("(",
                               names(table(dataa[,ncol(dataa)])[1]),
                               ": ", format(table(dataa[,ncol(dataa)])[1]/sum(table(dataa[,ncol(dataa)])),digits = 2),"; ",
                               names(table(dataa[,ncol(dataa)])[2]),
                               ": ", format(table(dataa[,ncol(dataa)])[2]/sum(table(dataa[,ncol(dataa)])),digits = 2),")", sep = ""
    )

    x = ncol(dataa)

    if (purity(dataa[, x]) == 'pure') {

      hijo <- node$AddChild(unique(dataa[, ncol(dataa)]))

      hijo$nElements <- nrow(dataa)

      tabla = table(dataa[,ncol(dataa)])

      node$Elements <- paste("(", names((tabla)[1]),": ", format((tabla)[1]),"; ",
                             names((tabla)[2]),": ", format((tabla)[2]),")", sep = ""
      )

      node$ElementsProp <- paste("(", names((tabla)[1]), ": ", format((tabla)[1]/sum(tabla),digits = 2),"; ",
                                 names((tabla)[2]),": ", format((tabla)[2]/sum(tabla),digits = 2),")", sep = "")
      hijo$VarClass <- ''

      cuent = cuenta

    } else {
      tabla = table(dataa[,ncol(dataa)])

      nData = split(dataa, dataa[,c("splitF")], drop = T)

      for (i in 1:length(nData)) {

        if(length(nData)<1){
          hijo = node$AddChild(names(nData)[i])
          hijo = node$AddChild(names(which.max(tabla)))
          hijo$nElements <- nrow(nData[[i]])
          break
        }

        if(nrow(nData[[i]])<=3){
          hijo = node$AddChild(names(nData)[i])
          hijo = node$AddChild(names(which.max(tabla)))
          hijo$nElements <- nrow(nData[[i]])
          break
        }

        if((tabla)[1]/sum(tabla)>0.80||(tabla)[2]/sum(tabla)>0.80){
          hijo = node$AddChild(names(which.max(tabla)))
          hijo$nElements <- nrow(nData[[i]])
          break
        }


        if(cuenta == depth){
          hijo = node$AddChild(names(which.max(tabla)))
          hijo$nElements <- nrow(nData[[i]])
          # hijo <- node$AddChild(unique(dataa[, ncol(dataa)]))
          break
        }

        hijo = node$AddChild(names(nData)[i])

        cuent = cuenta

        Recall(hijo, formula, nData[[i]],cuent)

      }
    }
  }

  tree <- Node$new("Root Node")

  loopColsE(tree, formula, data)

  print(tree,"nElements")

  return(tree)

}
