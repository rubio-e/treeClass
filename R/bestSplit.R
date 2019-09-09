#' bestSplit function
#'
#' This function makes splits among columns, computes the GINI index a espefic split and select the lowest.
#'
#' @param z data base where the last column is the dependent variable
#'
#' @return list
#'
#' @examples
#' bestSplits(iris)
#'
#' @export
bestSplits<-function(z){

  y = z[,ncol(z)]

  z1 = z[,-ncol(z)]

  if(ncol(z)>2){

    bbs = data.frame()

    for (i in 1:length(z1)) {
      bbs[1,i] = sspf(y,z1[,i])[2]
    }

    colnames(bbs) = names(z1)

    bbs = bbs[,order(bbs[1,])]

    name = as.character(colnames(bbs)[1])

    bbsa = sspf(y,z1[,name])[1]

    yz = data.frame(cbind(y,z1))

    yz = na.omit(yz)

    if(is.numeric(bbsa[1,1])){

      yz$splitF = ifelse(yz[,name] >= bbsa[1,1],
                         paste(name,">=",bbsa[1,1]),
                         paste(name,"<",bbsa[1,1]))

      colnames(yz)[1] = colnames(z)[ncol(z)]

      yz = yz[,c(2:ncol(yz), 1)]

      yz = list(base = yz,G = bbs[1,1],splitVal = bbsa[1,1])


    }else{
      allFactors = unique(yz[,name])

      yz$splitF = ifelse(yz[,name] == bbsa[1,1],
                         as.character(bbsa[1,1]),
                         paste(allFactors[allFactors!=bbsa[1,1]],collapse = ","))

      colnames(yz)[1] = colnames(z)[ncol(z)]

      yz = yz[,c(2:ncol(yz), 1)]

      yz = list(base = yz,G = bbs[1,1],splitVal = bbsa[1,1])

    }


  }else{

    splitOne = sspf(y,z1)

    if(is.numeric(splitOne[1,1])==F){

      allFactors = unique(z1)

      yz = z

      yz$splitF = ifelse(z[,1]==splitOne[1,1],
                         as.character(splitOne[1,1]),
                         paste(allFactors[allFactors!=splitOne[1,1]],collapse = ","))

      yz = as.data.frame(cbind(yz[,-c(ncol(yz)-1)],z[,ncol(z)]))
      colnames(yz)[ncol(yz)] = colnames(z)[2]

      yz = list(base = yz,G = splitOne[1,2],splitVal = splitOne[1,1])


    }else{

      yz = z

      yz$splitF = ifelse(z[,1]>=splitOne[1,1],
                         paste(colnames(z)[1],">=",splitOne[1,1]),
                         paste(colnames(z)[1],"<",splitOne[1,1]))

      yz = as.data.frame(cbind(yz[,-c(ncol(yz)-1)],z[,ncol(z)]))
      colnames(yz)[ncol(yz)] = colnames(z)[2]

      yz = list(base = yz,G = splitOne[1,2],splitVal = splitOne[1,1])


    }

  }

  return(yz)

}
