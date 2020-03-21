##' match exp and pd in order
##'
##' make exp match with pd in order
##' @param exp  A numeric matrix export by exprs function
##' @param pd   A meta data.frame export by pData function
##' @return make exp match with pd in order
##' @examples
##' epmap(exp,pd)

epmap = function(exp,pd){
  if(!identical(rownames(pd),colnames(exp))) exp = exp[,match(rownames(pd1),colnames(exp))]
  identical(rownames(pd),colnames(exp))
}
##' count unique values in every colunms for data.frame
##'
##' in geo analysis,this function can help you simplify pdata, delete columns with unique values,which can't be used as group vector
##' @param x A data.frame.
##' @return The simple data.frame of columns unique values count in \code{x}
##' @importFrom dplyr arrange
##' @importFrom dplyr desc
##' @importFrom tibble tibble
##' @importFrom dplyr %>%
##' @export
##' @examples
##' dumd(iris)
##' data(ToothGrowth)
##' x = ToothGrowth
##' dumd(ToothGrowth)
##' @section just :
##' See what are you doing

dumd <- function(x){
  colname <- vector("character")
  count <- vector("integer")
  for(i in 1:ncol(x)){
    colname[i] = colnames(x)[[i]]
    count[i]=nrow(x[!duplicated(x[,i]),])
  }
  df <- tibble(colname,count) %>%
    arrange(desc(count))
  print(df)
}


