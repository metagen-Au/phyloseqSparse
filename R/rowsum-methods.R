#' sum values of otu by row, based on a grouping variable. Extended version of the base R function. 
#' @param x an otutable
#' @param group variable to group by
#' @param reorder logical. Whether to reorder taxa by their abundance. 
#' @rdname rowsum-methods
#' @export
setMethod("rowsum",c("otu_table","ANY","logical"),function(x,group,reorder){
  xgrouped<- Matrix.utils::aggregate.Matrix(x,group)
  #rownames(xgrouped)<- group
  if(reorder){
  xgrouped<- xgrouped[,order(Matrix::colSums(xgrouped),decreasing=TRUE)]
  }
  xgrouped<- otu_table(xgrouped,taxa_are_rows(x))
  return(xgrouped)
})