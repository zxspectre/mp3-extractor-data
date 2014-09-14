library("stringr")
library("R.matlab")

loadmatdata <- function(){
  matfiles<-list.files(path = ".", pattern = ".*\\.mat", all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata<-sapply(matfiles,processmatfile)
  alldata<-do.call(rbind, alldata)
  return(alldata)
}

processmatfile <- function(filename){
  varname<-str_match(filename,"(.*?)\\.mat")[2]
  data<-readMat(filename)
  varsize<-dim(data[[varname]])
  output<-matrix(data[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  names(res)<-varname
  return(res)
}