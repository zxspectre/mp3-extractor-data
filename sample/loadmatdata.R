library("stringr")
library("R.matlab")
library("neuralnet")

createneuralnet <- function(){
  #use registerDoMC 
  nn.data<-loadmatdata()  
  nn.data.vars<-paste(names(nn.data)[1:ncol(nn.data)-1],"",collapse="+")
  nn.expr=paste(names(nn.data)[ncol(nn.data)],"~",nn.data.vars)
  #print(summary(nn.data$label))
  print(dim(nn.data))
  weights<-readMat("100-10-weights2.mat")
  errfct<-function(x,y,z){
    1/2 * (y - x)^2 + z
  }
  net<-neuralnet(nn.expr,nn.data,hidden=c(100,10),rep=1,err.fct=errfct, linear.output=FALSE
                 ,stepmax = 1e+05,threshold=0.001,lifesign = "full",lifesign.step=1
                 ,learningrate.factor = list(minus = 0.5, plus = 1.2),startweights=weights)
  #net<-neuralnet(nn.expr,nn.data,hidden=0,rep=1,err.fct="sse", linear.output=TRUE)
}

loadmatdata <- function(){
  ##TODO: pass list of vectors (1st = fileprefix, 2nd = label number)
  ## 0 is for HEAVY
  matfiles<-list.files(path = ".", pattern = "^heavy.*\\.mat", all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata1<-lapply(matfiles,processmatfile)
  alldata1<-do.call(rbind, alldata1)
  label<-rep(0,dim(alldata1)[1])
  nn.data1<-data.frame(alldata1,label)

  #1 is for POWER
  matfiles<-list.files(path = ".", pattern = "^power.*\\.mat", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata2<-lapply(matfiles,processmatfile)
  alldata2<-do.call(rbind, alldata2)
  label<-rep(1,dim(alldata2)[1])
  nn.data2<-data.frame(alldata2,label)
  
  alldata<-rbind(nn.data1,nn.data2)
  return(alldata)
}

processmatfile <- function(filename){
  varname<-str_match(filename,"(.*?)\\.mat")[2]
  datam<-readMat(filename)
  varsize<-dim(datam[[varname]])
  #print(filename)
  output<-matrix(datam[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  #names(res)<-varname
  #return(res)
  return(res[floor(dim(res)[1]*0.3):floor(dim(res)[1]*0.6),])
}

processtestfile <- function(filename){
  varname<-str_match(filename,"(.*?)\\.mat")[2]
  datam<-readMat(filename)
  varsize<-dim(datam[[varname]])
  output<-matrix(datam[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  return(res[floor(dim(res)[1]*0.49):floor(dim(res)[1]*0.51),])
}

#mean(compute(nnet,processtestfile("heavy6.mat"))$net.result)
#writeMat("100-10-weights.mat",weights=nnet$weights)

