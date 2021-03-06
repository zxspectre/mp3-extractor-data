library("stringr")
library("R.matlab")
library("nnet")

createneuralnet <- function(){
  #use registerDoMC 
  nn.data<-loadmatdata()  
  #nn.data.vars<-paste(names(nn.data)[1:ncol(nn.data)-1],"",collapse="+")
  #nn.expr=paste(names(nn.data)[ncol(nn.data)],"~",nn.data.vars)
  #print(summary(nn.data$label))
  print(dim(nn.data))
  net<-nnet(nn.data[,1:512], nn.data[,513], size = 10, rang = 0.05, decay = 0.1, maxit = 200,MaxNWts=5141)
  
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

processmatfile2 <- function(filename){
  varname<-str_match(filename,"(.*?)\\.mat")[2]
  datam<-readMat(filename)
  varsize<-dim(datam[[varname]])
  #print(filename)
  output<-matrix(datam[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  #names(res)<-varname
  #return(res)
  return(res[floor(dim(res)[1]*0.49):floor(dim(res)[1]*0.51),])
}

processtestfile <- function(filename){
  varname<-str_match(filename,"test(.*?)\\.mat")[2]
  datam<-readMat(filename)
  varsize<-dim(datam[[varname]])
  output<-matrix(datam[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  return(res[floor(dim(res)[1]*0.49):floor(dim(res)[1]*0.51),])
}

printtestres <- function(nnet3){
  print("Train res")
  print("Should be 0:")
  print(mean(predict(nnet3,processmatfile2("heavy0.mat"))))
  print(mean(predict(nnet3,processmatfile2("heavy1.mat"))))
  print(mean(predict(nnet3,processmatfile2("heavy2.mat"))))
  print("Should be 1:")
  print(mean(predict(nnet3,processmatfile2("power0.mat"))))
  print(mean(predict(nnet3,processmatfile2("power1.mat"))))
  print(mean(predict(nnet3,processmatfile2("power2.mat"))))
  print("Test res")
  print("Should be 0:")
  print(mean(predict(nnet3,processtestfile("testheavy3.mat"))))
  print(mean(predict(nnet3,processtestfile("testheavy4.mat"))))
  print(mean(predict(nnet3,processtestfile("testheavy5.mat"))))
  print(mean(predict(nnet3,processtestfile("testheavy6.mat"))))
  print("Should be 1:")
  print(mean(predict(nnet3,processtestfile("testpower3.mat"))))
  print(mean(predict(nnet3,processtestfile("testpower4.mat"))))
  print(mean(predict(nnet3,processtestfile("testpower5.mat"))))
  print(mean(predict(nnet3,processtestfile("testpower6.mat"))))
}

#mean(predict(nnet3,processtestfile("heavy6.mat")))

#writeMat("100-10-weights.mat",weights=nnet$weights)

