library("stringr")
library("R.matlab")
#library("tuneR")

process.folder <- function(){
  matfiles<-list.files(path = ".", pattern = "^heavy.*\\.mat", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata1<-lapply(matfiles,process.histogram)
  alldata1<-do.call(rbind, alldata1)
  writeMat("sumheavy.mat",sumheavy=alldata1)
  
  #1 is for POWER
  matfiles<-list.files(path = ".", pattern = "^power.*\\.mat", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata2<-lapply(matfiles,process.histogram)
  alldata2<-do.call(rbind, alldata2)
  writeMat("sumpower.mat",sumpower=alldata2)
}

process.histogram <- function(filename){
  ### Histogram frequency bin amount
  bins <- 32
  hists <- loadmatfile(filename)
  hist.length <- dim(hists)[2]
  ## each bin grows in size linearly: width_j = bin.growfactor*j + hist.length/256; 
  bin.growfactor <- 2*hist.length*(1-bins/256)/(1+bins)/bins
  res<-matrix
  for(i in 1:floor((dim(hists)[1])/1000)){
    start<- 1+(i-1)*1000
    finish <- i*1000
    hists.beats <- sapply(c(1:bins), process.detectbeats, hists[start:finish,], bin.growfactor, bins)  
    hists.beats<-matrix(hists.beats,nrow=1)
    if(is.null(dim(res))){
      res<-hists.beats
    }else{
      res<-rbind(res,hists.beats)
    }
    
  }
  print(sprintf("Processed file %s", filename))
  return(res)
}


loadmatfile <- function(filename){
  varname<-str_match(filename,"(.*?)\\.mat")[2]
  datam<-readMat(filename)
  varsize<-dim(datam[[varname]])
  output<-matrix(datam[[varname]],ncol=varsize[2],nrow=varsize[1])
  res<-t(output)
  #names(res)<-varname
  #return(res)
  return(res)
}

process.detectbeats <- function(binNo, hists, bin.growfactor, bins){
  ###Memory length - 43.11 hists ~ 1 sec
  hist.memory <- 43
  if(bin.growfactor<1){
    ##reorder bin width growth if it is shrinking
    binRevrtNo <- bins-binNo+1
    ##lower frequency (the bin bounds)
    hist.lower <- floor( bin.growfactor*(2*bins-binNo+2)/2*(binNo-1) + dim(hists)[2]/256*(binNo-1) )
    ##upper frequency
    hist.upper <- ceiling( hist.lower + bin.growfactor*(bins-binNo+1)+dim(hists)[2]/256 )
  }else{
    ##TBD: fix this:
    ##lower frequency (the bin bounds)
    hist.lower <- floor( bin.growfactor*binNo/2*(binNo-1)+(binNo-1)*dim(hists)[2]/256 )
    ##upper frequency
    hist.upper <- ceiling( hist.lower + bin.growfactor*binNo + dim(hists)[2]/256)
  }
  if(hist.lower < 2){
    hist.lower = 2
  }
  if(hist.upper > dim(hists)[2]){
    hist.upper = dim(hists)[2] 
  }
  ##beat threshold (dB delta of sound that is perceived as 'beat')
  threshold <- 0.5#(max(hists[,hist.lower:hist.upper]) - mean(hists[,hist.lower:hist.upper]))*0.3
  ##compute energies for current bin
  energies <- array(, dim(hists)[1])
  for(histNo in 1:dim(hists)[1]){
    energies[histNo]<-sum(hists[histNo, hist.lower:hist.upper])/(hist.upper-hist.lower+1)
  }
  ##first histograms init to no-beat
  beats <- array(, dim(hists)[1])
  for(histNo in 1:hist.memory){
    beats[histNo] <- FALSE
  }
  ##compute beat? vector starting from memory+1
  prevRes<-FALSE
  for(histNo in (hist.memory+1):dim(hists)[1]){
    ##check instant energy is greater than history
    history.avg<-sum(energies[histNo - 1:hist.memory])/hist.memory
    res<-energies[histNo]>history.avg+threshold
    ##check variance
    variance<-0
    for(i in 1:hist.memory){
      variance<-variance+(energies[histNo-i]-history.avg)^2
    }
    variance<-variance/hist.memory
    res <- res & (variance > 5)&(variance < 30)
    ##save TRUE only if previous was truly false 
    ###we care only about the start/activation of the beat peak, not its length
    beats[histNo] <- !prevRes&res
    prevRes<-res
  }
  res.mean<-mean(energies)
  res.bpm<-round(2640*mean(beats))
  res.var<-var(energies)
  return(c(res.bpm,res.mean,res.var))
}
#for(i in 1:32){print(2640*mean(s[,i]))}

prepare.traindata <- function(){
  ##TODO: pass list of vectors (1st = fileprefix, 2nd = label number)
  ## 0 is for HEAVY
  matfiles<-list.files(path = ".", pattern = "^heavy.*\\.mat", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata1<-lapply(matfiles,process.histogram)
  alldata1<-do.call(rbind, alldata1)
  label<-rep(0,dim(alldata1)[1])
  nn.data1<-data.frame(alldata1,label)
  
  #1 is for POWER
  matfiles<-list.files(path = ".", pattern = "^power.*\\.mat", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  alldata2<-lapply(matfiles,process.histogram)
  alldata2<-do.call(rbind, alldata2)
  label<-rep(1,dim(alldata2)[1])
  nn.data2<-data.frame(alldata2,label)
  
  alldata<-rbind(nn.data1,nn.data2)
  return(alldata)
}