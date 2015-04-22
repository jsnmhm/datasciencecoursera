pollutantmean <- function(directory, pollutant, id){
  files <- list.files("specdata", full.names=TRUE)
  
  pollutants <- lapply(files[id], read.csv)
  pollutants <- do.call("rbind",pollutants)
  
  mean(pollutants[[pollutant]],na.rm=TRUE)   
}

complete <- function(directory, id){
  files <- list.files("specdata", full.names=TRUE)
  
  p <- lapply(files[id], read.csv)
  p <- do.call("rbind",p)
  
  p <- aggregate(complete.cases(p), by=list(p$ID), FUN=sum) 
  colnames(p) <- c('id', 'nobs')
  p
  
}

createdf <- function(directory, id){
  files <- list.files("specdata", full.names=TRUE)
  
  pollutants <- lapply(files[id], read.csv)
  pollutants <- do.call("rbind",pollutants)
  
  pollutants  
}

corr <- function(directory, threshold=0){
  files <- list.files("specdata", full.names=TRUE)
  
  data <- lapply(files, read.csv)
  data <- do.call("rbind",data)
  
  p <- aggregate(complete.cases(data), by=list(data$ID), FUN=sum) 
  colnames(p) <- c('id', 'nobs')
  
  p<-p[p$nobs>threshold,]
  
  complete<-complete.cases(data)
  data<-data[complete,]
  
  p<-merge(data, p, by.x='ID', by.y='id')
  
  p_list<-split(p, p$ID)
  
  ##P-list<-
  
  cr<-sapply(p_list, cor(p_list['sulfate'], p_list['nitrate']))
  
  cr
}