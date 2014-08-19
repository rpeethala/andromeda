best<-function(state,outcome)
{
  dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  u<-unique(dat[,7])
  if (state%in%u==FALSE)
  {
    stop("invalid state")
  }
    
  if (outcome!="heart attack"&outcome!="heart failure"&outcome!="pneumonia")
  {
    stop("invalid outcome")
  }
  
  if (outcome=="heart attack")
  {
  a<-subset(dat[,c(2,7,11)],State==state)
  a[,3][a[,3] == "Not Available"] <- "NA"
  a<-na.omit(a)
  aa<-a[order(as.numeric(a[,3]),a[,1]),]
  x<-aa[1,1]
  return(x)
  }
  
  if (outcome =="heart failure")
  {
    a<-subset(dat[,c(2,7,17)],State==state)
    a[,3][a[,3] == "Not Available"] <- "NA"
    a<-na.omit(a)
    aa<-a[order(as.numeric(a[,3]),a[,1]),]
    x<-aa[1,1]
    return(x)
  }
  if (outcome =="pneumonia")
  {
    a<-subset(dat[,c(2,7,23)],State==state)
    a[,3][a[,3] == "Not Available"] <- "NA"
    a<-na.omit(a)
    aa<-a[order(as.numeric(a[,3]),a[,1]),]
    x<-aa[1,1]
    return(x)
  }

}