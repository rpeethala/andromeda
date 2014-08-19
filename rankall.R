rankall<-function(outcome,num="best")
{
  
  dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  u<-sort(unique(dat[,7]))

  if (outcome!="heart attack"&outcome!="heart failure"&outcome!="pneumonia")
  {
    stop("invalid outcome")
  }
  
  if (outcome=="heart attack")
  {
    a<-subset(dat[,c(2,7,11)])
    a[a=="Not Available"]=NA
    a<-na.omit(a)
    aa<-a[order(a[,2],as.numeric(a[,3]),a[,1]),]
    #x<-aa[1,1]
    ##return(x)
  }
  
  if (outcome =="heart failure")
  {
    a<-subset(dat[,c(2,7,17)])
    a[a=="Not Available"]=NA
    a<-na.omit(a)
    aa<-a[order(a[,2],as.numeric(a[,3]),a[,1]),]
    #x<-aa[1,1]
    ##return(x)
  }
  if (outcome =="pneumonia")
  {
    a<-subset(dat[,c(2,7,23)])
    a[a=="Not Available"]=NA
    a<-na.omit(a)
    aa<-a[order(a[,2],as.numeric(a[,3]),a[,1]),]
    #x<-aa[1,1]
    ##return(x)
  }
  
  df <- data.frame(matrix(ncol = 2, nrow = length(u)))
  colnames(df) <- c("hospital","state")
  
  if (num=="best")
  {
    for (i in 1:length(u))
    {
      x<-subset(aa,aa[,2]==u[i])
      df[i,1]=x[1,1]
      df[i,2]=u[i]
    }
    return(df)
  }
  else if (num=="worst")
  {
    for (i in 1:length(u))
    {
      x<-subset(aa,aa[,2]==u[i])
      df[i,1]=x[nrow(x),1]
      df[i,2]=u[i]
    }
    return(df)
  }
  else if (all.equal(num,as.integer(num))==TRUE)
  {
    for (i in 1:length(u))
    {
      x<-subset(aa,aa[,2]==u[i])
      if (num<=nrow(x))
      {
        df[i,1]=x[num,1]
        df[i,2]=u[i]
      }
      else
      {
        df[i,1]=NA
        df[i,2]=u[i]
      }
    }
    return(df)
  }
  else
  {
    stop("Invalid NUM")
  }
}