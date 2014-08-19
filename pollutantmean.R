pollutantmean <- function(a,b,id)
{
  setwd(a)
  fc<-list.files(pattern ="*.csv")
  cnt<-id
  for (i in seq_along(cnt))
  {
    c<-read.csv(fc[cnt[i]])
    m<-numeric(i)
    m[i]<-mean(c[,b],na.rm=TRUE)
  }
  n<-mean(m)
  m
  n
}
