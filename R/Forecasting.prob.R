#' Creates Marginal Distributions in the Forecasting of Time series
#' @export
#' @param k number of categories
#' @param q The number of the domain in which the value of the independent variable is located
Forecasting.prob<-function(k,q)
{
  # length
  t<-length(data$x)
  #Rang for an independent time series
  rx<-max(data$x)-min(data$x)
  #Class length for independent time series
  m1<-rx/k
  m1<-round(m1,2)
  #Rang for an dependent time series
  ry<-max(data$y)-min(data$y)
  #Class length for dependent time series
  m2<-ry/k
  m2<-round(m2,2)
  #zero matrix for data dump
  matrix.prob<-matrix(0,k,k)
  for(h in 1:k)
  {
    for(l in 1:k)
    {

      for (j in 1:t) {
        if ((data$x [j]>=(min(data$x)+(l-1)*m1))&( data$x [j]<( min(data$x)+(l)*m1))&( data$y[j]>=( min(data$y)+(h-1)*m2))&( data$y [j]<( min(data$y)+(h)*m2)))
          matrix.prob[h,l]=matrix.prob[h,l]+1
      }}}

  matrix.prob<-matrix.prob/t
  print(matrix.prob)
  m<-matrix(0,k,1)
  n<-matrix(0,k,1)
  m[1]<-min(data$y)
  i<-1
  while(i<=k)
  {
    m[i+1]<-m[i]+m2
    n[i]<-(m[i]+m[i+1])/2
    i<-i+1
  }
  Totalcolumns<-apply(matrix.prob, 2,sum)
  s=0
  for (i in 1:k)
    s<-s+(matrix.prob[i,q]*n[i])/Totalcolumns[q]
  print(s)
}
