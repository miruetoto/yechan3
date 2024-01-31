source("utils.R")
ebt<-function(f,t=0,noise=0,tau,M="mean",V="var",interpolation="linear",bound="fix",sampling="equally_spaced")
{
  fsave<-f
  tsave<-t+(length(t)==1)*1:length(f)
  f<-c(rep(f[1],tau*2),f,rep(f[length(f)],tau*2))
  t=1:length(f)
  if(sum(noise)==0) noise=0*f
  len<-length(f)
  sampled_index<-list()
  missing_index<-list()
  if (interpolation=="const")
  {
    lband<-rep(0,len*tau); dim(lband)<-c(len,tau)
    rband<-rep(0,len*tau); dim(rband)<-c(len,tau)
    band<-rep(0,len*tau*2); dim(band)<-c(len,tau*2)
  }else{
    band<-rep(0,len*tau); dim(band)<-c(len,tau)
  }

  if (interpolation=="cubic")
  {
    for (eta in 1:tau){
      if (sampling=="randomly") sampled_index[[eta]]<-sort(sample(1:len,1/tau*1000,replace=F))
      else if(sampling=="equally_spaced") sampled_index[[eta]]<-seq(from=eta,to=len,by=tau)
      if (sampled_index[[eta]][1]!=1) sampled_index[[eta]]=c(1,sampled_index[[eta]])
      if (sampled_index[[eta]][length(sampled_index[[eta]])]!=len) sampled_index[[eta]]=c(sampled_index[[eta]],len)
      missing_index[[eta]]<-(1:len)[-sampled_index[[eta]]]
      result<-smooth.spline(t[sampled_index[[eta]]],f[sampled_index[[eta]]]+noise[sampled_index[[eta]]],spar=0,all.knots=TRUE)
      band[,eta][sampled_index[[eta]]]<-result$y
      band[,eta][missing_index[[eta]]]<-predict(result,t[missing_index[[eta]]])$y
    }
  }else if (interpolation=="linear"){
    for (eta in 1:tau){
      if (sampling=="randomly") sampled_index[[eta]]<-sort(sample(1:len,1/tau*1000,replace=F))
      else if(sampling=="equally_spaced") sampled_index[[eta]]<-seq(from=eta,to=len,by=tau)
      if (sampled_index[[eta]][1]!=1) sampled_index[[eta]]<-c(1,sampled_index[[eta]])
      if (sampled_index[[eta]][length(sampled_index[[eta]])]!=len) sampled_index[[eta]]<-c(sampled_index[[eta]],len)
      missing_index[[eta]]<-(1:len)[-sampled_index[[eta]]]
      band[,eta]<-lin_impute(t,f+noise,missing_index[[eta]])
    }
  }else if (interpolation=="const"){
    for (eta in 1:tau){
      if (sampling=="randomly") sampled_index[[eta]]<-sort(sample(1:len,1/tau*1000,replace=F))
      else if(sampling=="equally_spaced") sampled_index[[eta]]<-seq(from=eta,to=len,by=tau)
      if (sampled_index[[eta]][1]!=1) sampled_index[[eta]]=c(1,sampled_index[[eta]])
      if (sampled_index[[eta]][length(sampled_index[[eta]])]!=len) sampled_index[[eta]]=c(sampled_index[[eta]],len)
      missing_index[[eta]]<-(1:len)[-sampled_index[[eta]]]
      lband[,eta]<- .left_const(f+noise,missing_index[[eta]])
      rband[,eta]<- .right_const(f+noise,missing_index[[eta]])
    }
    band<-cbind(lband,rband)
  }
  L<-apply(band,1,min)
  U<-apply(band,1,max)

  M1<-apply(band,1,mean)
  M2<-apply(band,1,median)
  M3<-(L+U)/2
  V1=U-L
  V2=apply(band,1,var) ; V2=V2*(tau-1)/tau;

  if (M=="mean") M<-M1
  else if (M=="median") M<-M2
  else if (M=="volume") M<-M3

  if (V=="volume"){
    V<-V1
    L<-L
    U<-U
  }
  else if (V=="var"){
    V<-V2
    L<-M-V
    U<-M+V
  }

  index<-(2*tau+1):(length(f)-2*tau)
  sampled_index <- lapply(sampled_index, function(x) {
  filtered <- x[x <= length(fsave)]
  return(filtered)
  })    
    
  list(t=tsave,f=fsave,V=V[index],L=L[index],U=U[index],M=M[index],tau=tau,band=band[index,],sampled_index=sampled_index)
}