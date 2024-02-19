source("utils.R")

ebt<-function(t,f=NULL,tau,mfunc="mean",vfunc="var",inter_method="linear")
{
  if (is.null(f)){
    f <- t 
    t <- 1:length(f)
  }
  tsave<-t
  fsave<-f
  f<-c(rep(f[1],tau*2),f,rep(f[length(f)],tau*2))
  t<-1:length(f)
  len<-length(f)
  sampled_index<-list()
  missing_index<-list()
  band<-rep(0,len*tau); dim(band)<-c(len,tau)
  if (inter_method=="cubic")
  {
    for (eta in 1:tau){
      sampled_index[[eta]]<-seq(from=eta,to=len,by=tau)
      if (sampled_index[[eta]][1]!=1) sampled_index[[eta]]=c(1,sampled_index[[eta]])
      if (sampled_index[[eta]][length(sampled_index[[eta]])]!=len) sampled_index[[eta]]=c(sampled_index[[eta]],len)
      missing_index[[eta]]<-(1:len)[-sampled_index[[eta]]]
      result<-smooth.spline(t[sampled_index[[eta]]],f[sampled_index[[eta]]],spar=0,all.knots=TRUE)
      band[,eta][sampled_index[[eta]]]<-result$y
      band[,eta][missing_index[[eta]]]<-predict(result,t[missing_index[[eta]]])$y
    }
  }else if (inter_method=="linear"){
    for (eta in 1:tau){
      sampled_index[[eta]]<-seq(from=eta,to=len,by=tau)
      if (sampled_index[[eta]][1]!=1) sampled_index[[eta]]<-c(1,sampled_index[[eta]])
      if (sampled_index[[eta]][length(sampled_index[[eta]])]!=len) sampled_index[[eta]]<-c(sampled_index[[eta]],len)
      missing_index[[eta]]<-(1:len)[-sampled_index[[eta]]]
      band[,eta]<-lin_impute(t,f,missing_index[[eta]])
    }
  }
  U<-apply(band,1,max)    
  L<-apply(band,1,min)


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

mvmap<-function(t,f=NULL,maxtau,M="mean",V="var",inter_method="linear")
{
  VM<-rep(0,length(f)*maxtau); dim(VM)<-c(length(f),maxtau);
  MM<-VM 
  for(tau in 2:maxtau)
  {
    out <- ebt(t,f,tau=tau,M,V,inter_method)
    VM[,tau]<-out$V 
    MM[,tau]<-out$M 
  }  
  list(t=t,vmap=VM, mmap=MM)
}
