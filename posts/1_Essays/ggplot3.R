# IRdisplay 패키지 로드
library(IRdisplay)
# 그래프 출력 크기 설정
options(jupyter.plot_scale=4)
options(repr.plot.width=9,repr.plot.height=6,repr.plot.res=300)
library(tidyverse)
library(patchwork)

figsize <- function(width=9,height=6){
    options(repr.plot.width=width,repr.plot.height=height,repr.plot.res=300)
}

ggplot <- function(...){
    ggplot2::ggplot(...)+      
    theme_bw()+theme(panel.border=element_blank(),axis.line=element_line(colour="black"))+
    theme(axis.title.x=element_text(size=rel(1),lineheight=0.9,face="bold.italic"))+
    theme(axis.title.y=element_text(size=rel(1),lineheight=0.9,face="bold.italic"))+
    theme(plot.title=element_text(size=rel(2),lineheight=0.9,face="bold.italic"))+
    theme(plot.margin = unit(c(3,3,0,0), "mm"))
}

line <- function(x, y=NULL,label=NULL, ...) {
  if (is.null(y)){
    y <- x
    x <- 1:length(y)
  }
  return(geom_line(data=data.frame(x = x, y = y), mapping=aes(x = x, y = y, col=label), ...))
}

point <- function(x, y=NULL,label=NULL, ...) {
  if (is.null(y)){
    y <- x
    x <- 1:length(y)
  } 
  return(geom_point(data=data.frame(x = x, y = y), mapping=aes(x = x, y = y, col=label), ...))
}