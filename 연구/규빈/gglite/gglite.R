# IRdisplay 패키지 로드
library(IRdisplay)
# 그래프 출력 크기 설정
options(jupyter.plot_scale=4)
options(repr.plot.width=6,repr.plot.height=4,repr.plot.res=300)
library(tidyverse)
library(patchwork)

figsize <- function(width=6,height=4){
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

prepare_data <- function(x, y = NULL) {
  if (is.null(y)) {
    y=x 
    if (!is.vector(y)) {
      x = 1:dim(y)[1]
    } else {
      x = 1:length(y)
    }    
  }
  if (!is.vector(y)) {
      dfx = data.frame(x)
      dfy = data.frame(y)
      df = cbind(dfx,dfy) 
      df = pivot_longer(df,cols = colnames(y), names_to = "label", values_to = "y")        
  } else {
      df = data.frame(x=x,y=y)
  }
  return(df)
}

## main geoms

line <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)    
  df <- prepare_data(x, y)
  
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }
  
  return(do.call(geom_line, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

point <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)        
  df = prepare_data(x, y)
    
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }    
    
  return(do.call(geom_point, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

## 2d geoms 

smooth <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)        
  df = prepare_data(x, y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }        
  return(do.call(geom_smooth, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

area <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)        
  df = prepare_data(x, y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour","fill"))) {
    args[names(args) %in% c("col", "color", "colour","fill")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') and 'fill' options will be ignored.")
  }        
  return(do.call(geom_area, c(list(data = df, mapping = aes(x = x, y = y, fill = label, col = label), alpha = 0.1), args)))    
}

step <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)        
  df = prepare_data(x, y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }        
  return(do.call(geom_step, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

jitter <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)       
  df = prepare_data(x, y) 
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }        
  return(do.call(geom_jitter, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

## 1d geoms
histogram <- function(y,label=NULL, ...) {
  args <- list(...)       
  df = prepare_data(y)
  if (!is.null(label) && any(names(args) %in% c("fill"))) {
    args[names(args) %in% c("fill")] <- NULL
    warning("When the label option is used, the 'fill' option will be ignored.")
  }           
  return(do.call(geom_histogram, c(list(data = df, mapping = aes(x = y, y = stat(density), fill = label), alpha = 0.5, bins = 30, position = "identity"), args)))
}

density <- function(y,label=NULL, ...) {
  args <- list(...)       
  df = prepare_data(y) 
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour","fill"))) {
    args[names(args) %in% c("col", "color", "colour","fill")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') and 'fill' options will be ignored.")
  }            
  return(do.call(geom_density, c(list(data = df, mapping = aes(x = y, fill = label, col = label), alpha = 0.25), args)))
}

qq <- function(y,label=NULL, ...) {
  args <- list(...)        
  df = data.frame(y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }         
  return(do.call(geom_qq, c(list(data = df, mapping = aes(sample = y, col = label)), args)))
}

qq_line <- function(y,label=NULL, ...) {
  args <- list(...)    
  df = data.frame(y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }         
  return(do.call(geom_qq_line, c(list(data = df, mapping = aes(sample = y, col = label)), args)))
}

## compare geoms 

col <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)    
  df = prepare_data(x, y)
  df$x = as.factor(df$x)
  if (!is.null(label) && any(names(args) %in% c("fill"))) {
    args[names(args) %in% c("fill")] <- NULL
    warning("When the label option is used, the 'fill' option will be ignored.")
  }          
  return(do.call(geom_col, c(list(data = df, mapping = aes(x = x, y = y, fill = label), position = 'dodge'), args)))
}

boxplot <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)    
  if (is.null(y)) {
    y=x 
    x=0
  }
  df = prepare_data(x, y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour"))) {
    args[names(args) %in% c("col", "color", "colour")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') option will be ignored.")
  }       
  return(do.call(geom_boxplot, c(list(data = df, mapping = aes(x = x, y = y, col = label)), args)))
}

violin <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)    
  if (is.null(y)) {
    y=x 
    x=0
  }
  df = prepare_data(x, y)
  if (!is.null(label) && any(names(args) %in% c("col", "color", "colour","fill"))) {
    args[names(args) %in% c("col", "color", "colour","fill")] <- NULL
    warning("When the label option is used, the 'color' (or 'colour' or 'col') and 'fill' options will be ignored.")
  }     
  return(do.call(geom_violin, c(list(data = df, mapping = aes(x = x, y = y, fill = label, color = label), alpha = 0.5, scale = 'area'), args)))
}