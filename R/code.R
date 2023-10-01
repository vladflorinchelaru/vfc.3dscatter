# Copyright (C) 2023 Vlad-Florin Chelaru; vladflorinchelaru@gmail.com
#' A set of 6 standard colors to use
#' @examples \dontrun{ggplot()+scale_fill_manual(values=vfc.colors[1:3])}
#' @export
vfc.colors<-c("#6495ED", "#ee82ee", "#9BC53D", "#5B3758", "#BE1E2D","#8c1a6a")
#' Create a factor vector of random values
#'
#' This is used to test the \code{drawTogether.allOrdinal()} functions and graphs.
#'
#' @param levels The levels of the factor.
#' @param n How many values should the vector have.
#' @return A factor vector.
getRandomFactors<-function(levels=c("A","B","C","D"),n=50){
  ret<-round(runif(n = n,min = 0,max = length(levels)))
  ret[ret==length(levels)]<-0
  ret<-factor(ret,levels=0:(length(levels)-1),labels=levels)
  return(ret)
}
#' Generate test data for \code{.allOrdinal} functions and graphs
#'
#' @param n Number of individual data points.
#'
#' @return A \code{data.frame} with 4 columns: \code{x}, \code{y}, \code{z}, and \code{fill}.
#' @export
#'
#' @examples drawTogether.allOrdinal(testData.allOrdinal(100))
testData.allOrdinal<-function(n=50){
  return(data.frame(
    x=getRandomFactors(levels=c("A","B","C","D"),n=n),
    y=getRandomFactors(levels=c("M","N","Q","P"),n=n),
    z=getRandomFactors(levels=c("S","T","U","V"),n=n),
    fill=getRandomFactors(levels=c("#6495ED", "#ee82ee", "#9BC53D"),n=n)
  ))
}
#' Create a 3D scatterplot adapted for ordinal values
#'
#' @param d The data frame, containing the factors as the first three columns and \code{fill} as RGB codes for colors.
#' @param ... To be passed to underlying functions.
#' @import rgl
#' @export
#'
#' @examples onlyScatter.allOrdinal(testData.allOrdinal(100))
onlyScatter.allOrdinal<-function(d,...){
  plot3d(x=d[,1],y=d[,2],z=d[,3],col=d$fill,type="s",xlab = names(d)[1],ylab=names(d)[2],zlab = names(d)[3],...)
  bbox3d(
    xat=1:length(levels(d[,1])),xlab = levels(d[,1]),
    yat=1:length(levels(d[,2])),ylab = levels(d[,2]),
    zat=1:length(levels(d[,3])),zlab = levels(d[,3]),
    col=c("white","black"),front="culled",back="culled"
  )
  box3d()
  view3d(phi=-70)
}
onlyBarplotXZ.allOrdinal<-function(d,...){
  dns=list(x=levels(d[,1]),z=levels(d[,3]),fill=levels(d$fill))
  counts<-array(0,dim=c(length(dns[[1]]),length(dns[[2]]),length(dns[[3]])),dimnames = dns)
  for(i in 1:nrow(d))
    counts[d[i,1],d[i,3],d[i,4]]<-counts[d[i,1],d[i,3],d[i,4]]+1
  dfl<-data.frame(x=as.character(),z=as.character(),height_start=as.numeric(),height_end=as.numeric(),fill=as.character())
  for(x in dns[[1]])
    for(z in dns[[2]])
      for(f in 1:length(dns[[3]])){
        cr<-nrow(dfl)+1 # current row
        dfl[cr,1]<-x
        dfl[cr,2]<-z
        dfl[cr,3]<-ifelse(f==1,0,sum(counts[x,z,1:(f-1)]))
        dfl[cr,4]<-dfl[cr,3]+counts[x,z,f]
        dfl[cr,5]<-dns[[3]][f]
      }
  dfl$x<-factor(dfl$x,levels=dns[[1]])
  dfl$z<-factor(dfl$z,levels=dns[[2]])
  dfl<-dfl[dfl$height_end!=0,]
  plot3d(dfl$x,dfl$height_end,dfl$z,col=dfl$fill,type="s",xlab = names(d)[1],ylab = "Count",zlab = names(d)[3],ylim = c(-0.5,max(dfl$height_end)+0.5))
  bbox3d(
    xat=1:length(levels(d[,1])),xlab = levels(d[,1]),
    zat=1:length(levels(d[,3])),zlab = levels(d[,3]),
    col=c("white","black"),front="culled",back="culled"
  )
  box3d()
  for(i in 1:nrow(dfl))
    if(dfl[i,3]!=dfl[i,4])
      segments3d(
        x=c(dfl[i,1],dfl[i,1]),
        y=c(dfl[i,3],dfl[i,4]),
        z=c(dfl[i,2],dfl[i,2]),
        col=dfl[i,5],lwd=100
      )
  view3d(phi=-70)
}
onlyBarplotXY.allOrdinal<-function(d,...){
  dns=list(x=levels(d[,1]),y=levels(d[,2]),fill=levels(d$fill))
  counts<-array(0,dim=c(length(dns[[1]]),length(dns[[2]]),length(dns[[3]])),dimnames = dns)
  for(i in 1:nrow(d))
    counts[d[i,1],d[i,2],d[i,4]]<-counts[d[i,1],d[i,2],d[i,4]]+1
  dfl<-data.frame(x=as.character(),y=as.character(),height_start=as.numeric(),height_end=as.numeric(),fill=as.character())
  for(x in dns[[1]])
    for(y in dns[[2]])
      for(f in 1:length(dns[[3]])){
        cr<-nrow(dfl)+1 # current row
        dfl[cr,1]<-x
        dfl[cr,2]<-y
        dfl[cr,3]<-ifelse(f==1,0,sum(counts[x,y,1:(f-1)]))
        dfl[cr,4]<-dfl[cr,3]+counts[x,y,f]
        dfl[cr,5]<-dns[[3]][f]
      }
  dfl$x<-factor(dfl$x,levels=dns[[1]])
  dfl$y<-factor(dfl$y,levels=dns[[2]])
  dfl<-dfl[dfl$height_end!=0,]
  plot3d(dfl$x,dfl$y,dfl$height_end,col=dfl$fill,type="s",xlab = names(d)[1],zlab = "Count",ylab = names(d)[2],zlim=c(-0.5,max(dfl$height_end)+0.5))
  bbox3d(
    xat=1:length(levels(d[,1])),xlab = levels(d[,1]),
    yat=1:length(levels(d[,2])),ylab = levels(d[,2]),
    col=c("white","black"),front="culled",back="culled"
  )
  box3d()
  for(i in 1:nrow(dfl))
    if(dfl[i,3]!=dfl[i,4])
      segments3d(
        x=c(dfl[i,1],dfl[i,1]),
        z=c(dfl[i,3],dfl[i,4]),
        y=c(dfl[i,2],dfl[i,2]),
        col=dfl[i,5],lwd=100
      )
  view3d(phi=-70)
}
onlyBarplotYZ.allOrdinal<-function(d,...){
  dns=list(y=levels(d[,2]),z=levels(d[,3]),fill=levels(d$fill))
  counts<-array(0,dim=c(length(dns[[1]]),length(dns[[2]]),length(dns[[3]])),dimnames = dns)
  for(i in 1:nrow(d))
    counts[d[i,2],d[i,3],d[i,4]]<-counts[d[i,2],d[i,3],d[i,4]]+1
  dfl<-data.frame(y=as.character(),z=as.character(),height_start=as.numeric(),height_end=as.numeric(),fill=as.character())
  for(y in dns[[1]])
    for(z in dns[[2]])
      for(f in 1:length(dns[[3]])){
        cr<-nrow(dfl)+1 # current row
        dfl[cr,1]<-y
        dfl[cr,2]<-z
        dfl[cr,3]<-ifelse(f==1,0,sum(counts[y,z,1:(f-1)]))
        dfl[cr,4]<-dfl[cr,3]+counts[y,z,f]
        dfl[cr,5]<-dns[[3]][f]
      }
  dfl$y<-factor(dfl$y,levels=dns[[1]])
  dfl$z<-factor(dfl$z,levels=dns[[2]])
  dfl<-dfl[dfl$height_end!=0,]
  plot3d(dfl$height_end,dfl$y,dfl$z,col=dfl$fill,type="s",ylab = names(d)[2],xlab = "Count",zlab = names(d)[3],xlim=c(-0.5,max(dfl$height_end)+0.5))
  bbox3d(
    yat=1:length(levels(d[,2])),ylab = levels(d[,2]),
    zat=1:length(levels(d[,3])),zlab = levels(d[,3]),
    col=c("white","black"),front="culled",back="culled"
  )
  box3d()
  for(i in 1:nrow(dfl))
    if(dfl[i,3]!=dfl[i,4])
      segments3d(
        y=c(dfl[i,1],dfl[i,1]),
        x=c(dfl[i,3],dfl[i,4]),
        z=c(dfl[i,2],dfl[i,2]),
        col=dfl[i,5],lwd=100
      )
  view3d(phi=-70)
}
#' Create a panel of 3D scatterplot and corresponding distribution graphs
#'
#' @param d The data frame, containing the factors as the first three columns and \code{fill} as RGB codes for colors.
#' @param ... To be passed to underlying functions.
#' @import rgl
#' @export
#'
#' @examples drawTogether.allOrdinal(testData.allOrdinal(100))
drawTogether.allOrdinal<-function(d,...){
  mfrow3d(2,2,sharedMouse = T)
  onlyBarplotYZ.allOrdinal(d,...)
  onlyScatter.allOrdinal(d,...)
  onlyBarplotXY.allOrdinal(d,...)
  onlyBarplotXZ.allOrdinal(d,...)
}
#' Rotate the graph(s) and maybe save them as subsequent png files
#'
#' This works both with \code{.allOrdinal()} and \code{.allNumeric()} graphs.
#'
#' @param saveloc If provided, will create a folder in the current working directory and save subsequent frames as separate png files of 1000x1000px.
#' @param theta Starting theta angle. See \code{rgl::view3d()}.
#' @param phi Starting phi angle. See \code{rgl::view3d()}.
#' @param theta_delta Increment with each frame.
#' @param phi_delta Increment with each frame. Whenever \code{abs(phi)-89.99>0}, \code{phi_delta<- -phi_delta}.
#' @import rgl
#' @importFrom stringr str_pad
#' @export
#'
#' @examples drawTogether.allOrdinal(testData.allOrdinal(100));rotateTogether(theta_delta=2,phi_delta=1)
rotateTogether<-function(saveloc=NA,theta=0,phi=0,theta_delta=0.2,phi_delta=0.1){
  if(!is.na(saveloc))
    dir.create(saveloc)
  for(i in 1:(360/theta_delta)){
    theta<-theta+theta_delta
    phi<-phi+phi_delta
    cat(stringr::str_pad(i,width = 5),stringr::str_pad(theta,width = 3),stringr::str_pad(phi,width = 2),"\n")
    for(s in subsceneList()){
      useSubscene3d(subscene=s)
      view3d(theta = theta,phi = phi,zoom = 0.83)
    }
    if(!is.na(saveloc))
      snapshot3d(filename = paste0(saveloc,"/",stringr::str_pad(i,width = 4,pad = '0'),".png"),width = 1000,height = 1000,webshot = F)
    if(abs(phi)-89.99>0)
      phi_delta<- -phi_delta
  }
}
#' Generate test data for \code{.allNumeric} functions and graphs
#'
#' @param n Number of individual data points.
#'
#' @return A \code{data.frame} with 4 columns: \code{x}, \code{y}, \code{z}, and \code{fill}.
#' @export
#'
#' @examples drawTogether.allNumeric(testData.allNumeric(100))
testData.allNumeric<-function(n=50){
  x=rnorm(n)
  xp=x*8+runif(n,min = -5,max = 5)
  fill=ifelse(xp< -10,"#6495ED",ifelse(xp<10,"#ee82ee","#9BC53D"))
  return(data.frame(
    x=x,
    y=x*runif(n),
    z=rnorm(n),
    fill=factor(fill,levels=c("#6495ED", "#ee82ee", "#9BC53D"))
  ))
}
#' Create a 3D scatterplot adapted for numeric values
#'
#' @param d The data frame, containing the numeric values as the first three columns and \code{fill} as RGB codes for colors.
#' @param ... To be passed to underlying functions.
#' @import rgl
#' @export
#'
#' @examples onlyScatter.allNumeric(testData.allNumeric(100))
onlyScatter.allNumeric<-function(d,...){
  plot3d(x=d[,1],y=d[,2],z=d[,3],
    col=d$fill,type="s",
    xlab = names(d)[1],ylab=names(d)[2],zlab = names(d)[3],...)
  view3d(phi=-70)
}
onlyDensityXZ.allNumeric<-function(d,...){
  e_list<-list()
  mind<-NA
  maxd<-NA
  for(f in levels(d$fill)){
    e_list[[f]]<-MASS::kde2d(d[d$fill==f,1],d[d$fill==f,3])
    if(!is.na(mind)){
      mind<-min(mind,e_list[[f]][["z"]])
      maxd<-max(maxd,e_list[[f]][["z"]])
    } else {
      mind<-min(e_list[[f]][["z"]])
      maxd<-max(e_list[[f]][["z"]])
    }
  }
  plot3d(c(),c(),c(),ylab = "Density (relative)",xlab=names(d)[1],zlab=names(d)[3])
  for(f in levels(d$fill))
    surface3d(e_list[[f]]$x,e_list[[f]]$z/maxd*(max(d[,1])-min(d[,1])),e_list[[f]]$y,color=f,alpha=0.3)
  bbox3d(front="culled",back="culled")
  box3d()
  view3d(phi=-70)
}
onlyDensityXY.allNumeric<-function(d,...){
  e_list<-list()
  mind<-NA
  maxd<-NA
  for(f in levels(d$fill)){
    e_list[[f]]<-MASS::kde2d(d[d$fill==f,1],d[d$fill==f,2])
    if(!is.na(mind)){
      mind<-min(mind,e_list[[f]][["z"]])
      maxd<-max(maxd,e_list[[f]][["z"]])
    } else {
      mind<-min(e_list[[f]][["z"]])
      maxd<-max(e_list[[f]][["z"]])
    }
  }
  plot3d(c(),c(),c(),zlab = "Density (relative)",ylab=names(d)[2],xlab=names(d)[1])
  for(f in levels(d$fill))
    surface3d(e_list[[f]]$x,e_list[[f]]$y,e_list[[f]]$z/maxd*(max(d[,3])-min(d[,3])),color=f,alpha=0.3)
  bbox3d(front="culled",back="culled")
  box3d()
  view3d(phi=-70)
}
onlyDensityYZ.allNumeric<-function(d,...){
  e_list<-list()
  mind<-NA
  maxd<-NA
  for(f in levels(d$fill)){
    e_list[[f]]<-MASS::kde2d(d[d$fill==f,2],d[d$fill==f,3])
    if(!is.na(mind)){
      mind<-min(mind,e_list[[f]][["z"]])
      maxd<-max(maxd,e_list[[f]][["z"]])
    } else {
      mind<-min(e_list[[f]][["z"]])
      maxd<-max(e_list[[f]][["z"]])
    }
  }
  plot3d(c(),c(),c(),xlab = "Density (relative)",ylab=names(d)[2],zlab=names(d)[3])
  for(f in levels(d$fill))
    surface3d(e_list[[f]]$z/maxd*(max(d[,1])-min(d[,1])),e_list[[f]]$x,e_list[[f]]$y,color=f,alpha=0.3)
  bbox3d(front="culled",back="culled")
  box3d()
  view3d(phi=-70)
}
#' Create a panel of 3D scatterplot and corresponding distribution graphs
#'
#' @param d The data frame, containing the numeric values as the first three columns and \code{fill} as RGB codes for colors.
#' @param ... To be passed to underlying functions.
#' @import rgl
#' @importFrom MASS kde2d
#' @export
#'
#' @examples drawTogether.allNumeric(testData.allNumeric(100))
drawTogether.allNumeric<-function(d,...){
  mfrow3d(2,2,sharedMouse = T)
  onlyDensityYZ.allNumeric(d,...)
  onlyScatter.allNumeric(d,...)
  next3d()
  onlyDensityXY.allNumeric(d,...)
  next3d()
  onlyDensityXZ.allNumeric(d,...)
}
