#!/usr/bin/env Rscript

TimeTransF=function(TimeV,TimeMinUnit="d"){
  
  TransV=switch(TimeMinUnit,
                 d=as.POSIXct(TimeV,format="%Y-%m-%d",tz=Sys.timezone()),
                 S=as.POSIXct(TimeV,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
  )
  
  return(TransV)
  
}