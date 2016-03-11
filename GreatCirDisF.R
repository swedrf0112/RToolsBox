#!/usr/bin/env Rscript


rad=function(d){
  return( (d * pi) /180 )
}

GreatCirDisF=function(lat1,lng1,lat2,lng2){

  er=6378.245 # Semi-major axis

  radlat1=rad(lat1)
  radlat2=rad(lat2)
  a=radlat1-radlat2
  b=rad(lng1)-rad(lng2)
  s=2*asin(sqrt(sin(a/2)^2+cos(radlat1)*cos(radlat2)*sin(b/2)^2))
  s=s*er
  s=round(s*10000)/10000
  return(s)   # km
   
}
