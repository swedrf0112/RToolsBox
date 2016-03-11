#!/usr/bin/env Rscript

####### By Krasovsky 1940
#  a = 6378245.0             Semi-major axis
#  1/f = 298.3               Flattening
#  b = a * (1 - f)           Semi-minor axis
#  ee = (a^2 - b^2) / a^2
#######

OutOfChinaF=function(LonV,LatV){
  
  ClassifyLon = ( LonV < 72.004 | LonV > 137.8347 )
  ClassifyLat = ( LatV < 0.8293 | LatV > 55.8271 )
  
  return( ClassifyLon | ClassifyLat )
  
}

TransLatF=function(x,y){
  
   ret = as.double( -100 + 2 * x + 3 * y +0.2 * y^2 + 0.1 * x * y * 0.2 * sqrt(abs(x)) )
   ret = ret + ( 20 * sin(6*x*pi) + 20 * sin(2*x*pi) ) * 2 / 3
   ret = ret + ( 20 * sin(y*pi) + 40 * sin(y/3*pi) ) * 2 / 3
   ret = ret + ( 160 * sin(y/12*pi) + 320 * sin(y*pi/30) ) * 2 / 3
   return(ret)
   
}

TransLonF=function(x,y){
  
  ret = as.double( 300 + x + 2 * y + 0.1 * x^2 + 0.1 * x * y + 0.1 * sqrt(abs(x)) )
  ret = ret + ( 20 * sin(6*x*pi) + 20 * sin(2*x*pi) ) * 2 / 3
  ret = ret + ( 20 * sin(x*pi) + 40 * sin(x/3*pi) ) * 2 / 3
  ret = ret + ( 150 * sin(x/12*pi) + 300 * sin(x/30*pi) ) * 2 / 3
  return(ret)
  
}

TransCalF=function(WGLonV,WGLatV){
  
   a=as.double(6378245.0)
   ee=as.double(0.00669342162296594323)
  
   dLatV=TransLatF(WGLonV-105,WGLatV-35)
   dLonV=TransLonF(WGLonV-105,WGLatV-35)
   radLat=WGLatV/180 * pi
   magic=sin(radLat)
   magic=1-ee*magic^2
   
   sqrtMagic=sqrt(magic)
   dLatV = (dLatV*180) / ( (a*(1-ee)) / ( magic * sqrtMagic ) * pi )
   dLonV = (dLonV*180) / ( a / sqrtMagic * cos(radLat) *pi )
   MGlonV = WGLonV + dLonV
   MGlatV = WGLatV + dLatV
   
   return(data.frame(MGlonV,MGlatV))
}

OutputMGF=function(InputLonV,InputLatV){
  
   ClassifyOutV=OutOfChinaF(InputLonV,InputLatV)
   
   OutputMGD=data.frame(InputLonV,InputLatV)
   OutputMGD[ClassifyOutV==FALSE,]=TransCalF(InputLonV[ClassifyOutV==FALSE],InputLatV[ClassifyOutV==FALSE])
   
   return(OutputMGD)
}
