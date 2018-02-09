# Install dev version of leaflet for R - required
#if (!require('devtools')) install.packages('devtools')
#devtools::install_github('rstudio/leaflet')

if (!require("shinydashboard")) install.packages("shinydashboard")
library("shinydashboard")
if (!require("raster")) install.packages("raster")
library("raster")
if (!require("leaflet")) install.packages("leaflet")
library("leaflet")
if (!require("magrittr")) install.packages("magrittr")
library("magrittr")
if (!require("shiny")) install.packages("shiny")
library("shiny")
if (!require("colorspace")) install.packages("colorspace")
library("colorspace")
if (!require("rgeos")) install.packages("rgeos")
library("rgeos")
if (!require("ggplot2")) install.packages("ggplot2")
library("ggplot2")
if (!require("shinyjs")) install.packages("shinyjs")
library("shinyjs")
#library(V8)

#library(grDevices)

# Define directories and files
# To use with uploaded app set work directory to directory of app and modify data locations accordingly
# setwd( "/Users/yimizhao/Desktop/Study/Data_science/Shiny/census_app")

# For Server
dir_rasters<-"rasters/"
dir_timeseries<-"timeseries/"
dir_counties<-"countymasks/"
dir_about<-"about/"
dir_vineyards<-"vineyards/"


# Link to vineyards file
load(paste(dir_vineyards,"vineyards.RData",sep="") ) # loads vineyards.shp
new.crs<-"+init=epsg:3857"
res<-c(100,100)

t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_line(size=1)
)

# About Figures
website_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
                      axis.line = element_line(size = 0.7, color = "black"), 
                      legend.position = c(0.85, 0.7), text = element_text(size = 14),
                      panel.background = element_blank())

# Load data
swfilein<-paste(dir_about,"SW_GS_MeanT_1910-2014.csv",sep="")
swclimate<-read.csv(swfilein,header=TRUE)
sefilein<-paste(dir_about,"SE_GS_MeanT_1910-2014.csv",sep="")
seclimate<-read.csv(swfilein,header=TRUE)
filein<-paste(dir_about,"vineyards_stats.csv",sep="")
ukstats<-read.csv(filein,header=TRUE)

# Jave script functions - show/hide spinners
jsCode1 <- "shinyjs.showid = function(id){var elem = document.getElementById(id); elem.style.zIndex = '10';}"
jsCode2 <- "shinyjs.hideid = function(id){var elem = document.getElementById(id); elem.style.zIndex = '-5';}"
# refer to js$showid(Id)

# CSS must include any elements show/hide using above functions - must be positioned
# Ref: https://github.com/daattali/advanced-shiny/blob/master/plot-spinner/app.R
mycss <- "
#map-container {
position: relative;
z-index: 3;
}
#yrmap-container {
position: relative;
z-index: 3;
}
#searchmap-container {
position: relative;
z-index: 3;
}
#hist-container {
position: relative;
z-index: 3;
}
#ts-container {
position: relative;
z-index: 3;
}
#map-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#yrmap-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#searchmap-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#hist-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#ts-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#yrhist-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#yrts-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 0;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
"

#################################################
# FUNCTIONS CALLED BY SERVER
#################################################
# Figures in About
# Vineyard hectares
plot_fig1a<-function(ukstats) {
  ggplot(ukstats, aes(x = ukstats$Year, y = ukstats$Total_Vines))  + 
    geom_line(color="red") +
    scale_y_continuous(limits=c(0,2000),"Hectares of Vines", breaks=c(0,500,1000,1500,2000) ) +
    scale_x_continuous(limits=c(1990,2015),"Year")
  
}

# Mean size of UK vineyards
plot_fig1b<-function(ukstats){
  ggplot(ukstats, aes(x = ukstats$Year, y = ukstats$Mean_Size_Vineyards))  + 
    geom_line(color="red") +
    scale_y_continuous(limits=c(0,4),"Vineyard size (Ha)") +
    scale_x_continuous(limits=c(1990,2015),"Year")
}

# Production bottles
plot_fig1c<-function(ukstats) {
  ggplot(ukstats, aes(x = ukstats$Year, y = ukstats$Production_Bottles))  + 
    geom_line(color="red") +
    scale_y_continuous(limits=c(0,8),"Millions of bottles") +
    scale_x_continuous(limits=c(1990,2015),"Year")
  
}

# Timeseries plot of south-west mean growing season temperature
plot_fig2a<-function(swclimate){
  ggplot(swclimate, aes(x = swclimate$Year, y = swclimate$Temp))  + 
    geom_line(color="red") +
    scale_y_continuous(limits=c(11,15),"Mean Temperature") +
    scale_x_continuous(limits=c(1920,2015),breaks=c(1920,1940,1960,1980,2000,2020),"Year")
}

# Yield per Ha by Year
plot_fig2b<-function(ukstats) {
  ggplot(ukstats, aes(x = ukstats$Year, y = ukstats$Yield_per_Ha))  + 
    geom_line(color="red") +
    scale_y_continuous("Yield (HL) per Ha of Vines", breaks=c(0,10,20,30,40)) +
    scale_x_continuous(limits=c(1990,2015),"Year")
}

# Plot yield per Ha vs SE mean GS temp
plot_fig2c<-function(seclimate,ukstats){
  plotdata<-merge(ukstats,seclimate,by=c("Year"))
  ggplot(plotdata, aes(y = plotdata$Yield_per_Ha, x = plotdata$Temp))  + 
    geom_point(color="red") +
    scale_y_continuous(limits=c(0,50),"Yield (HL) per Hectare") +
    scale_x_continuous(limits=c(10,15),"Mean Temperature")
}

#########
# Function that will return file name of raster according to input choices
getmap<-function(county,var,stat_year){ 
  if (var %in% c("elevation","slope","aspect")) paste(dir_rasters,var,"_",county,".tif",sep="") else 
    if (nchar(stat_year==4)) paste(dir_rasters,var,"_",stat_year,"_",county,".tif",sep="") else
      if (nchar(stat_year>4)) paste(dir_rasters,var,"_",stat_year,"_",county,".tif",sep="") 
} # getmap

# Set colour scheme according to variable mapped
# mnmx = 2 element array holding min and max
mapcolour <- function(var,q) {
  mnmx<-c(min(q),max(q))
  switch(var,
         "gdd10_gs" = colorBin("YlOrRd",domain=c(round((floor(mnmx[1])/100)*100),round((ceiling(mnmx[2])/100)*100)),bins=unique(round(ceiling(q)/100)*100),pretty=TRUE),
         "gdd5_gs" = colorBin("YlOrRd",domain=c(round((floor(mnmx[1])/100)*100),round((ceiling(mnmx[2])/100)*100)),bins=unique(round(ceiling(q)/100)*100),pretty=TRUE) ,
         "tmean_gs" = colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE) ,
         "tmax_year" = colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "tmin_year" = colorBin(c("Blues"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "t20_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "t25_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "t30_gsdays"= colorBin(c("white","red"),domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "lastspfr_doy"=colorBin("Blues",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE) ,
         "firstautfr_doy"=colorBin("Blues",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "frostfree_days"=colorBin("BuGn",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "fl_tmean"=colorBin("YlOrRd",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "fl_numday"=colorBin("Greens",domain=round(mnmx),bins=unique(round(q)),pretty=TRUE),
         "elevation"=colorBin(c("blue","green"),domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE),
         "slope"=colorBin("Purples",domain=round(mnmx),bins=c(0,1,2,4,8,12,16,20,24,28,mnmx[2]),pretty=TRUE),
         "aspect"=colorBin("PRGn",domain=c(round((floor(mnmx[1])/10)*10),round((ceiling(mnmx[2])/10)*10)),bins=unique(round(ceiling(q)/10)*10),pretty=TRUE) 
  )
} # mapcolour

# Returns filename of matrix var.m[cellnumber,year] holding values of variable for each cell and year
# or "" if not approoriate data (eg terrain)
get_timeseries<-function(county,var){ 
  if (var %in% c("elevation","slope","aspect")) "" else 
    paste(dir_timeseries,var,"_",county,"_timeseries.R",sep="")  
} 

# Calculate quantiles of displayed map
calc_quantiles<-function(r){
  quantile(getValues(r),c(0,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,1),names=FALSE,na.rm=TRUE)
}

# Return min max values for different variables - used in setting search entry values
set_min_max<-function(var){
  switch(var,
         "gdd10_gs" = list(0,1500),
         "gdd5_gs" = list(0,2000),
         "tmean_gs" = list(10,20),
         "tmax_year" = list(20,45),
         "tmin_year" =list(-10,15),
         "t20_gsdays"=list(0,200),
         "t25_gsdays"=list(0,100),
         "t30_gsdays"=list(0,100),
         "lastspfr_doy"=list(0,150),
         "firstautfr_doy"=list(260,366),
         "frostfree_days"=list(0,366),
         "fl_tmean"=list(5,40),
         "fl_numday"=list(0,14),
         "elevation"=list(0,500),
         "slope"=list(0,35),
         "aspect"=list(0,360)
  )
} # set_min_max

# Return long text label for variable
var_longlabel<-function(var){
  switch(var,
         "gdd10_gs" = "Growing degree days (base 10C)",
         "gdd5_gs" = "Growing degree days (base 5C)",
         "tmean_gs" = "Mean Growing Season Temperature (C)",
         "tmax_year" ="Max Temperature (C)",
         "tmin_year" ="Min Temperature (C)",
         "t20_gsdays"="No. days where max temperature>20C",
         "t25_gsdays"="No. days where max temperature>25C",
         "t30_gsdays"="No. days where max temperature>30C",
         "lastspfr_doy"="Last spring frost (day of year)",
         "firstautfr_doy"="First autumn frost (day of year)",
         "frostfree_days"="Frost free days in year",
         "fl_tmean"="Mean flowering temperature",
         "fl_numday"="No. of good flowering days",
         "elevation"="Elevation (metres)",
         "slope"="Slope (degrees)",
         "aspect"="Aspect (degrees)"
  )
} # var_longlabel

# Return long text label for variable
var_unit<-function(var){
  switch(var,
         "gdd10_gs" = "degree days",
         "gdd5_gs" = "degree days",
         "tmean_gs" = "degrees C",
         "tmax_year" ="degrees C",
         "tmin_year" ="degrees C",
         "t20_gsdays"="days",
         "t25_gsdays"="days",
         "t30_gsdays"="days",
         "lastspfr_doy"="day of year",
         "firstautfr_doy"="day of year",
         "frostfree_days"="days",
         "fl_tmean"="degrees C",
         "fl_numday"="days",
         "elevation"="metres",
         "slope"="degrees",
         "aspect"="degrees"
  )
} # var_unit

stat_longlabel<-function(var){
  switch(var,
         "mean_1983-2013"= ": mean values for 1983 to 2013",
         "min_1983-2013"= ": minimum values for 1983 to 2013",
         "max_1983-2013"= ": maximum values for 1983 to 2013",
         paste(" for the year ",var,sep="") ) 
} # var_unit stat_longlabel(input$sum_stat)


# Calculate cell number from lon lat values
lonlat_to_cellnumber<-function(r,x,y,zone=0){ #http://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+init=epsg:4326")  ## lat lon
  xy <- spTransform(xy, CRS("+init=epsg:3857")) # convert to raster xy
  cell <- cellFromXY(r, c(xy$X,xy$Y))
  return(cell)
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# Plot histogram
plothist<-function(values){
  sel<-which(!is.na(values))
  values<-values[sel]
  numcells<-length(values)
  mnx<-min(values,na.rm=TRUE)
  mxx<-max(values,na.rm=TRUE)
  #f <- hist(values, maxpixels=length(values),breaks=50) # calculate from all cells of raster
  f <- hist(values,breaks=50) 
  #abline(v=100, col="blue",lwd=3)
  dat <- data.frame(counts= ((f$counts/numcells)*100),breaks = f$mids)
  ggplot(dat, aes(x = breaks, y = counts, fill =counts)) + ## Note the new aes fill here
    geom_bar(stat = "identity",alpha = 0.8,fill="blue")+
    xlab("value")+ ylab("%")+
    scale_x_continuous(breaks = seq(0,mxx,(roundUpNice(mxx/5))),
                       labels = seq(0,mxx,(roundUpNice(mxx/5))) )
}

# Function to create raster of cells meeting search criteria
# create_search_map(vars,mins,maxs,input$searchcounty,input$search_stat)
create_search_map<-function(vars,mins,maxs,searchcounty,search_stat) {
  r<-raster(paste(dir_counties,searchcounty,"mask.tif",sep=""))
  crs(r)<-new.crs
  if (vars[1]!="") {
    new.r<-raster(getmap(searchcounty,vars[1],search_stat))
    new.r<-calc(new.r,fun=function(x){ifelse(x>=mins[1] & x<=maxs[1],1,0)})     
    crs(new.r)<-new.crs
    r<-mask(r,new.r,maskvalue=0,updatevalue=0)  
  }
  if (vars[2]!="") {
    new.r<-raster(getmap(searchcounty,vars[2],search_stat))
    new.r<-calc(new.r,fun=function(x){ifelse(x>=mins[2] & x<=maxs[2],1,0)})  
    crs(new.r)<-new.crs
    r<-mask(r,new.r,maskvalue=0,updatevalue=0) 
  }
  if (vars[3]!="") {
    new.r<-raster(getmap(searchcounty,vars[3],search_stat))
    new.r<-calc(new.r,fun=function(x){ifelse(x>=mins[3] & x<=maxs[3],1,0)})    
    crs(new.r)<-new.crs
    r<-mask(r,new.r,maskvalue=0,updatevalue=0)
  }
  return(r)
  
}# create_search_map

############################################
# Define UI
############################################
ui <- dashboardPage(
  dashboardHeader(title="Wine-clim"),
  dashboardSidebar(width=300,
                   sidebarMenu(id="sidebar_tab",
                               menuItem("About", icon=icon("info"),
                                        menuSubItem("Climate & Viticulture", tabName="viticulture"),
                                        menuSubItem("Wineclim", tabName="wineclim") ),
                               menuItem("Explore Maps",  icon=icon("map"),
                                        menuSubItem("Summary data (1983-2013)", tabName = "explore_summary"),
                                        menuSubItem("Individual years", tabName = "explore")   
                               ) ,
                               menuItem("Search Maps", tabName="search",icon=icon("search"),
                                        menuSubItem("Summary data (1983-2013)", tabName = "search")  
                                        # menuSubItem("Individual years", tabName = "search") 
                               )
                   ),
                   conditionalPanel("input.sidebar_tab =='explore_summary'", 
                                    h4(style="padding: 10px 10px 0px;","Select variables for mapping:"),
                                    div(style="height: 70px;",
                                                selectInput("sum_county",label="Geographical area", 
                                                choices=  c("Cornwall" = "cornwall","Devon" = "devon","Dorset" = "dorset", "Somerset" = "somerset") ,
                                                selected="cornwall",selectize=TRUE  ) ),
                                    div(style="height: 70px;",
                                        selectInput(inputId = "sum_stat",label="Summary statistic (1983-2013)", 
                                                choices=  c("Average" = "mean_1983-2013","Lowest value" = "min_1983-2013","Highest value" = "max_1983-2013") ,
                                                selected="mean_1983-2013",selectize=TRUE  ) ) ,
                                    div(style="height: 40px;",
                                        selectInput(inputId="sum_vargroup",label="Select data:",
                                                choices=c("Growing Season Temperatures"="gst",
                                                          "Frost events"="frost",
                                                          "Flowering conditions"="flowering",
                                                          "Terrain features"="terrain") ) ) ,
                                    
                                    conditionalPanel("input.sum_vargroup == 'gst'",
                                                     selectInput("sum_variable1","", c("Choose variable"="","Degree days (base 10C)" = "gdd10_gs","Degree days (base 5C)" = "gdd5_gs", 
                                                                                                  "Mean temperature" = "tmean_gs", "Max temperature"="tmax_year","Min temperature"="tmin_year",
                                                                                                  "No. days > 20C"="t20_gsdays","No. days > 25C"="t25_gsdays","No. days > 30C"="t30_gsdays") ,
                                                                 selected="",selectize=TRUE  ),
                                                     div(class="control-label",style="padding: 10px 10px;","NB: Growing season: 1 April to 31 October.")
                                                     
                                    ),
                                    conditionalPanel("input.sum_vargroup == 'frost'",
                                                     selectInput("sum_variable2","", c("Choose variable"="","Last spring frost" = "lastspfr_doy","First autumn frost" = "firstautfr_doy", 
                                                                                                  "Frost free period" = "frostfree_days") ,
                                                                 selected="",selectize=TRUE  ),
                                                      p(style="padding: 10px 10px;","NB: Frost event defined as < 1C. Dates given as days of year (1 to 366)")
                                                     
                                    ),
                                    conditionalPanel("input.sum_vargroup == 'flowering'",
                                                     selectInput("sum_variable3","Select data", c("Choose variable"="","Mean flowering temperature" = "fl_tmean",
                                                                                              "Good flowering days" = "fl_numday") ,
                                                                 selected="",selectize=TRUE   ),
                                                     p(style="padding: 10px 10px;","Flowering: 22 June-5 July. Good flowering day: mean temperature>15 C")
                                                     
                                    ),
                                    conditionalPanel("input.sum_vargroup == 'terrain'",
                                                     selectInput("sum_variable4","Select data", c("Choose one"="","Elevation" = "elevation","Slope" = "slope", "Aspect" = "aspect"), selected="",selectize=TRUE  )
                                    ) 

                   ), # explore_summary
                   
                   conditionalPanel("input.sidebar_tab =='explore'", 
                                    h4(style="padding: 10px 10px 0px;","Select variables for mapping:"),
                                    div(style="height: 70px;",
                                        selectInput("county",label="Geographical area", 
                                                    choices=  c("Cornwall" = "cornwall","Devon" = "devon","Dorset" = "dorset", "Somerset" = "somerset") ,
                                                    selected="cornwall",selectize=TRUE  ) ),
                                    div(style="height: 70px;",
                                        selectInput(inputId = "stat",label="Select Year", 
                                                    choices=  c("1983" = "1983","1984" = "1984","1985" = "1985",
                                                                "1986" = "1986","1987" = "1987","1988" = "1988","1989" = "1989","1990" = "1990",
                                                                "1991" = "1991","1992" = "1992","1993" = "1993","1994" = "1994","1995" = "1995",
                                                                "1996" = "1996","1997" = "1997","1998" = "1998","1999" = "1999","2000" = "2000",
                                                                "2001" = "2001","2002" = "2002","2003" = "2003","2004" = "2004","2005" = "2005",
                                                                "2006" = "2006","2007" = "2007","2008" = "2008","2009" = "2009","2010" = "2010",
                                                                "2011" = "2011","2012" = "2012","2013" = "2013") ,
                                                    selected="2012",selectize=TRUE  )  ), 
                                    div(style="height: 40px;",
                                        selectInput(inputId="vargroup",label="Select data:",
                                                    choices=c("Growing Season Temperatures"="gst",
                                                              "Frost events"="frost",
                                                              "Flowering conditions"="flowering",
                                                              "Terrain features"="terrain") ) ) ,
                                    
                                    conditionalPanel("input.vargroup == 'gst'",
                                                     selectInput("variable1","", c("Choose variable"="","Degree days (base 10C)" = "gdd10_gs","Degree days (base 5C)" = "gdd5_gs", 
                                                                                   "Mean temperature" = "tmean_gs", "Max temperature"="tmax_year","Min temperature"="tmin_year",
                                                                                   "No. days > 20C"="t20_gsdays","No. days > 25C"="t25_gsdays","No. days > 30C"="t30_gsdays") ,
                                                                 selected="",selectize=TRUE  ),
                                                     div(class="control-label",style="padding: 10px 10px;","NB: Growing season: 1 April to 31 October.")
                                                     
                                    ),
                                    conditionalPanel("input.vargroup == 'frost'",
                                                     selectInput("variable2","", c("Choose variable"="","Last spring frost" = "lastspfr_doy","First autumn frost" = "firstautfr_doy", 
                                                                                   "Frost free period" = "frostfree_days") ,
                                                                 selected="",selectize=TRUE  ),
                                                     p(style="padding: 10px 10px;","NB: Frost event defined as < 1C. Dates given as days of year (1 to 366)")
                                                     
                                    ),
                                    conditionalPanel("input.vargroup == 'flowering'",
                                                     selectInput("variable3","Select data", c("Choose variable"="","Mean flowering temperature" = "fl_tmean",
                                                                                              "Good flowering days" = "fl_numday") ,
                                                                 selected="",selectize=TRUE   ),
                                                     p(style="padding: 10px 10px;","Flowering: 22 June-5 July. Good flowering day: mean temperature>15 C")
                                                     
                                    ),
                                    conditionalPanel("input.vargroup == 'terrain'",
                                                     selectInput("variable4","Select data", c("Choose one"="","Elevation" = "elevation","Slope" = "slope", "Aspect" = "aspect"), selected="",selectize=TRUE  )
                                    ) 
                                    
                   ) ,
                   
                   conditionalPanel("input.sidebar_tab =='search'",
                                    h4(style="padding: 10px 10px 0px;","Select map to be searched:"),
                                    div(style="height: 60px;",
                                        selectInput("search_county",label="Geographical area", 
                                                    choices=  c("Cornwall" = "cornwall","Devon" = "devon","Dorset" = "dorset", "Somerset" = "somerset") ,
                                                    selected="cornwall",selectize=TRUE  ) ),
                                    div(style="height: 60px;",
                                        selectInput(inputId = "search_stat",label="Summary statistic (1983-2013)", 
                                                    choices=  c("Average" = "mean_1983-2013","Lowest value" = "min_1983-2013","Highest value" = "max_1983-2013") ,
                                                    selected="mean_1983-2013",selectize=TRUE  ) ) ,
                                    h4(style="padding: 30px 10px 0px;","Choose search variables & range:"),
                                    div(style="height: 60px;",
                                        selectInput("search1",label="Search variable 1:", c("None chosen"="","Degree days (base 10C)" = "gdd10_gs","Degree days (base 5C)" = "gdd5_gs", 
                                                                                            "Mean temperature" = "tmean_gs", "Max temperature"="tmax_year","Min temperature"="tmin_year",
                                                                                            "No. days > 20C"="t20_gsdays","No. days > 25C"="t25_gsdays","No. days > 30C"="t30_gsdays",
                                                                                            "Last spring frost" = "lastspfr_doy","First autumn frost" = "firstautfr_doy", "Frost free period" = "frostfree_days",
                                                                                            "Mean flowering temp" = "fl_tmean","Good flowering days"="fl_numday",
                                                                                            "Elevation" = "elevation","Slope" = "slope", "Aspect" = "aspect"), 
                                                    selected="",selectize=FALSE  ) ),
                                    div(style="height: 80px;",
                                        sliderInput("slider1", label=NULL, min = 0, 
                                                    max = 100, value = c(20,50))
                                    ) ,
                                    div(style="height: 60px;",
                                        selectInput("search2",label="Search variable 2:", c("None chosen"="","Degree days (base 10C)" = "gdd10_gs","Degree days (base 5C)" = "gdd5_gs", 
                                                                                            "Mean temperature" = "tmean_gs", "Max temperature"="tmax_year","Min temperature"="tmin_year",
                                                                                            "No. days > 20C"="t20_gsdays","No. days > 25C"="t25_gsdays","No. days > 30C"="t30_gsdays",
                                                                                            "Last spring frost" = "lastspfr_doy","First autumn frost" = "firstautfr_doy", "Frost free period" = "frostfree_days",
                                                                                            "Mean flowering temp" = "fl_tmean","Good flowering days"="fl_numday",
                                                                                            "Elevation" = "elevation","Slope" = "slope", "Aspect" = "aspect"), 
                                                    selected="",selectize=FALSE  ) ),
                                    div(style="height: 80px;",
                                        sliderInput("slider2", label=NULL, min = 0, 
                                                    max = 100, value = c(20,50))
                                    ),
                                    div(style="height: 60px;",
                                        selectInput("search3",label="Search variable 3:", c("None chosen"="","Degree days (base 10C)" = "gdd10_gs","Degree days (base 5C)" = "gdd5_gs", 
                                                                                            "Mean temperature" = "tmean_gs", "Max temperature"="tmax_year","Min temperature"="tmin_year",
                                                                                            "No. days > 20C"="t20_gsdays","No. days > 25C"="t25_gsdays","No. days > 30C"="t30_gsdays",
                                                                                            "Last spring frost" = "lastspfr_doy","First autumn frost" = "firstautfr_doy", "Frost free period" = "frostfree_days",
                                                                                            "Mean flowering temp" = "fl_tmean","Good flowering days"="fl_numday",
                                                                                            "Elevation" = "elevation","Slope" = "slope", "Aspect" = "aspect"), 
                                                    selected="",selectize=FALSE  ) ),
                                    div(style="height: 80px;",
                                        sliderInput("slider3", label=NULL, min = 0, 
                                                    max = 100, value = c(20,50))
                                    )
                   ) # cond panel - search
  ),
  
  dashboardBody(
    # Allow javascript use - requires installation of package V8
    useShinyjs(),
    extendShinyjs(text = jsCode1),
    extendShinyjs(text = jsCode2),
    tags$head(tags$style(HTML(mycss))),
    
    tabItems(
      tabItem(tabName="viticulture",
              
              h3(strong("PLEASE NOTE:"),"THIS WEBSITE IS UNDER DEVELOPMENT THE MAPS DISPLAYED ARE ILLUSTRATIVE ONLY"),
              hr(),
              h4("Viticulture in England & Wales"),
              p("Viticulture in England and Wales has grown considerably over the past 30 years 
                both in terms of quantity and quality, with the hectares of planted vines and the average size of vineyards
                almost doubling in the past 25 years (figs 1a-b). 
                Recent years have seen record levels in the number of wine bottles produced (fig 1c). "),
              
              fluidRow(
                box(width=4, title="Fig 1a: Total hectares of UK vines",
                    plotOutput("fig1a",height="200px") ),
                box(width=4, title="Fig 1b: Average UK Vineyard size",
                    plotOutput("fig1b",height="200px") ),
                box(width=4, title="Fig 1c: UK Wine Production",
                    plotOutput("fig1c",height="200px") )
              ),# fluid row
              
              p("Many vineyard managers and winemakers attribute much of this growth to the warmer
                growing seasons evident over the past 30 years (fig 2a). 
                Low growing season temperatures are widely seen as a key factor restricting 
                viticulture in England and other higher latitude regions. Even a slight increase 
                in mean or aggregated measures of growing season temperature can greatly improve 
                the range of grapevine cultivars that can be grown and the size and quality of harvests."),
              p("Yet vineyard yields in England and Wales fluctuate from one year to another more than
                any other comparable crop (fig 2b). Furthermore, this variability in yields does not show any simple
                relationship to growing season temperature (fig 2c). Instead, vineyard
                yields are affected by a range of climate risks, including weather conditions at key
                periods of crop development; not easily captured by seasonal and regional conditions. "),
              
              fluidRow(
                box(width=4, title="Fig 2a: Mean Growing Season Temperature in Southeast England",
                    plotOutput("fig2a",height="200px") ),
                box(width=4, title="Fig 2b: UK Vineyard Productivity",
                    plotOutput("fig2b",height="200px") ),
                box(width=4, title="Fig 2c: Productivity vs Mean Growing Season Temperature",
                    plotOutput("fig2c",height="200px") )
              ),  # fluid row       
              
              p("There are many strategies available to viticulture to mitigate unfavourable weather and 
                climate risks, including:"),
              p("•	the careful selection of vineyard sites;"),
              p("•	selection of grapevine cultivars displaying appropriate phenology;"),
              p("•	a range of management techniques and interventions that can mitigate the effects of particular weather events."),
              p("All these strategies require an understanding of local vineyard conditions –
                not simply regional or seasonal averages that do not fully reflect the conditions
                experienced within vineyards. ",em("Wineclim"), "estimates these local conditions –
                taking into account variation in topography, sea temperatures, wind and a
                variety of other factors influencing these local conditions. "),
              p("Further Information on UK viticulture:"),
              p(a(href="http://www.ukva.org.uk/","United Kingdom Vineyards Association")),
              p(a(href="http://www.englishwineproducers.co.uk/","English Wine Producers")),
              p(a(href="https://www.food.gov.uk/business-industry/winestandards","Food Standards
                  Agency"))
              ),
      
      tabItem(tabName="wineclim",
              h3(strong("PLEASE NOTE:"),"THIS WEBSITE IS UNDER DEVELOPMENT THE MAPS DISPLAYED ARE ILLUSTRATIVE ONLY"),
              hr(),
              h4("About Wineclim"),
              p("Wineclim models fine-scale microclimate conditions at a spatial resolution of 100 metres."),
              p("Unlike traditional metrics of climate risks, based on growing season aggregate or mean values,
                and/or calculated at low spatial resolution, Wineclim can estimate climate risks to individual vineyards.
                Wineclim calculates more accurate seasonal metrics at a higher spatial resolution, and also permits the
                calculation of climate risks to key stages of vine growth.  Spring frosts after budbreak, or unfavourable
                weather during grapevine flowering, for example, can greatly reduce harvest yields irrespective of weather
                conditions during the rest of the season "),
              p("The model captures the effects of topography on the reception of solar radiation and how other
                features of the landscape including distance from the sea, altitude, and night-time cold-air drainage
                into valleys affect local conditions. The underlying physically-based mechanistic models have been
                developed and empirically validated for locations across southern England."),
              hr(),
              p("Input data used for the calculation of Wineclim microclimate data includes:"),
              p(a(href="http://www.metoffice.gov.uk/climatechange/science/monitoring/ukcp09/download/daily/gridded_daily.html",
                  "•	Met Office: 5km daily gridded temperature data")),
              p(a(href="http://www.metoffice.gov.uk/hadobs/hadisst/index.html","•	Met Office: Hadley
                  Centre sea surface temperatures")),
              p(a(href="http://www.cmsaf.eu/EN/Home/home_node.html", "•	The Satellite Application Facility 
                  on Climate Monitoring (CMSAF): total surface and direct normal irradiation and 
                  cloud albedo data")),
              p(a(href="http://www.esrl.noaa.gov/psd/data/gridded/", "•	Earth System
                  Research Laboratory: wind speed &amp; direction, relative humidity, surface pressure.")),
              p(a(href="http://landsat.usgs.gov/","•	Landsat images: for calculation of surface albedo")),
              p(a(href="http://www2.jpl.nasa.gov/srtm/","•	NASA Shuttle Radar Topograpy Mission: digital terrain map")),
              p("Additional mapping data provided by:"),
              p(a(href="https://www.ordnancesurvey.co.uk/business-and-government/products/finder.html?Licensed%20for=OpenData%20(Free)&amp;withdrawn=on",
                  "•	Ordnance Survey: Boundary-Line for county boundaries data." )),
              p(a(href="https://www.openstreetmap.org/","•	OpenStreetMap: background tile maps")),
              hr(),
              p("Work has been funded by:"),
              p(a(href="http://www.nerc.ac.uk/","•	Natural Environment Research Council")),
              p("•	European Social Fund"),
              hr(),
              p("For further technical details of the models underlying the estimates of microclimate 
                displayed in",em("Wineclim"),"please see the following scientific publications:"),
              p(a(href="http://onlinelibrary.wiley.com/doi/10.1111/gcb.13343/full%20Using%20the%20Site",
                  "Maclean IMD, Suggitt AJ, Wilson RJ , Duffy JP, Bennie JJ (2016) Fine-scale climate change:
                  modelling spatial variation in biologically meaningful rates of warming.
                  Global Change Biology, d.o.i. 10.1111/gcb.13343.")),
              p("Recent publications offering an overview of the effects of changing climatic conditions on UK and world viticulture:"),
              p(a(href="http://onlinelibrary.wiley.com/doi/10.1111/gcb.13406/abstract",
                  "Mosedale JR, Abernethy KE, Smart RE, Wilson RJ, Maclean IMD (2016)
                  Climate change impacts and adaptive strategies: lessons from the grapevine.
                  Global Change Biology, d.o.i. 10.1111/gcb.13406." )),
              p(a(href="http://onlinelibrary.wiley.com/doi/10.1111/ajgw.12215/abstract",
                  "Nesbitt A, Kemp B, Steele C, Lovett A, Dorling S (2016) Impact of recent
                  climate change and weather variability on the viability of UK viticulture –
                  combining weather and climate records with producers' perspectives.
                  Australian Journal of Grape and Wine Research, 22, 324-335." )) 
              ),
      
      tabItem(tabName="explore_summary",
              fluidRow(height=8,
                            box(width=10 ,
                                htmlOutput("sum_maptitle")  
                            )
              ),

              fluidRow( height=260,
                        box(width=10,
                            div(id = "map-container",
                                img(src = "spinner.gif",
                                    id = "map-spinner"),
                                leafletOutput("summary_map")
                            )                       
                        ) , # box
                        
                        box(title="Map information", width=2,
                            # htmlOutput("sum_mapmessage"),
                            sliderInput("sum_visibility","Set transparency:",min=0,max=1,value=0.7),
                            checkboxInput("sum_legend", "Show legend", FALSE) ,
                            checkboxInput("sum_vineyards", "Show vineyards", FALSE),
                            h5(tags$b("Map statistics:")),
                            tableOutput("sum_mapinfo")
                            # , textOutput("sum_maptext")
                        )  # box"
              ) , # fluidRow

              shinyjs::hidden(
                div(id = "sum_cell",
                  fluidRow(height=75,
                         box(title="Distribution of map values (cell value in red)", width=5,
                             div(id = "hist-container",
                                 img(src = "spinner.gif",
                                 id = "hist-spinner"),
                                plotOutput("sum_hist",height="200px") )
                         ),
                         box(title="Historic variation of cell values", width=5,
                             div(id = "ts-container",
                                 img(src = "spinner.gif",
                                     id = "ts-spinner"),
                                 plotOutput("sum_timeseriesplot",height="200px")) 
                             ),
                         box(title="Cell Selected:", width=2,
                                 htmlOutput("sum_celltext") 
                         )
                     )# fluidRow
               ) 
            )
      ), # tabItem    
    
      tabItem(tabName="explore",
              fluidRow(height=8,
                       box(width=10 ,
                           htmlOutput("maptitle")  
                       )
              ),
              
              fluidRow( height=260,
                        box(width=10,
                            div(id = "yrmap-container",
                                img(src = "spinner.gif",
                                    id = "yrmap-spinner"),
                                leafletOutput("map")
                            )                       
                        ) , # box
                        
                        box(title="Map information", width=2,
                            # htmlOutput("mapmessage"),
                            sliderInput("visibility","Set transparency:",min=0,max=1,value=0.7),
                            checkboxInput("legend", "Show legend", FALSE) ,
                            checkboxInput("vineyards", "Show vineyards", FALSE),
                            h5(tags$b("Map statistics:")),
                            tableOutput("mapinfo")
                            # , textOutput("maptext")
                        )  # box"
              ) , # fluidRow
              
              shinyjs::hidden(
                div(id = "cell",
                    fluidRow(height=75,
                             box(title="Distribution of map values (cell value in red)", width=5,
                                 div(id = "hist-container",
                                     img(src = "spinner.gif",
                                         id = "yrhist-spinner"),
                                     plotOutput("hist",height="200px") )
                             ),
                             box(title="Historic variation of cell values", width=5,
                                 div(id = "ts-container",
                                     img(src = "spinner.gif",
                                         id = "yrts-spinner"),
                                     plotOutput("timeseriesplot",height="200px")) 
                             ),
                             box(title="Cell Selected:", width=2,
                                 htmlOutput("celltext") 
                             )
                    )# fluidRow
                ) 
              )
      ), # tabItem explore
      
      tabItem(tabName="search",
              
              fluidRow(height=8,
                       box(width=8 ,
                           htmlOutput("search_maptitle")  
                       ),
                       
                       box(width=2,
                           actionButton(inputId="startsearch",icon("refresh"),label=" CREATE MAP ")
                           )
              ),
              
              fluidRow( height=800,
                        box(  width=10,
                          div(id = "searchmap-container",
                              img(src = "spinner.gif",
                                  id = "searchmap-spinner"),
                              leafletOutput("search_map")
                          )
                        ) , # box
                        
                        box(title="Map controls", width=2,
                            p(),p(),
                            sliderInput("search_visibility","Set transparency:",min=0,max=1,value=0.7),
                            # checkboxInput("legend", "Show legend", FALSE) ,
                            checkboxInput("search_vineyards", "Show vineyards", FALSE)
                        )  # box
                        
              ),  # fluidRow
              
              fluidRow( height=500,
                        box(  title="About Map",width=10,
                              htmlOutput("search_maptext")
                        ) 
              )
      ) # tabItem
      
    ) # tabItems
    ) # dashboardBody
) # dashboardPage


####################################################################################################################################
# Server function
####################################################################################################################################
server<-function(input,output,session) {
  
  show_timeseries<-function(filename,cellnumber){
    # plot timeseries if appropriate or blank figure if not
    if (filename==""){
      years<-c() 
      values<-c()
    }
    if (filename!=""){
      load(filename) # load var.m
      values<-var.m[cellnumber,]
      years<-c(1983:2013)
      #y.labels<-c("2012")
    }
    values.df = data.frame(years,values) 
    maxy<-max(values.df$values,na.rm=TRUE); miny<-min(values.df$values,na.rm=TRUE)
    maxx<-max(values.df$years,na.rm=TRUE); minx<-min(values.df$years,na.rm=TRUE)
    ggplot(values.df, aes(x = values.df$years, y = values.df$values))  + 
      geom_line(color="red") +
      scale_y_continuous(limits=c(miny,maxy),"Value") +
      scale_x_continuous(limits=c(minx,maxx),"Years")
  }
  
  # Function - describing search map - ASSUMES summary stats used
  create_search_map_text<-function(vars,mins,maxs,searchcounty,search_stat) { 
    if (substr(search_stat,1,3)=="mea") statlabel<-"Average (1983-2013)" else
      if (substr(search_stat,1,3)=="max") statlabel<-"Highest (1983-2013)" else
        if (substr(search_stat,1,3)=="min") statlabel<-"Lowest (1983-2013)" 
        output$search_maptext<-renderUI({  # Add more  information such as mean for map etc
          line0<-"<h4>Highlighted locations where:<h4/>"
          line1<-paste("<h4>  1.) ",statlabel,var_longlabel(vars[1]),"between",mins[1],"and",maxs[1],"<h4>")
          if (vars[2]!="") line2<-paste("<h4>  2.) ",statlabel,var_longlabel(vars[2]),"between",mins[2],"and",maxs[2],"</h4>") else line2<-""
          if (vars[3]!="") line3<-paste("<h4>  3.) ",statlabel,var_longlabel(vars[3]),"between",mins[3],"and",maxs[3],"</h4>") else line3<-""
          HTML(paste(line0, line1, line2, line3, sep = '<br/>'))
        })
  } # end search_map_text
  
  #################################################
  # ABOUT PAGES - FIGURES (later make dynamic)
  #################################################  
  output$fig1a<-renderPlot(plot_fig1a(ukstats) + website_theme)
  output$fig1b<-renderPlot(plot_fig1b(ukstats) + website_theme)
  output$fig1c<-renderPlot(plot_fig1c(ukstats) + website_theme)
  
  output$fig2a<-renderPlot(plot_fig2a(seclimate)+website_theme) # Trend in GS mean T for SW
  output$fig2b<-renderPlot(plot_fig2b(ukstats) + website_theme) # Yield per Ha by Year
  output$fig2c<-renderPlot(plot_fig2c(seclimate,ukstats) + website_theme) # Yield per Ha vs Mean GS temp in SE England
  
  #################################################
  # EXPLORE SUMMARY MAP PAGE
  #################################################  
  # Outline of Map
  output$summary_map<-renderLeaflet({
    leaflet() %>% setView(lng = -4.5, lat = 50.75, zoom = 8) %>% 
      addTiles()  %>%  
      clearImages() %>% 
      addScaleBar()
  })
  
  # Observe to change map data if UI changes
  observe({
    priority=1
    # if (!is.null(sum_chosenlayer())) js$showid("map-spinner")  # if required else runs at setup
    leafletProxy("summary_map") %>% 
      #clearPopups() %>%
      clearImages() %>% 
      addRasterImage(sum_chosenlayer(),color=mapcolour(sum_chosenvar(),sum_mapquantiles()), 
                     opacity = sum_visibility(),project=FALSE)
      js$hideid("map-spinner")
  })
      
  # Observe to change map information if map changes
  observe({
    mapstats<-sum_mapstats()
    table.df<-data.frame(statistic=c("Mean","SD","Max","Min","No.Obs"),value=mapstats)
    output$sum_mapinfo<-renderTable({table.df}, include.spacing = c("l"),include.colnames=FALSE,include.rownames=FALSE)
  })
  
  # Observe to change legend if requested
  observe({
    proxy <- leafletProxy("summary_map")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$sum_legend){
      #pal<-pal
      proxy %>% addLegend(position = "bottomright",pal=mapcolour(sum_chosenvar(),sum_mapquantiles()),values=values(sum_chosenlayer()),na.label="Sea" )
    }
  })  
  
  # Observe to add Vineyards
  observe({
    proxy <- leafletProxy("summary_map")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearShapes()
    if (input$sum_vineyards){
      #pal<-pal
      proxy %>% addPolygons(data=vineyards.shp, fillOpacity=0.9,color=("black"),label = ~as.character(vineyard))
    } 
  })  
  
  output$sum_maptitle<- renderUI({ 
    if ((sum_chosenvar()!="") ) {
      text<-paste("<h4>Map of ",var_longlabel(sum_chosenvar())," for ", input$sum_county,stat_longlabel(input$sum_stat),"</h4>",sep="") 
    }
    if ((sum_chosenvar()=="") ) text<-"Select variables in sidebar to display map" 
    HTML(paste("<h4>",text,"</h4>",sep=""))
    })
  
  # Calculate value for mapclick - Display lat lon of mouse click
  observe({  #Observer to show Popups on click
    click <- input$summary_map_click
    if (!is.null(click)) {
      # With colour: output$sum_mapmessage<-renderText(paste("<font color=\" #0000FF \"><b>","CALCULATING CELL VALUES...", "</b></font>",sep=""))
      lon=click$lng
      lat=click$lat
      cellnumber<-lonlat_to_cellnumber(sum_chosenlayer(),lon,lat)
      value = as.numeric(sum_chosenlayer()[cellnumber])
      if (!is.na(value)){
        show("sum_cell") # shinyjs function
        js$showid("hist-spinner")  # Displays loading spinner - hidden in Observe that displays raster
        js$showid("ts-spinner")  # Displays loading spinner - hidden in Observe that displays raster
        
        content <- paste("Value=",round(value,0)," ",var_unit(sum_chosenvar()),sep="")
        proxy <- leafletProxy("summary_map")
        proxy %>% clearPopups() %>% addPopups(lon, lat, popup = content)
        # Shows cell text
        mapstats<-sum_mapstats()
        output$sum_celltext<-renderUI({  # Add more  information such as mean for map etc
        line1<-paste("Latitude:",round(lat, 2))
        line2<-paste("Longitude:",round(lon,2))
        line3<-paste(var_longlabel(sum_chosenvar()),": ",round(value,0),sep="")
        #lineskip=""
        #line3<-paste("Statistics for whole of ",input$sum_county,sep="")
        #line4<-paste("Map values range from",round(mapstats[3],1),"to", round(mapstats[4],1) )
        #line5<-paste("Map mean=",round(mapstats[1],1) )
        #line6<-paste("Standard deviation=", round(mapstats[2],1) )
        #line7<-paste("Number of land cells=", mapstats[5])
        HTML(paste(line1, line2,line3, sep = '<br/>'))
        })
        # Shows histogram
        output$sum_hist<-renderPlot( plothist(getValues(sum_chosenlayer())) + geom_vline(xintercept = value,color = "red", size=1) )
        # Show timeseries if appropriate
        output$sum_timeseriesplot<-renderPlot(show_timeseries(sum_chosendata(),cellnumber))
      } # if
    }# if
    js$hideid("hist-spinner")
    js$hideid("ts-spinner")
  })
  
  # Set variable to be mapped - dependent on tab panel selected - CHECK if works without!
  #sum_chosenvar <- reactive({
  #  input$sum_variable
  #})
  
  # Set variable to be mapped - dependent on tab panel selected - CHECK if works without!
  sum_chosenvar <- reactive({
    vgroup<-isolate({input$sum_vargroup})
    var<-c(input$sum_variable1,input$sum_variable2,input$sum_variable3,input$sum_variable4)
    if (vgroup == "gst") var[1] else 
      if (vgroup == "frost") var[2]  else 
        if (vgroup == "flowering") var[3]  else 
          if (vgroup == "terrain") var[4]  else ""
  })
  
  sum_chosenlayer <- reactive({
    req(input$sum_county); #print(input$sum_county)
    req(input$sum_stat); #print(input$sum_stat)
    req(sum_chosenvar()); #print(sum_chosenvar())
    js$showid("map-spinner")  # Displays loading spinner - hidden in Observe that displays raster
    raster(getmap(input$sum_county,sum_chosenvar(),input$sum_stat))
  }) 
  
  sum_mapquantiles <-reactive({
    req(sum_chosenlayer())
    calc_quantiles(sum_chosenlayer())
  }) 
  
  sum_mapstats <-reactive({
    req(sum_chosenlayer())
    c(cellStats(sum_chosenlayer(),stat='mean',na.rm=TRUE),
      cellStats(sum_chosenlayer(),stat='sd',na.rm=TRUE, asSample=FALSE),
      minValue(sum_chosenlayer()),
      maxValue(sum_chosenlayer()),
      length(which(!is.na(getValues(sum_chosenlayer()))))  )
  })
  
  sum_chosendata<-reactive({
    req(input$sum_county)
    req(sum_chosenvar())
    get_timeseries(input$sum_county,sum_chosenvar())
  }) 
  
  sum_visibility<-reactive({
    req(input$sum_visibility)
    input$sum_visibility
  }) 
  
  
  #################################################
  # EXPLORE YEARS PAGE
  #################################################  
  # Outline of Map
  output$map<-renderLeaflet({
    leaflet() %>% setView(lng = -4.5, lat = 50.75, zoom = 8) %>% 
      addTiles()  %>%  
      clearImages() %>% 
      addScaleBar()
  })
  
  # Observe to change map data if UI changes
  observe({
    priority=1
    leafletProxy("map") %>% 
      #clearPopups() %>%
      clearImages() %>% 
      addRasterImage(chosenlayer(),color=mapcolour(chosenvar(),mapquantiles()), 
                     opacity = visibility(),project=FALSE)
    js$hideid("yrmap-spinner")
  })
  
  # Observe to change map information if map changes
  observe({
    mapstats<-mapstats()
    table.df<-data.frame(statistic=c("Mean","SD","Max","Min","No.Obs"),value=mapstats)
    output$mapinfo<-renderTable({table.df}, include.spacing = c("l"),include.colnames=FALSE,include.rownames=FALSE)
  })
  
  # Observe to change legend if requested
  observe({
    proxy <- leafletProxy("map")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend){
      #pal<-pal
      proxy %>% addLegend(position = "bottomright",pal=mapcolour(chosenvar(),mapquantiles()),values=values(chosenlayer()),na.label="Sea" )
    }
  })  
  
  # Observe to add Vineyards
  observe({
    proxy <- leafletProxy("map")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearShapes()
    if (input$vineyards){
      #pal<-pal
      proxy %>% addPolygons(data=vineyards.shp, fillOpacity=0.9,color=("black"),label = ~as.character(vineyard))
    } 
  })  
  
  output$maptitle<- renderUI({ 
    if ((chosenvar()!="") ) {
      text<-paste("<h4>Map of ",var_longlabel(chosenvar())," for ", input$county,stat_longlabel(input$stat),"</h4>",sep="") 
    }
    if ((chosenvar()=="") ) text<-"Select variables in sidebar to display map" 
    HTML(paste("<h4>",text,"</h4>",sep=""))
  })
  
  # Calculate value for mapclick - Display lat lon of mouse click
  observe({  #Observer to show Popups on click
    click <- input$map_click
    if (!is.null(click)) {
      lon=click$lng
      lat=click$lat
      cellnumber<-lonlat_to_cellnumber(chosenlayer(),lon,lat)
      value = as.numeric(chosenlayer()[cellnumber])
      if (!is.na(value)){
        show("cell") # shinyjs function
        js$showid("yrhist-spinner")  # Displays loading spinner - hidden in Observe that displays raster
        js$showid("yrts-spinner")  # Displays loading spinner - hidden in Observe that displays raster
        
        content <- paste("Value=",round(value,0)," ",var_unit(chosenvar()),sep="")
        proxy <- leafletProxy("map")
        proxy %>% clearPopups() %>% addPopups(lon, lat, popup = content)
        # Shows cell text
        mapstats<-mapstats()
        output$celltext<-renderUI({  # Add more  information such as mean for map etc
          line1<-paste("Latitude:",round(lat, 2))
          line2<-paste("Longitude:",round(lon,2))
          line3<-paste(var_longlabel(chosenvar()),": ",round(value,0),sep="")
          HTML(paste(line1, line2,line3, sep = '<br/>'))
        })
        # Shows histogram
        output$hist<-renderPlot( plothist(getValues(chosenlayer())) + geom_vline(xintercept = value,color = "red", size=1) )
        # Show timeseries if appropriate
        output$timeseriesplot<-renderPlot(show_timeseries(chosendata(),cellnumber))
      } # if
    }# if
    js$hideid("yrhist-spinner")
    js$hideid("yrts-spinner")
  })
  
  # Set variable to be mapped - dependent on tab panel selected - CHECK if works without!

  chosenvar <- reactive({
    vgroup<-isolate({input$vargroup})
    var<-c(input$variable1,input$variable2,input$variable3,input$variable4)
    if (vgroup == "gst") var[1] else 
      if (vgroup == "frost") var[2]  else 
        if (vgroup == "flowering") var[3]  else 
          if (vgroup == "terrain") var[4]  else ""
  })
  
  chosenlayer <- reactive({
    req(input$county); #print(input$county)
    req(input$stat); #print(input$stat)
    req(chosenvar()); #print(chosenvar())
    js$showid("yrmap-spinner")  # Displays loading spinner - hidden in Observe that displays raster
    raster(getmap(input$county,chosenvar(),input$stat))
  }) 
  
  mapquantiles <-reactive({
    req(chosenlayer())
    calc_quantiles(chosenlayer())
  }) 
  
  mapstats <-reactive({
    req(chosenlayer())
    c(cellStats(chosenlayer(),stat='mean',na.rm=TRUE),
      cellStats(chosenlayer(),stat='sd',na.rm=TRUE, asSample=FALSE),
      minValue(chosenlayer()),
      maxValue(chosenlayer()),
      length(which(!is.na(getValues(chosenlayer()))))  )
  })
  
  chosendata<-reactive({
    req(input$county)
    req(chosenvar())
    get_timeseries(input$county,chosenvar())
  }) 
  
  visibility<-reactive({
    req(input$visibility)
    input$visibility
  })   
  
  
  #################################################
  # SEARCH MAPS PAGE
  #################################################
  output$search_maptitle<- renderUI({ 
    HTML(paste("<h4>","Select up to three variables and values in sidebar then click 'Create Map'","</h4>",sep=""))
  })
  
  # Modify slider input range according to variables chosen
  observe({ 
    minmax1 <- set_min_max(input$search1)
    minmax2 <- set_min_max(input$search2)
    minmax3 <- set_min_max(input$search3)
    updateSliderInput(session, "slider1", min = minmax1[[1]], max=minmax1[[2]])
    updateSliderInput(session, "slider2", min = minmax2[[1]], max=minmax2[[2]])
    updateSliderInput(session, "slider3", min = minmax3[[1]], max=minmax3[[2]])
  }) # observe 
  
  # Display basic map 
  output$search_map<-renderLeaflet({
    leaflet() %>% setView(lng = -4.5, lat = 50.75, zoom = 8) %>% 
      addTiles()  %>%  
      clearImages() %>% 
      addScaleBar()
  })
  
  # Observe to change data if UI changes
  observeEvent(input$startsearch,{
    js$showid("searchmap-spinner")  # Displays loading spinner - hidden in Observe that displays raster
    
    vars<-c(input$search1,input$search2,input$search3)
    mins<-c(min(input$slider1),min(input$slider2),min(input$slider3))
    maxs<-c(max(input$slider1),max(input$slider2),max(input$slider3))
    
    # Observe to add Vineyards
    observe({
      proxy <- leafletProxy("search_map")
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearShapes()
      if (input$search_vineyards){
        proxy %>% addPolygons(data=vineyards.shp, fillOpacity=0.75,color=("black"),label = ~as.character(vineyard))
      } 
    })  
    
    #searchmap<-create_search_map(vars,mins,maxs,input$searchcounty,input$search_stat)
    leafletProxy("search_map") %>% 
      clearPopups() %>%
      clearImages() %>% 
      addRasterImage(create_search_map(vars,mins,maxs,input$search_county,input$search_stat), 
                     color=colorBin(c("grey","green"),domain=c(0,1),bins=2),opacity = search_visibility(),project=FALSE)   
    
      create_search_map_text(vars,mins,maxs,input$search_county,input$search_stat)
      js$hideid("searchmap-spinner")
    }) 
    
  
  search_visibility<-reactive({
    req(input$search_visibility)
    input$search_visibility
  })
  
  
 
} # server

shinyApp(ui=ui, server=server) 
