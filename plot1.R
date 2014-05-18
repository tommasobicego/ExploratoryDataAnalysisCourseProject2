# Have total emissions from PM2.5 decreased in the United 
# States from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.


plot1 <- function(){
  library(data.table)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  dt <- as.data.table(df)
  dt[,sum := c(sum(Emissions)),by=year]
  #   dt <- transform(dt, year = factor(year))
  
  dtK <- dt[,c("sum","year"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  print(dtK)
  
  png(file="plot1.png",width=480,height=480)
  plot(x=dtK$year, y=dtK$sum, xlab="Year", 
       ylab="tons of PM2.5 Total Emission", main="PM2.5 Total Emission/Year", pch=16)

  model <- lm(dtK$sum ~ dtK$year)
  abline(model, lwd=2)
  dev.off()
}


# fips      SCC Pollutant Emissions  type year     sum    ~