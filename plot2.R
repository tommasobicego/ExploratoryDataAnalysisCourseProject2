# Have total emissions from PM2.5 decreased in 
# the Baltimore City, Maryland (fips == 24510) 
# from 1999 to 2008? Use the base plotting system 
# to make a plot answering this question.

plot2 <- function(){
  library(data.table)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  dt <- as.data.table(df)
  dt <- dt[dt$fips == 24510,]
  dt[,sum := c(sum(Emissions)),by=year]
  #   dt <- transform(dt, year = factor(year))
  
  dtK <- dt[,c("sum","year"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  print(dtK)
  png(file="plot2.png")#,width=480,height=480)
  plot(x=dtK$year, y=dtK$sum, xlab="Year", ylab="tons of PM2.5 Total Emission", main="PM2.5 Total Emission (in Baltimore City, Maryland)/Year", pch=16)
  
  model <- lm(dtK$sum ~ dtK$year)
  abline(model, lwd=2)
  dev.off()
}