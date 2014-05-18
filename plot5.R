# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 

plot5 <- function(){
  library(data.table)
  #   library(datasets)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  
  dfIni <- readRDS("exdata/Source_Classification_Code.rds")
  #   write.csv(dfIni, "exdata/Source_Classification_Code.csv")
  dfIni <- dfIni[grepl("Motorcycles", dfIni$Short.Name,  ignore.case = TRUE),]
  
  print(length(dfIni$SCC))
  
  df <- df[df$SCC %in% dfIni$SCC ,]
  
  dt <- as.data.table(df)
  dt <- dt[dt$fips == 24510,]
  dt[,sum := c(sum(Emissions)),by=year]
  #   dt <- transform(dt, year = factor(year))
  
  dtK <- dt[,c("sum","year"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  print(dtK)
  png(file="plot5.png",width=480,height=480)
  
  plot(x=dtK$year, y=dtK$sum, xlab="Year", 
       ylab="tons of PM2.5 Total Emission", main="PM2.5 Total Emission/Year related to motor vehicles sources", pch=16)
  
  model <- lm(dtK$sum ~ dtK$year)
  abline(model, lwd=2)
  dev.off()
}