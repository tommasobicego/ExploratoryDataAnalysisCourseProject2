# Across the United States, how have emissions 
# from coal combustion-related sources changed 
# from 1999–2008?

plot4 <- function(){
  library(data.table)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  
  dfIni <- readRDS("exdata/Source_Classification_Code.rds")
  dfIni <- dfIni[grepl("coal", dfIni$Short.Name,  ignore.case = TRUE),]
  dfIni <- dfIni[grepl("comb", dfIni$Short.Name,  ignore.case = TRUE),]
  
  print(length(dfIni$SCC))

  df <- df[df$SCC %in% dfIni$SCC ,]
  
  dt <- as.data.table(df)
  dt[,sum := c(sum(Emissions)),by=year]
  #   dt <- transform(dt, year = factor(year))
  
  dtK <- dt[,c("sum","year"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  print(dtK)
  png(file="plot4.png",width=480,height=480)
  
  plot(x=dtK$year, y=dtK$sum, xlab="Year", 
       ylab="tons of PM2.5 Total Emission", main="PM2.5 Total Emission/Year related to Coal Combustion sources", pch=16)
  
  model <- lm(dtK$sum ~ dtK$year)
  abline(model, lwd=2)
  dev.off()
}


# for(sd in dfIni$SCC){
#   #     print(sd)
#   rbind(dfNew,df[df$SCC == sd, ]) -> dfNew
#   #     dfNew <- df[df$SCC == sd, ]
# }