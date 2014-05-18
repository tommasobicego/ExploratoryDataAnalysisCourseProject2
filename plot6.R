# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

plot6 <- function(){
  library(data.table)
  library(ggplot2)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  
  dfIni <- readRDS("exdata/Source_Classification_Code.rds")
  write.csv(dfIni, "exdata/Source_Classification_Code.csv")
  dfIni <- dfIni[grepl("Motorcycles", dfIni$Short.Name,  ignore.case = TRUE),]
  
  print(length(dfIni$SCC))
  
  df <- df[df$SCC %in% dfIni$SCC ,]
  
  dt <- as.data.table(df)
  dt <- dt[dt$fips == "06037" | dt$fips == 24510,]
  dt[,sum := c(sum(Emissions)),by=list(year, fips)]
  
  dt[,countr := {ifelse(fips == "06037", "Los Angeles County, California", "Baltimore City, Maryland")}]
  
  #   dt <- transform(dt, year = factor(year))
  
  dtK <- dt[,c("sum","year","countr"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  print(dtK)
  
  #   qplot(x=year, y=sum, data=dtK, color=fips, xlab="Year", 
  #                ylab="PM2.5 Total Emission", geom=c("point","smooth"), method="lm" )
  p <- ggplot(dtK, aes(year, sum)) + geom_point(aes(color=countr)) + facet_grid(. ~ countr) + geom_smooth(method="lm", aes(color=countr)) + labs(title="PM2.5 Total Emission/Year country comparison") + labs(x="Year", y="tons of PM2.5 Total Emission")
  ggsave(filename="plot6.png", plot=p)
  
}