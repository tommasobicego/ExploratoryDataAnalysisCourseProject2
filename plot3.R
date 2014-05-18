# Of the four types of sources indicated by the 
# type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer 
# this question.

plot3 <- function(){
  library(ggplot2)
  library(data.table)
  df <- readRDS("exdata/summarySCC_PM25.rds")
  df <- df[df$fips == 24510,]
  dt <- as.data.table(df)
  dt[,somma := c(sum(Emissions)),by=list(year,type)]
  df <- as.data.frame(dt)
  head(df, 20)
  
  
  dtK <- dt[,c("somma","year","type"), with=FALSE]
  dtK <- unique(dtK)
  unique(dtK) 
  
  
  # qplot(x=year, y=somma, data=dtK, color=type, xlab="Year", 
  #       ylab="PM2.5 Total Emission", geom=c("point","smooth"), method="lm", facets = .~type, ncol=2  )
  p <- ggplot(dtK, aes(year, somma)) + geom_point(aes(color=type)) + facet_grid(. ~ type) + geom_smooth(method="lm", aes(color=type)) + labs(title="PM2.5 Total Emission/Year (by type)") + labs(x="Year", y="tons of PM2.5 Total Emission")
  ggsave(filename="plot3.png", plot=p)
}
