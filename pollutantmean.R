pollutantmean <- function(directory,pollutant,id=1:332) {

id<-formatC(id, width = 3, format = "d", flag = "0") ##file names in 3 digit
old.path <- getwd()
setwd(paste(getwd(),"/",directory,sep=""))
combined_data<-c()

for (i in id) 
{
data.id<- read.csv(paste(i,".csv",sep=""))
combined_data<-rbind(combined_data,data.id)
}

setwd(old.path)
pollutant_data <- combined_data[,pollutant]
pollutant_data <- pollutant_data[!is.na(pollutant_data)]	
return(mean(pollutant_data))


}