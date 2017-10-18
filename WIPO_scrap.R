GettingZips <- function(remDr, regNo, path=NULL) {
  
  regNo<-gsub(",","",regNo)
  regNo<-gsub("/","",regNo)
  regNo<-gsub("-","",regNo)
  regNo<-gsub(" ","",regNo)
  regNo<-trimws(regNo)
  
  print(regNo)

  urlS<-"http://www.wipo.int/romarin/search.xhtml"
  remDr$navigate(urlS)
  
  Sys.sleep(1)
  
  try(search<-remDr$findElement(using = "name", value = "j_idt66:search-cs:0:j_idt137"), silent = TRUE)
  try(search$clearElement(),silent = TRUE)
  try(search$sendKeysToElement(list(regNo, "\uE007")),silent = TRUE)
  #try(search$sendKeysToElement(list(keys.enter)),silent = TRUE)
  Sys.sleep(2)
  
  #New page - download xml
  try(download<-remDr$findElements(using = "xpath", value = "//a[contains(@title,'Download')]"), silent = TRUE)
  
  try(download[[1]]$clickElement(), silent = TRUE)
  Sys.sleep(3)
  file.rename('C:/Users/Godnov/Downloads/romarin-list.zip',paste('C:/Users/Godnov/Downloads/romarin',regNo,'.zip',sep=""))
  file.copy(paste('C:/Users/Godnov/Downloads/romarin',regNo,'.zip',sep=""),paste(getwd(),"/WipoZips/romarin",regNo,".zip",sep=""), overwrite = TRUE)
  if (file.exists(paste('C:/Users/Godnov/Downloads/romarin',regNo,'.zip',sep="")) && 
      file.exists(paste(getwd(),"/WipoZips/romarin",regNo,".zip",sep=""))) {
    
      file.remove(paste('C:/Users/Godnov/Downloads/romarin',regNo,'.zip',sep=""))
      final<-1 
  } else{final<-0}
  return(final)
}

driver <-try(rsDriver(verbose = FALSE, port = 4445L), silent = TRUE)
remDr <- try(driver[["client"]], silent = TRUE)

try(remDr$open(silent = TRUE), silent = TRUE)

dataWipo<-as.data.frame(read_excel(path="./InputData/WIPO.xlsx"))
dataWipo<-dataWipo[,4:5]
regNo<-data.frame(filter(dataWipo,`APPLICATION NO`==`REGISTRATION NO`)%>%select(`REGISTRATION NO`))
regNoList<-as.list((regNo$REGISTRATION.NO))

not<-list()
yes<-list()

for (i in 1:length(regNoList))
{ 
  print(i)
  status<-GettingZips(remDr,regNoList[[i]]) 
  
  if (status==0)
  {  not[[i]]<-regNoList[[i]]} else {yes[[i]]<-regNoList[[i]]}  

} 

not<-as.data.frame(unlist(not))
colnames(not)<-"RegNo"

yes<-as.data.frame(unlist(yes))
colnames(yes)<-"RegNo"

NoImport<-as.data.frame(filter(dataWipo,`APPLICATION NO`!=`REGISTRATION NO`)%>%select(`REGISTRATION NO`))
colnames(NoImport)<-"RegNo"


#noImport<-rbind(not,NoImport)
write.xlsx(not, file="NotFoundInRoarin.xlsx")
write.xlsx(NoImport, file="ProblemWithAppNo.xlsx")

#write.xlsx(noImport,file="NoWipoDowload.xlsx")
write.xlsx(yes,file="WipoDowloaded.xlsx")


#######Unzip files
fileson <- list.files(path = "./WipoZips/",
                      pattern = "*.zip",
                      full.names = FALSE)

lapply(fileson, function(x) {
  
  x<-paste("./WipoZips/",x,sep="")
  
  if (file.exists(x))
  {   
    print(x)
    unzip(zipfile=x,exdir = "./WipoZips")
  }
})




