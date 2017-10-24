
##Creating subfolders
#Storing logos
dir.create(file.path("logos"), showWarnings = FALSE)

#Storing tmp data - after every download, I save it
dir.create(file.path("tmpData"), showWarnings = FALSE)

#Storing data
dir.create(file.path("data"), showWarnings = FALSE)

#VerificationResults
dir.create(file.path("VerificationResults"), showWarnings = FALSE)

execute<-function(country,what, verificationFileP, sourceFileP)
{
  
  if (country=="Australia") {
    
   localTime<-Sys.getlocale("LC_TIME")
   
   #local time to Australiaan
   Sys.setlocale("LC_TIME", "English") 
  
  
  }
   
   source<-readingSource(country,verificationFileP)
   
   
   if (what=="only compare") {

       verificationFile<-as.data.frame(read_excel(path=sourceFileP))
   
     }
    else {
          insert(outputConsole,"Downloading started....")
          downloadData(country,source)
          path<-paste("./data/",country,"_online.xlsx",sep="")
          verificationFile<-as.data.frame(read_excel(path=path)) }
 

    insert(outputConsole,"Joing and comparing started...")
    joinAndCompare(verificationFile,source,country)
  

  
    #restore locale
    Sys.setlocale("LC_TIME", localTime) 
  
  
  
}