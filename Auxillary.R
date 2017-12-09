readingSource <- function(which = "Australia", filev) {
  # if (which=="Australia") {
  
  #source<-read_excel(path="./Inputdata/testArgentina.xlsx", skip=1)
  
  source <- read_excel(path = filev, skip = 1)
  #Selecting all rows with recordID
  recordID <-
    source %>% filter(grepl("Record", Trademark)) %>% dplyr::rename(Record_ID =
                                                                      Trademark) %>%
    select(Record_ID)
  
  recordID <-
    recordID[rep(seq_len(nrow(recordID)), each = 3), "Record_ID"]
  
  source <- cbind(recordID, source)
  
  source$Record_ID <- gsub("Record ID:", "", source$Record_ID)
  
  source <- source %>% filter(!grepl("Record", Trademark))
  
  source$`Application no.` <- gsub("/", "", source$`Application no.`)
  source$`Application no.` <- gsub(",", "", source$`Application no.`)
  #cleaning appno
  
  rowsPerAppNum<-source%>%select(`Application no.`,Record_ID)%>%
    group_by(`Application no.`,Record_ID)%>%dplyr::mutate(Number=n())%>%
    select(Number,Record_ID,`Application no.`)%>%filter(Number==1)
  
  if (nrow(rowsPerAppNum)>0)
  {insert(outputConsole,paste('There is a problem with data. There are more than 1 AppNo per RecordID!',as.character(rowsPerAppNum)))}
  
  return(source)
  
  #  }
}

downloadData <- function(Country,
                         source,
                         from = NULL,
                         to = NULL) {
  #Country="Argentina"
  
  dir.create(file.path("./logos"), showWarnings = FALSE)
  
  dir.create(file.path(paste("./tmpData/", Country, sep = "")), showWarnings = FALSE)
  
  unlink(paste("./tmpData/", Country, "/*.*", sep = ""))
  
  if (Country=="BENELUX") {
    sourceAPP <- unique(source$`Registration no.`)
  }else {
    sourceAPP <- unique(source$`Application no.`)
    }
  
  sourceAPP <- na.omit(sourceAPP)
  
  if (is.null(from) | is.null(to))
  {
     from <- 1
    to <- length(sourceAPP)
  }
  
  
  
  
  appNo <- as.list(sourceAPP)[from:to]
  
  #If country is US, start Selenium
  

  
  for (i in 1:length(appNo)) {
    appNumber <- appNo[[i]]
    appNumber <- gsub(",", "", appNumber)
    appNumber <- gsub("/", "", appNumber)
    appNumber <- gsub("-", "", appNumber)
    appNumber <- trimws(gsub("\\.", "", appNumber))
    
    cat(paste("Downloading Trademark ", appNumber, "...\n", sep = ""))
    
    if (Country == "Australia") {
      try(scrapData <- AUSScrap(appNumber), silent = TRUE)
    } else if (Country == "USA") 
    {
      localSys <- Sys.getlocale("LC_ALL")
      
      Sys.setlocale("LC_ALL", "english")
      
      try(scrapData <- USScrap(appNumber), silent = FALSE)
      Sys.sleep(1)
      
      Sys.setlocale("LC_ALL", localSys)
      
    } else if (Country == "Argentina")
    {
      try(scrapData <- ARScrap(appNumber), silent = FALSE)
      
    }else if (Country == "BENELUX")
    {
      try(scrapData <- BXScrap(appNumber), silent = FALSE)
      
    }
    
    if (exists("scrapData") && nrow(scrapData)==1) {
      #Saving into tempData
      
      
      if (Country=="BENELUX") {
        
        appNumber<-as.vector(scrapData$`Application no.`)
      }
      
      insert(outputConsole,
             paste("Downloading Trademark ", appNumber))

      
      filenm = paste("./tmpData/", Country, "/", appNumber, ".Rda", sep = "")
      
     
      
      save(scrapData, file = filenm)
      try(rm("scrapData"), silent = TRUE)
      
    }
    
    try(rm("scrapData"), silent = TRUE)
    
  }
  
  

  
  #Country<-"BENELUX"
  path <- paste("./tmpData/", Country, "/", sep = "")
  filename <- paste("./data/", Country, "_online.xlsx", sep = "")
  
  
  
  
  #gather data
  fileson <- list.files(path = path,
                        pattern = "*.Rda",
                        full.names = FALSE)
  fileson <- gsub(".Rda", "", fileson)
  data_s = lapply(fileson, function(x) {
    filenm = paste(path, as.character(x), ".Rda", sep = "")
    
    if (file.exists(filenm))
    {
      load(filenm)
      if (class(scrapData)=="data.frame") {
        return(scrapData) }
    }
  })
  
  
  localTime <- Sys.getlocale("LC_TIME")
  
  #local time to Australiaan
  Sys.setlocale("LC_TIME", "German")
  
  dataTmp = as.data.frame(try(do.call(rbind.fill, data_s)))
  
  
  #removing dummy dates
  dataTmp[dataTmp  ==  "01.01.1800"]  <-  NA
  
  #Removing empty Appno
  dataTmp<-dataTmp[!is.na(dataTmp$`Application no.`),]
  dataTmp<-dataTmp[dataTmp$`Application no.`!="000000",]
  
  dataTmp<-dataTmp[!duplicated(dataTmp),]
  
  
  
  
  #changing format
  Sys.setlocale("LC_TIME", localTime)
  
  write.xlsx2(
    dataTmp,
    file = filename,
    sheetName  =  "verification",
    row.names = FALSE
  )
  
  insert(
    outputConsole,
    paste("Online data are scraped and stored in ", filename)
  )
  galert(
    paste("Online data are scraped and stored in ", filename),
    parent = w
  )
  
  rm(list = ls())
}