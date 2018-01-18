notIn <- function(fromAgent, allData, what = 1) {
  if (what == 1) {
    VerJoin <- "Application no."
    DesJoin <- "Application no."
    
    notInA <-
      anti_join(fromAgent, allData, by = setNames(DesJoin, VerJoin))
    
    VerJoin1 <- "Registration no."
    DesJoin1 <- "Registration no."
    
    notInB <-
      anti_join(fromAgent, allData, by = setNames(DesJoin1, VerJoin1))
    
    if (length(notInA) == 0) {
      notInA$Eval <- NA
    }
    
    if (length(notInB) == 0) {
      notInB$Eval <- NA
    }
    if (nrow(notInB) <= nrow(notInA)) {
      notIn <- notInB
    } else
    {
      notIn <- notInA
      
    }
    
    
    
    
    
  }  else{
    VerJoin <- "Application no."
    DesJoin <- "Application no."
    
    notInA <-
      anti_join(fromAgent, allData, by = setNames(DesJoin, VerJoin))
    
    
  }
  
  allData <- rbind.fill(allData, notIn)
  
  allData[is.na(allData$Eval), "Eval"] <- 2
  
  return(allData)
}


GetAllData <-
  function(verificationFile,
           destinationFile,
           Country,
           VerJoin,
           DesJoin) {
    colnames <- names(destinationFile)
    
    verificationFile$`Application no.` <-
      trimws(verificationFile$`Application no.`)
    destinationFile$`Application no.` <-
      trimws(destinationFile$`Application no.`)
    checkDest <- destinationFile
    forExcelComparison <- verificationFile
    #Add additional column with source type
    destinationFile <-
      cbind(data.frame(sourceType = rep(
        c("client", "agent"), nrow(destinationFile) / 2
      )), destinationFile)
    
    
    colnames(destinationFile) <-
      gsub(" ", "__", names(destinationFile))
    
   
    
    if (Country == "HK") {
      destinationFile$Application__no. <-
        gsub("[^[:alnum:]]", "", destinationFile$Application__no.)
      destinationFile$Registration__no. <-
        gsub("[^[:alnum:]]", "", destinationFile$Registration__no.)
      verificationFile$`Registration no.` <-
        gsub("[^[:alnum:]]", "", verificationFile$`Registration no.`)
    }
    
    
    if (Country == "Macao") {
      verificationFile$`Application no.` <-
        gsub("\\D", "", verificationFile$`Application no.`)
    }
    

    if (Country == "Japan") {
      verificationFile$`Application no.` <-
        gsub("\\D", "", verificationFile$`Application no.`)
      destinationFile$Registration__no. <-
        gsub("\\D", "", destinationFile$Registration__no.)
      verificationFile$`Registration no.` <-
        gsub("\\D", "", verificationFile$`Registration no.`)
      
        try(verificationFile$`Application date`<-as.character(format(as.Date(verificationFile$`Application date`,"%Y/%m/%d"),"%d.%m.%Y")))
        try(verificationFile$`Registration date`<-as.character(format(as.Date(verificationFile$`Registration date`,"%Y/%m/%d"),"%d.%m.%Y")))
        try(verificationFile$`Next renewal date`<-as.character(format(as.Date(verificationFile$`Next renewal date`,"%Y/%m/%d"),"%d.%m.%Y")))

    }
    
    
    #removing leading zeros for EU
    if (Country == "EU" || Country == "Macao") {
      destinationFile$Application__no. <-
        gsub("(?<![0-9])0+",
             "",
             destinationFile$Application__no.,
             perl = TRUE)
    }
    
    
    verificationFile <-
      cbind(data.frame(sourceType = rep(
        "verification", nrow(verificationFile)
      )), verificationFile)
    
    
    #i will create column names with double__. Before exporting, I will switch back
    namesV <- names(verificationFile)
    colnames(verificationFile) <- gsub(" ", "__", namesV)
    
    #Based on VerJoin and DesJoin I must prepare columns
    
   
    destinationRecordName <-
      unique(destinationFile[, c("Record_ID", "Trademark", gsub(" ", "__", DesJoin))])
    
     if (VerJoin!=DesJoin) {
      
      if (VerJoin=="Registration no.") {
        
        destinationRecordName$Registration__no.<-NA
       
        
      } else {
        
        destinationRecordName$Application__no.<-NA
        
      }
      
      
    }
    
    if (DesJoin == "Application no.") {
      destinationRecordName$Application__no. <-
        gsub("\\D", "", destinationRecordName$Application__no.)
    }
    else {
      destinationRecordName$Registration__no. <-
        gsub("\\D", "", destinationRecordName$Registration__no.)
    }
    
    
    
    #I'll take the shorter version of Trademark name
    
    
    
    if (DesJoin == "Application no.") {
      destinationRecordName <-
        as.data.table(destinationRecordName[!is.na(destinationRecordName$Application__no.), ])
      destinationRecordName <-
        destinationRecordName[destinationRecordName[, .I[Trademark == max(Trademark)], by =
                                                      Application__no.]$V1]
    } else {
      destinationRecordName <-
        as.data.table(destinationRecordName[!is.na(destinationRecordName$Registration__no.), ])
      destinationRecordName <-
        destinationRecordName[destinationRecordName[, .I[Trademark == max(Trademark)], by =
                                                      Registration__no.]$V1]
    }
    
    
    destinationRecordName <- as.data.frame(destinationRecordName)
    
    
    v1 <- verificationFile
    
    verificationFile <- v1
    
    if (Country == "BENELUX") {
      
      if (VerJoin=="Application no.") {
      verificationFile$Application__no. <-
        as.numeric(verificationFile$Application__no.)
      
      verificationFile$AppOLD <- v1$Application__no.
      destinationRecordName$Application__no. <-
        as.numeric(destinationRecordName$Application__no.) 
      } else
      {
        
        verificationFile$Registration__no. <-
          as.numeric(verificationFile$Registration__no.)
        
        verificationFile$AppOLD <- v1$Registration__no.
        destinationRecordName$Application__no. <-
          as.numeric(destinationRecordName$Registration__no.) 
        }
      }
    
    
     
     verificationFile <-
      inner_join(destinationRecordName,
                 verificationFile,
                 by =setNames(gsub(" ","__",DesJoin),gsub(" ","__",VerJoin)) ,
                 copy = TRUE)
    
    
    
    if ("Trademark.y" %in% colnames(verificationFile)) {
      verificationFile <-
        dplyr::rename(verificationFile, Trademark = Trademark.y)
    }
    
    if (Country == "BENELUX") {
      verificationFile <-
        dplyr::rename(verificationFile, Application__no1. = Application__no.)
      verificationFile <-
        dplyr::rename(verificationFile, Application__no. = AppOLD)
      
    }
    
    
    colnames(verificationFile)[1] <- c("Record_ID")
    
    #creating same column structure in verification file
    if (Country == "BENELUX")
    {
      tempVer <-
        verificationFile[intersect(c(names(destinationFile), "Application__no1."), names(verificationFile))]
    } else {
      tempVer <-
        verificationFile[intersect((names(destinationFile)), names(verificationFile))]
    }
    tempCol <-
      destinationFile[destinationFile$Record_ID == -99, setdiff(names(destinationFile), names(verificationFile))] #that never happens, but I get the right datatypes
    
    
    #same length
    verificationFile <-
      cbind.fill(tempVer, tempCol, stringAsFactor = FALSE)
    colnames(verificationFile) <- c(names(tempVer), names(tempCol))
    
    
    
    
    #factor to columns Due to special names it doesn't work with dplyr
    indx <- sapply(verificationFile, is.factor)
    verificationFile[indx] <-
      lapply(verificationFile[indx], function(x)
        (as.character(x)))
    
    indx <- sapply(verificationFile, is.POSIXct)
    verificationFile[indx] <-
      lapply(verificationFile[indx], function(x)
        (format(x, "%d.%m.%Y")))
    
    
    
    #selecting order of columns as in destination file to perform union
    if (Country == "BENELUX") {
      verificationFile <-
        verificationFile[, c(names(destinationFile), "Application__no1.")]
      
    } else {
      verificationFile <- verificationFile[, names(destinationFile)]
    }
    
    #to be on the safe side, I convert Registration_no. to character
    verificationFile$Registration__no. <-
      as.character(verificationFile$Registration__no.)
    
    
    
    #Performing Union
  
    
    allData <- union_all(destinationFile, verificationFile)
    
    
    #Excluding elements, that are not in verification file
    #Those will be managed with erros
    #recordID to numeric
    allData$Record_ID <- as.integer(allData$Record_ID)
    # if (Country == "BENELUX") {
    #   allData <-
    #     allData[(allData$Record_ID %in% verificationFile$Record_ID),]
    #   
    # } else {
    #   allData <-
    #     allData[(allData$Application__no. %in% verificationFile$Application__no.),]
    # }
    
    
   allData <-
        allData[(allData$Record_ID %in% verificationFile$Record_ID),]
   
    #Sorting and adding Trademark
    #allData$Application__no. <- as.integer(allData$Application__no.)
    
   
  
    allData <- allData %>% dplyr::arrange(Record_ID, sourceType)
    
    allData$Image <- NA
    
    #Rearrange columns
    allData <-
      allData[, c(2, 1, 3:9, ncol(allData), 10:(ncol(allData) - 1))]
    
    #Adding dummy column  - if record should be evaluated in excel
    allData$Eval <- 1
    
    colnames(allData) <- gsub("__", " ", names(allData))
    
    # add all rows from source, which are not included - errors - check them according to record_id
    destinationFile$Record_ID <-
      as.integer(destinationFile$Record_ID)
    verificationFile$Record_ID <-
      as.integer(verificationFile$Record_ID)
    
    error <-
      anti_join(destinationFile, verificationFile, by = "Record_ID")
    
    if ("sourceType" %in% colnames(error))
    {
      error <- error %>% select(-sourceType)
    }
    #Eclude again this from allData
    
    error <-
      cbind(data.frame(sourceType = rep(c("client", "agent"), nrow(error) / 2)), error)
    colnames(error) <- gsub("__", " ", names(error))
    
    indx <- sapply(error, is.factor)
    error[indx] <- lapply(error[indx], function(x)
      (as.character(x)))
    
    if (nrow(error) > 0) {
      diffnames <- as.list(setdiff(names(allData), names(error)))
      
      for (i in 1:length(diffnames))  {
        x <- diffnames[[i]]
        if (nrow(error) == 0) {
          error <- cbind(error, x)
          
        } else {
          errordf <- data.frame(rep(NA, nrow(error)))
          
          colnames(errordf) <- x
          
          error <- cbind(error, errordf)
        }
      }
      
      
      error$Eval <- 0
      
      error <- error[, names(allData)]
      
      error$Record_ID <- as.integer(error$Record_ID)
      
      
    }
    #Alldata application number to string as errors have app number with this type
    allData$`Application no.` <-
      as.character(allData$`Application no.`)
    
    if (Country == "BENELUX") {
      allData <- allData %>% dplyr::select(-(`Application no1.`))
      
    }
    
    #Due to possible appno change, I have to overwrite some of the appno
    allData$`Application no.`[!is.na(allData$`Application no. assigned upon renewal`)] <-
      as.character(allData$`Application no. assigned upon renewal`[!is.na(allData$`Application no. assigned upon renewal`)])
   
    
    if (nrow(error) > 0) {
      allData <- union_all(allData, error)
      
    }
    
    #moving client's comment at the end
    comment <- which(colnames(allData) == "Client's comment")
    
    if (is.numeric(comment)) {
      col_idx <- grep("Client's comment", names(allData))
      
      allData <- allData[, c((1:ncol(allData))[-col_idx], col_idx)]
      
      
      col_idx <- grep("Eval", names(allData))
      
      allData <- allData[, c((1:ncol(allData))[-col_idx], col_idx)]
      #allData<-allData%>%select(-`Client's comment`,`Client's comment`)
      
    }

    return(allData)
    
  }



