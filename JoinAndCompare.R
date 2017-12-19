createExcel<- function(allData,Country,startRow=3,startColumn=1) {
insert(outputConsole,"Started creating excel...")

errors<-allData[allData$Eval==0,-ncol(allData)]
errors<-errors%>%arrange(Record_ID,sourceType)
allData<-allData[allData$Eval==1,-ncol(allData)]
if (Country!="BENELUX"){
  allData$`Application no.`<-as.integer(allData$`Application no.`)}
allData<-allData%>%arrange(Record_ID,sourceType)
#selecting cells which will be formated differently
mdata<-as.matrix(allData)

nrow<-nrow(mdata)/3
ncol<-ncol(mdata)

highlight <- "test"

for (i in 3:ncol) {
  
  for (j in 1:nrow) {
    
    value1<-ifelse(is.na(mdata[3*j-1,i]),"",mdata[3*j-1,i])
    value2<-ifelse(is.na(mdata[3*j,i]),"",mdata[3*j,i])
    
    
    if (tolower(value1)!=tolower(value2))
    {
      #cells actually begin at 4
      highlight<-c(highlight,paste(3*j+3,".",i,sep=""))
    }
    
  }
}
highlight<-highlight[-1]

#creating xlsx report
wb<-createWorkbook(type="xlsx")


TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK"))   


sheet <- createSheet(wb, sheetName = "Verification")

addDataFrame(allData, sheet, startRow=startRow, startColumn=startColumn,row.names=FALSE,
             colnamesStyle = TABLE_COLNAMES_STYLE)

#Applying highlightin where differences

cols <- ncol(allData)
rows <- getRows(sheet, 
                rowIndex=(startRow+1):(startRow+nrow(allData)))    # get rows
cells <- getCells(rows, colIndex = 1:cols)    # get cells
# change start column if loadings do not start in second column of excel
values <- lapply(cells, getCellValue)             # extract the values

fo <- Fill(foregroundColor="green")                  # create fill object
cs <- CellStyle(wb, fill=fo) 
# apply style to cells that meet criteria
lapply(names(cells[highlight]),
       function(ii)setCellStyle(cells[[ii]],cs))


#Merge rows
for (i in 1:(nrow(allData)/3)) {
  addMergedRegion(sheet, startRow=startRow+3*i-2, endRow=startRow+3*i, startColumn=startColumn, endColumn=startColumn)
  
}

#Checking ending of first data frame
end<-nrow(allData)

#adding another data frame to sheet if any!
if (nrow(errors)>0) {
  addDataFrame(errors, sheet, startRow=startRow+end+1, startColumn=startColumn,row.names=FALSE,
               col.names = FALSE)
  
  cols <- ncol(errors)
  rows <- getRows(sheet, 
                  rowIndex=(startRow+nrow(allData)+1):(startRow+nrow(allData)+1+nrow(errors)))    # get rows
  cells <- getCells(rows, colIndex = 1:cols)    # get cells
  # change start column if loadings do not start in second column of excel
  values <- lapply(cells, getCellValue)             # extract the values
  
  fo <- Fill(foregroundColor="red")                  # create fill object
  cs <- CellStyle(wb, fill=fo) 
  # apply style to cells that meet criteria
  lapply(names(cells),
         function(ii)setCellStyle(cells[[ii]],cs))
  
  
  #Merge rows
  for (i in 1:(nrow(errors)/2)) {
    addMergedRegion(sheet, startRow=startRow+end+2*i-1, endRow=startRow+end+2*i, startColumn=startColumn, endColumn=startColumn)
    
  }
  
}

#here i put app no. Watch what is unique
RegNumList<-as.list(unique(gsub("\\D","",allData$`Application no.`[allData$sourceType=='verification'])))
#Adding logos
for (i in 1:length(RegNumList)) {
  
  
  #Read registration number
  RegNum<-RegNumList[[i]]
  
  
  
  #RegNum<-"1067746"
  imagFile<-paste("./logos/",RegNum,".jpeg",sep="")
  if (!file.exists(imagFile)) {
    imagFile<-paste("./logos/",RegNum,".jpg",sep="")
    if (!file.exists(imagFile)) {
      imagFile<-paste("./logos/",RegNum,".png",sep="")
      if (!file.exists(imagFile)) {
        imagFile<-paste("./logos/",RegNum,".gif",sep="")
      }
    }
  }
  #imagFile1<-paste("./logos/",RegNum,".gif",sep="")
  # if(file.exists(imagFile) || file.exists(imagFile1)) {
  # 
  #   img <- try(readJPEG(imagFile, native = FALSE),silent = TRUE)
  #   target<-try(paste("./logos/",RegNum,".png",sep=""), silent = TRUE)
  #   try(writePNG(img,target=target), silent = TRUE)
  #   
  #   if (class(img)=="try-error") {
  #     executable<-"C:/phantomjs/bin/phantomjs.exe"
  #     img<-try(read.gif(filename = imagFile1),silent = TRUE)
  #     convertGraph(from=imagFile, to=target,path=executable)
  #     
  #   }
  
  
  
  try(addPicture(imagFile, sheet, scale=0.4,startRow =startRow+3*i, startColumn =startColumn+9 ))
  
  # }
}

saveWorkbook(wb, paste("./VerificationResults/",Country,"_VerificationResults.xlsx"))

insert(outputConsole,paste("Joing and comparing finished!","\n","Results are in ","./VerificationResults/",Country,"_VerificationResults.xlsx"))

galert(paste("Joing and comparing finished!","\n"," Results are in ","./VerificationResults/",Country,"_VerificationResults.xlsx"), parent=w)

rm(list = ls())
}

joinAndCompare<-function(verificationFile,destinationFile, Country){
    
  localTime <- Sys.getlocale("LC_TIME")

  #local time to Australiaan
  # Sys.setlocale("LC_TIME", "German")
  # 
  #    path<-"./Inputdata/AR.xlsx"
  #    Country<-"Argentina"
  #   # #  # #
  #   # # ##destinationFile<-read_excel(path=path, skip=1)
  #   # # # #
  #      source<-read_excel(path=path, skip=1)
  #   # # # #Selecting all rows with recordID
  #    recordID<-source%>%filter(grepl("Record",Trademark))%>%dplyr::rename(Record_ID=Trademark)%>%
  #     select(Record_ID)
  #   # # #
  #      recordID<-recordID[rep(seq_len(nrow(recordID)), each=3),"Record_ID"]
  #   # # #
  #      source<-cbind(recordID,source)
  #   # # #
  #     source$Record_ID<-gsub("Record ID:","",source$Record_ID)
  #   # # #
  #      source<-source%>%filter(!grepl("Record",Trademark))
  # 
  #      source$`Application no.` <- gsub("/", "", source$`Application no.`)
  #      source$`Application no.` <- gsub(",", "", source$`Application no.`)
  #   # # #
  #  destinationFile<-source
  #   # # #
  #  path<-"./data/Argentina_online.xlsx"
  # 
  #  # print(path)
  #   verificationFile<-as.data.frame(read_excel(path=path))
   # # #names(verificationFile)<-tolower(names(verificationFile))

  
    colnames<-names(destinationFile)
    
    verificationFile$`Application no.`<-trimws(verificationFile$`Application no.`)
    destinationFile$`Application no.`<-trimws(destinationFile$`Application no.`)
    forExcelComparison<-verificationFile
    #Add additional column with source type
    destinationFile<-cbind(data.frame(sourceType=rep(c("client","agent"),nrow(destinationFile)/2)),destinationFile)
   
    #names(destinationFile)<-tolower(names(destinationFile))

    #destinationFile<-destinationFile[!is.na(destinationFile$`Application no.`),]
    
    colnames(destinationFile)<-gsub(" ","__",names(destinationFile))
    destinationFile$Application__no.<-gsub("\\D", "", destinationFile$Application__no.)
    
    verificationFile<-cbind(data.frame(sourceType=rep("verification",nrow(verificationFile))),verificationFile)

  
    #i will create column names with double__. Before exporting, I will switch back
    namesV<-names(verificationFile)
    colnames(verificationFile)<-gsub(" ","__",namesV)
    destinationRecordName<-unique(destinationFile[,c("Record_ID","Trademark","Application__no.")])
    destinationRecordName$Application__no.<-gsub("\\D", "", destinationRecordName$Application__no.)

    #I'll take the shorter version of Trademark name
     destinationRecordName<-as.data.table(destinationRecordName[!is.na(destinationRecordName$Application__no.),])
     
     destinationRecordName<- destinationRecordName[destinationRecordName[, .I[Trademark == max(Trademark)], by=Application__no.]$V1]

     destinationRecordName<-as.data.frame(destinationRecordName)
     
     
      # destinationRec<-destinationRecordName%>%select(Record_ID,Application__no.)%>%
      #   group_by(Record_ID,Application__no.)%>%dplyr::mutate(Number=n())%>%
      #  select(Number,Record_ID,Application__no.)%>%filter(Number>1)    
    #getting record ID and Trademark
     v1<-verificationFile
     
     verificationFile<-v1

    if (Country=="BENELUX") {
      verificationFile$Application__no.<-as.numeric(verificationFile$Application__no.)
      verificationFile$AppOLD<-v1$Application__no.
      destinationRecordName$Application__no.<-as.numeric(destinationRecordName$Application__no.)
      
     }
    verificationFile<-inner_join(destinationRecordName,verificationFile, by="Application__no.",copy=TRUE)
   
     if("Trademark.y" %in% colnames(verificationFile)) {
           verificationFile<-dplyr::rename(verificationFile,Trademark=Trademark.y)
     }
    
    if (Country=="BENELUX") {
      verificationFile<-dplyr::rename(verificationFile,Application__no1.=Application__no.)
      verificationFile<-dplyr::rename(verificationFile,Application__no.=AppOLD)
      
    }
    colnames(verificationFile)[1]<-c("Record_ID")
    
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
    verificationFile<-cbind.fill(tempVer,tempCol, stringAsFactor=FALSE)
    colnames(verificationFile)<-c(names(tempVer),names(tempCol))
    
    
    

    #factor to columns Due to special names it doesn't work with dplyr
     indx <- sapply(verificationFile, is.factor)
    verificationFile[indx] <- lapply(verificationFile[indx], function(x) (as.character(x)))
    
    indx <- sapply(verificationFile, is.POSIXct)
    verificationFile[indx] <- lapply(verificationFile[indx], function(x) (format(x, "%d.%m.%Y")))
    

  
    #selecting order of columns as in destination file to perform union
    if (Country=="BENELUX") {
      verificationFile<-verificationFile[,c(names(destinationFile),"Application__no1.")]
      
    } else {
    verificationFile<-verificationFile[,names(destinationFile)]}
    
    #to be on the safe side, I convert Registration_no. to character
    verificationFile$Registration__no.<-as.character(verificationFile$Registration__no.)
    
    #Performing Union

    allData<-union_all(destinationFile,verificationFile)
    
    #Excluding elements, that are not in verification file
    #Those will be managed with erros
    #recordID to numeric
    allData$Record_ID<-as.integer(allData$Record_ID)
    if (Country=="BENELUX") {
      allData<-allData[(allData$Record_ID %in% verificationFile$Record_ID),]
      
    } else {
    allData<-allData[(allData$Application__no. %in% verificationFile$Application__no.),] }
    
    #Sorting and adding Trademark

    if (Country!="BENELUX") {
         allData$Application__no.<-as.integer(allData$Application__no.)
    }
    allData<-allData%>%dplyr::arrange(Record_ID,sourceType)
    
    allData$Image<-NA
    
    #Rearrange columns
    allData<-allData[,c(2,1,3:9,ncol(allData),10:(ncol(allData)-1))]
    
    #Adding dummy column  - if record should be evaluated in excel
    allData$Eval<-1
    
    colnames(allData)<-gsub("__"," ",names(allData))
    
    # add all rows from source, which are not included - errors - check them according to record_id
    destinationFile$Record_ID<-as.integer(destinationFile$Record_ID)
    verificationFile$Record_ID<-as.integer(verificationFile$Record_ID)
    
    error<-anti_join(destinationFile, verificationFile, by="Record_ID")
    
    if("sourceType" %in% colnames(error))
    {
      error<-error%>%select(-sourceType)
    }
    #Eclude again this from allData
    
    error<-cbind(data.frame(sourceType=rep(c("client","agent"),nrow(error)/2)),error) 
    colnames(error)<-gsub("__"," ",names(error))
    
    indx <- sapply(error, is.factor)
    error[indx] <- lapply(error[indx], function(x) (as.character(x)))
    
    if (nrow(error)>0) {
      
    diffnames<-as.list(setdiff(names(allData),names(error)))
    
    for (i in 1:length(diffnames))  {
      
           x<-diffnames[[i]]
           if (nrow(error)==0) {
             
             error<-cbind(error,x)
             
           } else {
           
           errordf<-data.frame(rep(NA,nrow(error)))
      
           colnames(errordf)<-x
           
           error<-cbind(error,errordf)
           }
      }
      
    
    error$Eval<-0
    
    error<-error[,names(allData)]
    
    error$Record_ID<-as.integer(error$Record_ID)
    }
    #Alldata application number to string as errors have app number with this type
    allData$`Application no.`<-as.character(allData$`Application no.`)
    
    if (Country=="BENELUX") {
      allData<-allData%>%dplyr::select(-(`Application no1.`))
      
    }
    
    #Due to possible appno change, I have to overwrite some of the appno
    allData$`Application no.`[!is.na(allData$`Application no. assigned upon renewal`)]<-as.character(allData$`Application no. assigned upon renewal`[!is.na(allData$`Application no. assigned upon renewal`)])
    
    ExcelIsCreated<-TRUE

       
      if (Country=="China" && !("Again" %in% colnames(forExcelComparison))) {
         
        error$`Registration no.`<-gsub("\\.","",error$`Registration no.`)
        error$`Registration no.`<-gsub(",","",error$`Registration no.`)
        
        lookup<-unique(error$`Registration no.`)
        
        error1<-as.data.frame(lookup, stringsAsFactors=FALSE)
        colnames(error1)<-c("RegNo")
        error1<-na.omit(error1)
        
       newAgentData<- inner_join(forExcelComparison,error1, by=c("Application no."="RegNo"), copy=TRUE)%>%
         select(`Application no.`)
       
       forExcelComparison$Again<-""
       
       write.xlsx2(
         forExcelComparison,
         file = paste("./data/",Country,".xlsx",sep="") ,
         row.names = FALSE
       )
        
       fileName<- paste("./InputData/",Country,"_UpdateList.xlsx",sep="") 
         print(fileName)
         
         write.xlsx2(
         newAgentData,
         file = fileName,
         row.names = FALSE
       )

         insert(outputConsole,"Go to you verification file and update values with macro!")
         
         ExcelIsCreated<-FALSE
    
      }
    
    if (ExcelIsCreated) {
    
    if (nrow(error)>0) {
    allData<-union_all(allData,error)
    
    }
  
  
    
    
    #Before creating excel, final check if there are 3 lines per app number
    #If there are more, i delete all verification rows but one
    rowsPerAppNum<-allData%>%select(sourceType,`Application no.`,Record_ID)%>%
      filter(sourceType=="verification")%>%
      group_by(sourceType,`Application no.`,Record_ID)%>%dplyr::mutate(Number=n())%>%
      select(Number,Record_ID,`Application no.`)%>%filter(Number>1)
    
    if (nrow(rowsPerAppNum)==0)
    {
    #create excel
    
      tryCatch(createExcel(allData,Country,3,1),
               error = function(e) {
                 insert(outputConsole,"creation of Excel failed! Check whether your destination file is opened!")
                 
               }
      )
      
      
    
      
    } else {insert(outputConsole,paste('There is a problem with data.',as.character(rowsPerAppNum)))}
    
    #changing format
    Sys.setlocale("LC_TIME", localTime)
    rm(list = ls())
}

} 


