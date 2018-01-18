createExcel <- function(allData,
                        Country,
                        startRow = 3,
                        startColumn = 1) {
  insert(outputConsole, "Started creating excel...")
  
  errors <- allData[allData$Eval == 0,-ncol(allData)]
  notIn<- allData[allData$Eval == 2,-ncol(allData)]
  errors <- errors %>% arrange(Record_ID, sourceType)
  allData <- allData[allData$Eval == 1,-ncol(allData)]

  

  if (Country != "BENELUX" && Country !="Japan" && Country !="HK" && Country !="China") {
    allData$`Application no.` <- as.integer(allData$`Application no.`)
  }
  allData <- allData %>% arrange(Record_ID, sourceType)
  #selecting cells which will be formated differently
  mdata <- as.matrix(allData)
  
  nrow <- nrow(mdata) / 3
  ncol <- ncol(mdata)
  
  highlight <- "test"
  
  for (i in 3:ncol) {
    for (j in 1:nrow) {
      value1 <-
        ifelse(is.na(mdata[3 * j - 1, i]), "", mdata[3 * j - 1, i])
      value2 <- ifelse(is.na(mdata[3 * j, i]), "", mdata[3 * j, i])
      
      
      if (tolower(value1) != tolower(value2))
      {
        #cells actually begin at 4
        highlight <-
          c(highlight, paste(3 * j + 3, ".", i, sep = ""))
      }
      
    }
  }
  highlight <- highlight[-1]
  
  #creating xlsx report
  wb <- createWorkbook(type = "xlsx")
  
  
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
    Border(
      color = "black",
      position = c("TOP", "BOTTOM"),
      pen = c("BORDER_THIN", "BORDER_THICK")
    )
  
  
  sheet <- createSheet(wb, sheetName = "Verification")
  
  addDataFrame(
    allData,
    sheet,
    startRow = startRow,
    startColumn = startColumn,
    row.names = FALSE,
    colnamesStyle = TABLE_COLNAMES_STYLE
  )
  
  #Applying highlightin where differences
  
  cols <- ncol(allData)
  rows <- getRows(sheet,
                  rowIndex = (startRow + 1):(startRow + nrow(allData)))    # get rows
  cells <- getCells(rows, colIndex = 1:cols)    # get cells
  # change start column if loadings do not start in second column of excel
  values <-
    lapply(cells, getCellValue)             # extract the values
  
  fo <-
    Fill(foregroundColor = "#FFFF99")                  # create fill object
  cs <- CellStyle(wb, fill = fo)
  # apply style to cells that meet criteria
  lapply(names(cells[highlight]),
         function(ii)
           setCellStyle(cells[[ii]], cs))
  
  
  #Merge rows
  for (i in 1:(nrow(allData) / 3)) {
    addMergedRegion(
      sheet,
      startRow = startRow + 3 * i - 2,
      endRow = startRow + 3 * i,
      startColumn = startColumn,
      endColumn = startColumn
    )
    
  }
  
  #Checking ending of first data frame
  end <- nrow(allData)
  endErr<-nrow(errors)
  
  #adding another data frame to sheet if any!
  if (nrow(errors) > 0) {
    addDataFrame(
      errors,
      sheet,
      startRow = startRow + end + 1,
      startColumn = startColumn,
      row.names = FALSE,
      col.names = FALSE
    )
    
    cols <- ncol(errors)
    rows <- getRows(sheet,
                    rowIndex = (startRow + nrow(allData) + 1):(startRow +
                                                                 nrow(allData) + 1 + nrow(errors)))    # get rows
    cells <- getCells(rows, colIndex = 1:cols)    # get cells
    # change start column if loadings do not start in second column of excel
    values <-
      lapply(cells, getCellValue)             # extract the values
    
    fo <-
      Fill(foregroundColor = "red")                  # create fill object
    cs <- CellStyle(wb, fill = fo)
    # apply style to cells that meet criteria
    lapply(names(cells),
           function(ii)
             setCellStyle(cells[[ii]], cs))
    
    
    #Merge rows
    for (i in 1:(nrow(errors) / 2)) {
      addMergedRegion(
        sheet,
        startRow = startRow + end + 2 * i - 1,
        endRow = startRow + end + 2 * i,
        startColumn = startColumn,
        endColumn = startColumn
      )
      
    }
    
  }
  
  endErr<-ifelse(nrow(errors)>0,nrow(errors),0)
  
  #Adding blue
  #adding another data frame to sheet if any!
  if (nrow(notIn) > 0) {
    addDataFrame(
      notIn,
      sheet,
      startRow = startRow + end + endErr + 1,
      startColumn = startColumn,
      row.names = FALSE,
      col.names = FALSE
    )
    
    cols <- ncol(notIn)
    rows <- getRows(sheet,
                    rowIndex = (startRow + end + endErr + 1):(startRow + end + endErr + 1+nrow(notIn)))    # get rows
    cells <- getCells(rows, colIndex = 1:cols)    # get cells
    # change start column if loadings do not start in second column of excel
    values <-
      lapply(cells, getCellValue)             # extract the values
    
    fo <-
      Fill(foregroundColor = "#00FFFF")                  # create fill object
    cs <- CellStyle(wb, fill = fo)
    # apply style to cells that meet criteria
    lapply(names(cells),
           function(ii)
             setCellStyle(cells[[ii]], cs))

    
  }
  
  imageCol <- which(colnames(allData) == "Image")
  #print(imageCol)
  #here i put app no. Watch what is unique
  RegNumList <-
    as.list(unique(gsub("\\D", "", allData$`Application no.`[allData$sourceType ==
                                                               'verification'])))
  #Adding logos
  for (i in 1:length(RegNumList)) {
    #Read registration number
    RegNum <- RegNumList[[i]]
    goFurther <- 1
    
    
    #RegNum<-"1002921"
    imagFile <- paste("./logos/", RegNum, ".jpeg", sep = "")
    
    if (file.exists(imagFile)) {
      try(addPicture(
        imagFile,
        sheet,
        scale = 0.2,
        startRow = startRow + 3 * i,
        startColumn = imageCol
      ))
      goFurther <- 0
      
    }
    
    imagFile <- paste("./logos/", RegNum, ".jpg", sep = "")
    
    if (file.exists(imagFile) && goFurther == 1) {
      img <- readJPEG(imagFile)
      
      writeJPEG(img, paste("./logos/", RegNum, ".jpeg", sep = ""))
      
      try(addPicture(
        paste("./logos/", RegNum, ".jpeg", sep = ""),
        sheet,
        scale = 0.2,
        startRow = startRow + 3 * i,
        startColumn = imageCol
      ))
      
      goFurther <- 0
    }
    
    imagFile <- paste("./logos/", RegNum, ".png", sep = "")
    
    if (file.exists(imagFile) && goFurther == 1) {
      try(addPicture(
        imagFile,
        sheet,
        scale = 0.2,
        startRow = startRow + 3 * i,
        startColumn = imageCol
      ))
      goFurther <- 0
    }
    
    imagFile <- paste("./logos/", RegNum, ".gif", sep = "")
    
    if (file.exists(imagFile) && goFurther == 1) {
      try(addPicture(
        imagFile,
        sheet,
        scale = 0.2,
        startRow = startRow + 3 * i,
        startColumn = imageCol
      ))
      goFurther <- 0
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
    
    
    
    
    # }
  }
  
  saveWorkbook(wb,
               paste(
                 "./VerificationResults/",
                 Country,
                 "_VerificationResults.xlsx"
               ))
  
  insert(
    outputConsole,
    paste(
      "Joing and comparing finished!",
      "\n",
      "Results are in ",
      "./VerificationResults/",
      Country,
      "_VerificationResults.xlsx"
    )
  )
  
  galert(
    paste(
      "Joing and comparing finished!",
      "\n",
      " Results are in ",
      "./VerificationResults/",
      Country,
      "_VerificationResults.xlsx"
    ),
    parent = w
  )
  
  rm(list = ls())
}







joinAndCompare <- function(verificationFile,
                           destinationFile,
                           Country) {
  localTime <- Sys.getlocale("LC_TIME")
  
  #local time to Australiaan
  Sys.setlocale("LC_TIME", "German")
  
  # path <- "./Inputdata/Japan_input.xls"
  # Country <- "Japan"
  # # #  # #
  # # # ##destinationFile<-read_excel(path=path, skip=1)
  # # # # #
  # source <- read_excel(path = path, skip = 1)
  # # # # #Selecting all rows with recordID
  # recordID <-
  #   source %>% filter(grepl("Record", Trademark)) %>% dplyr::rename(Record_ID =
  #                                                                     Trademark) %>%
  #   select(Record_ID)
  # # # #
  # recordID <-
  #   recordID[rep(seq_len(nrow(recordID)), each = 3), "Record_ID"]
  # # # #
  # source <- cbind(recordID, source)
  # # # #
  # source$Record_ID <- gsub("Record ID:", "", source$Record_ID)
  # # # #
  # source <- source %>% filter(!grepl("Record", Trademark))
  # 
  # source$`Application no.` <-
  #   gsub("/", "", source$`Application no.`)
  # source$`Application no.` <-
  #   gsub(",", "", source$`Application no.`)
  # # # #
  # destinationFile <- source
  # # # #
  # path <- "./data/Japan_online.xlsx"
  # 
  # # print(path)
  # verificationFile <- as.data.frame(read_excel(path = path))
  # #names(verificationFile)<-tolower(names(verificationFile))
  
  
  
  #############################Working on status
  
  
  #Status for Macao
  if (Country == "Macao") {
    verificationFile[verificationFile$Status == "Not to renew", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "[ Registered ]", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "Allow to lapse", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Instructed not to renew", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "No longer in our care – no reminders", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "Published", "Status"] <-
      "Filed"
    
  }
  
  #Status for Japan
  if (Country == "Japan") {
    try(verificationFile[verificationFile$Status == "Pending", "Status"] <-
          "Filed", silent = TRUE)
  }
  
  
  #Status for HK
  if (Country == "HK") {
    verificationFile[verificationFile$Status == "[ Registered ]", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "[ Registration Merged ]", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "Instructed not to renew", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Office action received", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Registered", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "Registration cancelled by merger", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Removed", "Status"] <-
      "Inactive"
    
  }
  
  #Status for China
  if (Country == "China") {
    verificationFile[verificationFile$Status == "[ Invalid ]", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "[ Registered ]", "Status"] <-
      "Registered"
    verificationFile[verificationFile$Status == "Application filed", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Application Filed", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Being cancelled by third party", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Awaiting client's instructions", "Status"] <-
      "Filed"
    
    verificationFile[verificationFile$Status == "Being opposed by third party", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Instructed not to renew", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Instructed to let lapse", "Status"] <-
      "Inactive"
    
    verificationFile[verificationFile$Status == "Lapsed – no instructions", "Status"] <-
      "Inactive"
    verificationFile[verificationFile$Status == "Notification of Rejection Received", "Status"] <-
      "Filed"
    
    verificationFile[verificationFile$Status == "Published", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Rejection Maintained", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "Removed", "Status"] <-
      "Inactive"
    
    verificationFile[verificationFile$Status == "Request for Review Filed", "Status"] <-
      "Filed"
    verificationFile[verificationFile$Status == "To Be Advertised", "Status"] <-
      "Filed"
  }
  
  # Country="Japan"
  #source("CallData.R")
  #source("GetAllData.R")
  
  fromAgent<-verificationFile
  
  #HK, JP, China
  #Exracting errors and try RegNo on RegNo
  if (Country %in% c("Japan", "HK", "China")) {
    #Getting allData -appNo=AppNo - the most common way
    
   
    
    VerJoin <- "Application no."
    DesJoin <- "Application no."
    
    #browser()

    allDataA <-
      GetAllData(verificationFile,
                 destinationFile,
                 Country,
                 VerJoin,
                 DesJoin)
    

    VerJoin <- "Registration no."
    DesJoin <- "Registration no."
    errorTry <- allDataA[allDataA$Eval == 0, ]
    errorTry <- errorTry %>% select(-Eval, -sourceType)
    
   
    allDataR <-
      GetAllData(verificationFile, errorTry, Country, VerJoin, DesJoin)
    
    allData<-rbind(allDataA[allDataA$Eval == 1, ],allDataR)
      
    #finding those from agent, that are not in
    
    allData <- notIn(fromAgent,allData,1)
    

    
  } else {
    VerJoin <- "Application no."
    DesJoin <- "Application no."
    
    allData <-
      GetAllData(verificationFile,
                 destinationFile,
                 Country,
                 VerJoin,
                 DesJoin)
    
    #finding those from agent, that are not in
    allData <- notIn(fromAgent,allData,2)
  }
  
  
  #Geting data that are in from agent but not in platform
  
  #write_xlsx(allData,"jeba.xlsx")
  
  #Before creating excel, final check if there are 3 lines per app number
  #If there are more, i delete all verification rows but one
  rowsPerAppNum <-
    allData %>% select(sourceType, Record_ID) %>%
    filter(sourceType == "verification") %>%
    group_by(sourceType, Record_ID) %>% dplyr::mutate(Number =
                                                        n()) %>%
    select(Number, Record_ID) %>% filter(Number > 1)
  
  if (nrow(rowsPerAppNum) == 0)
  {
    #create excel
    
    tryCatch(
      createExcel(allData, Country, 3, 1),
      error = function(e) {
        insert(
          outputConsole,
          "creation of Excel failed! Check whether your destination file is opened!"
        )
        
      }
    )
    
    
    
    
  } else {
    insert(outputConsole,
           paste('There is a problem with data.', as.character(rowsPerAppNum)))
  }
  
  #changing format
  Sys.setlocale("LC_TIME", localTime)
  rm(list = ls())
  
  
}


