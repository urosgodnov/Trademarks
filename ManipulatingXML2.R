library(xml2)
library(tidyr)
colNames <- read_excel(path = "colnames.xlsx")

node<-function(data, path) {

  nodeSet<-getNodeSet(data, path)
  
  elements<-xmlSApply(nodeSet[[1]],xmlValue)
  
  back<-""

  for (z in 1:length(elements)) {
    
      back<-paste(back,as.character(elements[z]), sep=", ")
  }
  
  
  back<-trimws(gsub("^,","",back))
  Encoding(back)<-"UTF8"
  
  return(back)
  
}

Page <- function(file) {
  #file <- "./WipoZips/293830A.xml"
  
  
  xmlDoc <- xmlParse(file)
  
  rootNode <- xmlRoot(xmlDoc)
  
  current <- xmlSApply(rootNode[[1]], function(x)
    xmlSApply(x, xmlValue))
  
  for (i in 1:length(current$BASICGS)) {
    if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue)) >
        0) {
      current$BASICGS[[i]] <-
        xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue)[i]
    } else if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMFR", xmlValue)) >
               0)
    {
      current$BASICGS[[i]] <-
        xpathSApply(rootNode[[1]], "//CURRENT//GSTERMFR", xmlValue)[i]
      
    } else if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMES", xmlValue)) >
               0)
    {
      current$BASICGS[[i]] <-
        xpathSApply(rootNode[[1]], "//CURRENT//GSTERMES", xmlValue)[i]
      
    }
    
    else {
      current$BASICGS[[i]] <- ""
    }
    
    
    Encoding(current$BASICGS[[i]]) <- "UTF-8"
  }
  
  #Manually adding HOLDER
  owner<-node(rootNode[[1]], "//CURRENT//HOLGR//NAME")
  ownerAdd<-node(rootNode[[1]], "//CURRENT//HOLGR//ADDRESS")
  
  owner<-gsub(",,",",",paste(owner,ownerAdd,sep=", "))
  
  # owner<-node(rootNode[[1]], "//CURRENT//REPGR//NAME")
  # ownerAdd<-node(rootNode[[1]], "//CURRENT//REPGR//ADDRESS")
  # 
  # rep<-paste(owner,ownerAdd,sep=", ")
  
  # owner<-node(rootNode[[1]], "//CURRENT//PHOLGR//NAME")
  # ownerAdd<-node(rootNode[[1]], "//CURRENT//PHOLGR//ADDRESS")
  # 
  # previous<-paste(owner,ownerAdd,sep=", ")
  
  ####Front page
  dffront <-
    data.frame (
      INTREGN =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGN"),
      BILING =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "BILING"),
      OOCD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "OOCD"),
      INTREGD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGD"),
      EXPDATE =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "EXPDATE"),
      ORIGLAN =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "ORIGLAN")
    )
  
  regNumber <- dffront$INTREGN
  CLASSNO <-
    paste(xpathSApply(rootNode[[1]], "//CURRENT//GSGR", xmlGetAttr, "NICCLAI"),
          collapse = "|")
  CLASSDESC <- paste(current$BASICGS, collapse = "|")
  
  for (i in 1:length(current)) {
    #i=1
    dftemp <-
      as.data.frame(gsub("\n", " ", paste(as.vector(current[[i]]), collapse = ", ")), stringsAsFactors = FALSE)
    colnames(dftemp) <- names(current[i])
    
    if (names(current[i])=="HOLGR") {
         

     
      
      dftemp$HOLGR<-owner
      
    }
    # 
    # if (names(current[i])=="REPGR") {
    #   
    #   
    #   dftemp$REPGR<-rep
    #   
    # }
    
    
    # if (names(current[i])=="PHOLGR") {
    #   
    #   
    #   dftemp$PHOLGR<-previous
    #   
    # }
    dffront <- cbind(dffront, dftemp)
    
  }
  
  
  results0 <- list()
  results1 <- list()
  ####Otherpages
  for (i in 2:xmlSize(rootNode)) {
    #i=22
    
    # print(paste("Začetek ",i,sep=""))
    
    if (xmlSize(rootNode[[i]]) == 0) {
      
      nodesDF <- as.data.frame(xmlAttrs(rootNode[[i]]))
      
      #Adding regNo and What
      
      what <- xmlName(rootNode[[i]])
      what <- colNames[colNames$Short == what, 2]
      
      if (what == "character(0)") {
        what <- xmlName(rootNode[[i]])
      }
      
      tmpRandW <-
        data.frame(regNumber, what, stringsAsFactors = FALSE)
      
      colnames(tmpRandW) <- c("RegNo", "Task")
      #Rows to columns
      nodesDF <- data.frame(t(nodesDF), row.names = NULL)
      
      #cbind
      results0[[i]] <- cbind(tmpRandW, nodesDF)
      
    } else {
      #i=24
      #print(i)
      # print(paste("Zanka ",i,sep=""))
      current <-
        xmlSApply(rootNode[[i]], function(x)
          xmlSApply(x, xmlValue))
      
      if (grepl("A|B|C",file,ignore.case=TRUE)) {
        
        current<-rbind(current,xmlSApply(rootNode[[i]], function(x)
          xmlSApply(x, xmlAttrs))) }
      
      
      
      if (!is.null(xmlAttrs(rootNode[[i]]))) {
        nodesDF <- as.data.frame(xmlAttrs(rootNode[[i]]))
        #Rows to columns
        nodesDF <- data.frame(t(nodesDF), row.names = NULL)
      }
      #Adding regNo and What
      
      what <- xmlName(rootNode[[i]])
      what <- colNames[colNames$Short == what, 2]
      
      if (what == "character(0)") {
        what <- xmlName(rootNode[[i]])
      }
      
      tmpRandW <-
        data.frame(regNumber, what, stringsAsFactors = FALSE)
      
      colnames(tmpRandW) <- c("RegNo", "Task")
      
      
      #cbind
      if (length(nodesDF) > 0) {
        preDF <- cbind(tmpRandW, nodesDF)
      } else {
        preDF <- tmpRandW
      }
      
      tmpNames <- NA
      DCPCD <- NA
      
      if (class(current) == "matrix") {
        rown <- as.vector(as.character(nrow(current)))
        statesDF <-
          as.data.frame(current,
                        row.names = rown,
                        stringsAsFactors = FALSE)
        
        
      } else if (class(current) == "list") {
        keys <- unique(names(current))
        current <-
          sapply(keys, function(name) {
            unlist(current[grep(name, names(current))])
          })
        
        try(DCPCD <-
              paste(current$DCPCD, collapse = ","), silent = TRUE)
        
        if (length(DCPCD) == 0) {
          DCPCD <- NA
        }
        
        try(LIMTO <- current$LIMTO[[1]], silent = TRUE)
        
        if (length(LIMTO) == 0) {
          LIMTO <- NA
        } else if (!is.na(LIMTO)) {
          Encoding(LIMTO) <- "UTF-8"
          
        }
        
        current <- lapply(current, `length<-`, max(lengths(current)))
        
        rown <- as.vector(as.character(length(current[[1]])))
        
        try(statesDF <-
              as.data.frame(current, stringsAsFactors = FALSE))
        
      } else {
        names <- gsub("\\.text", "", unique(names(current)))
        names <- gsub("\\.LIMTO", "", unique(names(current)))
        names <- gsub("\\.DCPCD", "", unique(names(current)))
        if (length(names) == 1) {
          statesDF <- as.data.frame(current, stringsAsFactors = FALSE)
          colnames(statesDF) <- names
        }
        else {
          tmpNames <- as.data.frame(paste(current[grep(names[1], names(current))],collapse=","), stringsAsFactors = FALSE)
          colnames(tmpNames) <- names[1]
          for (m in 2:length(names)) {
            tmpNames <-
              cbind(tmpNames,
                    as.data.frame(paste(current[grep(names[m], names(current))],collapse=","), stringsAsFactors = FALSE))
            
          }
          
          colnames(tmpNames) <- names
          
          
        }
        
        
      }
      
      if (any(nchar(names(statesDF)) > 25)) {
        next
      }
      
      statesDF[is.na(statesDF)] <- ""
      
      colnames(statesDF) <- gsub("\\.text", "", names(statesDF))
      
      if (length(statesDF) > 0 && is.na(DCPCD) && is.na(tmpNames)) {
        for (j in 1:ncol(statesDF)) {
          states <- ""
          
          for (k in 1:nrow(statesDF)) {
            states <- c(states, statesDF[k, j])
            
          }
          
          if (length(states) > 1) {
            states <- states[-1]
            states <- paste(states, collapse = ",")
          }
          
          statesDF1 <-
            as.data.frame(states, stringsAsFactors = FALSE)
          
          colnames(statesDF1) <- names(statesDF)[j]
          
          preDF <- cbind(preDF, statesDF1)
          
        }
        
      } else if (!is.na(DCPCD) && !is.na(LIMTO)) {
        dp <- as.data.frame(list(DCPCD, LIMTO), stringsAsFactors = FALSE)
        colnames(dp) <- c("DCPCD", "LIMTO")
        preDF <- cbind(preDF, dp)
        
      } else if (!is.na(tmpNames)) {
        preDF <- cbind(preDF, tmpNames)
        
      }
      
      if ("INTREGG" %in% colnames(preDF) && grepl("A|B|C",file,ignore.case=TRUE)) {
        cols <- names(preDF) == "INTREGG"
        names(preDF)[cols] <- paste0("INTREGG", seq.int(sum(cols)))
        
        INTREGG<-paste(preDF$INTREGG1,preDF$INTREGG2,sep="|")
        preDF<-preDF%>%select(-INTREGG1,-INTREGG2)
        preDF$INTREGG<-gsub("NULL","",INTREGG)
      }
      
      results0[[i]] <- preDF
      
      
    }
    
    
    
  }
  
  
  #changing datatype
  colNames <- c('INTREGD', 'EXPDATE')
  
  dffront[colNames] <- lapply(dffront[colNames], as.Date, "%Y%m%d")
  
  if (nchar(trimws(CLASSDESC)) == 1) {
    CLASSDESC <- gsub(",", "", CLASSDESC)
  }
  
  dffront$CLASSNO <- CLASSNO
  dffront$BASICGS <- CLASSDESC
  
  results0[sapply(results0, is.null)] <- NULL
  

  #getting renewal, where there are no DCPCD, DESAG, DESAG2, DCPCD2
  for (n in 1:length(results0)) {
    
    if (length(results0[[n]])>0){
      if (results0[[n]]$Task=="Renewal"  && !("DCPCD" %in% colnames(results0[[n]])) &&
          !("DESAG" %in% colnames(results0[[n]])) &&
          !("DESAG2" %in% colnames(results0[[n]])) &&
          !("DCPCD2" %in% colnames(results0[[n]])) &&
          !("DESPG2" %in% colnames(results0[[n]]))
      ) {
        
        results0[[n]]$Valid<-"A" 
      } else if (results0[[n]]$Task=="New International Registration")
       {
        
        results0[[n]]$Valid<-"A" 
      } else {
        
        results0[[n]]$Valid<-"L" 
      }
      
    }
  }
  
  AllNodesDF <- do.call(rbind.fill, results0)
  
  
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(PRF=PRF.LIMTO),silent = TRUE)
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(DCPCD=DCPCD.text),silent = TRUE)
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(LIMTO=`LIMTO.GSTERMFR`),silent = TRUE)
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(DESAG=`DESAG.DCPCD`),silent = TRUE)
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(DESPG2=`DESPG2.DCPCD`),silent = TRUE)
  try(AllNodesDF<-AllNodesDF%>%dplyr::rename(DESPG=`DESPG.DCPCD`),silent = TRUE)
  
  #finding those rows that have NA everywhere but in the first 6 columns
  # otherColumns<-AllNodesDF[,8:ncol(AllNodesDF)]
  # 
  # where1 <-which(!rowSums(!is.na(otherColumns))) 
  # 
  # rows<-unique(c(rows,where1))
  
  AllNodesDF <- Filter(function(x)
    ! (all(x == "")), AllNodesDF)
  
  
  
  
  if (length(AllNodesDF) > 0)
  {
    
    if (!("NOTDATE" %in% colnames(AllNodesDF))) {
      
      AllNodesDF$NOTDATE<-NA
      
    }
    
    if (!("PUBDATE" %in% colnames(AllNodesDF))) {
      
      AllNodesDF$PUBDATE<-NA
      
    }
    colNames <- c("REGRDAT", "NOTDATE", "REGEDAT", "PUBDATE")
    
    AllNodesDF[colNames] <-
      lapply(AllNodesDF[colNames], as.Date, "%Y%m%d")
  }
  
  return(list(dffront, AllNodesDF))
  
}


#test<-Page("./WipoZips/903889.xml")

fileson <- list.files(path = "./WipoZips/",
                      pattern = "*.xml",
                      full.names = FALSE)


#fileson<-c(fileson[1:50],"182040.xml")

tmp <- lapply(fileson, function(x) {
  x <- paste("./WipoZips/", x, sep = "")
  
  if (file.exists(x))
  {
    print(x)
    return(Page(x))
  }
})

front <- do.call(rbind.fill, lapply(tmp, `[[`, 1))


#Preparing codes for holder

write_xlsx(front[,c("INTREGN","HOLGR")],path="Holders.xlsx")

listNames <- as.list(names(front))

for (i in 1:length(listNames)) {
  OldName <- listNames[[i]]
  NewName <- as.character(colNames[colNames$Short == OldName, 2])
  
  if (NewName != "character(0)") {
    names(front)[i] <- NewName
  }
  
}


# setting reference list
recordID <- read_excel("WebTMSReferenceList.xlsx")
recordID$RegNo <- as.character(recordID$RegNo)
recordID <- recordID[recordID$RegNo != "NA", ]
recordID$RegNo <- trimws(recordID$RegNo)
recordID$CC <- trimws(recordID$CC)
recordID$RegNo <- gsub("^[^0-9]*", "", recordID$RegNo)
recordID$RegNo <- gsub(",", "", recordID$RegNo)
#parent record


parent <- front %>% select(-`Basic registration details`) %>%
  mutate(Designations = paste(
    ifelse(
      is.na(`Designations under the Protocol by virtue of Article 9sexies`),
      "",
      `Designations under the Protocol by virtue of Article 9sexies`
    ),
    ifelse(
      is.na(`Designations under the Protocol`),
      "",
      `Designations under the Protocol`
    ),
    ifelse(
      is.na(`Designations under the Agreement`),
      "",
      `Designations under the Agreement`
    ),
    sep = ","
  )) %>%
  mutate(Designations = gsub(",,", ",", Designations)) %>%
  select(
    -`Designations under the Protocol by virtue of Article 9sexies`,-`Designations under the Protocol`,-`Designations under the Agreement`
  ) %>% mutate(Parent_Child = "Parent")


parent <- parent[, c(1:3, 5:30, 4)]

#child recordid
child <-
  front %>% select(-INTREGD, -`Basic registration details`) %>%
  mutate(Designations = paste(
    ifelse(
      is.na(`Designations under the Protocol by virtue of Article 9sexies`),
      "",
      `Designations under the Protocol by virtue of Article 9sexies`
    ),
    ifelse(
      is.na(`Designations under the Protocol`),
      "",
      `Designations under the Protocol`
    ),
    ifelse(
      is.na(`Designations under the Agreement`),
      "",
      `Designations under the Agreement`
    ),
    sep = ","
  )) %>%
  mutate(Designations = strsplit(as.character(Designations), ",")) %>%
  unnest(Designations) %>% select(
    -`Designations under the Protocol by virtue of Article 9sexies`,-`Designations under the Protocol`,-`Designations under the Agreement`
  ) %>%
  mutate(Parent_Child = "Child", INTREGD = NA)



#Joining parent
parent$`International Registration Number` <-
  as.character(parent$`International Registration Number`)
parent$Designations <- trimws(parent$Designations)

recordIDParent <- recordID %>% filter(CC == "WO")
parent <-
  left_join(parent,
            recordIDParent,
            by = c("International Registration Number" = "RegNo")) %>%
  select(-CC)



#Joining child
child$`International Registration Number` <-
  as.character(child$`International Registration Number`)
child$Designations <- trimws(child$Designations)

child <-
  left_join(
    child,
    recordID,
    by = c(
      "International Registration Number" = "RegNo",
      "Designations" = "CC"
    )
  )


allWipo <- rbind(parent, child)

forAllOthers<-allWipo

allWipo$INTREGD <- format(allWipo$INTREGD, "%d.%m.%Y")
allWipo$EXPDATE <- format(allWipo$EXPDATE, "%d.%m.%Y")

allWipo <- allWipo[, c(1:2, 43, 29, 31, 32, 28, 30, 3:27)]

allWipo <- allWipo %>% select(-`Mark in colour indicator`)

allWipo$Designations <- trimws(allWipo$Designations)
allWipo <-
  allWipo %>% arrange(`International Registration Number`, desc(Parent_Child)) %>%
  filter(Designations != "")
allWipo <-
  allWipo %>% mutate(dolzina = nchar(allWipo$`Basic Goods and services details`))

allWipo$RECORDID <- as.numeric(allWipo$RECORDID)

#checking against verification
allWipo$InVerification <- NA

verification <- read_excel("verification.xlsx")

notInVer <-
  anti_join(allWipo, verification, by = c("RECORDID" = "recordid")) %>% select(RECORDID)
InVer <-
  inner_join(allWipo, verification, by = c("RECORDID" = "recordid")) %>% select(RECORDID)

notInVer <-
  as.data.frame(notInVer[!is.na(notInVer)]) %>% filter(!is.na(.))
colnames(notInVer) <- "RECORDID"
notInVer$value <- "no"

InVer <- as.data.frame(InVer[!is.na(InVer)]) %>% filter(!is.na(.))
colnames(InVer) <- "RECORDID"
InVer$value <- "yes"




allWipo[!is.na(match(allWipo$RECORDID, notInVer$RECORDID)), 34] <-
  "no"
allWipo[!is.na(match(allWipo$RECORDID, InVer$RECORDID)), 34] <-
  "yes"



#Updating empty trademarks names
TMNames <- read_excel("MissingRecordIDList.xlsx")
TMNames$`International Registration Number` <-
  as.character(TMNames$`International Registration Number`)

allWipo <- allWipo %>%
  left_join(TMNames, by = "International Registration Number") %>%
  mutate(TRADEMARK = ifelse(is.na(TRADEMARK.x), TRADEMARK.y, TRADEMARK.x)) %>%
  select(-TRADEMARK.x,-TRADEMARK.y)

allWipo <- allWipo[, c(1, 2, 34, 3:33)]

allWipo<-allWipo %>%
  mutate_if(is.character, funs(substr(.,1,31999)))%>%select(-dolzina)

allWipo[is.na(allWipo)] <- ""
allWipo <- allWipo[, c(1:6, 33, 7:12, 20, 18, 13:17, 19, 21:32)]

#Adding a link column
allWipo$link <-
  paste(
    "http://www.wipo.int/madrid/monitor/en/showData.jsp?ID=BRN:",
    allWipo$`International Registration Number`,
    sep = ""
  )

allWipo <- allWipo[, c(1, 34, 2:33)]

#write_xlsx(allWipo,path ="WipoDATA1.xlsx")

listNo <- c("")
listDesc <- c("")

for (j in 1:34) {
  listNo <- c(listNo, paste("classNo", j, sep = ""))
  listDesc <- c(listDesc, paste("classDesc", j, sep = ""))
}

num <- listNo[-1]
desc <- listDesc[-1]


allWipo <-
  allWipo %>% separate(., col = CLASSNO, into = num, sep = "\\|")

allWipo <-
  allWipo %>% separate(., col = `Basic Goods and services details`, into =
                         desc, sep = "\\|")

#allWipo <- allWipo %>%
# mutate_if(is.character, funs(substr(., 1, 31999))) %>% select(-dolzina)

#write.csv(allWipo,file="Preliminary.csv")


#####Adding country name
countryName<-read_excel(path="Country List.xlsx", sheet="Tabelle2")
countryName[complete.cases(countryName), ]

allWipo<-left_join(allWipo,countryName, by=c("Designations"="Country Code"))
allWipo<-allWipo[,c(1:9,101,10:100)]

#Searching for duplicates= parent regNo>1
duplicates<-allWipo%>%
  select(`International Registration Number`,Designations)%>%
  group_by(`International Registration Number`,Designations)%>%dplyr::summarise(No=n())%>%
  dplyr::filter(.,No>1)

duplicatesList<-as.data.frame(duplicates[,1:2], stringAsFactors=FALSE)
allWipo$RECORDID1<-""
allWipo$RECORDID2<-""

temp<-allWipo

allWipo<-temp
for (i in 1:nrow(duplicatesList)) {
  
  #i=1
  print(i)
  regNo1<-duplicatesList[i,1]
  cc1<-duplicatesList[i,2]
  
  #regNo1<-"275125"
  #Does regNo have a Active record ID
  tempDF<-allWipo[trimws(allWipo$Designations)==cc1 & trimws(allWipo$`International Registration Number`)==regNo1,c(1,5,7)]
  
  
  tempDF<-tempDF%>%arrange(desc(Active))
  keep<-head(tempDF$RECORDID,1)
  
  #print(head(tempDF))
  
  if (nrow(tempDF)==2) {
    
    allWipo[allWipo$RECORDID==keep & allWipo$Designations==cc1,102]<-tempDF$RECORDID[2]
    
    allWipo<-allWipo[!(allWipo$RECORDID==tempDF$RECORDID[2] & allWipo$Designations==cc1 &
                         allWipo$`International Registration Number`==regNo1),]
  } else
  {
    allWipo[allWipo$RECORDID==keep & allWipo$Designations==cc1,102]<-tempDF$RECORDID[2]
    allWipo<-allWipo[!(allWipo$RECORDID==tempDF$RECORDID[2] & allWipo$Designations==cc1 &
                         allWipo$`International Registration Number`==regNo1),]
    allWipo[allWipo$RECORDID==keep & allWipo$Designations==cc1,103]<-tempDF$RECORDID[3]
    allWipo<-allWipo[!(allWipo$RECORDID==tempDF$RECORDID[3] & allWipo$Designations==cc1 &
                         allWipo$`International Registration Number`==regNo1),]
    
  }
  
  #print(head(allWipo[allWipo$`International Registration Number`=="1002999",5:7]))
  
}


allWipo<-allWipo[,c(1:7,102,103,8:101)]

allWipo<-allWipo%>%arrange(.,`International Registration Number`,desc(Parent_Child))
write_xlsx(allWipo, path = "WipoDATA.xlsx")















AllOther <- do.call(rbind.fill, lapply(tmp, `[[`, 2))

AllOther<-AllOther[,c(1:8,10,9,11,14,21,36,42,12,13,15:20,22:35,37:41,43:ncol(AllOther))]

listNames <- as.list(names(AllOther))

for (i in 1:length(listNames)) {
  OldName <- gsub("\\.text", "", listNames[[i]])
  OldName <- gsub("\\.1", "", listNames[[i]])
  NewName <- as.character(colNames[colNames$Short == OldName, 2])
  
  if (NewName != "character(0)") {
    names(AllOther)[i] <- NewName
  } else {
    names(AllOther)[i] <- OldName
  }
  
}

#Appending classNo and ClassDesc
classData<-read_excel(path="WipoData.xlsx")
classData<-unique(classData%>%select(starts_with("class"),"International Registration Number" ))

AllOther[is.na(AllOther$`Designated Contracting Party Code`), "Designated Contracting Party Code"] <-
  ""
AllOther[is.na(AllOther$`Goods and Services limited to:`), "Goods and Services limited to:"] <-
  ""

cols <- names(AllOther) == "Designated Contracting Party Code"
names(AllOther)[cols] <- paste0("Designated Contracting Party Code", seq.int(sum(cols)))

cols <- names(AllOther) == "Goods and services header (French)"
names(AllOther)[cols] <- paste0("Goods and services header (French)", seq.int(sum(cols)))

cols <- names(AllOther) == "Opposition Period End date"
names(AllOther)[cols] <- paste0("Opposition Period End date", seq.int(sum(cols)))

cols <- names(AllOther) == "Partial Refusal details"
names(AllOther)[cols] <- paste0("Partial Refusal details", seq.int(sum(cols)))

cols <- names(AllOther) == "Designations under the Agreement"
names(AllOther)[cols] <- paste0("Designations under the Agreement", seq.int(sum(cols)))

#Adding data from allWipo
final<-unique(AllOther)
vlookup<-unique(forAllOthers[forAllOthers$Parent_Child=="Parent",c(1,32,6,13,14)])


final<-unique(left_join(final,vlookup, by=c("RegNo"="International Registration Number")))
final<-unique(left_join(final,classData, by=c("RegNo"="International Registration Number")))  

final<-final%>%mutate_if(is.character, funs(substr(.,1,31999)))

final$Code<-""
######Preparing the code column
#Just registration
tmp<-final%>%select(RegNo,Task,Valid)%>%group_by(RegNo)%>%dplyr::summarise(N=n_distinct(RegNo,Valid),N1=n())%>%
  dplyr::filter(N==1 & N1==1)%>%select(RegNo)
tmp<-inner_join(tmp,final, by="RegNo")%>%select(RegNo,Task)%>%
  filter(Task=="New International Registration")%>%select(RegNo)

final[!is.na(match(final$RegNo, tmp$RegNo)),"Code"]<-"NewIR only"

#Just transfer Ownership
tmp<-final%>%select(RegNo,Task,Valid)%>%group_by(RegNo)%>%dplyr::summarise(N=n_distinct(RegNo,Valid),N1=n())%>%
  dplyr::filter(N==1 & N1==1)%>%select(RegNo)
tmp<-inner_join(tmp,final, by="RegNo")%>%select(RegNo,Task)%>%
  filter(Task=="Partial Transfer of owenership")%>%select(RegNo)

final[!is.na(match(final$RegNo, tmp$RegNo)),"Code"]<-"PartialTransferOwner only"

#NewIR+REN
tmp<-final%>%select(RegNo,Task,Valid)%>%group_by(RegNo)%>%dplyr::summarise(N=n_distinct(RegNo,Valid),N1=n())%>%
     dplyr::filter(N==1 & N1==2)%>%select(RegNo)
tmp<-inner_join(tmp,final, by="RegNo")%>%select(RegNo,Task,Valid)%>%
     filter(Valid=="A")%>%select(RegNo)

final[!is.na(match(final$RegNo, tmp$RegNo)),"Code"]<-"NewIR+REN"

#NewIR+REN
tmp<-final%>%select(RegNo,Task,Valid)%>%group_by(RegNo)%>%dplyr::summarise(N=n_distinct(RegNo,Valid),N1=n())%>%
  dplyr::filter(N==1 & N1==3)%>%select(RegNo)
tmp<-inner_join(tmp,final, by="RegNo")%>%select(RegNo,Task,Valid)%>%
  filter(Valid=="A")%>%select(RegNo)

final[!is.na(match(final$RegNo, tmp$RegNo)),"Code"]<-"NewIR+REN+REN"

final<-final[,c(1,2,124,3:(ncol(final)-1))]

write_xlsx(final, path = "GermanyWipoDATA.xlsx")
