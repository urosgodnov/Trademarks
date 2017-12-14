library(xml2)
library(tidyr)
colNames<-read_excel(path="colnames.xlsx")

Page<-function(file) {

  #file <- "./WipoZips/135074.xml"
  
xmlDoc <- xmlParse(file, encoding="ISO-8859-1")
  
rootNode <- xmlRoot(xmlDoc)

current<-xmlSApply(rootNode[[1]],function(x) xmlSApply(x, xmlValue))

for (i in 1:length(current$BASICGS)) {
  
  if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue))>0) {
  current$BASICGS[[i]]<-xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue)[i]
  } else {
    
    current$BASICGS[[i]]<-""
  }
}




####Front page
dffront<-data.frame (INTREGN=  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGN" ), 
            BILING =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "BILING"),
            OOCD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "OOCD"),
            INTREGD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGD"),
            EXPDATE =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "EXPDATE"),
            ORIGLAN =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "ORIGLAN")
)

regNumber<-dffront$INTREGN

for (i in 1:length(current)) {
  
  #i=1
  dftemp<-as.data.frame(gsub("\n","",paste(as.vector(current[[i]]),collapse=", ")), stringsAsFactors = FALSE)
  colnames(dftemp)<-names(current[i])
  dffront<-cbind(dffront,dftemp)
  
}
  
dffront$BASICGS<-gsub(",","",dffront$BASICGS)

results0<-list()
results1<-list()
####Otherpages
for (i in 2:xmlSize(rootNode)) {
  
 #i=33
  if (xmlSize(rootNode[[i]])==0) {
  
 
  nodesDF<-as.data.frame(xmlAttrs(rootNode[[i]]))
  
  #Adding regNo and What
  
  what<-xmlName(rootNode[[i]])
  what<-colNames[colNames$Short==what,2]
  
  if (what=="character(0)") {
    what<-xmlName(rootNode[[i]])
  }
  
  tmpRandW<-data.frame(regNumber,what, stringsAsFactors = FALSE)
  
  colnames(tmpRandW)<-c("RegNo","Task")
  #Rows to columns
  nodesDF<-data.frame(t(nodesDF), row.names = NULL)
  
  #cbind
  results0[[i]]<-cbind(tmpRandW,nodesDF)
  
  } else {
    
    current<-xmlSApply(rootNode[[i]],function(x) xmlSApply(x, xmlValue))
    
    if (!is.null(xmlAttrs(rootNode[[i]]))) {
    nodesDF<-as.data.frame(xmlAttrs(rootNode[[i]]))
    #Rows to columns
    nodesDF<-data.frame(t(nodesDF), row.names = NULL)
    }
    #Adding regNo and What
    
    what<-xmlName(rootNode[[i]])
    what<-colNames[colNames$Short==what,2]
    
    if (what=="character(0)") {
      what<-xmlName(rootNode[[i]])
    }
    
    tmpRandW<-data.frame(regNumber,what, stringsAsFactors = FALSE)
    
    colnames(tmpRandW)<-c("RegNo","Task")

    
    #cbind
    if (length(nodesDF)>0) {
    preDF<-cbind(tmpRandW,nodesDF)
    } else {preDF<-tmpRandW}
   
    for (j in 1:length(current)) {
     
      dftemp<-as.data.frame(gsub("\n","",paste(as.vector(current[[j]]),collapse=", ")), stringsAsFactors = FALSE)
      colnames(dftemp)<-names(current[j])

      preDF<-cbind(preDF,dftemp)
      
    }
    
    results0[[i]]<-preDF
    
  }

 

}

AllNodesDF<-do.call(rbind.fill, results0)
#changing datatype
colNames <- c('INTREGD','EXPDATE')

dffront[colNames] <- lapply( dffront[colNames], as.Date, "%Y%m%d" )

colNames <- c("REGRDAT","NOTDATE","REGEDAT","PUBDATE")

AllNodesDF[colNames] <- lapply( AllNodesDF[colNames], as.Date, "%Y%m%d" )

return(list(dffront,AllNodesDF))

}


#test<-Page("./WipoZips/903889.xml")

fileson <- list.files(path = "./WipoZips/",
                      pattern = "*.xml",
                      full.names = FALSE)

tmp<-lapply(fileson, function(x) {
  
  x<-paste("./WipoZips/",x,sep="")
  
  if (file.exists(x))
  {   
    print(x)
    return(Page(x))
  }
})

front<-do.call(rbind.fill, lapply(tmp, `[[`, 1))

listNames<-as.list(names(front))

for (i in 1:length(listNames)){
  
   
  OldName<-listNames[[i]]
  NewName<-as.character(colNames[colNames$Short==OldName,2])
  
  if (NewName!="character(0)") {
  names(front)[i]<-NewName
  }
  
}

#adding webtms recordid
recordID<-read_xlsx("WebTMSReferenceList.xlsx")
recordID$RegNo2<-as.character(recordID$RegNo2)
recordID<-recordID[recordID$RegNo2!="NA",]
recordID$RegNo2<-trimws(recordID$RegNo2)
recordID$RegNo2<-gsub("^[^0-9]*","", recordID$RegNo2)
front$`International Registration Number`<-as.character(front$`International Registration Number`)

front<-left_join(front,recordID, by=c("International Registration Number"="RegNo2"))

before<-front%>%select(-INTREGD,-`Basic registration details`)%>%
  mutate(Designations=paste(ifelse(is.na(`Designations under the Protocol by virtue of Article 9sexies`),"",
                                   `Designations under the Protocol by virtue of Article 9sexies`),
                            ifelse(is.na(`Designations under the Protocol`),"",
                                   `Designations under the Protocol`),
                            ifelse(is.na(`Designations under the Agreement`),"",
                                   `Designations under the Agreement`), sep=","))%>%
  mutate(Designations = strsplit(as.character(Designations), ",")) %>%
  unnest(Designations)%>%select(-`Designations under the Protocol by virtue of Article 9sexies`,
                                -`Designations under the Protocol`,
                                -`Designations under the Agreement`)%>% 
  select(-RegNo,-RegNo1)%>%filter(Designations!="")

before<-before[,c(1:2,27,28,29,3:26)]

before$Designations<-trimws(before$Designations)

options(java.parameters = "-Xmx2024m")

write.csv(before,file="Preliminary.csv")
#write.xlsx2(before,file="Preliminary.xlsx",sheetName = "Data")

