library(xml2)
library(tidyr)
colNames<-read_excel(path="colnames.xlsx")

Page<-function(file) {

  #file <- "./WipoZips/409576.xml"
  
xmlDoc <- xmlParse(file)

rootNode <- xmlRoot(xmlDoc)

current<-xmlSApply(rootNode[[1]],function(x) xmlSApply(x, xmlValue))

for (i in 1:length(current$BASICGS)) {
  
  if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue))>0) {
  current$BASICGS[[i]]<-xpathSApply(rootNode[[1]], "//CURRENT//GSTERMEN", xmlValue)[i]
  } else if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMFR", xmlValue))>0)
  {
    current$BASICGS[[i]]<-xpathSApply(rootNode[[1]], "//CURRENT//GSTERMFR", xmlValue)[i]
    
  } else if (length(xpathSApply(rootNode[[1]], "//CURRENT//GSTERMES", xmlValue))>0)
  {
    current$BASICGS[[i]]<-xpathSApply(rootNode[[1]], "//CURRENT//GSTERMES", xmlValue)[i]
    
  }
  
  else {
    current$BASICGS[[i]]<-""
  }
  
  Encoding(current$BASICGS[[i]]) <- "UTF-8" 
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
  if (length(trimws(dffront$BASICGS))==1) {
      dffront$BASICGS<-gsub(",","",dffront$BASICGS)
  }

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


#fileson<-c(fileson[1:50],"182040.xml")

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


# setting reference list
recordID<-read_excel("WebTMSReferenceList.xlsx")
recordID$RegNo<-as.character(recordID$RegNo)
recordID<-recordID[recordID$RegNo!="NA",]
recordID$RegNo<-trimws(recordID$RegNo)
recordID$CC<-trimws(recordID$CC)
recordID$RegNo<-gsub("^[^0-9]*","", recordID$RegNo)
recordID$RegNo<-gsub(",","", recordID$RegNo)
#parent record


parent<-front%>%select(-`Basic registration details`)%>%
  mutate(Designations=paste(ifelse(is.na(`Designations under the Protocol by virtue of Article 9sexies`),"",
                                   `Designations under the Protocol by virtue of Article 9sexies`),
                            ifelse(is.na(`Designations under the Protocol`),"",
                                   `Designations under the Protocol`),
                            ifelse(is.na(`Designations under the Agreement`),"",
                                   `Designations under the Agreement`), sep=","))%>%
  mutate(Designations=gsub(",,",",",Designations))%>%
select(-`Designations under the Protocol by virtue of Article 9sexies`,
       -`Designations under the Protocol`,
       -`Designations under the Agreement`)%>%mutate(Parent_Child="Parent")


parent<-parent[,c(1:3,5:29,4)]

#child recordid
child<-front%>%select(-INTREGD,-`Basic registration details`)%>%
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
  mutate(Parent_Child="Child",INTREGD=NA)



#Joining parent
parent$`International Registration Number`<-as.character(parent$`International Registration Number`)
parent$Designations<-trimws(parent$Designations)

recordIDParent<-recordID%>%filter(CC=="WO")
parent<-left_join(parent,recordIDParent, by=c("International Registration Number"="RegNo"))%>%
  select(-CC)

#Joining child                 
child$`International Registration Number`<-as.character(child$`International Registration Number`)
child$Designations<-trimws(child$Designations)

child<-left_join(child,recordID, by=c("International Registration Number"="RegNo","Designations"="CC"))

                  
allWipo<-rbind(parent,child)

allWipo$INTREGD<-format(allWipo$INTREGD,"%d.%m.%Y")
allWipo$EXPDATE<-format(allWipo$EXPDATE,"%d.%m.%Y")

allWipo<-allWipo[,c(1:2,42,28,30,31,27,29,3:26)]

allWipo<-allWipo%>%select(-`Mark in colour indicator`)

allWipo$Designations<-trimws(allWipo$Designations)
allWipo<-allWipo%>% arrange(`International Registration Number`,desc(Parent_Child))%>%
  filter(Designations!="")
allWipo<-allWipo%>%mutate(dolzina=nchar(allWipo$`Basic Goods and services details`))

allWipo$RECORDID<-as.numeric(allWipo$RECORDID)

#checking against verification
allWipo$InVerification<-NA

verification<-read_excel("verification.xlsx")

notInVer<-anti_join(allWipo, verification, by = c("RECORDID"="recordid"))%>%select(RECORDID)
InVer<-inner_join(allWipo, verification, by = c("RECORDID"="recordid"))%>%select(RECORDID)

notInVer<-as.data.frame(notInVer[!is.na(notInVer)])%>%filter(!is.na(.))
colnames(notInVer)<-"RECORDID"
notInVer$value<-"no"

InVer<-as.data.frame(InVer[!is.na(InVer)])%>%filter(!is.na(.))
colnames(InVer)<-"RECORDID"
InVer$value<-"yes"




allWipo[!is.na(match(allWipo$RECORDID,notInVer$RECORDID)),33]<-"no"
allWipo[!is.na(match(allWipo$RECORDID,InVer$RECORDID)),33]<-"yes"

allWipo<-allWipo[,c(1,2,33,3:32)]



allWipo<-allWipo %>%
  mutate_if(is.character, funs(substr(.,1,31999)))%>%select(-dolzina)

allWipo[is.na(allWipo)]<-""

#write.csv(allWipo,file="Preliminary.csv")
write_xlsx(allWipo,path ="WipoDATA.xlsx")

