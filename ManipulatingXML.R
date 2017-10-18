library(xml2)
colNames<-read_excel(path="colnames.xlsx")

firstPage<-function(file) {

  #file <- "135074.xml"
  
xmlDoc <- xmlParse(file)
  
rootNode <- xmlRoot(xmlDoc)

current<-xmlSApply(rootNode[[1]],function(x) xmlSApply(x, xmlValue))


df<-data.frame (INTREGN=  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGN" ), 
            BILING =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "BILING"),
            OOCD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "OOCD"),
            INTREGD =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "INTREGD"),
            EXPDATE =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "EXPDATE"),
            ORIGLAN =  xpathSApply(rootNode, "//MARKGR", xmlGetAttr, "ORIGLAN")
)

for (i in 1:length(current)) {
  
  #i=1
  dftemp<-as.data.frame(gsub("\n","",paste(as.vector(current[[i]]),collapse=", ")), stringsAsFactors = FALSE)
  colnames(dftemp)<-names(current[i])
  df<-cbind(df,dftemp)
  
}

return(df)
}

fileson <- list.files(path = "./WipoZips/",
                      pattern = "*.xml",
                      full.names = FALSE)

tmp<-lapply(fileson, function(x) {
  
  x<-paste("./WipoZips/",x,sep="")
  
  if (file.exists(x))
  {   
    print(x)
    return(firstPage(x))
  }
})

result<-do.call(rbind.fill, tmp)

listNames<-as.list(names(result))

for (i in 1:length(listNames)){
  
   
  OldName<-listNames[[i]]
  NewName<-as.character(colNames[colNames$Short==OldName,2])
  
  if (NewName!="character(0)") {
  names(result)[i]<-NewName
  }
  
}

write.xlsx(result,file="Test.xlsx",sheetName = "Front")
