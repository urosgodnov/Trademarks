BXScrap <- function(regNo) {
  
  #AppNo <- 1067745
  #regNo<-0759852
  #Making URL and Reading data
  url <-
    paste(
      "https://register.boip.int/bmbonline/search/bynumber/perform.do?markNumber=",
      regNo,"&markNumberType=REG",
      sep = ""
    )
  

  
  data <- url %>% read_html()
  
  threeThings <-
    data %>% html_node(xpath = "//h3[text()='Nummer en dagtekening (dag en uur) van het depot']/following::p[1]") %>%
    html_text()
  
  threeThings<-unlist(strsplit(threeThings,"\n"))
  
  AppNo<-trimws(gsub("/","",threeThings[[1]]))
  
  registrationNo <-
    data %>% html_node(xpath = "//h3[text()='Inschrijvingsnummer']/following::p[1]") %>%
    html_text()
  
  try(application <-
    as.Date(threeThings[[2]],
      "%d-%m-%Y"
    ))
  
  application<-format(application, "%d.%m.%Y")
  
  if (length(application)==0 || is.na(application)) {
    
    application<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  
  try(renewal <-
    as.Date(
      data %>% html_node(xpath = "//h3[text()='Vervaldatum']/following::p[1]") %>%
        html_text(),
      "%d-%m-%Y"
    ))
  
  renewal<-format(renewal, "%d.%m.%Y")
  
  if (length(renewal)==0 || is.na(renewal)) {
    
    #when rbind I want dates to stay dates
    renewal<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  
  try(acceptance <-
        as.Date(
          data %>% html_node(xpath = "//h3[text()='Datum inschrijving']/following::p[1]") %>%
            html_text(),
          "%d-%m-%Y"
        ))
  
  acceptance<-format(acceptance, "%d.%m.%Y")
  
  if (length(acceptance)==0 || is.na(acceptance)) {
    
    #when rbind I want dates to stay dates
    acceptance<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  owner <-
      data %>% html_nodes(xpath = "//h3[text()='Naam en adres van de houder']/following::p[1]")
  
  owner<-unlist(strsplit(as.character(owner),"<br>"))
  
  ownerAddr<-paste(gsub("</p>","",tail(owner,length(owner)-1)),collapse ="/n")
    
  owner<-head(owner,1)
  
  owner<-gsub("<p>","",owner)
  
  
  #Agent on record
  agentOnRecord <-
      data %>% html_nodes(xpath = "//h3[text()='Naam en adres van de gemachtigde of vermelding van het correspondentie-adres van de houder']/following::p[1]")

  agentOnRecord<-unlist(strsplit(as.character(agentOnRecord),"<br>"))
  
  agentOnRecord<-paste(gsub("<p>|</p>","",agentOnRecord),collapse ="/n")
  
  

  trademark <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//h3[text()='Woordmerk']/following::p[1]")%>% html_text()
    )
  
  if (length(trademark)==0) {
    
    trademark<-NA
    
  }
  
  
  kind <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//h3[text()='Woordmerk']")%>% html_text()
    )
  
  if (length(kind)==0) {
    
    kind<- "Image"
    
  } else {kind<- "Word"}
  
  status <-
    gsub(
      "\n",
      "",
      data %>% html_node(xpath = "//h3[text()='Status']/following::p[1]") %>% html_text()
    )
  

  

  
  #words
  words <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//h3[text()='Classificatie van de beeldelementen, type merk, kleuren, onderscheidende elementen']/following::p[1]") %>% html_text()
    )
  

  if (length(words)==0) {
    
    words<-NA
    
  }
  
  
  ######Classes
  #define dataframe with 9 classes and desc
  tmpDF <- data.frame(matrix(ncol = 18, nrow = 1))
  class <-
    sapply(as.list(1:9), function(x) {
      return(paste("class", x, sep = ""))
    })
  desc <-
    sapply(as.list(1:9), function(x) {
      return(paste("description", x, sep = ""))
    })
  
  colnames(tmpDF) <- c(class, desc)
  
  
  
  #Search which classes are in
  classes <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//h3[text()='Klasse-aanduiding en opgave van de waren en diensten']/following::p[1]")%>% html_text()
    )
  classes <- strsplit(classes, "\\.")
  
  #Search and fill columns in tmpDF
  for (i in 1:(length(classes[[1]])-1))
  {
    currentClassNumber <- classes[[1]][[i]]
    
    currentClassDesc <-
     str_sub(currentClassNumber,8)
    
    tmpDF[, i] <- as.numeric(gsub("\\D+","",str_sub(currentClassNumber,1,7)))
    
    tmpDF[, i + 9] <- currentClassDesc
    
  }
  
  
  ###Dealing with images
  imageUrl <-
    data %>% html_nodes(xpath = "//h3[text()='Afbeelding van het beeldmerk']/following::p[1]//@src") %>% html_text()

  if (length(imageUrl) == 1 && !is.na(imageUrl)) {
    cat(paste("\n","Downloading image...",sep=""))
    imageName<-paste("./logos/", AppNo, ".jpg", sep ="")
    try(download.file(imageUrl,imageName, mode = 'wb',cacheOK=FALSE), silent = TRUE)
  } else {imageUrl<-NA}
  
  
  #return DF
  tmpDF <- cbind(
    data.frame(
      AppNo,
      trademark,
      registrationNo,
      renewal,
      application,
      acceptance,
      status,
      kind,
      words,
      image,
      imageUrl,
      owner,
      ownerAddr,
      agentOnRecord,
      stringsAsFactors = FALSE
    ),
    tmpDF
  )

  tmpDF<-tmpDF%>%dplyr::rename(
    `Application no.`=AppNo,
    Trademark=trademark,
    `Application date`=application,
    `Registration no.`=registrationNo,
    `Registration date`=acceptance,
    `Next renewal date`=renewal,
    `TM Type`=kind,
     Status=status,
    `Agent on record`=agentOnRecord,
    Owner=owner,
    `Owner address`=ownerAddr,
    `1st Class`=   class1,         
    `1st Goods & Services` =description1,
    `2nd Class` = class2,          
    `2nd Goods & Services`=description2,
    `3rd Class` =class3,          
    `3rd Goods & Services`=description3,
    `4th Class`  =class4,          
    `4th Goods & Services`=description4,
    `5th Class`   =class5,        
    `5th Goods & Services`=description5,
    `6th Class`   =class6,        
    `6th Goods & Services`=description6,
    `7th Class`  =class7,        
    `7th Goods & Services`=description7,
    `8th Class`  =class8,        
    `8th Goods & Services`=description8,
    `9th Class`  =class9,         
    `9th Goods & Services`=description9
  )

  
  if (class(tmpDF) != "data.frame")
  {
    tmpDF = as.data.frame(NULL)
  }
  
  return(tmpDF)
}