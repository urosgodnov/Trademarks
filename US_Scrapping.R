GetOwner<-function(dataOwner) {
  
  
  assignements <- dataOwner %>%
    html_nodes(xpath = "//div[@class='assignmentsContainer persist-area' and (@data-assign-type='Ownership and Name Change' or @data-assign-type='Others')]/@id")%>%
    html_text()
  
  assignementsStatus <- dataOwner %>%
    html_nodes(xpath = "//div[@class='assignmentsContainer persist-area' and (@data-assign-type='Ownership and Name Change' or @data-assign-type='Others')]/@data-assign-type")%>%
    html_text()
  
  assignementsL<-as.list(gsub(".*-([0-9]+).*", "\\1", assignements))
  
  for (i in length(assignementsL):1) {
    
   
    #Command
    x<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[@class='value']",sep="")
    
    name<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[contains(text(),'Assignee')]/following::div[1]",sep="")
    
    address<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[contains(text(),'Address')]/following::div[1]",sep="")

    
    Conveyance<-dataOwner %>% 
    html_nodes(xpath = x) %>% html_text()
    
    Conveyance<-Conveyance[1]
    
    if ("Legal" %in% Conveyance && assignementsStatus[[i]]=='Others' || assignementsStatus[[i]]=='Ownership and Name Change') {
      
      
    OwnerName<-dataOwner %>% 
        html_nodes(xpath = name) %>% html_text()
      

    
    OwnerName<-gsub("\r","",OwnerName)
    OwnerName<-gsub("\n","",OwnerName)
    OwnerName<-gsub("Name:","",OwnerName)  
    OwnerName<-trimws(OwnerName)
    
    OwnerAddr<-dataOwner %>% 
      html_node(xpath = address) %>% html_text()
  
    OwnerAddr<-gsub("\r","",OwnerAddr)
    

    break
    
    }
    
    
    

    
  }
  
 
  
  return(data.frame(OwnerName,OwnerAddr, stringsAsFactors = FALSE))
  
  
}




USRenewal <- function(url) {
  x <- data %>%
    html_nodes(xpath = "//div[@class='tabBody']//li[@id='maintenanceTab']") %>% html_text()
  
  if (length(x) > 0 ) {
    remDr$navigate(url)
    
    Sys.sleep(1)
    
    try(MTab <-
          remDr$findElement(using = "id", value = "maintenanceTabBtn"), silent = TRUE)
    try(MTab$clickElement(), silent = TRUE)
    
    page_source <- remDr$getPageSource()
    t <- read_html(page_source[[1]])
    
    renewal <- t %>%
      html_nodes(xpath = "//div[@class='tabBody']//li[@id='maintenanceTab']//div[contains(text(),'§9') and contains(text(),'without')]/following::div[1]") %>%
      html_text()
    
    DAU<-t %>%
      html_nodes(xpath = "//div[@class='tabBody']//li[@id='maintenanceTab']//div[contains(text(),'§8 & 9') and contains(text(),'without')]/following::div[1]") %>%
      html_text()
    
    
    commentAll<-c("")
    
    comment<-t %>%
      html_nodes(xpath = "//div[@class='tabBody']//li[@id='maintenanceTab']//div[contains(text(),'Latest') and contains(text(),'§8') and not(contains(text(),'without'))]") %>%
      html_text()
    
    commentAll<-c(commentAll,gsub("^\\s+|\\s+$","",comment))
    
    comment<-t %>%
      html_nodes(xpath = "//div[@class='tabBody']//li[@id='maintenanceTab']//div[contains(text(),'Latest') and contains(text(),'§8') and not(contains(text(),'without'))]/following::div[1]") %>%
      html_text()
    
    commentAll<-c(commentAll,gsub("^\\s+|\\s+$","",comment))
    
    commentAll<-paste(commentAll, collapse = "")
    
    renewal <- as.Date(renewal, "%B. %d, %Y")
    
    
    renewal <- format(renewal, "%d.%m.%Y")
    
    DAU <- as.Date(DAU, "%B. %d, %Y")
    
    
    DAU <- format(DAU, "%d.%m.%Y")
    
  } else {
    renewal <- NA
    commentAll<-NA
    DAU<-NA
  }
  
  if (length(renewal) == 0 || is.na(renewal)) {
    #when rbind I want dates to stay dates
    renewal <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
  }
  
  if (length(DAU) == 0 || is.na(DAU)) {
    #when rbind I want dates to stay dates
    DAU <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
  }
  return(data.frame(renewal,commentAll,DAU, stringsAsFactors=FALSE))
  
}  
  


USClasses<-function(data) {
  
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
  
  classdesc<-data %>% html_nodes(xpath = "//div[text()='For:']/following::div[1]") %>% html_text()
  
  classdesc<-gsub("\r\n","",classdesc)
  
  #removing text between characters
  
  classdesc<-gsub('\\[.*?\\]', '', classdesc)
  classdesc<-gsub('\\*', '', classdesc)
  classdesc<-gsub(' ,', ',', classdesc)
  
  classn<-data %>% html_nodes(xpath = "//div[text()='International Class(es):']/following::div[1]") %>% html_text()
  
  classn<-gsub(".*([0-9]+).*$", "\\1", classn)
  
  
  classStatus<-data %>% html_nodes(xpath = "//div[text()='Class Status:']/following::div[1]") %>% html_text()
  
  classStatus<-gsub("\r\n","",classStatus)
  
  
  classes<-data.frame(classn,classdesc,classStatus, stringsAsFactors = FALSE)
  
  classes<-classes[classes$classStatus=="ACTIVE",]
  
  if (length(classes)>0 && nrow(classes)>0) {
  
  for (i in 1:nrow(classes))
  {
    
    
    tmpDF[, i] <- classes[i,1]
    
    tmpDF[, i + 9] <- classes[i,2]
    
  }
  
  return(tmpDF)
  
  } else {return(tmpDF<-as.data.frame(NULL))}
  
  
  
}



USScrap <- function(AppNo) {
  #AppNo <-85853329

  #Making URL and Reading data
  
  AppNo<-gsub(",","",AppNo)
  AppNo<-gsub("/","",AppNo)
  AppNo<-gsub("-","",AppNo, fixed=TRUE)
  
  url <-
    paste(
      "http://tsdr.uspto.gov/#caseNumber=",
      AppNo,
      "&caseType=SERIAL_NO&searchType=statusSearch",
      sep = ""
    )
  
  statusURL<-paste("http://tsdr.uspto.gov/statusview/sn",AppNo,sep="")

  
  
  
  try(data <- statusURL %>% read_html(), silent=TRUE)
  

  application <-
    as.Date(
      gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Application Filing Date:']/following::div[1]") %>% html_text()),
      "%B. %d, %Y"
    )
  
  application<-format(application, "%d.%m.%Y")
  
  if (length(application)==0 || is.na(application)) {

    application<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }

   registrationNo<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='US Registration Number:']/following::div[1]") %>% html_text())
  
  if (length(registrationNo)==0) {
    
    registrationNo<-NA
  }
  
  
  acceptance <-
    as.Date(
      gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Registration Date:']/following::div[1]") %>% html_text()),
      "%B. %d, %Y"
    )

  acceptance<-format(acceptance, "%d.%m.%Y")
  if (length(acceptance)==0 || is.na(acceptance)) {

    acceptance<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  priority <-
    as.Date(
      gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Application Filing Date:']/following::div[1]") %>% html_text()),
      "%B. %d, %Y"
    )

  priority<-format(priority, "%d.%m.%Y")

  if (length(priority)==0 || is.na(priority)) {

    priority<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  NoticeOfAllowanceDate <-
    as.Date(
      gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Notice of Allowance Date:']/following::div[1]") %>% html_text()),
      "%B. %d, %Y"
    )
  
  NoticeOfAllowanceDate<-format(NoticeOfAllowanceDate, "%d.%m.%Y")
  
  if (length(NoticeOfAllowanceDate)==0 || is.na(NoticeOfAllowanceDate)) {
    
    NoticeOfAllowanceDate<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  priorityNo<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Application Number:']/following::div[1]") %>% html_text())
  
  if (length(priorityNo)==0) {
    
    priorityNo<-NA
  }
  
  priorityCountry<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Application/Registration Country:']/following::div[1]") %>% html_text())
  
  if (length(priorityCountry)==0) {
    
    priorityCountry<-NA
  }
  
  publication <-
    as.Date(
      gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Publication Date:']/following::div[1]") %>% html_text()),
      "%B. %d, %Y"
    )
  
  publication<-format(publication, "%d.%m.%Y")
  
  if (length(publication)==0 || is.na(publication)) {
    
    publication<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  #First use date
  FirstUseDate<-
    as.Date(gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='First Use:']/following::div[1]") %>% html_text()),
               "%B. %d, %Y"
                )
  
  FirstUseDate<-format(FirstUseDate, "%d.%m.%Y")
  

  if (length(FirstUseDate)==0 || is.na(FirstUseDate)) {
    
    FirstUseDate<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }

  FirstUseDate<-min(FirstUseDate)
  
  #TM Type
  kind<- gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Mark Drawing Type:']/following::div[1]") %>% html_text())

  kind<-gsub(".*([0-9]+).*$", "\\1", kind)
  
  kind<-ifelse(kind %in% c("1","2","3","4","5"),kind,"")
  
  
  kind<-switch(as.numeric(kind),"WORD","DEVICE","WORD-DEVICE","WORD","WORD-DEVICE")

  
  AppType<- try(gsub("\r\n","",data %>% html_nodes(xpath = "//span[contains(@data-sectiontitle,'International Registration')]/following::div[1]") %>% html_text()), silent=TRUE)

  AppType<-ifelse(length(AppType)>0,"International","National")
  

  
  agentOnRecord<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Attorney Name:']/following::div[1]") %>% html_text())
  
  if (length(agentOnRecord)==0) {
    
    agentOnRecord<-NA
  }
  
  
  associatedTMs<-gsub("\r\n","",data %>% 
                        html_nodes(xpath = "//div[contains(text(),'Claimed Ownership')]/following::div[1]//a")%>% html_text()) 
  
  associatedTMs<-paste(associatedTMs,collapse = ",")
  
  if (length(associatedTMs)==0) {
    
    associatedTMs<-NA
  }
  
  ###Dealing with images
  imageUrl<-paste("http://tmsearch.uspto.gov/ImageAgent/ImageAgentProxy?getImage=",AppNo,sep="")

  
  if (length(imageUrl) == 1 && !is.na(imageUrl)) {
    cat(paste("\n","Downloading image...",sep=""))
    imageName<-paste("./logos/", AppNo, ".jpg", sep ="")
    try(download.file(imageUrl,imageName, mode = 'wb',cacheOK=FALSE), silent = TRUE)
    
    size<-file.info(imageName)$size
    #delete files with problems
    if (size<1000)
    {
      file.remove(imageName)
    }
  } else {imageUrl<-NA}


  
  #####Classes
  tmpDF<-USClasses(data)
  
  if (length(tmpDF)==0) {
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
  
  }
  

  #Color
  color<- gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Color(s) Claimed:']/following::div[1]") %>% html_text())
  
  if (length(color)>0) {
  color<-ifelse("not" %in%color ,"Black and white,","Color")
  } else {color<-NA}
  
  #Owner na ta zajeban način
  #I have to call this tab
  urlA<-paste("http://tsdr.uspto.gov/assignments/",AppNo,"?searchprefix=sn",sep="")
  
  try(dataOwner <- urlA %>% read_html(), silent=TRUE)
  
  if ( !exists("dataOwner")) {
  
  owner<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Owner Name:']/following::div[1]") %>% html_text())
  
  if (length(owner)==0) {
    
    owner<-NA
  }
  
  ownerAddr<-gsub("\r","",data %>% html_nodes(xpath = "//div[text()='Owner Address:']/following::div[1]") %>% html_text())
  

  
  if (length(ownerAddr)==0) {
    
    ownerAddr<-NA
  }
  } else
  {
    owner1<-GetOwner(dataOwner)
    
    ownerAddr<-owner1$OwnerAddr
    owner<-trimws(gsub("Name:","",owner1$OwnerName))
  }
  
  renewalAll <-USRenewal(url)
  
  comments<-renewalAll$commentAll
  
  renewal<-renewalAll$renewal
  
  DAU<-renewalAll$DAU
  
  LimDis<-comments

  status<-NA
  

  
  words<-NA
  
  image<-NA


  #return DF
  tmpDF <- cbind(
    data.frame(
      AppNo,
      registrationNo,
      renewal,
      application,
      acceptance,
      FirstUseDate,
      priority,
      priorityNo,
      priorityCountry,
      NoticeOfAllowanceDate,
      publication,
      agentOnRecord,
      associatedTMs,
      status,
      kind,
      AppType,
      DAU,
      color,
      words,
      image,
      imageUrl,
      LimDis,
      owner,
      ownerAddr,
      stringsAsFactors = FALSE
    ),
    tmpDF
  )

  tmpDF<-tmpDF%>%dplyr::rename(
    `Application no.`=AppNo,
    `Application date`=application,
    `Registration no.`=registrationNo,
    `Registration date`=acceptance,
    `First use date`=FirstUseDate,
    `Next renewal date`=renewal,
    `Priority date`=priority,
    `Priority no.`=priorityNo,
    `Priority country`=priorityCountry,
    `Notice of allowance date`=NoticeOfAllowanceDate,
    `Publication date`=publication,
    `TM Type`=kind,
    `Application type`=AppType,
    `Next DAU date`=DAU,
    `Is color`=color,
     Status=status,
    `Limitations & Disclaimers`=LimDis,
    `Agent on record`=agentOnRecord,
    Owner=owner,
    `Owner address`=ownerAddr,
    `Associated TMs`=associatedTMs,
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