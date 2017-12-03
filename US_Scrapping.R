GetOwner<-function(dataOwner) {
  

  assignements <- dataOwner %>%
    html_nodes(xpath = "//div[@class='assignmentsContainer persist-area' and (@data-assign-type='Ownership and Name Change' or @data-assign-type='Others')]/@id")%>%
    html_text()
  
  assignementsStatus <- dataOwner %>%
    html_nodes(xpath = "//div[@class='assignmentsContainer persist-area' and (@data-assign-type='Ownership and Name Change' or @data-assign-type='Others')]/@data-assign-type")%>%
    html_text()
  
  assignementsL<-as.list(gsub(".*-([0-9]+).*", "\\1", assignements))

  if (length(assignementsL)>0 && class(assignementsL)=="list") {  
  for (i in length(assignementsL):1) {
    
  
    #Command
    x<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[@class='value']",sep="")
    
    name<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[contains(text(),'Assignee')]/following::div[1]",sep="")
    
    address<-paste("//div[@id='assignments-",assignementsL[[i]],"']//div[contains(text(),'Address')]/following::div[1]",sep="")

    
    Conveyance<-dataOwner %>% 
    html_nodes(xpath = x) %>% html_text()
    
    Conveyance<-Conveyance[1]
    
    if (grepl("Legal",Conveyance) && assignementsStatus[[i]]=='Others' || assignementsStatus[[i]]=='Ownership and Name Change') {
      
      
    OwnerName<-dataOwner %>% 
        html_node(xpath = name) %>% html_text()
      

    
    OwnerName<-gsub("\r","",OwnerName)
    OwnerName<-gsub("\n","",OwnerName)
    OwnerName<-gsub("Name:","",OwnerName)  
    OwnerName<-trimws(OwnerName)
    
    OwnerAddr<-dataOwner %>% 
      html_node(xpath = address) %>% html_text()
  
    OwnerAddr<-gsub("\r","",OwnerAddr)
    OwnerAddr<-sub("\n","",OwnerAddr)    

    break
    
    }
    
    
    

    
  }
}
 else {OwnerName<-NA
       OwnerAddr<-NA} 
 
  
  return(data.frame(OwnerName,OwnerAddr, stringsAsFactors = FALSE))
  
  
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
  
  classStatus<-data %>% html_nodes(xpath = "//div[text()='Class Status:']/following::div[1]") %>% html_text()
  
  classStatus<-gsub("\r\n","",classStatus)
  
  classn<-data %>% html_nodes(xpath = "//div[text()='International Class(es):']/following::div[1]") %>% html_text()
  
  if (length(classn)==1 && grepl(",",classn)) {
    
    classn<-gsub("(^|[^0-9])0+","\\1",unlist(str_split(classn,",",simplify = FALSE)))
    classn<-gsub("\r\n", "", classn, perl = TRUE)
    
    classes<-as.data.frame(cbind.fill(classn,classdesc,classStatus), stringsAsFactors = FALSE)
    colnames(classes)<-c("classn","classdesc","classStatus")
    
  } else {
  
  classn<-gsub("\\D", "", classn)
  
  classn<-gsub("(^|[^0-9])0+", "\\1", classn, perl = TRUE)
 
  classes<-data.frame(classn,classdesc,classStatus, stringsAsFactors = FALSE)
 
  }
   
  rows<-nrow(classes)
  classes<-classes[classes$classStatus=="ACTIVE",]
  
  if (nrow(classes)==0 && rows>0) {
    
    classes<-data.frame(classn,classdesc,classStatus, stringsAsFactors = FALSE)
    
    classes<-head(classes,1)
  
  }
  
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
  #AppNo <-"74132499"


  #Making URL and Reading data
  current<-Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","English") 
 
  AppNo<-gsub(",","",AppNo)
  AppNo<-gsub("/","",AppNo)
  AppNo<-gsub("-","",AppNo, fixed=TRUE)
  
  try(rm("data"), silent = TRUE) 
  try(rm("tmpDF"), silent = TRUE) 
  try(rm("dataOwner"), silent = TRUE)
  try(rm("statusURL"), silent = TRUE)
  try(rm("imageUrl"), silent = TRUE)
  
  
  if (!grepl('^[0-9]+$', AppNo)) {
    
    tmpDF = as.data.frame(NULL)
    return(tmpDF)
  }
  
  url <-
    paste(
      "http://tsdr.uspto.gov/#caseNumber=",
      AppNo,
      "&caseType=SERIAL_NO&searchType=statusSearch",
      sep = ""
    )
  

  statusURL<-paste("http://tsdr.uspto.gov/statusview/sn",AppNo,sep="")

  
  
  
  try(data <- statusURL %>% read_html(), silent=TRUE)
  
  if (!(class(data)[1] %in% "xml_document")) {
    

    tmpDF = as.data.frame(NULL)
    return(tmpDF)
    
  
}
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
  
  priorityNo<-tail(priorityNo,1)
  if (length(priorityNo)==0) {
    
    priorityNo<-NA
  }
  
  if (is.na(priorityNo)) {
    
    priorityNo<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Registration Number:']/following::div[1]") %>% html_text())
    
    priorityNo<-tail(priorityNo,1)
    if (length(priorityNo)==0) {
      
      priorityNo<-NA
    }
    
    priority <-
      as.Date(
        gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Registration Date:']/following::div[1]") %>% html_text()),
        "%B %d, %Y"
      )
    
    priority<-format(priority, "%d.%m.%Y")
    
    if (length(priority)==0 || is.na(priority)) {
      
      priority<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
    }
    
    
  }
  
  priorityCountry<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Foreign Application/Registration Country:']/following::div[1]") %>% html_text())
  
  priorityCountry<-tail(priorityCountry,1)
  
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

  kind<-ifelse(is.null(kind),NA,kind)
  
  AppType<- try(gsub("\r\n","",data %>% html_nodes(xpath = "//span[contains(@data-sectiontitle,'International Registration')]/following::div[1]") %>% html_text()), silent=TRUE)

  AppType<-ifelse(length(AppType)>0,"International","National")
  

  
  agentOnRecord<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Attorney Name:']/following::div[1]") %>% html_text())
  
  if (length(agentOnRecord)==0) {
    
    agentOnRecord<-NA
  }
  
  
  agentOnRecordAddr<-gsub("\r","",data %>% html_nodes(xpath = "//div[text()='Correspondent Name/Address:']/following::div[1]") %>% html_text())

  agentOnRecordAddr<-gsub(agentOnRecord,"",agentOnRecordAddr)
  
  if (length(agentOnRecordAddr)==0) {
    
    agentOnRecordAddr<-NA
  }
  
  agentOnRecord<-paste(agentOnRecord,agentOnRecordAddr,sep="")
  
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
    imageName<-paste("./logos/", AppNo, ".jpeg", sep ="")
    try(download.file(imageUrl,imageName, mode = 'wb',cacheOK=FALSE), silent = TRUE)
    
    size<-file.info(imageName)$size
    #delete files with problems
    if (size<400)
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
  color<-ifelse(grepl('not',color),"Black and white",color)
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
  
  ownerAddr<-sub("\n","",ownerAddr)

  
  if (length(ownerAddr)==0) {
    
    ownerAddr<-NA
  }
  } else
  {
    owner1<-GetOwner(dataOwner)
    
    if (nrow(na.omit(owner1))==0) {
      
      owner<-gsub("\r\n","",data %>% html_nodes(xpath = "//div[text()='Owner Name:']/following::div[1]") %>% html_text())
      
      if (length(owner)==0) {
        
        owner<-NA
      }
      
      ownerAddr<-gsub("\r","",data %>% html_nodes(xpath = "//div[text()='Owner Address:']/following::div[1]") %>% html_text())
      
      ownerAddr<-sub("\n","",ownerAddr)
      
      
      if (length(ownerAddr)==0) {
        
        ownerAddr<-NA
      }
    }
    else {
    ownerAddr<-owner1$OwnerAddr
    owner<-trimws(gsub("Name:","",owner1$OwnerName))
    }
  }
  
   
 
   
   LimDis<-gsub('"','',data %>% html_nodes(xpath = "//div[text()='Disclaimer:']/following::div[1]") %>% html_text())
  
  if (length(LimDis)==0) {
    
    LimDis<-NA
    
  } 




  statusw<-data %>% html_nodes(xpath = "//div[text()='TM5 Common Status Descriptor:']/../div") %>% html_text()
  statusw<-paste(statusw,collapse = ",")
  
  if (grepl("LIVE/REGISTRATION",statusw)) {
    status<-"REGISTERED"
    
  } else if (grepl("LIVE/APPLICATION",statusw)) {
    
    status<-"FILED"
    
  }else if (grepl("DEAD/",statusw)) {
    
    status<-"INACTIVE"
    
  } else {status<-NA}

  if (acceptance!="01.01.1800" && status!="INACTIVE") {
  
  x <- 0 
  while (x<100) {
    
    x<-x+10
    
    tmpDate<-as.Date(acceptance,"%d.%m.%Y") %m+% years(x)
    
    if (tmpDate>today()) {
      
      renewal<- tmpDate
      
      break
      
    }
    
  }
  
  tmpDAU<-tmpDate<-as.Date(acceptance,"%d.%m.%Y") %m+% years(6)
  
  if (tmpDAU>today()) {
    
      DAU<-tmpDAU
      renewalGP<-as.Date(renewal,"%d.%m.%Y") %m+% months(6)
      DAUGP<-as.Date(DAU,"%d.%m.%Y") %m+% months(6)
  } else {
    
      DAU<- renewal
      renewalGP<-as.Date(renewal,"%d.%m.%Y") %m+% months(6)
      DAUGP<-as.Date(DAU,"%d.%m.%Y") %m+% months(6)

  }
  } else {
    renewal  <-as.Date("01.01.1800", "%d.%m.%Y")
    renewalGP<-as.Date("01.01.1800", "%d.%m.%Y") 
    DAU      <-as.Date("01.01.1800", "%d.%m.%Y")
    DAUGP    <-as.Date("01.01.1800", "%d.%m.%Y") 
  }

  renewal<-format(renewal, "%d.%m.%Y")
  if (length(renewal)==0 || is.na(renewal)) {
    
    renewal<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  renewalGP<-format(renewalGP, "%d.%m.%Y")
  if (length(renewalGP)==0 || is.na(renewalGP)) {
    
    renewalGP<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  DAU<-format(DAU, "%d.%m.%Y")
  if (length(DAU)==0 || is.na(DAU)) {
    
    DAU<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  DAUGP<-format(DAUGP, "%d.%m.%Y")
  if (length(DAUGP)==0 || is.na(DAUGP)) {
    
    DAUGP<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  words<-NA
  
  image<-NA
  

  trademark<-data %>% html_nodes(xpath = "//div[@id='summary']//div[text()='Mark:']/following::div[1]") %>% html_text()
  trademark<-gsub("\r","",trademark)
  trademark<-gsub("\t","",trademark)
  trademark<-gsub("\n","",trademark)
  trademark<-trimws(trademark)
  
  
  agComment<-data %>% html_nodes(xpath = "//div[text()='Status:']/following::div[1]") %>% html_text()
  agComment<-gsub("\r","",agComment)
  agComment<-gsub("\t","",agComment)
  agComment<-gsub("\n","",agComment)
  agComment<-trimws(agComment)
  
  ##AppNumber
  AppNumber<-data %>% html_nodes(xpath = "//div[text()='US Serial Number:']/following::div[1]")%>% html_text()
  AppNumber<-gsub("\r","",AppNumber)
  AppNumber<-gsub("\n","",AppNumber)
  AppNumber<-trimws(gsub("\t","",AppNumber))
  AppNumber<-gsub(",","",AppNumber)
  AppNumber<-gsub("/","",AppNumber)
  AppNumber<-gsub("-","",AppNumber, fixed=TRUE)
  
  #return DF
  tmpDF <- cbind(
    data.frame(
      AppNumber,
      trademark,
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
      renewalGP,
      DAUGP,
      color,
      words,
      image,
      imageUrl,
      agComment,
      LimDis,
      owner,
      ownerAddr,
      stringsAsFactors = FALSE
    ),
    tmpDF
  )

  tmpDF<-tmpDF%>%dplyr::rename(
    `Application no.`=AppNumber,
    Trademark=trademark,
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
    `Next renewal date: end of grace period`=renewalGP,
    `Next DAU date: end of grace period`=DAUGP,
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
    `9th Goods & Services`=description9,
    `Agent's comment`=agComment
  )


  if (class(tmpDF) != "data.frame")
  {
    tmpDF = as.data.frame(NULL)
  }

  rm(list=setdiff(ls(), "tmpDF"))
  
  Sys.setlocale("LC_TIME",current)
  
  return(tmpDF)
}