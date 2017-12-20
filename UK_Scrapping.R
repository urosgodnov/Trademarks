UKScrap <- function(AppNo) {
  #AppNo <-"2119496B"



  
  #Making URL and Reading data
  
  AppNo <- gsub(",", "", AppNo)
  AppNo <- gsub("/", "", AppNo)
  AppNo <- gsub(".", "", AppNo, fixed = TRUE)
  
  
  url <-
    paste("https://trademarks.ipo.gov.uk/ipo-tmcase/page/Results/1/UK",
          str_pad(AppNo, 11, pad = "0"),
          sep = "")
  
  data <- try(url %>% read_html, silent = TRUE)
  
  AppNo <-
    gsub("(?<![0-9])0+","",gsub(
      "UK",
      "",
      data %>% html_nodes(xpath = "//dt[text()='Trade mark number']/following::h1[1]") %>%
        html_text()
    ), perl = TRUE)
  
  if (length(AppNo)==0) {
    AppNo<-NA
    
  }
  
  if (class(data)[[1]] != "try-error" && !is.na(AppNo))
  {

    
    application <- try(as.Date(
      data %>% html_nodes(xpath = "//dt[text()='Filing date']/following::dd[1]") %>%
        html_text(),
      "%d %B %Y"
    ))
    
    application <- format(application, "%d.%m.%Y")
    
    if (length(application) == 0 || is.na(application)) {
      application <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
    }
    
    acceptance <- try(as.Date(
      data %>% html_nodes(xpath = "//dt[text()='Date of entry in register']/following::dd[1]") %>%
        html_text(),
      "%d %B %Y"
    ))
    
    acceptance <- format(acceptance, "%d.%m.%Y")
    
    if (length(acceptance) == 0 || is.na(acceptance)) {
      acceptance <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
    }
    
    
    renewal <-
      try(as.Date(
        data %>% html_nodes(xpath = "//dt[text()='Renewal date']/following::dd[1]") %>%
          html_text(),
        "%d %B %Y"
      ))
    
    renewal <- format(renewal, "%d.%m.%Y")
    
    if (length(renewal) == 0 || is.na(renewal)) {
      renewal <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
    }
    
    TMName <-
      data %>% html_node(xpath = "//dd[@style='text-align:center;']") %>%
      html_text()
    
    if (is.na(TMName)) {
      
      
      imageUrl <-
        data %>% html_nodes(xpath = "//div[@class='uploaded-image']/a")%>%html_attr("href")
        
      
        if (length(imageUrl) == 1 && !is.na(imageUrl)) {
          cat(paste("\n","Downloading image...",sep=""))
          imageName<-paste("./logos/", AppNo, ".jpg", sep ="")
          try(download.file(imageUrl,imageName, mode = 'wb',cacheOK=FALSE), silent = TRUE)
        } else {
          
          imageUrl<-NA}
      
    }
    
    #Priority date
    priority <-
      try(as.Date(
        data %>% html_nodes(xpath = "//dt[text()='Priority date']/following::dd[1]") %>%
          html_text(),
        "%d %B %Y"
      ))
    
    priority <- format(priority, "%d.%m.%Y")
    
    if (length(priority) == 0 || is.na(priority)) {
      priority <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
    }
    
    
    priorityCountry <-
      data %>% html_node(xpath = "//dt[text()='Priority country']/following::dd[1]") %>%
      html_text()
    
    
    if (length(priorityCountry) == 0) {
      priorityCountry <- NA
    }
    
    priorityNo<-NA
    
    owner <-
      data %>% html_nodes(xpath = "//h3[text()='Owner(s) name']/following::dt[1]") %>%
      html_text()
    
    
    if (length(owner) == 0) {
      owner <- NA
    }
    
    
    ownerAddr <-
      data %>% html_nodes(xpath = "//h3[text()='Owner(s) name']/following::dd[1]") %>%
      html_text()
    
    
    ownerAddr <- str_trim(ownerAddr)
    
    if (length(ownerAddr) == 0) {
      ownerAddr <- NA
    }
    
    
    
    agentOnRecord1 <-
      data %>% html_nodes(xpath = "//h3[text()='IPO representative name']/following::dt[1]") %>%
      html_text()
    
     if (length(agentOnRecord1) == 0) {
       agentOnRecord1 <- ""
    }
    
    agentOnRecord2 <-
      data %>% html_nodes(xpath = "//h3[text()='IPO representative name']/following::dd[1]") %>%
      html_text()
    
    if (length(agentOnRecord2) == 0) {
      agentOnRecord2 <- ""
    }
  
  agentOnRecord <-
    paste(agentOnRecord1, agentOnRecord2, sep = "\n ")
  
  
  #PublicationDate
  publication <- try(as.Date(
    data %>% html_nodes(xpath = "//dt[text()='Date of publication']/following::dd[1]") %>%
      html_text(),
    "%d %B %Y"
  ))
  
  publication <- format(publication, "%d.%m.%Y")
  
  if (length(publication) == 0 || is.na(priority)) {
    publication <- format(as.Date("1800-01-01", "%Y-%m-%d"), "%d.%m.%Y")
  }
  
  
  publicationNo <-
    trimws(gsub(
      "\r\n",
      "",
      data %>% html_nodes(xpath = "//dt[text()='Journal']/following::dd[1]") %>%
        html_text()
    ))
  
  if (length(publicationNo) == 0) {
    publicationNo <- NA
  }
  
  #NA
  associatedTMs <- NA
  
  
  #####Classes
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
  
  
  classNo <-
    trimws(gsub(
      "Class",
      "",
      data %>% html_nodes(xpath = "//div[@id='tab-2']//h2[@class='subsection__title']") %>%
        html_text()
    ))
  
  classDesc <-
    trimws(gsub(
      "Class",
      "",
      data %>% html_nodes(xpath = "//div[@id='tab-2']//p") %>%
        html_text()
    ))
  
  for (i in 1:length(classNo))
  {
    currentClassNumber <- classNo[[i]]
    
    
    currentClassDesc <- classDesc[[i]]
    
    tmpDF[, i] <- currentClassNumber
    
    tmpDF[, i + 9] <- currentClassDesc
    
  }
  
  status<-
    data %>% html_nodes(xpath = "//dt[text()='Status']/following::dd[1]") %>%
    html_text()
  
  if (length(status) == 0) {
    status <- NA
  }
  
  LimDis <-
    data %>% html_nodes(xpath = "//dt[text()='Disclaimer']/following::dd[1]") %>%
    html_text()
  
  
  if (length(LimDis) == 0) {
    LimDis <- NA
  }
  
  
  words <- NA
  
  image <- NA
  
  kind <- NA
  
  RegNo<-AppNo
  
  #return DF
  tmpDF <- cbind(
    data.frame(
      AppNo,
      TMName,
      RegNo,
      renewal,
      application,
      acceptance,
      priority,
      priorityNo,
      priorityCountry,
      publication,
      publicationNo,
      agentOnRecord,
      associatedTMs,
      status,
      kind,
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
  
  tmpDF <- tmpDF %>% dplyr::rename(
    `Application no.` = AppNo,
    Trademark=TMName,
    `Application date` = application,
    `Registration no.` = RegNo,
    `Registration date` = acceptance,
    `Next renewal date` = renewal,
    `Priority date` = priority,
    `Priority no.` = priorityNo,
    `Priority country` = priorityCountry,
    `Publication date` = publication,
    `Publication no.` = publicationNo,
    `TM Type` = kind,
    Status = status,
    `Limitations & Disclaimers` = LimDis,
    `Agent on record` = agentOnRecord,
    Owner = owner,
    `Owner address` = ownerAddr,
    `Associated TMs` = associatedTMs,
    `1st Class` =   class1,
    `1st Goods & Services` = description1,
    `2nd Class` = class2,
    `2nd Goods & Services` = description2,
    `3rd Class` = class3,
    `3rd Goods & Services` = description3,
    `4th Class`  = class4,
    `4th Goods & Services` = description4,
    `5th Class`   = class5,
    `5th Goods & Services` = description5,
    `6th Class`   = class6,
    `6th Goods & Services` = description6,
    `7th Class`  = class7,
    `7th Goods & Services` = description7,
    `8th Class`  = class8,
    `8th Goods & Services` = description8,
    `9th Class`  = class9,
    `9th Goods & Services` = description9
  )
  #
  
} else
{
  tmpDF = as.data.frame(NULL)
}


return(tmpDF)
}
