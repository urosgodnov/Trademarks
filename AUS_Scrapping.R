AUSScrap <- function(AppNo) {
  #AppNo <- 553230


  #Making URL and Reading data
  url <-
    paste(
      "https://search.ipaustralia.gov.au/trademarks/search/view/",
      AppNo,
      "/details",
      sep = ""
    )
  
  data <- url %>% read_html()
  
  trademark <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//tr[th/text()='Words']/td[1]") %>% html_text()
    )
  
  renewal <-
    as.Date(
      data %>% html_nodes(xpath = "//tr[td/text()='Renewal due']/td[2]") %>% html_text(),
      "%d %B %Y"
    )
  
  renewal<-format(renewal, "%d.%m.%Y")
  
  if (length(renewal)==0 || is.na(renewal)) {
    
    #when rbind I want dates to stay dates
    renewal<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  application <-
    as.Date(
      data %>% html_nodes(xpath = "//tr[td/text()='Lodgement']/td[2]") %>% html_text(),
      "%d %B %Y"
    )
  
  application<-format(application, "%d.%m.%Y")
  
  if (length(application)==0 || is.na(application)) {
    
    application<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  
  acceptance <-
    as.Date(
      data %>% html_nodes(xpath = "//tr[td/text()='Registered from']/td[2]") %>% html_text(),
      "%d %B %Y"
    )
  
  acceptance<-format(acceptance, "%d.%m.%Y")
  if (length(acceptance)==0 || is.na(acceptance)) {
    
    acceptance<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  datep<-data %>% html_nodes(xpath = "//tr[th/text()='Priority date']/td[1]") %>% html_text()
  
  datep<-gsub("(Lodgement)","",datep)
  datep<-gsub("Convention","",datep)
    
  
  
  priority <-
    as.Date(gsub(
      "\n",
      "",
     datep
    ), "%d %B %Y")
  
  priority<-format(priority, "%d.%m.%Y")
  
  if (length(priority)==0 || is.na(priority)) {
    
    priority<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  

  
  status <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//tr[th/text()='Status']/td[1]") %>% html_text()
    )
  
  kind <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//tr[th/text()='Kind']/td[1]") %>% html_text()
    )
  
  #words
  words <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//div[h5/text()='Indexing constituents']//span[text()='Word']/following::table[1]//td") %>%
        html_text()
    )
  words <- paste(words, collapse = ",")
  
  #Image
  image <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//div[h5/text()='Indexing constituents']//span[text()='Image']/following::table[1]//td") %>%
        html_text()
    )
  image <- paste(image, collapse = ",")
  
  #Limitations & Disclamiers
  LimDis <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//div[h5/text()='Endorsements']/p") %>% html_text()
    )
  if  (length(LimDis)==0){
     
    LimDis<-NA
  }
  
  #Owner
  owner <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//td[div/@id='addressHolder_OWNER_0_0']/div/span") %>% html_text()
    )
  
  #Put into one line
  owner<-paste(owner,collapse = "|")
  
  ownerAddr <-
    gsub(
      "\n",
      " ",
      data %>% html_nodes(xpath = "//td[div/@id='addressHolder_OWNER_0_0']//div[@class='js-address hidden']") %>% html_text()
    )
  
  ownerAddr<-paste(ownerAddr,collapse = "|")
  
  #Agent on record
  agentOnRecord <-
    gsub(
      "\n",
      "",
      data %>% html_nodes(xpath = "//td[div/@id='addressHolder_ADDRESS_FOR_SERVICE_0_0']/div/span") %>% html_text()
    )
  agentAddr <-
    gsub(
      "\n",
      " ",
      data %>% html_nodes(xpath = "//td[div/@id='addressHolder_ADDRESS_FOR_SERVICE_0_0']//div[@class='js-address hidden']") %>% html_text()
    )
  
  
  
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
      data %>% html_nodes(xpath = "//span[@class='title']") %>% html_text()
    )
  
  classes<-gsub('\\D+','', classes)
  classes <- strsplit(classes, ",")
  
  #Search and fill columns in tmpDF
  for (i in 1:length(classes))
  {
    
    currentClassNumber <- classes[[i]]
    
    currentClassDesc <-
      gsub("\n", "", data %>% html_nodes(
        xpath = paste(
          "//div[span/text()='Class ",
          currentClassNumber,
          ":']/span[2]",
          sep = ""
        )
      ) %>% html_text())
    
    tmpDF[, i] <- currentClassNumber
    
    tmpDF[, i + 9] <- currentClassDesc
    
  }
  
  ###Dealing with images
  imageUrl <-
    data %>% html_nodes(xpath = "//div[@id='imageContainer']//@src") %>% html_text()

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
      renewal,
      application,
      acceptance,
      priority,
      status,
      kind,
      words,
      image,
      imageUrl,
      LimDis,
      owner,
      ownerAddr,
      agentOnRecord,
      agentAddr,
      stringsAsFactors = FALSE
    ),
    tmpDF
  )

  tmpDF<-tmpDF%>%dplyr::rename(
    `Application no.`=AppNo,
    `Application date`=application,
    `Registration no.`=AppNo,
    `Registration date`=acceptance,
    `Next renewal date`=renewal,
    `Priority date`=priority,
    `TM Type`=kind,
     Status=status,
    `Limitations & Disclaimers`=LimDis,
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
  
  tmpDF<-tmpDF%>%mutate(`Application no.`=as.character(`Registration no.`))
  
  if (class(tmpDF) != "data.frame")
  {
    tmpDF = as.data.frame(NULL)
  }
  
  return(tmpDF)
}