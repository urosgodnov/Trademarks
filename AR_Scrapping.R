
getStatus<-function(x) {
  
  if (x=='Abandonada' || x=='Caducidad' || x=='Denegada' || x=='Desistida' || x=='Nulidad') {
    
        m<-'INACTIVE' }
  else {
    
        m<-'REGISTERED'}
 
  
  
  return(m)
   
}


getType<-function(x) {
  
  if (x=='Denominativa') {
    
    m<-'WORD'
    
  } else if (x=='Mixta') {
    
    m<-'WORD-DEVICE'
  
    } else if (x=='Figurativa') {
  
      m<-'DEVICE'
      
    } else if (x=='Tridimensional') {
      
      m<-'3D'
      
    } else if (x=='Sonora') {
      
      m<-'SOUND'
      
    } else {
      
      m<-'NO TM LIKE THAT'
    }
  
    return(m)
    
}

getDatePattern <- function(text,pattern) {

  result <- regmatches(text,regexec(pattern,text))
  r<-result[[1]][2]
  r<-strapplyc(r, "[0-9/]{8,}", simplify = TRUE)

  date<-try(as.Date(r,"%d/%m/%Y"),silent = TRUE)
  
  date<-try(format(date, "%d.%m.%Y"),silent=TRUE)
  
  if (length(date)==0 || is.na(date) || class(date)=='try-error') {
    
    date<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  return(date)
}

getNamesPattern <- function(text,pattern) {
 # pattern<-".*Denominación:(.*?)Tipo.*"
  result <- regmatches(text,regexec(pattern,text))
  r<-result[[1]][2]
  r<-gsub("\r\n","",r)
  r<-trimws(r,'b')
  
  return(r)
}

ARScrap <- function(AppNo) {
  #AppNo <-764566

  #Making URL and Reading data
  
  AppNo<-gsub(",","",AppNo)
  AppNo<-gsub("/","",AppNo)
  AppNo<-gsub(".","",AppNo, fixed = TRUE)
  
  url <-
    paste(
      "https://portaltramites.inpi.gob.ar/Clasico/Docs/ResultadosConsultas/ResultadoSolicitudMarca2.asp?Va=",
      AppNo,
      sep = ""
    )
  
  data <- url %>% read_html(encoding="utf-8")

  application <-data %>% html_node(xpath = "//tr//div[contains(.,'Presentaci')]")%>% 
                html_text()
  
  application<-getDatePattern(application,".*Presentaci(.*?)Denominaci.*")
  
 
  registrationNo<-data %>% html_node(xpath = "//strong[contains(.,'Resoluci')]/following::tr[1]//span[text()='Nro:']/following::span")%>% 
                  html_text()
  
  registrationNo<-try(as.numeric(registrationNo))
  
  if (length(registrationNo)==0) {

    registrationNo<-NA
  }
  
  
  
  #Previous registration number
  prevRegNo<-data %>% html_node(xpath = "//tr//div[contains(.,'Direccion de Marcas')]/following::tr[4]//td[span/text()='Renov. de']")%>% 
    html_text()
  
  prevRegNo<-try(as.numeric(prevRegNo))
  
  if (length(prevRegNo)==0) {
    
    prevRegNo<-NA
  }
  

  renewal<-try(as.Date(data %>% html_nodes(xpath = "//tr/td[text()='Vence:']/following::td[1]")%>% 
                html_text(),"%d/%m/%Y"))
  
  ####Acceptance - it has to be in unambiguous format
  acceptance<-try(as.POSIXlt(renewal))
  
  renewal<-format(renewal, "%d.%m.%Y")
  
  if (length(renewal)==0 || is.na(renewal)) {

    renewal<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  
  TMName<-data %>% html_node(xpath = "//tr//div[contains(.,'Presentaci')]")%>% 
    html_text()
    
  TMName<-getNamesPattern(TMName,".*Denominación:(.*?)Tipo.*")
    
  
  
  
  acceptance$year<-acceptance$year-10
  

  acceptance<-format(acceptance, "%d.%m.%Y")
  if (length(acceptance)==0 || is.na(acceptance)) {

    acceptance<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }
  

  
  #Priority date 
  priority <-
     as.Date(
      data %>% html_node(xpath = "//strong[contains(.,'Prioridades')]/following::tr[1]//span[text()='Fecha:']/following::td")%>% 
  html_text(),
       "%d/%m/%Y"
     )
   
  priority<-format(priority, "%d.%m.%Y")

  if (length(priority)==0 || is.na(priority)) {

    priority<-format(as.Date("1800-01-01","%Y-%m-%d"),"%d.%m.%Y")
  }


  priorityNo<-gsub(" ","",data %>% html_node(xpath = "//strong[contains(.,'Prioridades')]/following::tr[1]//span[text()='Prioridad: ']/following::td")%>% 
  html_text())

  if (length(priorityNo)==0) {

    priorityNo<-NA
  }

  priorityCountry<-gsub(" ","",  data %>% html_node(xpath = "//strong[contains(.,'Prioridades')]/following::tr[1]//div[contains(.,'Pais:')]/following::td")%>% 
  html_text())
  
  if (length(priorityCountry)==0) {

    priorityCountry<-NA
  }
  
  #PublicationDate
  publication <-data %>% html_node(xpath = "//span[contains(.,'PUBLICACI')]/following::tr[1]")%>% 
    html_text()
  
  publication<-getDatePattern(publication,".*Fecha(.*?)Numero.*")
  
  
 publicationNo<-data %>% html_nodes(xpath = "//span[contains(.,'PUBLICACI')]/following::tr[1]/td[contains(@class,'titulorojo')]//a")%>% 
   html_text()
 
 publicationNo<-gsub('([0-9]+).*','\\1',publicationNo)
 
 if (length(publicationNo)==0) {
   
   publicationNo<-NA
 }
  
  owner<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//p[text()='Nombre']/following::td[1]") 
    %>% html_text())
  
  
  
  if (length(owner)==0) {
    
    owner<-NA
  } 
  
  owner<-paste(owner, collapse=" | ")

  ownerAddr<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//td[text()='Domicilio Real']/following::span[1]") 
                  %>% html_text())
  
  ownerAddr<-str_trim(ownerAddr)
  
  OwnerCountry<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//td[text()='Territorio Leg.']/following::td[1]") 
                  %>% html_text())
  
  OwnerCountry<-str_trim(OwnerCountry)
  
  ownerAddr<-paste(ownerAddr,OwnerCountry,sep=" ")
  
  ownerAddr<-str_trim(gsub("-","",ownerAddr))
  
  if (length(ownerAddr)==0) {
    
    ownerAddr<-NA
  }
  
  ownerAddr<-paste(ownerAddr,collapse=" | ")
  
  agentOnRecord1<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Gestion')]/following::tr[1]//td[text()='Agente:']/following::td[1]") 
                      %>% html_text())
  
  agentOnRecord1<-str_trim(gsub('[[:digit:]]+', '', agentOnRecord1))
  
  agentOnRecord2<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Gestion')]/following::tr[1]//span[text()='Caracter:']/following::td[1]") 
                       %>% html_text())
 
  
  if (length(agentOnRecord2)>0) {
  agentOnRecord<-paste(agentOnRecord1," (",agentOnRecord2,")",sep="")
  } else {agentOnRecord<-agentOnRecord1}
  
  if (length(agentOnRecord)==0) {

    agentOnRecord<-NA
  }

  AgentOnRecordAddr<-data %>% html_node(xpath = "//tr//div[contains(.,'Presentaci')]")%>%
    html_text()
  
  AgentOnRecordAddr<-str_trim(getNamesPattern(AgentOnRecordAddr,".*Domicilo legal:(.*?)Renov. de*"))
  
  AgentOnRecordAddr<-paste(AgentOnRecordAddr,' ARGENTINA',sep=",")
  
  if (length(AgentOnRecordAddr)==0) {
    
    AgentOnRecordAddr<-NA
  }

  agentOnRecord<-paste(agentOnRecord,AgentOnRecordAddr,sep="\n ")
  
  #NA
  associatedTMs<-NA
  
  ###Dealing with images 
  imageUrl<-data %>% html_node(xpath = "//tr//td[@class='titulorojo']/a")%>% 
             html_attr("href")
  

  if (length(imageUrl) == 1 && !is.na(imageUrl)) {
    cat(paste("\n","Downloading image...",sep=""))
    
    img <- imageUrl%>%read_html()%>%html_nodes("img")
    
    img_src <- html_attr(img, "src")
    
    if (length(img_src)==1) {
    img_src <- gsub("^data.*base64,", "", img_src)
    
    imageJPG<-try(image_read(base64_decode(img_src)), silent=TRUE)
    
    if (class(imageJPG)!="try-error") {
    
    image_write(imageJPG,path=paste("./logos/",AppNo,".jpg",sep=""))
      
    }
    #python.call("decodeImgData", imageUrl,as.character(AppNo))
    } else {imageUrl<-NA}
  } else {imageUrl<-NA}
  
  
  
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
    
    classNo<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//td[contains(.,'Clase')]") 
                  %>% html_text())
    
    if (length(classNo)>0)
    {
    
    classNo<-getNamesPattern(classNo,".*Clase(.*?)Proteccion.*")
    
    } else 
      
      {classNo<-NA}
    
    classDes<-gsub("\r\n","",data %>% html_nodes(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//td[contains(.,'Clase')]") 
                  %>% html_text())
    
    if (length(classDes)>0)
    {
       classDes<-getNamesPattern(classDes,".*Proteccion.(.*?)Limitacion*")
    
    } else {classDes<-NA}
   
    tmpDF$class1<-classNo
    
    tmpDF$description1<-classDes
    
     
  
  
  LimDis<-gsub("\r\n","",data %>% html_node(xpath = "//strong[contains(.,'Datos de titulares')]/following::tr[1]//td[contains(.,'Limitacion:')]/following::td[1]") 
               %>% html_text())
  
 
  if  (length(LimDis)==0){

    LimDis<-NA }
  
  LimDis<-str_trim(gsub("-","",LimDis))
  
  words<-NA
  
  image<-NA
  
  




  status<-data %>% html_node(xpath = "//strong[contains(.,'Resoluci')]/following::tr[1]//strong[text()='Tipo:']/following::span")%>% 
    html_text()
  
  status<-getStatus(status)

  kind<-data %>% html_node(xpath = "//tr//div[contains(.,'Direccion de Marcas')]/following::tr[2]//td[span/text()='Tipo Marca :']")%>% 
    html_text()
  
  kind<-str_trim(gsub("Tipo Marca :","",kind))
  
  kind<-getType(kind)
  
  
  
  #return DF
  tmpDF <- cbind(
    data.frame(
      AppNo,
     # TMName,
      registrationNo,
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
      prevRegNo,
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
  
  tmpDF<-tmpDF%>%dplyr::rename(
    `Application no.`=AppNo,
    # Trademark=TMName,
    `Application date`=application,
    `Registration no.`=registrationNo,
    `Registration date`=acceptance,
    `Next renewal date`=renewal,
    `Priority date`=priority,
    `Priority no.`=priorityNo,
    `Priority country`=priorityCountry,
    `Publication date`=publication,
    `Publication no.`=publicationNo,
    `TM Type`=kind,
     Status=status,
    `Limitations & Disclaimers`=LimDis,
    `Agent on record`=agentOnRecord,
     Owner=owner,
    `Owner address`=ownerAddr,
    `Associated TMs`=associatedTMs,
    `Previous registration no.`=prevRegNo,
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
  #
  
  if (class(tmpDF) != "data.frame")
  {
    tmpDF = as.data.frame(NULL)
  }
  
  
  return(tmpDF)
}
