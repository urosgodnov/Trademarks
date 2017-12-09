# load libraries
packages=c("dplyr","XML","rvest","stringr","plyr","xml2","pryr","lubridate","readxl","xlsx","data.table",
           "rowr","jpeg","png","RGtk2","RSelenium","gsubfn","rPython","magick",
           "openssl","gWidgets2","utils","rowr","caTools","convertGraph")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#loading sources
source("Auxillary.R")
source("JoinAndCompare.R")
source("AUS_Scrapping.R")
source("US_Scrapping.R")
source("AR_Scrapping.R")
source("BX_Scrapping.R")
source("Main.R")
#python.load("PyhtonScrapDataURI.py")

## layout - one page with 2 boxes
w <- gwindow("Scraping and validating data", visible=FALSE)

pg <- ggroup(cont=w)

pg$set_borderwidth(10L)

lg <- gvbox(cont=pg)

rg <- gvbox(cont=pg, expand=TRUE, fill=TRUE)


## put main text widget and table widget into paned group so user can
## adjust allocated space - 2 windows
pg <- gpanedgroup(cont=rg, expand=TRUE, horizontal=FALSE)

fr <- gvbox(cont=pg, expand=TRUE, fill=TRUE, spacing=5)

l <- glabel(gettext("Messages:"), cont=fr, anchor=c(-1,0))

font(l) <- list(weight="bold")

#the style of a text in messages
outputConsole <- gtext("", wrap=FALSE,
                       font.attr=list(family="monospace"),
                       container=fr, expand=TRUE, fill=TRUE)

fr <- gvbox(cont=pg, expand=TRUE, fill=TRUE, spacing=5)

## buttons in a button group
bg <- ggroup(cont=rg)

addSpring(bg)

execute_btn <- gbutton("execute", cont=bg)


cancel_btn <- gbutton("cancel", cont=bg, handler=function(...)  dispose(w))


## use a form layout for ease in laying out the controls to adjust the
## arguments for `read.table`
flyt <- gformlayout(cont=lg)

addSpring(lg)

# choose country
countries <- c("Australia"="Australia", "BENELUX"="BENELUX", "USA"="USA", "Argentina"="Argentina", "China"="China")

country <- gcombobox(names(countries), cont=flyt, label="Country",
                     handler=function(h,...){
                       if (svalue(country)=="China")
                       {
                         visible(bgl2)<-TRUE
                         visible(what)<-FALSE
                         svalue(what)="only compare"
                         
                       } else {
                         
                         visible(bgl2)<-FALSE
                         visible(what)<-TRUE
                      
                       }
                       
                     } )

# choose wheter only compare or also scrap and compare

what <- gradio(c("scrap and compare", "only compare"), horizontal=TRUE, 
               cont=flyt, label="Action",
               handler=function(h,...){
                 if (svalue(what)=="only compare")
                 {
                   visible(bgl2)<-TRUE
                   
                 } else {visible(bgl2)<-FALSE}
                     

               })


#validation file with new group, so user can hide button
bgl1 <- ggroup(cont=flyt)
filev<- gfilebrowse(text = "Select validation file", type = "open",
                   initial.dir = getwd(),
                   filter=list("Excel"=list(patterns=c("*.xls","*.xlsx"))),
                   container = bgl1,
                   handler=function(h,...){
                   cat(svalue(filev))
                   })

#source file with new group, so user can hide button
bgl2 <- ggroup(cont=flyt)
files<- gfilebrowse(text = "Select file with scraped data (County_online.xlsx)", type = "open",
                    initial.dir = getwd(),
                    filter=list("Excel"=list(patterns=c("*.xls","*.xlsx"))),
                    container = bgl2
                    )

#init
visible(bgl2)<-FALSE
size(w) <- c(800, 400)
visible(w) <- TRUE



#adding handler for execute button
addHandlerChanged(execute_btn, handler=function(h,...) {

  svalue(outputConsole)<-""
  
  #checking if all requirements are met
  if (svalue(what)=="only compare") {
    
    if (length(svalue(filev))==0 || length(svalue(files))==0)
    {
      galert("Make sure validation and online files are selected!",parent=w)

      
    } else {execute(svalue(country),svalue(what),svalue(filev),svalue(files))} 
      
  } else if (length(svalue(filev))==0) {
    
    galert("Make sure validation file is selected!", parent=w)

    
    
  } else {
       execute(svalue(country),svalue(what),svalue(filev),svalue(files))}
  
  
  
})





