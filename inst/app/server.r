if (TRUE) {    # header
  #/*soh*************************************************************************
  # CODE NAME             : server.r
  # CODE TYPE							: Program 
  # DATE OF UPDATE:         6-Oct-2016
  # DESCRIPTION           : Server code for BEACH app 
  # SOFTWARE/VERSION#     : R 3.2.0
  # INFRASTRUCTURE        : 
  #  -----------------------------------------------------------------------------
  #  Ver   Author                      Program History Description
  #  ----  ---------------            --------------------------------------------
  #  001      Danni Yu                   Program
  #  002      Chenchen Yu                Program
  #  -----------------------------------------------------------------------------
} #Header
 

    rm(list=ls())
  
#install packages
if(TRUE){
  dep.packages <- c("shiny", "DT", "haven", "xtable", "rtf", "plyr", "sas7bdat", "WriteXLS", 
                    #"SASxport", 
                    "rJava", "devtools");
  na.packages <- dep.packages[!dep.packages %in% installed.packages()]
  if (length(na.packages)>0) install.packages(na.packages);
  
  #if(!"sas7bdat.parso" %in% installed.packages()) devtools::install_github('BioStatMatt/sas7bdat.parso', force=TRUE)
}
    
#required libraries
if(TRUE){    
    library(shiny)
  
    library(DT) #for render table
    library(sas7bdat) 
    library(haven) #for loading SAS datasets
  
    #load a libray not in cran
    if("sas7bdat.parso" %in% installed.packages() ){
      library(sas7bdat.parso)
    }
    
    library(xtable)
    library(rtf)
    library(plyr)
    
    library(WriteXLS)
    library(readxl)
    #library(SASxport)
    
 } #required libraries

#Key objects and functions
if (TRUE){
  use_haven<<-T
  tmpFig <<-"tmpFig.png"
  showBrush<<-FALSE
  click_data<<- near_points <<- brush_points <<-NULL
  currTabL <<- list()
  input0 <<- list()
  input0.code <<- new.code <<- NULL
  ault <<- '480, 480, NA'
  tfl.h <<- tfl.w <<- tfl.r <<- NA
  
  infilelab <<- 'Step 1 Upload data (csv, sas7bdat, xlsx, rdata, xpt)'
  infilenm <<- c('.csv', '.sas7bdat', '.xlsx', '.rdata', '.Rdata', '.xpt')

  na_sign <<- c('NA', '', '.', ' ', '-', 'NaN')
  
  #standard headers
  if(TRUE){
    sop_head<<-paste( paste(readLines("sopHead.txt"), collapse="\n "), "\n ")
  } else {
    data('sopHead')
  }
  
  if(!exists('muliHead.split')){
    muliHead.split <<- ";"
  }

  #get the current userid for usage track
  uids<<-NULL
  uid1<<-Sys.info()['user']
  countFnm<<-'counter_m.Rdata'
  countFnm.strt<<-'counter_startDate.Rdata'
  if(!file.exists(countFnm) | !file.exists(countFnm.strt) ){
    uids<<-uid1
    #2016-01-06 add study name and tumor type
    uids<<-data.frame(userID=uids, studyName='', tumorType="", time=Sys.time())
    try(save(uids, file=countFnm))
    counter.startDate<<-format(Sys.time(), "%Y-%m-%d")
    try(save(counter.startDate, file=countFnm.strt))
  }else{
    try(load(countFnm.strt))
    counter.startDate<<-counter.startDate
    
    load(file=countFnm)
    uid2<-data.frame(userID=uid1, studyName='', tumorType="", time=Sys.time())
    uids<<-rbind(uids, uid2)
    try(save(uids, file=countFnm))
  }

  #---------source code for shiny functions---------------#
  source(file=file.path(local.path1,'shinyFun.r'))
  source(file=file.path(local.path1,'RTF_functions_DY.r'))



  #function: read sas, csv or excel files.
  BeachRead<<-function(file, header=TRUE, nrow=-1,  
                          name=NULL, comment.char="",  
                          xlsx1=NULL, 
			  na.string=c('NA', '', '.', ' ', '-', 'NaN'),
                          SF=FALSE, use_haven=T, ...){
    #load a libray not in cran
    #library(sas7bdat.parso, lib.loc="libs")
    
    ot<-NULL
    if(is.null(name)){name<-file}
    is.sas<-grepl('.sas7bdat', tolower(name), fixed=TRUE)
    is.csv<-grepl('.csv', tolower(name), fixed=TRUE)
    is.xlsx<-grepl('.xls', tolower(name), fixed=TRUE)
    is.rdat<-grepl('.rda', tolower(name), fixed=TRUE)
    is.xpt<-grepl('.xpt', tolower(name), fixed=TRUE)
    if(is.sas){
      ot.t <- try(ot<-haven::read_sas(file))
      if(class(ot.t)[1]=='try-error' && 
	 "sas7bdat.parso" %in% installed.packages() &&
	 requireNamespace("sas7bdat.parso", quietly = TRUE)) {
         try(ot<-sas7bdat.parso::read.sas7bdat.parso(file))
      }
      if(is.null(ot)){
	 if("sas7bdat.parso" %in% installed.packages() ){
           return(paste("Error: fail to import", name))
         }else{
           return(paste("Error: fail to import", name,
		       ". Please import sas7bdat.parso at GitHub."))
	 }
      }
      ot <- data.frame(ot)
      for(i in 1:ncol(ot)){
        if(is.factor(ot[,i]))
          ot[,i] <- as.character(ot[,i])
      }
      if(nrow==1)
         ot <- ot[1,]
      if(nrow==1 & !header)
         ot <- data.frame(matrix(colnames(ot), nrow=1))
    }else if (is.csv){
      tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, 
                           na=na.string, stringsAsFactors=SF,
                           fileEncoding="UTF-8", 
                           comment.char=comment.char,...))
      if(class(tm)=='try-error')
        tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, 
                             na=na.string, stringsAsFactors=SF,
                             fileEncoding="Latin1", 
                             comment.char=comment.char, ...))
      if(class(tm)=='try-error')
        tm<-try(ot<-readLines(con=file))     
    }else if (is.xlsx){
      if(is.null(xlsx1))
        xlsx1 <-1
      tm<-try(ot<- readxl::read_excel(path=file, sheet=xlsx1, col_names=header, na=na.string, ...))
      if(class(tm)=='try-error')
        tm<-try(ot<-readLines(con=file))    
    }else if (is.rdat){ 
      load(file, ot<-new.env())
      ot <- as.list(ot)
      names(ot) <- paste0(name, ".", names(ot))
    }else if (is.xpt){
      #ot.t <- try( ot <- SASxport::read.xport(file) )
      ot.t <- try( ot <- haven::read_xpt(file) )
    }else{
      ot<-NA
    }
    return(ot) 
  }
  
  
  if(!'www'%in%dir(local.path2)) {dir.create(local.path3)}
  
  options(stringsAsFactors=FALSE, shiny.usecairo=FALSE, shiny.maxRequestSize=1024*1024^2)
  options(warn=0, error = NULL)
  
}#Key objects and functions


#-----------------------------------#
if(TRUE){#other setup#

  #define the users folder for Analysis Code
  cnt<-0
  user_folder<-c('users/')
  function_r<-dir(local.path1, ".r")
  function_r<-c(function_r, dir(local.path1, ".R"))
  
  tmConf<<-'configuration_empty_dy1.csv'
  
  #clean up open device
 if(TRUE){
    na_file<-dir()
    na_file<-na_file[substr(na_file,1,2)=='NA'|na_file=="Rplot.pdf"]
    na_file<-file.path(getwd(),na_file)
    if(length(na_file)>0){do.call(file.remove, as.list(na_file))}
    tm_png<-dir(local.path3)
    tm_png<-tm_png[substr(tm_png,1,3)=='tfl'|substr(tm_png,1,3)=='.nf'|
      substr(tm_png,nchar(tm_png)-5,nchar(tm_png))=='.rdata']
    loatextf<-dir(local.path2)
    loatextf<-file.path(local.path2, loatextf)
    if(length(tm_png)>5 | length(loatextf)>5){
      if(length(loatextf)>0)
        try(do.call(file.remove, as.list(loatextf)))
      tm_png<-file.path(local.path3, tm_png)
      if(length(tm_png)>0)
        try(do.call(file.remove, as.list(tm_png)))
    }
  } 


  #set up temporary tflfile names
  if(TRUE){
    randseed<-round(runif(1,1,10^7))
    loatext<<-tempfile(pattern=paste0("loa",randseed),
      tmpdir=local.path2, fileext=".txt")
    tflfile<<-tempfile(pattern=paste0("tfl",randseed),
      tmpdir=local.path2, fileext="_")
  }else{tflfile<-NULL}

  #save history of Rscript in expert
  text1<-'#For text output'
  text2<-'#For figure output'

}#other setup#
 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
BeachServer <- function(input,output, session){
  
  #close your app when the browser is closed
  session$onSessionEnded(function() {
    stopApp(NULL)
  })

  rawrow1<<-list() #for changing colnames with row 1 in a table data.
  indataset<<-list()
  indataR  <<-list()
  

  
  #-------Web Counter---------#   
  output$counter<-renderPrint({
    load(file=countFnm)
    
    if(is.null(uids)) return(NULL)
    
    uid.colnm<-c("userID", "studyName", "tumorType")
    uid<<-aggregate(Freq~., data=data.frame(uids[,uid.colnm], Freq=1), FUN=sum)
    tm<-aggregate(Freq~userID, data=uid, FUN=sum)
    colnames(tm)[2]<-"Freq.tot"
    uid<<-merge(uid, tm, by="userID")
    uid<<-uid[order(uid$Freq.tot, uid$Freq,decreasing=TRUE),]
    #print(paste("Start Date:", counter.startDate))
    #print(uid[, c(uid.colnm, "Freq")])
    
  })

  
  #------change the width of widget panel and the color------#
  output$beachColor <- renderUI({
    code0 <- selectInput_color(id='wpColor', 
                      label='Background color', 
                      choices=colors(), 
                      selected='azure3'
                      )
    return(shiny::HTML(code0))
  })
  
  output$wp.width <- renderUI({
    #width of the widgets panel
    widget.panel.width<-paste0(input$wpW, "%")
    
    wp.out <- paste0("<div class=\"panel panel-default draggable\" ",
                     "id=\"controls1\" ",
                     "style=\"top:50%;left:auto;right:0%;bottom:auto;width:",
                     widget.panel.width,
                     ";height:auto", #widget.panel.height,
                     ";position:absolute;cursor:move; ",
                     "background-color: ", HEXColors[input$wpColor], 
                     ";z-index: 199;\">",
                    # "<div id=\"set.ncol.widg\" class=\"shiny-html-output\"></div>",
                     "<div id=\"widgetSide\" class=\"shiny-html-output\"></div>", 
                     "</div> <script>$(\".draggable\").draggable();</script> ")
    return(shiny::HTML(wp.out))
  })
  
  
  #-------Webpage Title in the data input panel---------#   
  output$setTitle<-renderUI( try( {
    VdicTitle<<-reactive({ #note: only one or two rows should be defined.
      if (is.null(input$config) & input$config.sel==" "){
        Vdic0<<-read.csv(file.path(tmConf),header=TRUE,check.names=FALSE, encoding='UTF-8')
      }else{
        if(!is.null(input$config)){
          isolate({Vdic0<<-read.csv((input$config)$datapath,
                          header=TRUE,check.names=FALSE, encoding='UTF-8') })
        }        
        if(input$config.sel!=" "){
          isolate({ Vdic0<<-read.csv(file.path(local.path1, 
                                    cdpool[cdpool2==input$config.sel]),
                          header=TRUE,check.names=FALSE, encoding='UTF-8') })
        }
      }
      ret<-Vdic0        
      nCol<-ncol(ret)
      titleValue<-c("title_image","title_text")
      titleRowsID<-which((!is.na(ret$Type) & ret$Type%in%titleValue) | ret$Num==1)
      titleRows<-ret[titleRowsID,]
      return(titleRows)
    } )
    
    if(nrow(r123<-VdicTitle())>0){
      r1<-r123[r123$Type=='title_image',]
      r2<-r123[r123$Type=='title_text',]	
      r1c<-r2c<-''
      if(nrow(r1)>0&&grepl(".png", r1$Title, fixed=TRUE)){
        r1c<-as.character(img(src=r1$Title[1], height="10%",width="100%"))
      }
      if(nrow(r2)>0){
        r2c<-paste0('<h4 style="color:black; background-color:white; width:100%; height:10%;"> ',
                    #r2$Title[1], Sys.getlocale(), "</h4>")
                    r2$Title[1], "</h4>")
      }
      retTitle<-shiny::HTML(paste0(r1c,r2c))
      #run the getData source code 
      s1<-r123[ as.numeric(r123$Num)==1,'Source']
      if(length(s1)>0&&grepl('.r', tolower(s1), fixed=TRUE)){
        if(!grepl('users/', s1, fixed=TRUE)) s1<-file.path('users', s1)
        s1<-file.path(local.path0, s1)
        if(file.exists(s1)){
          source(s1)
        }
      }
      #run the uesr-free functions
      eval(parse(text=r123[ as.numeric(r123$Num)==1,"PlotCode"]))
      
    }else{
      retTitle<-shiny::HTML('<h4 style="color:black;">  BEACH-PARTY </h4>')
    }
    
    return(retTitle)
    
  }) )

  #-------Update webpage accroding to user-defined CD---------#  
  output$setconfig<-renderUI({
    Vdic<<-reactive({
      if (is.null(input$config) & input$config.sel==" "){
          Vdic0<<-read.csv(file.path(tmConf),header=TRUE,check.names=FALSE, encoding='UTF-8')
      }else{
        if(!is.null(input$config)){
          isolate({Vdic0<<-read.csv((input$config)$datapath,
                          header=TRUE,check.names=FALSE, encoding='UTF-8') })
        }        
        if(input$config.sel!=" "){
          isolate({ Vdic0<<-read.csv(file.path(local.path1, 
                                    cdpool[cdpool2==input$config.sel]),
                          header=TRUE,check.names=FALSE, encoding='UTF-8') })
        }
      }
      #Vdic0$Add<-unlist(lapply(Vdic0$Add,function(x){eval(parse(text=x))}))
      ret<-Vdic0[ as.numeric(Vdic0$Num)>0,]   
      if(!'res'%in%colnames(ret)){
        ret$res <- NA
      }
      if(!'width'%in%colnames(ret)){
        ret$width <- NA
      }
      if(!'height'%in%colnames(ret)){
        ret$height <- NA
      }
      
      #insert user folders if it is not exist in the source function list
      for(i in 1:nrow(ret)){
        if(!is.na(ret$Source[i])&ret$Source[i]!=''&!ret$Source[i]%in%function_r){
          xSel<-sapply(user_folder, function(x){
            substr(ret$Source[i], 1, nchar(x))==x
          })
          if(sum(xSel)==0){
            ret$Source[i]<-paste0(user_folder[1],ret$Source[i])
          }
        }      
      }
      runSource<-unique(ret$Source[!is.na(ret$select.label)&!is.na(ret$Source)])
      sapply(runSource,function(x){
         source(file=file.path(local.path0,x))})
      ret$Add<-unlist(lapply(ret$Add,function(x){eval(parse(text=x))}))
      ret<-ret[ret$Add,]
      return(ret)
    } )
    UIdic<<-reactive(Vdic()[Vdic()$Add & !is.na(Vdic()$Request.Name),] )

    UInames<-names(UIdic())[grepl('uiInput',names(UIdic()))]
    UInames<<-UInames[!sapply(UInames,function(x){all(is.na(UIdic()[,x]))})]
    
    temp<-strsplit(names(UIdic())[grepl('.label',names(UIdic()))],'.label',fixed=TRUE)
    temp1<-sapply(temp,function(x){
      if(x[1] %in% c('check','radio','dropdown','slide', 
        'date', 'dateR', 'num', 'text', 'textbox')){
        return(paste0(x,collapse=''))
      }else{
        return(NULL)
      }
    })

    #----params includes all the relative widgets----#
    params<<-sort(unlist(temp1))
    max.n<<-c(check=length(grep('check',params)),radio=length(grep('radio',params)),
      dropdown=length(grep('dropdown',params)),slide=length(grep('slide',params)),
      date=length(grep('date',params)),dateR=length(grep('dateR',params)),
      num=length(grep('num',params)),text=length(grep('text',params)),
      textbox=length(grep('textbox',params)) )

    max.n2<<-max.n#[max.n!=0]
    params.lab<<-paste0(params, '.lab')
    
    #--order params by its indes--#
    strsplit1 <- function(x, split){
      out1 <- NULL
      for(i in 1:length(x)){
        for(j in 1:length(split)){
          if(grepl(split[j], x[i], fixed=T)){
            t1 <- strsplit(x[i], split=split[j], fixed=TRUE)[[1]]
            if(length(t1)==1){
              out1 <- rbind(out1, data.frame(m1=split[j], m2=0))
              break
            }else{
              if(!is.na(as.numeric(t1[2]))){
                out1 <- rbind(out1, data.frame(m1=split[j], m2=as.numeric(t1[2])))
                break
              }
            }
          }
        }
      }
      return(out1)
    }
    params.id <<- strsplit1(params, names(max.n))
    params.ord <<- order(params.id$m1, params.id$m2)
    #params.ord <- order(params)
    
    #re-set the other tabs
	  
    tmp<-readLines(file.path(local.path0,Vdic()$Source[1]))
    r.lib<-tmp[grepl('library(',tmp,fixed=TRUE)|grepl('require(',tmp,fixed=TRUE)]
    r.lib.temp<-gsub(' ', '', r.lib, fixed=TRUE)
    r.lib<-r.lib[!grepl('#library', r.lib.temp, fixed=TRUE)&!grepl('#require',r.lib.temp, fixed=TRUE)]
    r.lib<-sort(unique(r.lib))
    r.lib.alert<<-paste0( #'  #required packages: \n',
      paste0(r.lib, collapse='\n') )
    #Get the libraries needed for running the analysis
    
    if(!is.null(tflfile)){
      randseed<-round(runif(1,1,10^7))
      loatext<<-tempfile(pattern=paste0("loa",randseed),
        tmpdir=local.path2, fileext=".txt")
      tflfile<<-tempfile(pattern=paste0("tfl",randseed),
        tmpdir='', fileext="_")
    }
    
    write.table(t(c("Request.Name","Type","Titles", "height", "width", "res",
                    "Footnote", "Abbreviations.Footnote","Statistical.Analysis",
                    "Test.Statistic.Footnote", as.vector(rbind(params.lab, params)))),
                file=loatext,append=FALSE,col.names=FALSE,row.names=FALSE)
    
    fileInput(inputId='config', label='Step 2 Input configuration file',
          accept=c('.csv', 'text/csv', 'text/comma-separated-values,text/plain'),
          multiple=FALSE)
  })

  #----------Obtain number of analysis request for TFL------------#
  loaTF<<-reactive(eval(parse(
    text=paste0("c(",paste0("input$result_",1:400,collapse=','),")"))))
  AnalyN<<-reactive({
    if(is.null(input$analysis)){
      return(1)
    }else{
      return(which(Vdic()$Request.Name==input$analysis))
    }
  }) # Capture the corresponding configuration for the selected analysis 
  AnalyC<<-reactive(which(Vdic()$Request.Name==input$analysis)[1])
 
  #----------Generate Tabs------------#
  output$tabs<-renderUI({
    tabset<-Vdic()[!is.na(Vdic()$Tab.label),c('Tab.value','Tab.label')]
    tabcode<-tabs(tab.id='Tabs',tab.label=tabset[,1],tab.value=tabset[,2])
    return(shiny::HTML(tabcode))
  })  # Generate the Tabs

  #----------Update webpage according to user-defined mock data------------#
  #indataset is a list object including all the uploaded dataset
  #indataset.i is a data.frame object, the 1st dataset in indataset.
  #indataR is a list obeject including all the elements in the uploaded R files
  output$file<-renderUI({
    if(is.null(input$infile)){
      return(conditionalPanel('true',
                              radioButtons('csv_enc', 'Encoding format for CSV file', choices=c('UTF-8', 'unkown'), inline=TRUE),
                              radioButtons('checknames', 'Change " " or "-" in column names into "."', 
                                           choices=c(FALSE, TRUE), inline=TRUE),
                              fileInput('infile', label=infilelab,
                                        accept=infilenm, 
					multiple=TRUE)))
    }else{
      
      #update the count file
      try(uids$studyName[nrow(uids)]<-ifelse(is.null(input$study), '', input$study))
      try(uids$tumorType[nrow(uids)]<-ifelse(is.null(input$tumor), '', input$tumor))
      try(save(uids, file=countFnm))
      

      isolate({
         input_infile <<- input$infile
        
        ###import the files under a pre-specified location
        if(nrow(input_infile)==1 & grepl('.csv', input_infile) & 
           grepl('File_Names', input_infile) ){
           
           input_infile_1 <- BeachRead(input_infile[1, 'datapath'], name=input_infile[1, 'name'])
           #check file name
           if(any(!colnames(input_infile_1)%in%c('datapath', 'name'))){
             print("Files in 'File_Names...csv' are not loaded. The colum names must be 'datapath' and 'name'.")
           } else {
             input_infile_1$datapath <- file.path(input_infile_1$datapath, input_infile_1$name)
             input_infile <<- input_infile_1[,c('name', 'datapath')]
           }
          
        }
        
      if(nrow(input_infile)>1 | length(indataset)>0){
        indataset0<-list()

       	indataset0.wh <- NULL
        for(i in 1:nrow(input_infile)){
          tmpload <- BeachRead((input_infile[i, 'datapath']),
                                        header=TRUE, name=input_infile[i,'name'],
                                        encoding=isolate(input$csv_enc), 
                                        comment.char=isolate(input$comm_chr), 
                                        check.names=as.logical(input$checknames))
     	    if(is.list(tmpload) & grepl('.rda', tolower(input_infile[i,'name']))){
	          indataR <<- c(indataR, tmpload)
       	  }else{
      	    indataset0.wh <- c(indataset0.wh, i)
            indataset0[[i]]<- tmpload
       	  }
        }
      	indataset0 <- indataset0[!sapply(indataset0, is.null)]
        names(indataset0)<-input_infile[indataset0.wh, 'name']
        indataset0<<-indataset0
        wh1<<-names(indataset)[names(indataset)%in%names(indataset0)]
        if(length(wh1)>0){
          indataset[wh1]<<-indataset0[wh1]
          indataset0<-indataset0[!names(indataset0)%in%wh1]
        }
        if(length(indataset0)>0){
          indataset<<-c(indataset, indataset0)
        }
      }else{
        indataset0<-list()
	tmpload <- BeachRead((input_infile)$datapath, header=TRUE, name=input_infile[1,'name'],
                                   encoding=isolate(input$csv_enc), 
                                   comment.char=isolate(input$comm_chr), 
	                                 check.names=as.logical(input$checknames))
        if( is.list(tmpload) & grepl('.rda', tolower(input_infile[1,'name'])) ){
	        indataR <<- c(indataR, tmpload)
	        indataset.i <<- indataR[[1]]
        } else {
    
          indataset.i <<- tmpload
          indataset0[[1]]<-indataset.i
          names(indataset0)<-input_infile[,'name']
          indataset<<-indataset0
        }
        
      }
      }) # End of isolate
      
      if(length(indataR)>0){
        indataR <<- indataR[unique(names(indataR))]
        indataset <<- c(indataset, indataR)
        indataset <<- indataset[unique(names(indataset))]
      }
      rawrow1 <<- lapply(indataset, function(x){x[1,]})

      studyname1 <-na.omit(unique(unlist(lapply(indataset,function(x){
        unique(x$STUDYID) }))))
      if(!is.null(studyname1)) studyname1 <-paste(unique(studyname1), collapse=", ")
      tumortype1 <-unlist(lapply(indataset,function(x){
        unique(x$TUMOR_TYPE) }))
      if(is.null(studyname1)) studyname1 <- ""
      if(is.null(tumortype1)) tumortype1 <- ""
      if(!is.null(input$study)) studyname1 <- input$study
      if(!is.null(input$tumor)) tumortype1 <- input$tumor

            
      if(length(indataset)<2){
        stnm<-inlinetext(text.id='study',text.label='Study Name ',
                         text.value=studyname1)      
        datanote<-paste0(input$infile$name,' has ', nrow(indataset.i),
                         ' rows, ',ncol(indataset.i),' columns.')
        tmtp<-inlinetext(text.id='tumor',text.label='Tumor Type ',
                         text.value=tumortype1)
        studyname <<- input$study
        tumortype <<- input$tumor
        datanote<-paste0('<h6>', datanote, ' </h6>')
        if(input$data_reload | (is.null(input$config) & input$config.sel==" ")){
          return(
            conditionalPanel(
              condition='true',
              fileInput('infile', label=infilelab,
                        accept=infilenm, multiple=TRUE),
              shiny::HTML(c(stnm, "<br>", 
                            tmtp, datanote)) ) )
        }else{
          return(conditionalPanel(condition='true',
          shiny::HTML(c(stnm, "<br>", tmtp, datanote)) ) )
        }
      }else{
        if(!is.null(tumortype1)) tumortype1<-paste(tumortype1, collapse=", ")
        stnm<-inlinetext(text.id='study',text.label='Study Name ',
                         text.value=studyname1)      
        datanote<-paste0(names(indataset),' has ', 
            sapply(indataset, nrow), ' rows, ',
            sapply(indataset, ncol),' columns.')
        tmtp<-inlinetext(text.id='tumor',text.label='Tumor Type ',
                         text.value=tumortype1)
        studyname <<- input$study
        tumortype <<- input$tumor
        datanote<-paste0('<h6>', datanote, ' </h6>')
        if(input$data_reload | (is.null(input$config) & input$config.sel==" ")){
          return(
            conditionalPanel('true',
                             radioButtons('csv_enc', 'Encoding format for CSV file', choices=c('UTF-8', 'unkown'), inline=TRUE),
                             radioButtons('checknames', 'Change "_" in column names into "."', choices=c(FALSE, TRUE), inline=TRUE),
                             fileInput('infile', label=infilelab,
                                       accept=infilenm, 
				       multiple=TRUE),
                             shiny::HTML(c(stnm, "<br>", tmtp, datanote)) ))
        }else{
          return(conditionalPanel(condition='true',
            shiny::HTML(c(stnm, "<br>", 
            tmtp, datanote)) ) )
        }
       }
    }
  })  # Widget to upload dataset
  
  #----------Get data.frame objects (subset of CD) only for widgets----------#
  output$getWidgets<-renderUI({
     #a row is a Request.name
     check.param<<-reactive({widgets.param(uidic=UIdic(),wid='check',max.n=max.n2)})
     radio.param<<-reactive({widgets.param(uidic=UIdic(),wid='radio',max.n=max.n2)})
     dropdown.param<<-reactive({widgets.param(uidic=UIdic(),wid='dropdown',max.n=max.n2)})
     slide.param<<-reactive({widgets.param(uidic=UIdic(),wid='slide',max.n=max.n2)})
     date.param<<-reactive({widgets.param(uidic=UIdic(),wid='date',max.n=max.n2)})
     dateR.param<<-reactive({widgets.param(uidic=UIdic(),wid='dateR',max.n=max.n2)})
     num.param<<-reactive({widgets.param(uidic=UIdic(), wid='num', max.n=max.n2)})
     text.param<<-reactive({widgets.param(uidic=UIdic(),wid='text',max.n=max.n2)})
     textbox.param<<-reactive({widgets.param(uidic=UIdic(),wid='textbox',max.n=max.n2)})
     return(NULL)
  })

  widgetOrd<<-reactive({
    widgets.order(analysis=input$analysis, UIdic1=UIdic(), UInames=UInames,
                  ncol.widg=as.numeric(input$ncol.widg.rd))
  })

  output$widgets<-renderUI({
    #HTML code of all the widgets
    eval(parse(text=unique(widgetOrd()$code)))
  })
 
  output$getData<-renderUI({#Declare global variables#
#    if(!is.null(input$infile)|!is.null(input$infileR)){
      tm.Vdic<-Vdic()
      if(!is.na(tm.Vdic$select.label))
        source(file.path(local.path0, tm.Vdic$Source[1]))
#    }
    return(NULL)
  }) # Data manipulation for the uploaded dataset
  
  output$getTFL<-renderUI({
    #only table data is output to local.path3 (a temporary folder)
    if(F){
      tflname<-sapply(AnalyN(),function(x){
        ifelse(all(is.na(widgetOrd()$names)),               #Condition
               paste0(tflfile,Vdic()$Num[x]),    #if TRUE
               paste0(tflfile,Vdic()$Num[x],'x',        #if FALSE
                      paste0(#Paste all UIs together
                        sapply(widgetOrd()$names,function(y){
                          y0<-eval(parse(text=paste0('input$',y)))
                          if(!is.null(y0)){y0[is.na(y0)]<-''}
                          y0<-paste0(y0[length(y0):1],collapse='_')#Paste all Params for each UI
                          substring(y0, max(nchar(y0)-30,1))
                        }),collapse=''))
        )
      })
      tflname<-gsub(" ",'',tflname)
      tab.name<-file.path(local.path3,paste0(input$submitcode,gsub("[[:punct:]]","",tflname),'.rdata'))
    }
    if('Table' %in% Vdic()$Type[(AnalyN())] ){ 
      AnalyNt<-reactive(AnalyN()[Vdic()$Type[AnalyN()]=='Table'])
      tmp<-eval(parse(text=Vdic()$tmp[AnalyNt()[1]]))
      sapply(AnalyNt(),function(x){
        if (eval(parse(text=Vdic()$Condition[AnalyNt()[1]]))){
          tmptab<-NULL
        }else{
          tmptab<-eval(parse(text=Vdic()$PlotCode[x]))

        }
         # save(tmptab,file=tab.name[Vdic()$Layout[x]])
        })
      }
    
    return(NULL)
  }) # Generate png files if the output is plot
  
  output$status<-renderUI({
    if(is.null(input$infile)&is.null(input$infileR)){
      tmpcode<-'<span class=\"label label-important\" style=\"background-color:red; color:black;\">Data not loaded</span>'
    }else{
      tmpcode<-'<span class=\"label label-success\" style=\"color:black;\">Data loaded</span>'
    }
    return(shiny::HTML(tmpcode))      
  }) # Status of data manipulation process
  
  output$select<-renderUI({
    selectN<-which(Vdic()$Tab.value==input$Tabs)
    if (all(is.na(Vdic()$select.label[selectN])))return(NULL)
    label<-unique(Vdic()$select.label[selectN])
    label<-label[!is.na(label)]
    selection<-unique(Vdic()$Request.Name[selectN])
    return(selectInput(inputId="analysis",label=strong(label),choices=selection, width="80%"))
  }) # Generate the downdrop list
 

  #-----Generate code for output$widgets
  widgetCode<-widgets.code(UInames=UInames,max.n=max.n1) 
  eval(parse(text=widgetCode))

  #-------Output results to the current screen------------#
  
  output$save_data_ext<-renderUI({

    choiceN<-AnalyN()[Vdic()$Type[AnalyN()]=='Table']
    choiceP<-AnalyN()[Vdic()$Type[AnalyN()]=='Figure']
    if(length(choiceN)==0 & length(choiceP)==0)return(NULL)
    if(length(choiceN)==0 & length(choiceP)>0){
      return( conditionalPanel(condition='true',
        div( downloadButton('getEPS','download EPS plot'),
           downloadButton('getPDF','download PDF plot') ) 
      ))
    }
    if(length(choiceN)>0 & length(choiceP)==0){
      choice<-sapply(choiceN, 
                     function(x){
                       ifelse(substr(Vdic()$Title[x],1,5)=='paste',
                              eval(parse(text=Vdic()$Title[x])),
                              Vdic()$Title[x])
                     })
      return( conditionalPanel(condition='true',
                       checkboxGroupInput('multdat',
                                          'Please Choose a table to save',
                                          choices=choice,
                                          selected=NULL, inline =TRUE),
                       radioButtons('multdat_ext', 
                                    'Format', 
                                    c('csv','rdata','xpt','xls','SAS code'='sas'), 
                                    selected = NULL, 
                                    inline = TRUE),
                       downloadButton("save_data","Save Data")                     
      ) )
      
    }
    
    if(length(choiceN)>0 & length(choiceP)>0){
      choice<-sapply(choiceN, 
                     function(x){
                       ifelse(substr(Vdic()$Title[x],1,5)=='paste',
                              eval(parse(text=Vdic()$Title[x])),
                              Vdic()$Title[x])
                     })
      return( conditionalPanel(condition='true',
                               div( downloadButton('getEPS','download EPS plot'),
                                    downloadButton('getPDF','download PDF plot') ),
                               checkboxGroupInput('multdat',
                                                  'Please Choose a table to save',
                                                  choices=choice,
                                                  selected=NULL, inline =TRUE),
                               radioButtons('multdat_ext', 
                                            'Format', 
                                            c('csv','rdata','xpt','xls','SAS code'='sas'), 
                                            selected = NULL, 
                                            inline = TRUE),
                               downloadButton("save_data","Save Data")                     
      ) )
      
    }   
    
  })
  
  output$TFL<-renderUI({
    plotcode <- NULL
    currTabL <<- list()
    currTabL_i <- 1
    
    if(is.null(input$hwr)) 
      hwr.c <- ''
    else 
      hwr.c <- as.character(input$hwr)
    tfl.h<<-ifelse(is.na(as.numeric(strsplit(hwr.c,split=';')[[1]])[1]), 480, 
                 as.numeric(strsplit(hwr.c, split=';')[[1]])[1])
    tfl.w<<-ifelse(is.na(as.numeric(strsplit(hwr.c,split=';')[[1]])[2]), 480, 
                  as.numeric(strsplit(hwr.c, split=';')[[1]])[2])
    tfl.r<<-ifelse(is.na(as.numeric(strsplit(hwr.c,split=';')[[1]])[3]), 72, 
               as.numeric(strsplit(hwr.c, split=';')[[1]])[3])

    if (all(!Vdic()$Type[AnalyN()] %in% c('Figure','Table'))||
      all(eval(parse(text=Vdic()$Condition[AnalyN()])))){
      return(NULL)
    }
    for (i in AnalyN()){
        input0.code <- Vdic()$PlotCode[i]
        
        myInput <<- Vdic()[i,UInames]
        mylab <<- Vdic()[i,gsub('Input', 'lab', UInames)]
        names(myInput) <<- mylab
        myInput <<- myInput[!is.na(myInput)]
        
        for(m0 in 1:length(myInput)){
          x <- myInput[m0]
          if(grepl('date', x))
            x <-paste0("paste(",x, ")")
          input0[[m0]] <- eval(parse(text=x))
        }
        if(length(myInput) == length(input0))
           names(input0) <- gsub("input$", "", myInput, fixed=TRUE)
        input0 <<- input0 
        
        #date and dateR widgets have to be a character before running eval.
        for(m1 in 1:length(input0)){
          if(length(input0)>0 && grepl('date', names(input0)[m1], fixed=TRUE)){
            date.value <- paste("c(", 
                                paste( paste0("\"", input0[[m1]], "\""), collapse=', '),
                                ")")
            input0.code<- gsub(paste0('input$',names(input0)[m1]), 
                               date.value, input0.code, fixed=TRUE)
          }
        }
        new.code <<- input0.code
        input0.code <<- input0.code

    if(!is.na(Vdic()$Title[i])){
        title<-ifelse(substr(Vdic()$Title[i],1,5)=='paste',
                      eval(parse(text=Vdic()$Title[i])),Vdic()$Title[i])
        titlecode<-paste0('<h3><textarea name=\"titlebox\" rows=\"1\" style=\"width:80%\">',
                          title,'</textarea></h3>')
      }else{titlecode<-NULL}

      
      
      try( addResourcePath('images',local.path3) )
      if(Vdic()$Type[i]=='Figure'){ 
        if(length(input0.code)>0 && grepl("dynamicData", input0.code, fixed=TRUE)){
          dynamicCode<<-input0.code
            tmpcode <- paste0(plotOutput('dynamicPlot', click='dynamicPlot_click',
                                         brush="dynamicPlot_brush"),
                              #click data
                              "<h4>Points near click</h4>", "\n",
                              downloadButton('click_info_dl', 'save clicked data'),
                              uiOutput('click_info_DT'),
                              #brush data
                              "<h4>Brushed points</h4>",  "\n", 
                              downloadButton('brush_info_dl', 'save brushed data'),
                              uiOutput('brush_info_DT')
                              )
        }else{
          tmpFig1 <- NULL
          for(k in 1:length(i)){
            tmpFig1 <- c(tmpFig1, tempfile(tmpdir=local.path3, fileext='.png'))
             
            png(tmpFig1[k], 
                height=tfl.h, 
                width=tfl.w,  
                res=tfl.r )
            
            plottry<-try(eval(parse(text=input0.code[k])))
            if(class(plottry)[1]=='try-error') {
              plot(0,axes=F, col='white',ylab='',xlab='')
              legend('top', 'Plotting\nError', cex=5, bty='n', text.col='red')
            }
            dev.off()
          }
          tmpFig2 <- gsub(local.path3, "", tmpFig1, fixed=TRUE)
          tmpFig2 <- gsub('/', "", tmpFig2, fixed=TRUE)
          tmpcode<-paste0("<img src=\"images/",   
                          tmpFig2,  
                          "\" alt=\"Calculating...\" style=\"width:",
                          100*input$figSize,
                          "% ; height: ", 
                          100*input$figSize, 
                          "%\"/> \n")
        }
      }else if(Vdic()$Type[i]=='Table'){
        
        
        if(class(try(tmptab<<-eval(parse(text=input0.code))))[1]=='try-error'){
           tmptab<-''
        }
        #save(tmptab, file=tab.name)
          
        outTable<-tmptab
        
        #update the current table list in the parent environment
        currTabL[[currTabL_i]] <<- outTable
        currTabL_i <- currTabL_i+1
        
        if(class(try(tmpcode<-xtable(outTable)))[1]=='try-error'){
          tmpcode<-outTable
        }        
        if(input$useDT){#if TRUE using render table
          tmpcode<-paste0(
            "<div id=\"outTbl_DT\" style=\"width:",input$figSize*100, "%;",
                          "height:auto\" class=\"shiny-html-output\"></div>"
            )
        }else{
          tmpcode<-capture.output(print(tmpcode,type="html",
                                        include.rownames=FALSE,include.colnames=TRUE))
          if(is.na(tmpcode[2])){ tmpcode[2]<-''}
          tmpcode <- toMultH(tbc=tmpcode, split1=muliHead.split)
          tmpcode[3]<-paste0("<TABLE style=\'width:",
                             input$figSize*100, 
                             "%;\' >")
        }
      }
      plotcode<-c(plotcode,titlecode,tmpcode)
    }

    return(shiny::HTML(plotcode))
  }) # Generate TFL with title
  
  output$footnote<-renderUI({
    footnote<-Vdic()$FootCode[AnalyC()]
    footnote<-gsub("\\\\n","\n",footnote)
    if(!is.na(footnote)&&gsub("", " ", footnote)!=''&&substr(footnote,1,5)=='paste')
      footnote<-eval(parse(text=footnote))
    #cat(footnote)
    fo1<-paste(footnote, collapse=' ')
    shiny::HTML(
       paste0("<textarea name=\"footnotebox\" rows=\"4\" 
              style=\"width:80%\"\">",
              fo1,
              "</textarea>"))
  }) # Generate footnote
  
  #------Output for user input code------#
  output$userExp<-renderUI({
    isolate(input$expert)
    if(is.null(input$expert)||!(input$expert)){
      return(NULL)
    } else {
      return(
        conditionalPanel(
          condition="true",
          div(shiny::HTML(paste0("<textarea name=\"Rscript\" rows=\"2\"",
                                 "  style=\"width:100%\"",
                                 "  placeholder=\"#For text output \"></textarea>")), 
          actionButton('submit','execute')),
          div(shiny::HTML(paste0("<textarea name=\"RscriptF\" rows=\"2\"",
                                 "  style=\"width:100%\"",
                                 "  placeholder=\"#For plot output \"></textarea>")), 
          actionButton('submit2','execute')),
        div(downloadButton('save_Rhist', 'save expert code history'))
      ))
    }   
  })
  output$Input_outExpert<-renderPrint({
    if(TRUE){
      myInput <<- Vdic()[AnalyN(),UInames]
      mylab <<- Vdic()[AnalyN(),gsub('Input', 'lab', UInames)]
      names(myInput) <<- mylab
      myInput <<- myInput[!is.na(myInput)]
      for(m0 in 1:length(myInput)){
        x <- myInput[m0]
        if(grepl('date', x))
          x <-paste0("paste(",x, ")")
        input0[[m0]] <- eval(parse(text=x))
      }
      names(input0) <- gsub("input$", "", myInput, fixed=TRUE)
      input0 <<- input0
    }
    input0
  })
  
  
  output$RoutExpert<-renderPrint({
    if(is.null(input$submit)||input$submit==0){
      return('For text output from Rscript')
    }
    isolate({
      text1<<-c(text1, input$Rscript)
      eval(parse(text=input$Rscript), env=sys.frame())
    })
  })
  output$RoutExpertF<-renderPlot({
    if(is.null(input$submit2)||input$submit2==0){
      return(NULL)
    }
    isolate({
      text2<<-c(text2, input$RscriptF)
      eval(parse(text=input$RscriptF), env=sys.frame())
    })
  })
  output$save_Rhist<-downloadHandler(
    filename=function(){paste0(input$study,"_",input$tumor,"_Rhist.r")},
    content=function(file){
      tmpout<-paste(text1,"\n#~~~~~~~~~~~~~~#\n", text2)
      colnames(tmpout)<-''
      write(tmpout,
            file=file, row.names=FALSE, col.names=FALSE)
    }
  ) # Download Source code

  #-----Tabs-----#
  output$hwrCtrl <- renderUI({
    if(TRUE){
      h1 <- eval(parse(text=Vdic()$height[(AnalyN())])) 
      w1 <- eval(parse(text=Vdic()$width[(AnalyN())]))
      r1 <- eval(parse(text=Vdic()$res[(AnalyN())]))
      h2 <- ifelse(is.null(h1), 480, as.numeric(eval(parse(text=h1))))
      w2 <- ifelse(is.null(w1), 480, paste(as.numeric(eval(parse(text=w1))), collapse=', ') )
      r2 <- ifelse(is.null(r1), 72, as.numeric(eval(parse(text=r1))) )
      hwr.default <<- paste(
        ifelse(is.na(h2)||h2==0, 480, h2),
        ifelse(is.na(w2)||w2==0, 480, w2),
        ifelse(is.na(r2)||r2==0, 72,  r2),
        sep='; ')
    }
    textInput('hwr', 'Input height(px); width(px); resolution(px/inch).', value=hwr.default, width="50%" )
  })
  output$AnalysisTab<-renderUI({
    selN<-!is.na(Vdic()$Tab.label) & is.na(Vdic()$Request.Name)
    unAnalyTab<-unique(Vdic()$Tab.value[selN])
    cpCon<-paste0(paste0("input.Tabs!=","\'",unAnalyTab,"\'"), collapse='&&')

    selN.addbar<-!is.na(Vdic()$Tab.label) & grepl("dynamicData", Vdic()$PlotCode, fixed=TRUE)
    cpCon.addbar<-paste0(paste0("input.analysis!=","\'",
                                unique(Vdic()$Request.Name[selN.addbar]),"\'"), collapse='&&')
    # print(cpCon.addbar)
    
    conditionalPanel(condition=cpCon, width='100%',
                     div(class='row'), div(class='span6', uiOutput('select')), 
                     div(class='span2', actionButton(inputId="add_analysis",label="Add Analysis")),
                     sliderInput('figSize', label="Relative Size", 
		         min=0.1, max=1, value=0.3, step=0.1, animate=FALSE),
		         uiOutput('hwrCtrl'), 
                     div(class='span12', 
                         uiOutput("TFL"), 
                         uiOutput("footnote"), br(), 
                         uiOutput("save_data_ext"),
                         conditionalPanel(condition="input.expert==true",
                                          div(verbatimTextOutput("Input_outExpert"), br(),
                                              verbatimTextOutput("RoutExpert"), br(),
                                              plotOutput("RoutExpertF") )),
                         conditionalPanel(condition="input.usage==true",
                                          verbatimTextOutput("counter"))  
                     ))
  })
  output$widgetSide<-renderUI({
    selN<-!is.na(Vdic()$Tab.label) & is.na(Vdic()$Request.Name)
    unAnalyTab<-unique(Vdic()$Tab.value[selN])
    cpCon<-paste0(paste0("input.Tabs!=","\'",unAnalyTab,"\'"), collapse='&&')
    conditionalPanel(condition=cpCon, uiOutput("widgets"))
  })
  
  output$SpecialTab<-renderUI({
    selN<-!is.na(Vdic()$Tab.label) & is.na(Vdic()$Request.Name)
    unAnalyTab<-unique(Vdic()$Tab.value[selN])
    if (!is.null(input$Tabs) && (input$Tabs%in%unAnalyTab)){
      eval(parse(text=readLines(
        file.path(local.path0,Vdic()$Source[which(Vdic()$Tab.value==input$Tabs)])
      )))
    } 
  })




  
  #-------List of Analysis, R code ------------#
  output$loa<-renderUI({
    
    loaAdd<-reactive({
      if (is.null(input$add_analysis) || input$add_analysis==0)return(NULL)
      isolate({
        tmp0<-sapply(params[1:length(params)],function(x){
          if (x %in% widgetOrd()$names){
            if(grepl('date', x)){
              tmp<-eval(parse(text=paste0('paste(input$',x, ")")))
            } else {
              tmp<-eval(parse(text=paste0('input$',x)))
            }
            tmp<-ifelse(is.null(tmp),'',tmp)
            tmp<-ifelse(length(eval(parse(text=paste0('input$',x))))>1,
	          paste0(eval(parse(text=paste0('input$',x))),collapse=','), tmp)
          }else{tmp<-NA}          
          return(tmp)
        })
        
        tmp1<-rep(NA,length(params))
        names(tmp1) <- params
        tmp1.lab <- widgetOrd()$labs
        names(tmp1.lab) <- widgetOrd()$names
        tmp1.nm <- params[params %in% names(tmp1.lab)]
        tmp1[tmp1.nm]<-tmp1.lab[tmp1.nm]

        footnote<-input$footnotebox
        if(is.na(footnote)){
          footnote<-''
        }
        footnote<-gsub("\\\\n","\n",footnote)
        if(!is.na(footnote)&&gsub("", " ", footnote)!=''&&substr(footnote,1,5)=='paste'){ 
          footnote<-eval(parse(text=footnote))
        }
        footnote2<-paste0(substr(footnote,1,10),'...')
        
        #title<-Vdic()$Title[AnalyC()]
        title<-input$titlebox

        if(!is.na(title) && grepl(title, 'paste', fixed=TRUE)){
          title<-eval(parse(text=title))
        }
        
        my.hwr <- strsplit(input$hwr,split=';')[[1]]
        
        w1<<-eval(parse(text=Vdic()$width[AnalyC()]))
        tmprow<-data.frame(t(c(input$analysis,Vdic()$Type[AnalyC()],title, 
                               height=my.hwr[1],
                               width=my.hwr[2],
                               res=my.hwr[3],
                               footnote,
                               footnote2,Vdic()$StatModel[AnalyC()],
                               Vdic()$StatNote[AnalyC()],
                               as.vector(rbind(tmp1,tmp0)) )))
        colnames(tmprow)<-c("Request.Name","Type","Titles", "height","width", "res", 
                            "Footnote", 
                            "Abbreviations.Footnote","Statistical.Analysis",
                            "Test.Statistic.Footnote", as.vector(rbind(params.lab,params)) )
        
        return(tmprow)
        
      })
    }) # Add analysis by selection
    
    loaLoad<-reactive({
      if (input$load_analysis==0||is.null(input$upfile))
        return(NULL)
      isolate({
        tmp<-read.csv((input$upfile)$datapath,header=TRUE, 
                      encoding='UTF-8', na = na_sign)
        if(!'height'%in%colnames(tmp)){
          tmp$height <- NA
        }
        if(!'width'%in%colnames(tmp)){
          tmp$width <- NA
        }
        if(!'res'%in%colnames(tmp)){
          tmp$res <- NA
        }
        
        return(tmp)
      })
    }) # Add analysis by upload
    myTTT <<- plyr::rbind.fill(loaAdd(),loaLoad())
    if(!is.null(nrow(myTTT)))  myTTT <<- myTTT[, colnames(loaAdd())]
    
    write.table(myTTT,
                loatext,append=TRUE,col.names=FALSE,row.names=FALSE)
    

    if((is.null(input$add_analysis)||is.null(input$load_analysis)) || 
      input$add_analysis+input$load_analysis==0){
      return(NULL)
    }else{
      ret<-read.table(loatext,header=TRUE,check.names=FALSE)
      ret<-unique(ret)
      write.table(ret,loatext,append=FALSE,row.names=FALSE,col.names=TRUE)
    }
    
  LOA<<-reactive({
    if((is.null(input$add_analysis)||is.null(input$load_analysis)) || 
      (input$add_analysis+input$load_analysis)==0){
      return(NULL)
    }else{
      return(ret)
    }
  })
    
    return(NULL)
  }) # Generate LOA
  
  output$LOA<-renderUI({
    if((is.null(input$add_analysis) || is.null(input$load_analysis)) || 
      input$add_analysis+input$load_analysis==0
    ){
      return(NULL)
    }else{
      tf<-reactive({
        tf<-isolate({loaTF()})
        #is a TRUE/FALSE vector checking the availability of input$result_i
        #see checkboxTableInput in shinyFun.r
        addrow<-nrow(LOA())-length(tf)
        if(addrow>0){
          tfb<-c(tf,rep(TRUE,addrow))
        }else{
          tfb<-tf
        }
        return(tfb)
      })  #TRUE/FALSE of LOA
      isolate({ 
        sel_Table<-tf()
        tmploaTable<-LOA()[,c('Titles','Abbreviations.Footnote')]
        tmploaTable<-data.frame(ID=1:nrow(tmploaTable), tmploaTable)
        input$delete_loa
      })
      if(nrow(tmploaTable)==0){return(NULL)}
      if(is.null(input$delete_loa)||!(input$delete_loa)){
        shiny::HTML(checkboxTableInput(
        tmploaTable,
        table.id="LOAtable",id.name="result",
        checked=sel_Table, check.name= " ",labels=c("Title","Stat.Method","Footnote")))
      }else{

        shiny::HTML(checkboxTableInput(
            tmploaTable,
            table.id="LOAtable",id.name="result",
            checked=sel_Table,
            check.name= " ",labels=c("Title","Stat.Method","Footnote"),
            showSelectedOnly=TRUE))
      }
    }
  }) # Display LOA
  
  output$rcode<-renderPrint({
    if (all(is.null(loaTF()))||length(which(loaTF()))==0){return(cat(''))
    }else{
      loalistT<-LOA()[loaTF(),]
      loalistT$order<-1:nrow(loalistT)
      if(!'width'%in%colnames(loalistT))  loalistT$width <- NA
      if(!'height'%in%colnames(loalistT)) loalistT$height <- NA
      if(!'res'%in%colnames(loalistT))    loalistT$res <- NA
      temp.vdic <- Vdic()
      temp.vdic <- temp.vdic[,!colnames(temp.vdic)%in%c('width', 'height','res')]
      codelist<-merge(loalistT,Vdic(),by=c('Request.Name'),all.x=TRUE)
      codelist<<-codelist[order(codelist$order,codelist$Layout,decreasing=FALSE),]
      
      if(is.null(input$tumor) || is.na(input$tumor))
        title0<-paste0("Study: ",input$study,"\t\t", "  ")
      else
        title0<-paste0("Study: ",input$study,"\t\t","Tumor Type: ",input$tumor)
      
      r_out<-runcode(codelist=codelist, datPath=input$datPath,
                     outPath=input$outPath,
                     libs=paste0(r.lib.alert,collapse='\n'), 
                     sourceCode=Vdic()$Source[!is.na(Vdic()$Request.Name)&!is.na(Vdic()$Source)],
                     subCode=subRcode,
                     title=title0,devpath=devpath,
                     params1=params[params.ord], paramLabs1=params.lab[params.ord],
		     outBcode="")
#                     outBcode=Vdic()$PlotCode[Vdic()$Num==1] )
      rcode<<-r_out  
      cat(r_out)
    }
  }) # Generate R code
  
  output$scode<-renderPrint({
    files<-file.path(local.path1,
                     unique(Vdic()$Source[!is.na(Vdic()$Source)]))
    f1<-files[tolower(substring(files, nchar(files)-1,))==".r"]
    f1<-f1[!grepl('rcode.r', f1)]

    rF<-fLoop<-f1
    while(length(fLoop)>0){
      f0<-NULL
      for(i in 1:length(f1)){
        f2<-file.path(local.path1, "users", checkSource(fnm=f1[i]))
        fLoop<-fLoop[fLoop!=f1[i]]
        if(length(f2)>0){
          f0<-c(f0, f2)
          rF<-c(rF, f2)
        }
      }
      if(length(f0)>0){
        fLoop<-f1<-f0
      }
    }    
    f1<-unique(rF)
    f1<-f1[length(f1):1] #so that source files can be run first
    if(allSourceIn1||input$showAllSource){ #copy all the source code into one file
      code00<-lapply(f1,readLines)
      code00<-lapply(code00, function(x){gsub("source", 
         "###Search the copied code in this file\n### source", x)})
      code00<-unlist(lapply(1:length(f1),
        function(x){c(" ", " ", 
          paste0('#---Source ',x,': "',f1[x],'"---#'), 
          " ", code00[[x]])}))
      code00<-c(paste0("if(TRUE) { #list of ", length(f1), " R source files"), 
                paste0("   # source(file=paste0(local.path1, \'", gsub(local.path1, '', f1), "\'))"), 
                paste0("} #end of ", length(f1), " R source files."), code00)
    } else {#only list the source files
      code00<-c(paste0("if(TRUE) { #list of ", length(f1), " R source files"), 
                paste0("    source(file=paste0(local.path1, \'", gsub(local.path1, '', f1), "\'))"), 
                paste0("} #end of ", length(f1), " R source files.") )
    }
    code11<<-code00
    cat(paste0(code00,collapse='\n'))
  }, width=60) # Generate Source code 
  
  #------ Download LOA, R code, RTF output ------#
  
  output$save_loa<-downloadHandler(
    filename=function(){paste0(input$study,"_", input$tumor,"_loa.csv")},
    content=function(file){
      write.csv(LOA()[loaTF(),],file,row.names=FALSE, fileEncoding = 'UTF-8')
    }) # Download LOA
  
  #------ Download EPS plot ------#
  output$getEPS <- downloadHandler(
    filename="current_BEACH_EPS_Plot.eps",
    content =function(file){
      postscript(file, 
          height=tfl.h/tfl.r, 
          width=tfl.w/tfl.r,  
          pointsize = 1/tfl.r )
      plottry<-try(eval(parse(text=input0.code[1])))
      if(class(plottry)[1]=='try-error') {
        plot(0,axes=F, col='white',ylab='',xlab='')
        legend('top', 'Plotting\nError', cex=5, bty='n', text.col='red')
      }
      dev.off()
    }
  )
  #------ Download PDF plot ------#
  output$getPDF <- downloadHandler(
    filename="current_BEACH_PDF_Plot.pdf",
    content =function(file){
        pdf(file, 
                   height=tfl.h/tfl.r, 
                   width=tfl.w/tfl.r,  
                   pointsize = 1/tfl.r )
      plottry<-try(eval(parse(text=input0.code[1])))
      if(class(plottry)[1]=='try-error') {
        plot(0,axes=F, col='white',ylab='',xlab='')
        legend('top', 'Plotting\nError', cex=5, bty='n', text.col='red')
      }
      dev.off()
    }
  )
  
    
  #--------save output as a RTF file------------#
  output$save_output<-downloadHandler(
    filename=function(){
      if(input$onefileRTF=='one file'){
        f1<-paste0(input$study,"_",input$tumor, "_", Sys.Date(), ".rtf")
      }else{
        f1<-paste0(input$study,"_",input$tumor, "_", Sys.Date(), ".zip")
      }
      return(f1)
    },
    content=function(file){
      if(is.null(input$tumor) || is.na(input$tumor) || gsub(" ", '', input$tumor)=='')
        title0<-c(paste0("Study: ",input$study), '')
      else
        title0<-c(paste0("Study: ",input$study), paste0("Tumor Type: ",input$tumor))
      loalistT<<-LOA()[loaTF(),]
      loalistT$order<-1:nrow(loalistT)
      codelist<-merge(loalistT, Vdic(),by='Request.Name',all.x=TRUE)
      codelist<-codelist[order(codelist$order,codelist$Layout,decreasing=FALSE),]
      codelist$width <- codelist$width.x
      codelist$height <- codelist$height.x
      codelist$res <- codelist$res.x
      codelist<<-codelist
      
      #define the page margin
      mag1<-1
      #define the font size on each page
      fs <<- 8
      
      if(input$landscp=="Landscape"){
        pageW<-11      #page width
        pageH<-8.5     #page height
        nline<-52      #number of lines on a page
        nline.max<- 35 #maximum number of table rows on a page
        nline.min<- 5  #minimum number of table rows on a page
      } else {
        pageW<-8.5       #page width
        pageH<-11        #page height
        nline<-68        #number of lines on a page
        nline.max <- 50  #maximum number of table rows on a page
        nline.min <- 5   #minimum number of table rows on a page
      }
      maxH<-pageH-4*mag1
      maxW<-pageW-2*mag1
      if(input$onefileRTF=='one file'){
        rtf<-RTF(file, width=pageW, height=pageH, font.size=fs,omi=rep(mag1, 4) )
      }
     # rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)  # make "Courier New" as default

      npage<-1
      prd.status<-''
      nfile<-0
      zipfs<-NULL
      
      #reorder codelist according to heading levels
      all.tit <- codelist$Titles
      all.tit0<- strsplit(all.tit, split='HHH:', fixed=TRUE)
      all.tit0<-lapply(all.tit0, function(x){
         if(gsub(" ", "", x[1])==""){return(x[-1])} else{ return(x)}
      })
      totLayer<-max(sapply(all.tit0, length))
      all.tit0<-lapply(all.tit0, function(x){
         c(x, rep(NA, max(0, totLayer-length(x))))
      })
      mat.tit <-t( matrix(unlist(all.tit0), nrow=totLayer) )
      if(ncol(mat.tit)>1){
        wh.tit <- do.call(order, as.data.frame(mat.tit))
        codelist<-codelist[wh.tit,]
        mat.tit <-mat.tit[wh.tit, ]
      }
      if(is.vector(mat.tit)){
        mat.tit <- matrix(mat.tit, nrow=1)
      }

      mat.1<-matrix(TRUE, nrow=1, ncol=ncol(mat.tit))
      mat.tit[is.na(mat.tit)] <- ""
      nr.mat <- nrow(mat.tit)
      mat.1<- rbind(mat.1, !apply(mat.tit, 2, function(x){x[2:nr.mat]==x[1:(nr.mat-1)]}) )
      if(ncol(mat.1)>1){
        mat.1<-t( apply(mat.1, 1, function(x){
          wh.T<- which(x)
          if(length(wh.T)>0)
            x[wh.T[1]:length(x)] <- TRUE
          x
        }) )
      }


      for (i in 1:nrow(codelist)){
        if (!is.na(codelist[i,params.lab[1]])&&is.na(codelist[i,params[1]]))
	  next
        if(!all(is.na(codelist[i,params]))){
          k.list<-params[!is.na(codelist[i,params])]
          for (k in k.list[length(k.list):1]){
          tmp0<-unlist(strsplit(as.character(codelist[i,k]),',',fixed=TRUE))
          tmp<-ifelse(any(is.na(as.numeric(tmp0))),paste0('c(\"',paste0(tmp0,collapse='\",\"'),'\")'),
                      paste0('c(',codelist[i,k],')'))
          codelist$PlotCode[i]<- gsub(paste0('input$',k),tmp,codelist$PlotCode[i],fixed=TRUE)
          }
        }      
        
        nfile<-nfile+1
        #if all TFLs are NOT saved to one RTF file, a zip file will be downloaded
        if(input$onefileRTF!='one file'){
          f2<-file.path(local.path2, paste0(input$study,"_",input$tumor, "_", Sys.Date(),'_', nfile, ".rtf"))
          zipfs<-c(zipfs, f2)
          rtf<-RTF(f2, width=pageW, height=pageH,font.size=fs, omi=rep(mag1, 4) )
        }
        
        #Add Plot
        tmp<-eval(parse(text=codelist$tmp[i]))
        tflname<-ifelse(is.na(codelist[i,params[1]]),        #Condition
                        paste0(tflfile,codelist$Num[i]),     #if TRUE
                        paste0(tflfile,codelist$Num[i],'x',  #if FALSE
                               paste0(#Paste all UIs together
                                 sapply(params,function(y){
                                   y0<-codelist[i,y]
                                   if(!is.null(y0)){y0[is.na(y0)]<-''}
                                   y0<-paste0(y0[length(y0):1],collapse='_') 
				   #Paste all Params for each UI
                                   substring(y0, max(nchar(y0)-30,1))
                                 }),collapse=''))
        )
        fig.name<-paste0(gsub("[[:punct:]]","",tflname),'.png')
        fig.name<-gsub(" ", "", fig.name)
        fig.name.i<-file.path(local.path3,fig.name)
        tab.name<-paste0(gsub("[[:punct:]]","",tflname),'.rdata')
        tab.name<-gsub(" ", "", tab.name)
        tab.name.i<-file.path(local.path3,tab.name)
        
        if(codelist$Type.y[i]=='Figure'){ 
          figH<-eval(parse(text=codelist$height[i]))
          figW<-eval(parse(text=codelist$width[i]))
          figR<-eval(parse(text=codelist$res[i]))
          png(file=fig.name.i, height=figH, width=figW, res=figR)
          plottry<<-try(eval(parse(text=codelist$PlotCode[i])))
          if(class(plottry)[1]=='try-error') {
            plot(0,axes=F, col='white',ylab='',xlab='')
            legend('top', 'Plotting\nError', cex=5, bty='n', text.col='red')
          }else if(grepl('gg', class(plottry))){
            ggsave(fig.name.i, plottry, height=figH/100, width=figW/100, dpi=figR, limitsize=FALSE)
          } else {}
          dev.off()

          #Add Header
          title00<-title0
          title00[2]<-paste(title0[2],'    Page', npage, '     ', 
              format(Sys.time(), "%H:%M %d%b%Y"), "   ", prd.status)  # main header 
          npage<-npage+1
          title<-codelist$Titles[i]        
          
          for(t1 in 1:ncol(mat.tit)){
            if(mat.1[i,t1] && mat.tit[i,t1]!=""){
              addHeader(rtf, title=mat.tit[i,t1], font.size=10, TOC.level=t1)
            }else if (mat.tit[i,t1]!="") {
              addParagraph(rtf, mat.tit[i,t1])
            }else{}
          }
          #addParagraph(rtf, title)
          addParagraph(rtf, title00)          
          rtf$.font.size <- fs
          
          tmW<- min(eval(parse(text=codelist$width[i]))/90, maxW)
          tmH<- min(eval(parse(text=codelist$height[i]))/90, maxH)
          tmR<- eval(parse(text=codelist$res[i]))
          addPng(rtf, fig.name.i, width=tmW, height=tmH, res=tmR, col.justify='L')
          
          #footnote for figures
          #addNewLine(rtf, n=1)
          footnote<-codelist$Footnote[i]
          addParagraph(rtf, footnote)   
        }
        
        #Add Table
        if(codelist$Type.y[i]=='Table'){
          tab.h <<- codelist$height[i]
          tab.w <<- as.numeric( eval(parse(text=paste("strsplit(' ",
                       codelist$width[i], " ', split=',')")   ))[[1]] )
          
          tmp<-eval(parse(text=codelist$tmp[i]))
          tmptab<-eval(parse(text=codelist$PlotCode[i]))
          save(tmptab,file=tab.name.i)
          
          
          #load(tab.name.i)
          outTable <<-tmptab #is a data.frame
	  
          #check colnames
          outTable.colNm<-colnames(outTable)
          ot1 <- strsplit(outTable.colNm, split=";", fixed=TRUE)

          ot1n<-sapply(ot1, length)

          if(max(ot1n)==1){
            multiHeader<-FALSE
          }else{
            multiHeader<-TRUE
            nheader <- max(ot1n)
            #add extra row if a column names' level is smaller than the others
            ot1ns <- ot1n < nheader
            outTable.colNm[ot1ns] <- paste0(outTable.colNm[ot1ns], 
                 sapply(nheader-ot1n[ot1ns], 
                    function(x){paste(rep('; ',x), collapse = '')}) )
            colnames(outTable) <- outTable.colNm
            
            ot2 <- sapply(outTable.colNm, strsplit, split=';', fixed=TRUE)

            names(ot2) <- NULL
            ot2<-matrix( unlist(ot2), ncol=length(ot2))
            var.ul <- unique(as.vector(ot2[-nrow(ot2),]))
            var.ul0 <- gsub(' ', '', var.ul)
            var.ul0 <- var.ul[var.ul0!=""]
          }
          
          
          footnote<-codelist$Footnote[i]
          footnote<-ifelse(is.na(footnote), '', footnote)

          nrTab<-nrow(outTable)
          ncTab<-ncol(outTable)
          nrFoot<-1+length(strsplit(footnote, split='\n', fixed=TRUE)[[1]])
          nline1<-max(1, nline-nrFoot - 10) #header lines
          
          tWidth<-pageW-mag1*2
          nc<-nchar(as.character(outTable[,1]))
          #use the 1st column width for table width
          c1W<-pmax(max(nc)*0.079/2, 20*0.1)
          caW<-nc*0.1
          
          if(pageW < 9){
            lcW<-.711
            if(ncol(outTable)>2){
              othW<-(tWidth-c1W-lcW)/(ncol(outTable)-2)
              centW<-rep(othW,ncol(outTable)-2)
              cWs<-c(c1W,centW,lcW)
            }else{
              cWs<-c(c1W, lcW)
              othW<-1
            }
          } else{
            if(ncol(outTable)>1){
              othW<-(tWidth-c1W)/(ncol(outTable)-1)
              centW<-rep(othW,ncol(outTable)-1)
              cWs<-c(c1W,centW)
            }else{
              cWs<-c(c1W,0)
              othW<-1
            }
          }
          max_cW<-max(apply(outTable,2, function(x){max(nchar(x))}))
          max_rW1<-ceiling(max_cW/othW)
          
          if(multiHeader) 
            nline1 <- nline.max
          else
            nline1<-max(min(nline1-max_rW1+1, nline.max), nline.min)
          
          #nline1 is the max number Table rows shown on each page
          if(tab.h<=30&tab.h>=1){
            nline1 <- tab.h
          }
          if(length(tab.w) == length(cWs) ){
            cWs <- tab.w
          }
          nline1 <<- nline1
          nrTab <<- nrTab
 
          for(j in 1:ceiling(nrTab/(nline1))){ #j matches the page number of the table
            #Add Header
            title00<-title0
            title00[2]<-paste0(title0[2], '    Page ', npage, '     ', 
              format(Sys.time(), "%H:%M %d%b%Y"), "   ", prd.status)  # main header 
            npage<-npage+1
            title<-codelist$Titles[i]        

            for(k in 1:length(title00))
              title00[k]<-paste0(title00[k], paste(rep(' ', max(0, pageW*7-nchar(title00[k]))), collapse=''))
            title00<-paste(c(title00, "\n"), collapse=' ')
            #add heading sections or bookmark
            for(t1 in 1:ncol(mat.tit)){
              if(j==1 && mat.1[i,t1] && mat.tit[i,t1]!=""){
                addHeader(rtf, title=mat.tit[i,t1], font.size=10, TOC.level=t1)
              }else if (mat.tit[i,t1]!="") {
                addParagraph(rtf, mat.tit[i,t1])
              }else{}
            }
            #addParagraph(rtf, title)
            rtf$.font.size <- 10   #define the font size of titles
            addParagraph(rtf, title00)   
            rtf$.font.size <- fs   #redefine the font size of text
            
            bottLine<-min(nline1*j, nrTab)
            if(TRUE){#the table structure previously used.
              tableBL<- bottLine-((j-1)*nline1)
              tb1<-outTable[((j-1)*nline1+1):bottLine,]
              if(is.null(nrow(tb1))){
                tb1<-data.frame(` `=tb1)
              }
              if(multiHeader){# 17-Mar-2016
                rtf.table.out(rtf,  tb=tb1, 
                              cell1=max(nheader+1,3), #number of levels in header and then plus 1
                              nheader=max(nheader,2), #number of levels in header
                              colFormat=c("L", rep("C",ncol(outTable)-1)),
                              nline.body= nline1, #min(tableBL, ncol(tb1)), #number of lines per page
                              height=pageH,
                              width=pageW,
                              omi=rep(mag1, 4),
                              cw=cWs,      #column width
                              var.ul=var.ul0 #key string for underline, must be the entire string 
                              #eg. if changing " X1" to "X1", then it will not have the underline
                              )
                addParagraph(rtf,footnote) 
              } else {
                rtf<-rtf_table_out_as_sas(rtf, tb1, 
                                          cw=cWs, colFormat=c("L", rep("J",ncol(outTable)-1)),
                                          nline.body=tableBL, 
                                          width=maxW, height=maxH, omi=rep(mag1, 4) )
                addParagraph(rtf,footnote) 
              } 
            }
            
            if(bottLine<nrTab )
              addPageBreak(rtf, width=pageW, height=pageH, font.size=fs, omi=rep(mag1, 4) )
          }
        }
        
        #get a new RTF page for the next analysis
        if(i < nrow(codelist))
          addPageBreak(rtf, width=pageW, height=pageH, font.size=fs, omi=rep(mag1, 4) )

        #one file is for one analysis 
        if(input$onefileRTF!='one file'){
          done(rtf)
          print(paste(f2, 'is saved.'))
        }
      }
      
      if(input$onefileRTF=='one file'){
        done(rtf)
      }else{
        zipfs.gb<<-zipfs        
        zip(zipfile=file, files=zipfs)
       }
    }) # Download rtf output
  #---------End of save output as a RTF file---------------#

  #--------save output as a shiny::HTML file------------#
  output$save_outputH<-renderUI({
    if(is.null(input$outputH)||input$outputH==0){
      return(NULL)
    }
    if(!dir.exists(htmlPath)){
      return(shiny::HTML('Please input an existing director for HTML output!'))
    }
    
    isolate({
      curP0<-getwd()

      tp<-try(setwd(input$hPath))
      if(class(tp)[1]=="try-error"){
         return(shiny::HTML('<h6>The path for HTML file output is not working.</h6>'))
      }else{setwd(curP0)}
      file00<-paste0(input$study,"_",input$tumor,"_", Sys.Date())
      file0<-file.path(input$hPath, file00)
      file01<-file.path(file0, file00)
      if(file.exists(file0)){
        setwd(file0)
        try(file.remove(dir()))
        if(file.exists(file00)){
          setwd(file00)
          try(file.remove(dir()))
        }else{dir.create(file00, showWarnings=FALSE, recursive=TRUE)}
      }else{ #create a folder for linked files
        dir.create(file0, showWarnings=FALSE, recursive=TRUE)
        dir.create(file.path(file0, file00), showWarnings=FALSE, recursive=TRUE)
      }

      title0<-paste0("Study: ",input$study,"\t\t","Tumor Type: ",input$tumor)
      loalistT<-LOA()[loaTF(),]
      loalistT$order<-1:nrow(loalistT)
      codelist<-merge(loalistT, Vdic(),by='Request.Name',all.x=TRUE)
      codelist<-codelist[order(codelist$order,codelist$Layout,decreasing=FALSE),]
      ind<-0
      
      #start HTML 

      #remember to create a folder under htmlPath
      setwd(curP0)
      indexH<-readLines(con=file.path(htmltem, 'index.htm'))
      bodyH<-readLines(con=file.path(htmltem, 'body.htm'))
      bodyH<-gsub("Biomarker Analyses Output From BEACH Platform",
                  title0, bodyH)
      contH<-readLines(con=file.path(htmltem, 'content.htm'))
      #one row in one codelist is one page in the study report
      for (i in 1:nrow(codelist)){#update source code in codelist$PlotCode
        if (!is.na(codelist[i,params.lab[1]])&&is.na(codelist[i,params[1]]))next
        if(!all(is.na(codelist[i,params]))){
          k.list<-params[!is.na(codelist[i,params])]
          for (k in k.list[length(k.list):1]){
            tmp0<-unlist(strsplit(as.character(codelist[i,k]),',',fixed=TRUE))
            tmp<-ifelse(any(is.na(as.numeric(tmp0))),
              paste0('c(\"',paste0(tmp0,collapse='\",\"'),'\")'),
              paste0('c(',codelist[i,k],')'))
            codelist$PlotCode[i] <- gsub(paste0('input$',k),
              tmp,codelist$PlotCode[i], fixed=TRUE)
          }
        }
        #Add Header
        title<-codelist$Titles[i]
        tmpTit<-paste(title0, "/n", title)
        
        #Add Plot&Table
        tmp<-eval(parse(text=codelist$tmp[i]))
        tflname<-ifelse(is.na(codelist[i,params[1]]),             #Condition
                        paste0(tflfile, codelist$Num[i]),         #if TRUE
                        paste0(tflfile, codelist$Num[i],'x',      #if FALSE
                               paste0(#Paste all UIs together
                                 sapply(params,function(y){
                                   y0<-codelist[i,y]
                                   if(!is.null(y0)){y0[is.na(y0)]<-''}
                                   y0<-paste0(y0[length(y0):1], collapse='_') 
				   #Paste all Params for each UI
                                   substring(y0, max(nchar(y0)-30,1))
                                 }),collapse=''))
        )
        tflname<-gsub('tfl', paste0('t',i,'fl'),tflname, fixed=TRUE)
        fig.name<-paste0(gsub("[[:punct:]]","",tflname),'.png')
        fig.name.i<-file.path(local.path3, fig.name)
        tab.name<-paste0(gsub("[[:punct:]]","",tflname),'.rdata')
        tab.name.i<-file.path(local.path3, tab.name)

        #insert figure
        if(codelist$Type.y[i]=='Figure'){ 
          myres<-ifelse(is.null(codelist$res[i])||is.na(codelist$res[i]), 150, 
                        eval(parse(text=codelist$res[i])))
          png(fig.name.i, height=eval(parse(text=codelist$height[i])),
              width=eval(parse(text=codelist$width[i])), res=myres)
          plottry<-try(eval(parse(text=codelist$PlotCode[i])))
          if(class(plottry)[1]=='try-error') {
            plot(0,axes=F, col='white',ylab='',xlab='')
            legend('top', 'Plotting\nError', cex=5, bty='n', text.col='red')
          }
          dev.off()
          fig.name.1<-strsplit(fig.name.i, split="www/",fixed=TRUE)[[1]][2]
          file.copy(from=fig.name.i, file01)
          ind<-ind+1
          bodyH<-c(bodyH, fig2html(figNm=fig.name.1, figTit=codelist$Titles[i], 
                                   figH=eval(parse(text=codelist$height[i])), 
                                   figW=eval(parse(text=codelist$width[i])) ,
                                   fnote=codelist$Footnote[i], ind=ind ))
          contH<-c(contH, cLink(tit=codelist$Titles[i], ind=ind))          
        }
        
        #Insert Table
        if(codelist$Type.y[i]=='Table'){
          #generate the temporary table
          tmp<-eval(parse(text=codelist$tmp[i]))
          tmptab<-eval(parse(text=codelist$PlotCode[i]))
          save(tmptab,file=tab.name.i)
          
          #load(tab.name.i)
          outTable<-tmptab

        
          ind<-ind+1
          bodyH<-c(bodyH, df2html(datF=outTable, tabTit=tmpTit, 
            fnote=codelist$Footnote[i], ind=ind))
          contH<-c(contH, cLink(tit=codelist$Titles[i], ind=ind))
        }
      }
      bodyH<-c(bodyH, '<br>', '</div>', '</body>', '</html>')
      contH<-c(contH, '<br>', '</ol>', '</body>', '</html>')

      write(bodyH, file=file.path(file01, 'body.htm'))
      write(contH, file=file.path(file01, 'content.htm'))
      
      indexH<-gsub("tmpfolder", file00, indexH, fixed=TRUE)
      file1<-paste0(file00,".htm")
      write(indexH, file=file.path(file0, file1))
      
      setwd(curP0)
      return(shiny::HTML(paste('<h6>', file.path(file0, file1), 
        'and its folder are successfully downloaded.</h6>')))
   }) })  # Download HTML output
   #---------End of save output as a HTML file---------------#
  
  output$save_data<-downloadHandler(
    filename=function(){
      tt <- paste0('download_tmpData.', input$multdat_ext)
      return(tt)
    },
    content=function(file){
      #if (is.null(input$infile)&&is.null(input$infileR))
      #  return(NULL)
      
      choice<-sapply(AnalyN(),function(x){
        ifelse(substr(Vdic()$Title[x],1,5)=='paste',
	  eval(parse(text=Vdic()$Title[x])),
	  Vdic()$Title[x])
      })

      outList <<- currTabL
      outdata <<- outList[[1]]

      sapply(1:length(currTabL),
        function(x){
	  assign(paste0('outmplkhdnttawr',x),currTabL[[x]],envir=globalenv() )
	      } )
      
      if(input$multdat_ext=='rdata'){
        save(currTabL,file=file)
      } else if(input$multdat_ext=='xls'){
        WriteXLS(paste0('outmplkhdnttawr',1:length(currTabL)),
	    ExcelFileName=file,SheetNames=1:length(input$multdat))
      }  else if(input$multdat_ext=='sas'){
        if(!is.vector(currTabL[[1]])){
          o1<-apply(currTabL[[1]], 2, paste, collapse='\t\t\t')
        }else{
          o1 <- currTabL[[1]]
        }
        writeLines(o1,con=file)
        
      } else if(input$multdat_ext=='csv'){
        write.csv(currTabL[[1]], file=file, row.names=FALSE, fileEncoding='UTF-8')
      } else if(input$multdat_ext=='xpt'){
        #write.xport(currTabL[[1]], file=file, autogen.formats=FALSE)
        haven::write_xpt(currTabL[[1]], path=file)
      }
      
    }) # Download data output
  
  
  output$save_rcode<-downloadHandler(
    filename=function(){paste0(input$study,"_",input$tumor,".r")},
    content=function(file){
      writeLines(rcode,file)}) # Download R code
  
  output$save_scode<-downloadHandler(
    filename=function(){paste0(input$study,"_",input$tumor,"_source.r")},
    content=function(file){
       writeLines(code11,file)
  }) # Download Source code
  
  output$delete<-renderUI({#remove temporary files    
      if(input$delete_tmpfile==0){
        return(NULL)
      } else {
        cnt<-cnt+input$delete_tmpfile
	#for files under the tmpfile folder
        loatextf<-dir(local.path2)
        loatextf<-loatextf[nchar(loatextf)>3]
        if(length(loatextf)>0)
          loatextf<-loatextf[substr(loatextf,1,3)=='loa']
	#for files under the www folder
        tm_png<-dir(local.path3)
        tm_png<-tm_png[nchar(tm_png)>3]
        if(length(tm_png)>0){
	      tmsel<-substr(tm_png,1,3)=='tfl'|substr(tm_png,2,4)=='tfl'|substr(tm_png,1,2)=='.n'|
            substr(tm_png,3,4)=='fl'|
            substr(tm_png,nchar(tm_png)-6,nchar(tm_png))=='.rdata'
          tm_png<-tm_png[tmsel]
        }
        if(length(tm_png)==0 & length(loatextf)==0){
          return('Empty now.')
        }else{
          if(length(loatextf)>0){
            loatextf<-file.path(local.path2, loatextf)
            try( do.call(file.remove, as.list(loatextf)) )
          }
          if(length(tm_png)>0){
            tm_png<-file.path(local.path3,tm_png)
            try( do.call(file.remove, as.list(tm_png)) )
          }
          return(paste('Done',cnt))
        }
      }     
  })

  #define data subset
  output$subsetcode1<-renderUI({
    if(is.null(input$submitcode)||input$submitcode==0){
      subRcode<<-NULL
    }else{subRcode<<-isolate(input$subRcode)}
    return(conditionalPanel(condition='true', 
      shiny::HTML("<textarea name=\"subRcode\" rows=\"4\"  style=\"width:100%\"
        placeholder=\"#drag the right bottom corner to make this larger.
#indataset is a list object including all the uploaded csv files.
#indataset.i is indataset[[1]] by default. indataR is a list object including
#all the objects in the uploaded R file.
#Input your R code here to re-define indataset.i, for example
indataset.i<<-indataset[[1]]
indataset.i<<-indataset.i[1:10,]
#or
indataset.i<<-indataset[[1]]; indataset.i<<-indataset.i[1:10,]\"></textarea>") ))
  })
  output$subsetcode2<-renderPrint({
    if(input$submitcode==0 && length(indataset)>0){
      indataset.i<<-indataset[[1]]
      return(NULL)
    }else if(input$submitcode!=0 && length(indataset)>0){
      rpt.cls<-class(try(rpt<-eval(parse(text=subRcode))))
      if(rpt.cls!='try-error'){
          #source(file.path(local.path1,Vdic()$Source[1]))
          #source(file.path(local.path1,Vdic()$Source[2]))
          n.row<-nrow(indataset.i)
          n.col<-ncol(indataset.i)
          #sub<-colnames(indataset.i)[grep('subjid',tolower(colnames(indataset.i)))[1]]
          #n.sub<-length(unique(indataset.i[,sub]))
          #tmprnt<-paste0('#indataset.i is re-defined. It has ', #n.sub,' subjects, ',
          #               n.row,' rows, ', n.col,' variables.')
          #print(tmprnt)
      }
      rpt
    }else{}
  })
  
  output$showAS<-renderUI({#show all source code or not
    if(input$showAllSource){
      allSourceIn1<<-TRUE
    }else{
      allSourceIn1<<-FALSE
    }
  })
  
  #######for dynamic output########
  output$dynamicPlot <- renderPlot({
    eval(parse(text=dynamicCode))
  })
  
  output$click_info_DT<-renderUI({
      return(DT::dataTableOutput('click_info'))
  })
  output$click_info <- renderDataTable({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    near_points <<- nearPoints(dynamicData, input$dynamicPlot_click,
               xvar=dynamicData.xvar, yvar=dynamicData.yvar,
               addDist = TRUE)  
    near_points
  },  rownames = FALSE)

  
  output$click_info_dl<-downloadHandler(
    filename=function(){paste0(input$study,"_",input$tumor,"_clicked_dynamicData.csv")},
    content=function(file){
      tmpout<-nearPoints(dynamicData, input$dynamicPlot_click,
                         xvar=dynamicData.xvar, yvar=dynamicData.yvar,
                         addDist = TRUE)
      write.csv(tmpout, file=file, row.names=FALSE, fileEncoding='UTF-8')
    }
  ) # Download clicked data
  
  
  output$brush_info_DT<-renderUI({
      return(DT::dataTableOutput('brush_info'))
  })
  output$brush_info <- renderDataTable({
    if(showBrush)
 #     print(trytry<<-input$dynamicPlot_brush) 
    brush_points<<-brushedPoints(dynamicData, input$dynamicPlot_brush,
                  xvar=dynamicData.xvar, yvar=dynamicData.yvar)
    brush_points
  }, rownames = FALSE)

  ####End for dynamic output########
  output$brush_info_dl<-downloadHandler(
    filename=function(){paste0(input$study,"_",input$tumor,"_brushed_dynamicData.csv")},
    content=function(file){
      tmpout<-brushedPoints(dynamicData, input$dynamicPlot_brush,
                            xvar=dynamicData.xvar, yvar=dynamicData.yvar)
      write.csv(tmpout, file=file, row.names=FALSE, fileEncoding='UTF-8')
    }
  ) # Download clicked data
  
  
  
  ####Dynamic data table####
  output$outTbl_DT<-renderUI({
      return(DT::dataTableOutput('outTbl'))
  })
  output$outTbl<-DT::renderDataTable({
    if (all(!Vdic()$Type[AnalyN()] %in% c('Figure','Table'))||
        all(eval(parse(text=Vdic()$Condition[AnalyN()])))){
      return(NULL)
    }
    for (i in AnalyN()[1]){#only render the first table
      if(!is.na(Vdic()$Title[i])){
        title<-ifelse(substr(Vdic()$Title[i],1,5)=='paste',
                      eval(parse(text=Vdic()$Title[i])),Vdic()$Title[i])
        titlecode<-paste0('<h3><textarea name=\"titlebox\" rows=\"1\" style=\"width:80%\">',
                          title,'</textarea></h3>')}else{titlecode<-NULL}
      
      tflname<-ifelse(all(is.na(widgetOrd()$names)),               #Condition
                      paste0(tflfile,Vdic()$Num[i]),    #if TRUE
                      paste0(tflfile,Vdic()$Num[i],'x',        #if FALSE
                             paste0(#Paste all UIs together
                               sapply(widgetOrd()$names,function(y){
                                 y0<-eval(parse(text=paste0('input$',y)))
                                 if(!is.null(y0)){y0[is.na(y0)]<-''}
                                 y0<-paste0(y0[length(y0):1],collapse='_') #Paste all Params for each UI
                                 substring(y0, max(nchar(y0)-30,1))
                               }),collapse=''))
      ) 
      tflname<-gsub(' ', '',tflname)   
      fig.name<-paste0(input$submitcode,gsub("[[:punct:]]", "",tflname),'.png')
      tab.name<-file.path(local.path3,paste0(input$submitcode,gsub("[[:punct:]]", "",tflname),'.rdata')) 
      

     if(Vdic()$Type[i]=='Table'){
        #if(!file.exists(tab.name)){
          if(class(try(tmptab<-eval(parse(text=input0.code))))[1]=='try-error'){
            tmptab<-''
          }
        #  save(tmptab, file=tab.name)
        #}else{
        #  load(tab.name)
        #}
        rd_tmptab <<- tmptab
        return(tmptab)
      }else{return(NULL)}
    }
  }, rownames = FALSE)


  output$getDataSelected <- renderUI({
    data_selected_col1 <<- unique(c(
                     input$click_info_rows_selected,
                     input$brush_info_rows_selected  ))
      
    if(is.null(input$outTbl_rows_selected)){
      data_selected <<- rbind(
        near_points[!is.na(near_points[,1]) & near_points[,1]%in%data_selected_col1,],
        brush_points[!is.na(brush_points[,1]) & brush_points[,1]%in%data_selected_col1,]
      )
    } else{
      data_selected <<- rd_tmptab[!is.na(rd_tmptab[,1]) & 
                                    rd_tmptab[,1] %in% input$outTbl_rows_selected,]
    }
    return(NULL)
  })
  
  
 ##########################
}  #close Shiny server#


shinyServer(BeachServer)
