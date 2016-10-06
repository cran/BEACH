
#----------UI Functions------------------#


#select color function
selectInput_color <- function(id='wpColor', 
                              label='Background color', 
                              choices=colors(), 
                              selected='azure3',
                              HEX=HEXColors){
  code <- paste0("<div class=\"form-group shiny-input-container\">\n",
                 "<label class=\"control-label\" for=\"",
                 id, "\">", label, ":</label>\n<div>\n",
                 "<select id=\"", id, "\"><option value=\"",
                 selected, "\" selected>", selected, "</option>\n")
  for(i in 1:length(choices)){
    code <- paste0(code, 
                   "<option value=\"",
                   choices[i], "\" style=\"background-color: ",
                   HEX[choices[i]], "\">", choices[i], "</option>\n")
  }
  code <- paste0(code, "</select>",
                 "<script type=\"application/json\" data-for=\"",
                 id, "\" data-nonempty=\"\">{}</script> \n</div>\n</div> ")

  return(code)
}










#Function to create Table with checkboxes

checkboxTableInput <- function(df,table.id="checktable",id.name, 
  checked, check.name,labels=NULL, showSelectedOnly=FALSE
){
  table_head0<-capture.output(print(xtable(df[0,]), type="html", 
    include.rownames=F,include.colnames=T))
  table_head0[3]<-"<table border=1 class=\"table table-bordered\">"
  table_head <- paste(table_head0, collapse=" ")
  table_head <- gsub("</TABLE>", "", table_head)
  table_head <- gsub("</table>", "", table_head)
  table_head <- gsub("<table", paste0("<table id=\"",table.id,"\""), table_head)
  table_head<- gsub('<TR>','<tr>',table_head)
  table_head<- gsub('</TR>','</tr>',table_head)
  table_head <- gsub("<tr>", paste("<tr> <th> ", check.name, " </th>", sep=""), 
    table_head)
  table_body <- paste(capture.output(print(xtable(df), type="html", 
    content.only=T, include.rownames=F, include.colnames=F)), collapse=" ")
  table_body<- gsub('<TR>','<tr>',table_body)
  table_body<- gsub('</TR>','</tr>',table_body)
  table_body<- gsub('<TD>','<td>',table_body)
  table_body<- gsub('</TD>','</td>',table_body)
  table_body<- gsub('</tr>','',table_body)
  table_body<- gsub('</table>','',table_body)
  which.tag <- regexpr("<tr>", table_body)[1]
  table_body <- substr(table_body, which.tag, nchar(table_body))
  table_body2 <- unlist(strsplit(table_body, "<tr>", fixed=T))
# print(table_body2)
  table_body3 <- '' #table_body2[1]

  if(showSelectedOnly){
    for (i in 1:nrow(df)){
      if(checked[i]){
        check_tag <- paste("<tr>  <td align=left>", 
          as.character(checkboxInput(paste0(id.name,'_',i), "", checked[i])),
          " </td>", sep="")
        table_body3<-paste(table_body3, check_tag, table_body2[i+1], "</tr>",  sep="")
      }
    }
  }else{
    for (i in 1:nrow(df)){
      check_tag <- paste("<tr>  <TD align=left>", 
        as.character(checkboxInput(paste0(id.name,'_',i), "", checked[i])),
        " </td>", sep="")
      table_body3<-paste(table_body3, check_tag, table_body2[i+1], "</tr>", sep="")
    }
  }
  table_out <- paste(table_head, table_body3, "</table>", collapse=" ")
  if (!is.null(labels)){
    for (k in 1:length(labels)){
      table_out<-gsub(paste0('<th> ',colnames(df)[k],' </th>'),
        paste0('<th> ',labels[k],' </th>'), table_out)
    }
  }
  # print(table_out)

  return(table_out)
}

#-------------Create Tabs-------------#
tabs<-function(tab.id,tab.label,tab.value){
  tabset<-unique(cbind(tab.label,tab.value))
  tabcode1<-paste0('<div class=\"tabbable tabs-above\">\n',
    '<ul class=\"nav nav-tabs shiny-tab-input\" id=\"',
    tab.id,'\">\n<li class=\"active\">\n')
  tabcode2<-paste0('<a href=\"#tab-',tab.id,'-',1:nrow(tabset),
    '\" data-toggle=\"tab\" data-value=\"',
    tabset[,1],"\">",tabset[,2],"</a>\n</li>\n<li>\n")
  tabcode2[length(tabcode2)]<-gsub('<li>','</ul>',tabcode2[length(tabcode2)])
  tabcode3<-"<div class=\"tab-content\">"
  tabcode4<-paste0('<div class=\"tab-pane active\" data-value=\"',
    tabset[,1],'\" id=\"tab-',tab.id,'-',1:nrow(tabset),
    '\"></div>\n',collapse=" ")
  tabcode5<-'</div>\n</div>'
  tabcode<-paste0(tabcode1,paste0(tabcode2,collapse=" "),
    tabcode3,tabcode4,tabcode5,collapse=" ")  
  return(tabcode)
}

#-------------side panel data input-------------#
inlinetext<-function(text.id,text.label,text.value, note=NULL){
  if(is.null(note)){
    nt <- ""
  }else {
    nt <- paste0('<h6 style=\"width:50%\">', note, ' </h6>')
  }
  if(FALSE){ #using textInput widget
    code <- paste0(
      "<div class = \"form-group shiny-input-container\", style = \"width:100%\">",
      paste0("<label for=\"", text.id, "\">", text.label, "</label>"),
      paste0("<input id=\"", text.id, "\" type=\"text\" class=\"form-control\" value=\"", text.value, "\"/>"),
      "</div>",
      collapse='\n'
    )
  } else { #using traditional widget
    code<-paste0( '<div class=\"input-prepend\" style=\"width:100%\">',  
      paste0('<span class=\"add-on\" >',text.label,'</span>'),
      paste0('<input style=\"width:50%\" id=\"',text.id,
        '\" class=\"form-control\" value=\"',text.value,'\" type=\"text\" />\n'),
      nt,
      '</div>',
      collapse='\n')
  }
  return(code)
}

#-------------For output R code-------------#
runcode<-function(codelist, # merged dataset of loa and configuration file
                  datPath, # dataset path from input
                  outPath, # output path from input
                  libs,
                  params1,    #a vector of widgets name (require ordered)
                  paramLabs1, #a vector of widgets label(1:1 matched to params)
                  sourceCode, # source files from configuration file
                  subCode,
                  title0, # Main title with study id and tumor type
                  devpath,
                  outBcode=NULL
){
  
  codelist<-codelist[!(!is.na(codelist[,paramLabs1[1]]) & is.na(codelist[,params1[1]])),]
  sourceCode<-unique(sourceCode[!is.na(sourceCode)])
  sourceCode<-sourceCode[!grepl("rcode.r",sourceCode)]
  head1<-sop_head  #paste0('#/*soh',paste0(rep('*',73),collapse=''))
  nParam<-length(params1)

  if(FALSE){
  head2<-'if(FALSE){ # DESCRIPTION: Parameters for generating following results:'
  tmploa<-unique(codelist[,c('Request.Name',params1,paramLabs1)])
  tmploa2<-sapply(1:nrow(tmploa),function(y){
    paste0(tmploa$Request.Name[y],' ',
           ifelse(is.na(tmploa[y,paramLabs1[1]]),
                  '',
                  paste0('(',paste0(sapply(which(!is.na(tmploa[y,paramLabs1])),function(x){
                    paste0(tmploa[y,paramLabs1[x]],'=',
                           ifelse(is.na(tmploa[y,params1[x]]),
                                        'Parameters Needed',
                                        tmploa[y,params1[x]]))
                    }),collapse=';'),') \n')
                  )
           )})
  tmploa2<-gsub(";", ";\n    #", tmploa2)
  head3<-paste0('  #',1:length(tmploa2),'. ',tmploa2)  
  
  head4<-paste0('\n}\n#', paste0(rep("-",90),collapse='')) #end of DESCRIPTION
  } else {
     head2<-''
     head3<-''
     head4<-''
  }
  head5<-''# paste0('  #', gsub('\n', '\n  #  ', libs))
  head6<-'\nif(TRUE){ #------Read in CSV Dataset------'
  head7<-paste0( '  indataset<-list() \n',
    '  for(i in 1:length(inf)){ \n',
    '    indataset[[i]]<-read.csv_sas(inf[i],stringsAsFactors=FALSE,header=TRUE, encoding=inf.enc[i], comment.char=\"\") \n',
    '  } \n ',
    '  names(indataset)<-inf  \n',
    '  indataset.i<-indataset[[1]]  ')
  head8<-paste0('  ', subCode)
  head6.1<-'\n}\nif(TRUE){ #------Read in one RData------'
  head7.1<-paste0(
      'load(inf.R, indataR<-new.env())\n',
      'indataR<<-as.list(indataR)\n' )
  head9<-'\n}\n\nif (TRUE){ #------Source Packages and Function Files------'
  #connect default folder for reusable functions
  head10<-paste0(
    '  if(FALSE){ #Please firstly check whether all the required pacakges are installed.\n    ',
    '#install.packages(c(...))\n    ', 
    gsub('\n', '\n    ', libs),
    '\n  }',
    '\n  require(rtf) \n  require(haven) \n',
    '\n  read.csv_sas<<-function(file, header=TRUE, nrow=-1, comment.char="", ...){',
    '\n   is.sas<-grepl(\'.sas7bdat\', file, fixed=TRUE)',
    '\n   is.csv<-grepl(\'.csv\', file, fixed=TRUE)',
    '\n   if(is.sas){',
    '\n     ot<-haven::read_sas(file)',
    '\n     if(header & nrow==1){ot<-names(ot)}',
    '\n   }else if (is.csv){',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE, comment.char=comment.char, ...))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE,fileEncoding=\"latin1\", comment.char=comment.char))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE,fileEncoding=\"UTF-8\", comment.char=comment.char))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-readLines(con=file))     ',
    '\n   }else{',
    '\n   ot<-NULL\n}',
    '\n   return(ot) \n  }',   
    "\n   try(source(SourceFile)) \n ",
    "\n  ", ifelse(is.null(eval(parse(text=outBcode))), " ", outBcode),
    "\n}")
  head11<-'\n\nif(OutputRTF){  #----- Generate RTF file ------'
  head12<-paste0('\n  pageW<-11 \n  pageH<-8.5 \n  figW<-9 \n  figH<-6 \n',
    '\n  rtf<-RTF(rtfFileName,width=pageW,height=pageH, font.size=8,omi=c(.5,.5,.5,.5))')
  r_out<-c(head1,head2,head3,head4,head5, head9,head10,head6,head7,head8,head6.1, head7.1, head11,head12)
  for (i in 1:nrow(codelist)){
    for(x in nParam:1){
      if(!is.na(codelist[i,params1[x]])){
        tmp0<-unlist(strsplit(as.character(codelist[i,params1[x]]),
	  ',',fixed=TRUE))
        tmp00 <- try( as.numeric(tmp0) )
        if( class(tmp00)[1]=='try-error' || any(is.na(as.numeric(tmp0))) )
            say1 <- TRUE
        else
          say1 <-FALSE
        tmp<-ifelse(say1,
	  paste0('c(\"',paste0(tmp0,collapse='\",\"'),'\")'),
          paste0('c(',paste0(codelist[i,params1[x]],collapse=','),')'))
        codelist$Title[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$Title[i],fixed=TRUE)
        codelist$PlotCode[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$PlotCode[i],fixed=TRUE)
        codelist$FootCode[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$FootCode[i],fixed=TRUE)
      }
    }
  
    #Add Header
    title<-codelist$Title[i]

    if(is.na(title) || length(title)<1) title <- " "
    if(grepl(title, 'paste', fixed=TRUE)) title<-eval(parse(text=title))
    addhead<-vector()
    addhead[1]<-paste0('\n\n  #------Add Header  ',codelist$order[i],' ------')
    addhead[2]<-paste0('  addHeader(rtf,title=\"',title0,'\",font.size=10)')
    addhead[3]<-paste0('  addHeader(rtf,title=\"',title,'\",font.size=10)')
    r_out<-c(r_out,addhead)
    
    #Add Plot
    if( (!is.na(codelist$Type.y[i]) & codelist$Type.y[i]=='Figure') || 
        (!is.na(codelist$Type.x[i]) & codelist$Type.x[i]=='Figure')  ){
      addplot<-vector()
      addplot[1]<-paste0('  #------Add Plot: ',codelist$Request.Name[i],' ------')
      addplot[2]<-paste0('  tmp<-',codelist$tmp[i])
      addplot[3]<-paste0('  pngtmp<-function(){',codelist$PlotCode[i],'}')
      addplot[4]<-paste0('  addPlot(rtf,pngtmp,width=',codelist$width[i]/160,',height=',codelist$height[i]/160,')')  
      r_out<-c(r_out,addplot)}
    
    #Add Table            
    if(  (!is.na(codelist$Type.y[i]) & codelist$Type.y[i]=='Table') || 
         (!is.na(codelist$Type.x[i]) & codelist$Type.x[i]=='Table')  ){
      addtab<-vector()
      addtab[1]<-paste0('  #------Add Table: ',codelist$Request.Name[i],' ------')
      addtab[2]<-paste0('  tmp<-',codelist$tmp[i])
      addtab[3]<-paste0('  outTable<-{',codelist$PlotCode[i],'}')
      addtab[4]<-paste0('  tWidth<-9\nnc<-nchar(as.character(outTable[,1]))\nc1W<-pmax(max(nc)*0.079/2, 20*0.079)\n',
                        '  caW<-nc*0.079\nlcW<-.711\nothW<-(tWidth-c1W-lcW)/(ncol(outTable)-2)\n',
                        '  centW<-rep(othW,ncol(outTable)-2)\ncWs<-c(c1W,centW,lcW)'
                        )
      addtab[5]<-paste0( '  addTable(rtf,outTable,col.widths=cWs,col.justify=rep(\'L\',length(outTable)))'
                       )
      r_out<-c(r_out,addtab)
    }
    
    #Add Footnote
    footnote<-codelist$FootCode[i]
    footnote<-gsub("\\\\n","\n",footnote)
    if(!is.na(footnote)&&gsub("", " ", footnote)!=''&&substr(footnote,1,5)=='paste')
      footnote<-eval(parse(text=footnote))
    addfoot<-vector()
    addfoot[1]<-'  addNewLine(rtf, n=2)'
    addfoot[2]<-paste0('  #------Add Footnote  ',codelist$order[i],' ------')
    addfoot[3]<-paste0('  addParagraph(rtf,\"',footnote,'\")')
    r_out<-c(r_out,addfoot)
    
    if(i!=nrow(codelist)){
      r_out<-c(r_out,'  addPageBreak(rtf,width=11,height=8.5,font.size=8,omi=c(.5,.5,.5,.5))')
    }else{
      r_out<-c(r_out,'\ndone(rtf)\n}  #------End of RTF------\n')
    }
  }
  return(paste0(r_out,collapse='\n'))
}

  checkSource<-function(fnm, keyS1='source('){#fnm must be a file under inpath2
    rF1<-NULL
    r1<-readLines(con=fnm)
    r2<-toupper(gsub(" ", '', r1))
    falseKey<-FALSE
    for(j in 1:length(r1)){
      if(grepl('IF(FALSE)', r2[j], fixed=TRUE)|grepl('IF(F)', r2[j], fixed=TRUE)){
        falseKey<-TRUE
        b1<-0
      }
      if(falseKey){
        if(gsub("IF(FALSE)","", r2[j],fixed=TRUE)=="[{]"|substr(r2[j],1,1)=="[{]") b1<-b1+1
        if(b1==0 & grepl("[{]", r2[j]))
          falseKey<-FALSE
        if(grepl("}", r2[j])) {
          b1<-b1-1
          if(b1==0) falseKey<-FALSE
        }
      }else{
        if(grepl(toupper(keyS1), r2[j], fixed=TRUE)){
          f3<-strsplit(r1[j],split='local.path1,', fixed=TRUE)[[1]][2]
          f3<-gsub("users/","",f3, fixed=TRUE)
          f3<-gsub("'","",f3, fixed=TRUE)
          f3<-gsub('"','',f3, fixed=TRUE)
          f3<-gsub('\"','',f3, fixed=TRUE)
          f3<-gsub(')','',f3, fixed=TRUE)
          f3<-gsub('(','',f3, fixed=TRUE)
          f3<-gsub(' ','',f3, fixed=TRUE)
          if(grepl(".r", tolower(f3), fixed=TRUE)){
            rF1<-c(rF1, f3)
          }
        }
      }
    }
    return(rF1)
  }




#-------------Creat Parameters-------------#
#use radiobutton, slidebar, checkbox, selectInput
widgets.code<-function(UInames, max.n){
  ind<-max.n!=0

  check.code<-lapply(max.n['check']:1,function(x){  
    ret<-'\n output$checkbox_xx<-renderUI({
    tmpN<-reactive(which(check.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if(is.na(check.param()[tmpN(),\'check.label_xx\'])
    ||class(try(default<-eval(parse(text=check.param()[tmpN(),\'check.default_xx\']))))==\"try-error\"
    ||class(try(choice<-eval(parse(text=check.param()[tmpN(),\'check.choice_xx\']))))==\"try-error\")return(NULL)
    if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'checkbox Options Are Not Available\')))
    if (is.na(default))default<-choice
    checkboxGroupInput(inputId=\'check_xx\',label=strong(check.param()[tmpN(),\'check.label_xx\']),choices=choice,selected=default)})\n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  check.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),check.code)
  
  radio.code<-lapply(max.n['radio']:1,function(x){
    ret<-'output$radiobutton_xx<-renderUI({
     tmpN<-reactive(which(radio.param()$Request.Name==input$analysis))
     if(length(tmpN())<1) return(NULL)
     try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
     if(is.na(radio.param()[tmpN(),\'radio.label_xx\'])
     ||class(try(default<-eval(parse(text=radio.param()[tmpN(),\'radio.default_xx\']))))==\"try-error\"
     ||class(try(choice<-eval(parse(text=radio.param()[tmpN(),\'radio.choice_xx\']))))==\"try-error\")return(NULL)
     if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'radiobutton Options Are Not Available\')))
     if (is.na(default))default<-choice[1]
    radioButtons(\'radio_xx\',label=strong(radio.param()[tmpN(),\'radio.label_xx\']),choices=choice,selected=default)})\n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  radio.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),radio.code)
  
  dropdown.code<-lapply(max.n['dropdown']:1,function(x){
    ret<-'output$dropdown_xx<-renderUI({   
    isolate({
      tmpN<-reactive(which(dropdown.param()$Request.Name==input$analysis))
      if(length(tmpN())<1) return(NULL)
      try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))   
    })
    if(is.null(dropdown.param()[tmpN(),\'dropdown.label_xx\'])) return(NULL)
    if (is.na(dropdown.param()[tmpN(),\'dropdown.label_xx\'])  
    ||class(try(default<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.default_xx\']))))==\"try-error\"
    ||class(try(default2<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.multiple_xx\']))))==\"try-error\"
    ||class(try(choice<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.choice_xx\']))))==\"try-error\")return(NULL)
    if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'dropdown Options Are Not Available\')))
    if (all(is.na(default)))default<-choice[1]
    if (all(is.na(default2)))default2<-TRUE
    selectInput(\'dropdown_xx\',label=strong(dropdown.param()[tmpN(),\'dropdown.label_xx\']),
      choices=choice,selected=default,multiple=default2,selectize=TRUE, width=\"100%\") }) \n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  dropdown.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),dropdown.code)
     
  slide.code<-lapply(max.n['slide']:1,function(x){
    ret<-'output$slider_xx<-renderUI({
    tmpN<-reactive(which(slide.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if(is.null(slide.param()[tmpN(),\'slide.label_xx\'])) return(NULL)
    if (is.na(slide.param()[tmpN(),\'slide.label_xx\'])  
    ||class(try(min<-eval(parse(text=slide.param()[tmpN(),\'slide.min_xx\']))))[1]==\"try-error\"
    ||class(try(max<-eval(parse(text=slide.param()[tmpN(),\'slide.max_xx\']))))[1]==\"try-error\"
    ||class(try(default<-eval(parse(text=slide.param()[tmpN(),\'slide.value_xx\']))))[1]==\"try-error\"
    ||class(try(by<-eval(parse(text=slide.param()[tmpN(),\'slide.by_xx\']))))[1]==\"try-error\")return(NULL)
    if (length(c(min,max,default,by))<4)return(div(class=\"alert alert-error\",strong(\'slider Options Are Not Available\')))
    if (is.na(default))default<-mean(min,max)
    sliderInput(\'slide_xx\',label=strong(slide.param()[tmpN(),\'slide.label_xx\']),min=min,max=max,value=default,step=by,animate = TRUE)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
    })  
  slide.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),slide.code)
  
  date.code<-lapply(max.n['date']:1,function(x){
    ret<-'output$date_xx<-renderUI({
    tmpN<-reactive(which(date.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(date.param()[tmpN(),\'date.label_xx\'])  
    ||class(try(default<-eval(parse(text="(date.param()[tmpN(),\'date.value_xx\'])"))))==\"try-error\")return(NULL)
    if (!is.null(default)&is.na(default)) return(div(class=\"alert alert-error\",strong(\'date Options Are Not Available\')))
    dateInput(\'date_xx\',label=strong(date.param()[tmpN(),\'date.label_xx\']),
      value=default, format = \"yyyy-mm-dd\")})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  date.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),date.code)

  dateR.code<-lapply(max.n['dateR']:1,function(x){
    ret<-'output$dateR_xx<-renderUI({
    tmpN<-reactive(which(dateR.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(dateR.param()[tmpN(),\'dateR.label_xx\'])  
    ||class(try(min<-eval(parse(text="(dateR.param()[tmpN(),\'dateR.start_xx\'])"))))==\"try-error\"
    ||class(try(max<-eval(parse(text="(dateR.param()[tmpN(),\'dateR.end_xx\'])"))))==\"try-error\") return(NULL)
    if ((!is.null(min)&is.null(max))&(is.na(min)&is.na(max)))
      return(div(class=\"alert alert-error\",strong(\'data range Options Are Not Available\')))
    dateRangeInput(\'dateR_xx\',label=strong(dateR.param()[tmpN(),\'dateR.label_xx\']),
       start=min, end=max, format = \"yyyy-mm-dd\")})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  dateR.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),dateR.code)

  num.code<-lapply(max.n['num']:1,function(x){
    ret<-'output$num_xx<-renderUI({
    tmpN<-reactive(which(num.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(num.param()[tmpN(),\'num.label_xx\'])  
    ||class(try(default<-eval(parse(text="num.param()[tmpN(),\'num.value_xx\']"))))==\"try-error\")return(NULL)
    if (is.na(default)) return(div(class=\"alert alert-error\",strong(\'numericInput Options Are Not Available\')))
    numericInput(\'num_xx\',label=strong(num.param()[tmpN(),\'num.label_xx\']),
      value=default)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  num.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),num.code)

  text.code<-lapply(max.n['text']:1,function(x){
    ret<-'output$text_xx<-renderUI({
    tmpN<-reactive(which(text.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(text.param()[tmpN(),\'text.label_xx\'])  
    ||class(try(default<-eval(parse(text=text.param()[tmpN(),\'text.value_xx\']))))==\"try-error\")return(NULL)
    if (is.na(default)) return(div(class=\"alert alert-error\",strong(\'textInput Options Are Not Available\')))
    textInput(\'text_xx\',label=strong(text.param()[tmpN(),\'text.label_xx\']),
      value=default)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  text.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),text.code)
  
  textbox.code<-lapply(max.n['textbox']:1,function(x){
    ret<-paste0('output$textbox_xx<-renderUI({
     tmpN<-reactive(which(textbox.param()$Request.Name==input$analysis))
     try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
     if(is.na(textbox.param()[tmpN(),\'textbox.label_xx\']))return(NULL)
     cols<-eval(parse(text=textbox.param()[tmpN(),\'textbox.cols_xx\']))
     rows<-eval(parse(text=textbox.param()[tmpN(),\'textbox.rows_xx\']))
     default<-textbox.param()[tmpN(),\'textbox.default_xx\']
     label<-textbox.param()[tmpN(),\'textbox.label_xx\']
     if(is.na(cols)||is.null(cols))cols<-40
     if(is.na(rows)||is.null(rows))rows<-2
     texttmp<-paste0(\'<h6>\',label,\'</h6>\n<textarea name=\"\',textbox_xx,\'\" 
     rows=\"\',rows,\'\"  cols=\"\',cols,\'\">\',default,\'</textarea>\')
     return(shiny::HTML(texttmp))
     })\n\n')
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  textbox.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),textbox.code)
  
     
  widgets.code<-paste0(c(check.codes,radio.codes,dropdown.codes,slide.codes,
    date.codes, dateR.codes, num.codes, text.codes, textbox.codes)[ind],
    collapse='\n\n')
  return(widgets.code)
}

widgets.order<<-function(analysis, UIdic1, UInames, ncol.widg=NULL){
#  print(UIdic1)
#  print(UInames)
#  print(analysis)
  
  if(is.null(ncol.widg)){ncol.widg<-1}
  input.names<-as.vector(t(UIdic1[UIdic1$Request.Name==analysis, UInames]))
  if(length(input.names)==0){
    widg.list=NULL
  }else{
    input.labs<-as.vector(t(UIdic1[UIdic1$Request.Name==analysis,
      gsub("uiInput", "uilab", UInames, fixed=TRUE)]))
    #names(input.names)<-1:length(input.names)
    s_1<-!is.na(input.names)
    input.names<-input.names[s_1]#unique(setdiff(as.vector(input.names),NA))
    input.labs<-input.labs[s_1]
    input.labs[is.na(input.labs)]<-""

    widg.list<-unlist(strsplit(input.names,'input$',fixed=TRUE))
    sel0<-widg.list!='' & !is.na(widg.list)
    input.names<-widg.list<-widg.list[sel0]
    #input.labs<-input.labs[sel0]

    widg.list<-gsub('check','checkbox',widg.list)
    widg.list<-gsub('radio','radiobutton',widg.list)
    widg.list<-gsub('dropdown','dropdown',widg.list)
    widg.list<-gsub('slide','slider',widg.list)
   # widg.list<-gsub('date','date',widg.list)
   # widg.list<-gsub('dateR','dateR',widg.list)
   # widg.list<-gsub('num','num',widg.list)
   # widg.list<-gsub('text','text',widg.list)
   # widg.list<-gsub('textbox','textbox',widg.list)
  }
  if(length(widg.list)==0){
    widg.code<-'return(NULL)';
    input.names<-input.labs<-NA
  }else{
    if(F){
    widg.code<-paste0('uiOutput(\'',widg.list,'\')',collapse=',')
    widg.code<-paste0('return(conditionalPanel(condition=\'true\', div(',
      widg.code,')))',collapse='')
    }else{
    widg.code<-paste0('uiOutput(\'',widg.list,'\')') #,collapse=','
    w.widg<-floor(12/ncol.widg)    #upto 4, column width
    nrow.widg<-ceiling(length(widg.code)/ncol.widg)#number of rows
    widg.code2<-NULL
    for(i in 1:nrow.widg){
      widg.code3<-NULL
      for(j in 1:ncol.widg){
        ij<-(i-1)*ncol.widg+j  
        if(ij <= length(widg.code)){
          wij<-paste0("column(", w.widg,", ", widg.code[ij], 
              ", offset=", 0, ")") #max(w.widg*(j-1)-2,0)
          widg.code3<-paste0(c(widg.code3, wij), collapse=",")
        }
      }
      widg.code2<-c(widg.code2, paste0("fluidRow(", widg.code3, ")"))
    }
    widg.code<-paste0(widg.code2, collapse=',')
    widg.code<-paste0('return(conditionalPanel(condition=\'true\', div(',
      widg.code,')))',collapse='')
    }
  }
  out1<-data.frame(code=widg.code,names=input.names,labs=input.labs)

  return(out1)
}

widgets.param<-function(uidic, wid, max.n){
  if(!wid %in% names(max.n)){
    return(NA)
  }else{
    temp<-lapply(setdiff(unique(uidic$Request.Name),NA),function(x){
      row_sel<-uidic$Request.Name==x
      ret<-unique(uidic[which(row_sel), grepl(wid,colnames(uidic))])
      ret$Request.Name<-x
      #dfnames is the widget name which default values not specified in CD
      wid2<-wid[!wid%in%c("num","text")]
      if(length(wid2)>0){
        dfnames<-c(paste0(wid2,'.default',c(max.n[wid2]:2, '')))
        dfnames<-dfnames[!dfnames%in%colnames(uidic)] 
        if (length(dfnames)>0){
          dfs<-matrix(NA,nrow=nrow(ret),ncol=length(dfnames))
          colnames(dfs)<-dfnames
          ret2<-data.frame(ret,dfs,row.names=NULL)
        }else{ ret2<-data.frame(ret,row.names=NULL)}
      }else{ ret2<-data.frame(ret,row.names=NULL)}
      #dfnames3 is the dropdown widget name which multiple not specified in CD
      wid3<-wid2[grepl("dropdown",wid2)]
      if(length(wid3)>0){
        dfnames3<-c(paste0(wid3,'.multiple',c(max.n[wid3]:2, '')))
        dfnames3<-dfnames3[!dfnames3%in%colnames(uidic)]
        if (length(dfnames3)>0){
          dfs<-matrix(NA,nrow=nrow(ret2),ncol=length(dfnames3))
          colnames(dfs)<-dfnames3
          ret3<-data.frame(ret2,dfs, row.names=NULL)
        }else{ ret3<-data.frame(ret2,row.names=NULL) }
      }else{ ret3<-data.frame(ret2,row.names=NULL) }
      return(ret3)
    })
    #convert the list object to data.frame
    ret0<-Reduce(function(x,y){unique(rbind(x,y))},temp)
    return(ret0)
  }
}



#-------------Functions for RTF output setup-------------#
if(TRUE){
  col_width <- function(tb){  #Function to Generate rtf table column widths
    cws <- NULL
    for(i in 1:length(names(tb))) {
      ncName <- nchar(names(tb)[i])
      ncString <- max(nchar(tb[,i]))
      nc <- max(ncName, ncString)
      cw <- nc*0.89/10                 # assume 10 characters per an inch
      cws <- c(cws, cw)
    }
    return(cws)
  }
  
  
  #--- convert factor to character colname
  convert_fac_to_char <- function(dat) {
    for(i in 1:ncol(dat)) {
      if(class(dat[, i]) == "factor")
        dat[, i] <- as.character(dat[, i])
      
    }
    return(dat)
  }

  .convert <- function (x) 
  {
    x <- gsub("\\n", " \\\\line ", x)
    x <- gsub("<=", "\\\\u8804\\\\3", x)
    x <- gsub(">=", "\\\\u8805\\\\3", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&Alpha;", "\\\\u0913\\\\3", x)
    x <- gsub("&Beta;", "\\\\u0914\\\\3", x)
    x <- gsub("&Gamma;", "\\\\u0915\\\\3", x)
    x <- gsub("&Delta;", "\\\\u0916\\\\3", x)
    x <- gsub("&Epsilon;", "\\\\u0917\\\\3", x)
    x <- gsub("&Zeta;", "\\\\u0918\\\\3", x)
    x <- gsub("&Eta;", "\\\\u0919\\\\3", x)
    x <- gsub("&Theta;", "\\\\u0920\\\\3", x)
    x <- gsub("&Iota;", "\\\\u0921\\\\3", x)
    x <- gsub("&Kappa;", "\\\\u0922\\\\3", x)
    x <- gsub("&Lambda;", "\\\\u0923\\\\3", x)
    x <- gsub("&Mu;", "\\\\u0924\\\\3", x)
    x <- gsub("&Nu;", "\\\\u0925\\\\3", x)
    x <- gsub("&Xi;", "\\\\u0926\\\\3", x)
    x <- gsub("&Omicron;", "\\\\u0927\\\\3", x)
    x <- gsub("&Pi;", "\\\\u0928\\\\3", x)
    x <- gsub("&Rho;", "\\\\u0929\\\\3", x)
    x <- gsub("&Sigma;", "\\\\u0931\\\\3", x)
    x <- gsub("&Tau;", "\\\\u0932\\\\3", x)
    x <- gsub("&Upsilon;", "\\\\u0933\\\\3", x)
    x <- gsub("&Phi;", "\\\\u0934\\\\3", x)
    x <- gsub("&Chi;", "\\\\u0935\\\\3", x)
    x <- gsub("&Psi;", "\\\\u0936\\\\3", x)
    x <- gsub("&Omega;", "\\\\u0937\\\\3", x)
    x <- gsub("&alpha;", "\\\\u0945\\\\3", x)
    x <- gsub("&beta;", "\\\\u0946\\\\3", x)
    x <- gsub("&gamma;", "\\\\u0947\\\\3", x)
    x <- gsub("&delta;", "\\\\u0948\\\\3", x)
    x <- gsub("&epsilon;", "\\\\u0949\\\\3", x)
    x <- gsub("&zeta;", "\\\\u0950\\\\3", x)
    x <- gsub("&eta;", "\\\\u0951\\\\3", x)
    x <- gsub("&theta;", "\\\\u0952\\\\3", x)
    x <- gsub("&iota;", "\\\\u0953\\\\3", x)
    x <- gsub("&kappa;", "\\\\u0954\\\\3", x)
    x <- gsub("&lambda;", "\\\\u0955\\\\3", x)
    x <- gsub("&mu;", "\\\\u0956\\\\3", x)
    x <- gsub("&nu;", "\\\\u0957\\\\3", x)
    x <- gsub("&xi;", "\\\\u0958\\\\3", x)
    x <- gsub("&omicron;", "\\\\u0959\\\\3", x)
    x <- gsub("&pi;", "\\\\u0960\\\\3", x)
    x <- gsub("&rho;", "\\\\u0961\\\\3", x)
    x <- gsub("&sigmaf;", "\\\\u0962\\\\3", x)
    x <- gsub("&sigma;", "\\\\u0963\\\\3", x)
    x <- gsub("&tau;", "\\\\u0964\\\\3", x)
    x <- gsub("&upsilon;", "\\\\u0965\\\\3", x)
    x <- gsub("&phi;", "\\\\u0966\\\\3", x)
    x <- gsub("&chi;", "\\\\u0967\\\\3", x)
    x <- gsub("&psi;", "\\\\u0968\\\\3", x)
    x <- gsub("&omega;", "\\\\u0969\\\\3", x)
    x <- gsub("TRUE", "Yes", x)
    x <- gsub("FALSE", "No", x)
    x
  }
  
  
  .get.space.before.after <- function (space.before = NULL, space.after = NULL) 
  {
    ret <- ""
    if (!is.null(space.before)) {
      ret <- paste(ret, "\\sb", (space.before * 1440), sep = "")
    }
    if (!is.null(space.after)) {
      ret <- paste(ret, "\\sa", (space.after * 1440), sep = "")
    }
    ret
  }
  
   
  .add.table.row <- function (col.data = c("c1", "c2", "c3"), 
    col.widths = c(1, 4.5, 1), 
    col.justify = NULL, font.size = 10, last.row = FALSE, 
    indent = 0, border.top = FALSE, border.bottom = FALSE, space.before = NULL, 
    space.after = NULL) 
  {
    header <- paste("\\trowd\\trgaph100\\trleft", indent, sep = "")
    if (length(col.data) != length(col.widths)) {
      stop(paste("The number of data columns (", length(col.data), 
                 ") doesn't match the column widths (", length(col.widths), 
                 ")!  Input data: ", col.data, sep = ""))
    }
    justify <- vector()
    justify["L"] <- "\\ql"
    justify["R"] <- "\\qr"
    justify["C"] <- "\\qc"
    justify["J"] <- "\\qj"
    justify.v <- rep(justify["L"], length(col.data))
    numeric.cols <- which(!is.na(suppressWarnings(as.numeric(col.data))))
    if (length(numeric.cols) > 0) {
      justify.v[numeric.cols] <- justify["R"]
    }
    if (!is.null(col.justify)) {
      if (length(col.justify) == 1) {
        if (col.justify %in% names(justify)) {
          justify.v <- rep(justify[col.justify], length(col.data))
        }
        else {
          stop(paste("col.justify parameter not recognized: ", 
                     col.justify, " (should be L, R, C, or J)", 
                     sep = ""))
        }
      }
      else if (length(col.justify) == length(col.data)) {
        justify.v <- justify[col.justify]
      }
      else {
        stop(paste("The number of data columns (", length(col.data), 
                   ") doesn't match the col.justify (", length(col.justify), 
                   ") parameter!  Input data: ", paste(col.data, 
                                                       sep = "", collapse = ", "), sep = ""))
      }
    }
    btop <- ""
    bbottom <- ""
    if (any(border.top))  #  
      btop <- sapply(border.top, ifelse, "\\clbrdrt\\brdrs\\brdrw15",'') # make border.top as a vector option
    if (last.row == TRUE | any(border.bottom))  # 
      bbottom <- sapply(border.bottom, ifelse, "\\clbrdrb\\brdrs\\brdrw15",'') #make border.bottom as a vector option
    cols.prefix <- paste("\\clvertalt\\clshdrawnil\\clwWidth", 
                         round(col.widths * 1440, 0), "\\clftsWidth3\\clheight260\\clpadl100\\clpadr100\\gaph", 
                         btop, bbottom, "\\cellx", c(1:length(col.widths)), "\n", 
                         sep = "", collapse = "")
    cols <- paste("\\pard", justify.v, .get.space.before.after(space.before, 
                                                               space.after), "\\widctlpar\\intbl\\fi0\\f2\\fs", font.size * 
                    2, " ", .convert(col.data), "\\cell\n", sep = "", collapse = "")
    end.row <- "\\widctlpar\\intbl\\row\n"
    paste(header, cols.prefix, cols, end.row, sep = "")
  }
  
  # add width basing on duplication
  adj.width <- function(x, cw, space=1){
    ret <- array(0, dim=dim(x))
    x2 <- x
    #dup <- t(apply(x,1, function(x) duplicated(x)& x!='')) 
    dup <- t(apply(x,1, function(x) c(FALSE, x[-1]==x[-length(x)])))		
    for (i in 1:nrow(x)) {
      for (j in ncol(x):1) {
        if (i > 1 & j <= ncol(x)) if (!dup[i-1,j]) dup[i,j] <- FALSE
        if (dup[i,j]) {
          ret[i,j-1] <- ret[i,j]+space*cw[j]
          ret[i,j  ] <-         -space*cw[j]		
          x2[i,j] <- ''		
        }		
      }
      ret[i,] <- ret[i,]+cw
    }
    list(x=x2, cw=ret)		
  }
  
  
  ###--- This one is added/modified a little bit from the original rtf.tab that Michael Man developed
  # It expanded the width for header or footnote
  rtf.tab <- function(dat,cw=NULL,titles,col.just=NULL,var.ul,prd.status='QA',footns='',header=FALSE,footer=FALSE,
                      nline.body=40, addSpaceHeader=0, addSpaceFoot=0, page.disp=TRUE,...) { # almost automated w/ modified .add.table.row		
    st <- attributes(dat)$'start cell'[1]
    hd <- 1:(st-1)
    if(is.null(cw)) cw <- c(1.5,rep(.7,ncol(dat)-1))
    if(is.null(col.just)) col.just <- rep('R', nrow(dat)) 
    if(is.null(dim(col.just))) col.just <- matrix(rep(col.just,ncol(dat)), nrow=nrow(dat), byrow=FALSE)
    x <- dat
    cw2 <- matrix(rep(cw, nrow(x)), ncol=ncol(x), byrow=TRUE)
    if (st > 2) {
      #undebug(adj.width)
      y <- adj.width(x=dat[hd,],cw=cw, space=1)
      x[hd,] <- y$x
      cw2[hd,] <- y$cw		
    } 
    
    #expr=expression({
    rtf.add.row <- function(rows=1:nrow(x)){
      ret <- "{\\pard\n"
      for (i in rows) if (i <= nrow(x)){
        sel <- which(cw2[i,]>0)
        ret <- paste(ret, 
                     .add.table.row(
                       col.data =     x[i,sel], 
                       col.widths = cw2[i,sel], 
                       col.justify = col.just[i,sel],
                       font.size = fs,
                       border.top = i %in% c(1), 
                       border.bottom = if (i > st-2) i %in% c(st-1,nrow(x),max(rows)) else x[i,sel] %in% var.ul
                     ),
                     sep='') 	
        if (i < st) if (!all(cw2[i,] == cw2[i+1,])) ret <- paste(ret,'{\\pard\\par}','', sep='')
      }	
      ret <- paste(ret, "}\n\n", sep = "")
      rtf$.rtf <- paste(rtf$.rtf, ret, sep='')				
    }
    #})
    
    rtf <- RTF(...)
    fs <- rtf$.font.size
    rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)	# make "Courier New" as default
    rtf$.rtf <- gsub('field{\\fldinst{page}}', '', rtf$.rtf, fixed=TRUE)	# delete page number
    #nline.body <- 2
    npage <- ceiling((nrow(x)-st+1)/nline.body)
    #addHeader(rtf,title=titles[[1]],subtitle=titles[[2]])
    #systems <- c('Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}', 
    systems <- c(ifelse(page.disp==TRUE, paste('Page \\chpgn  of ', npage, sep=''), ""), 
                 format(Sys.time(), "%H:%M %d%b%Y"),
                 prd.status)
    #systems <- c(paste('Page \\chpgn  of ', npage, sep=''), 
    #             format(Sys.time(), "%H:%M %d%b%Y"),
    #             prd.status)
    hd.m <- c(paste(titles,  collapse='\\line '), 
              paste(systems, collapse='\\line '))  # main header									
    tmp <- .add.table.row(
      col.data =  hd.m, 
      col.widths = c(sum(cw)-2+addSpaceHeader,2), 
      col.justify = c('L', 'R'),
      font.size = fs)
    tmp2 <- .add.table.row(
      col.data =  paste(footns, collapse='\\line '), 
      col.widths = sum(cw)+addSpaceFoot, 
      col.justify = 'L',
      font.size = fs)							
    if (FALSE) rtf$.rtf <- paste(rtf$.rtf, "{\\header\\pard", "\\fi0\\f2\\fs", fs*2, 
                                 "\\qr Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}\\par}", sep='')
    for (i in 1:npage){						
      rtf$.rtf <- paste(rtf$.rtf, ifelse(i==1,'','\\page'), '{\\pard\\par}', 
                        ifelse(header,'{\\header',''), tmp, 
                        ifelse(header,'}',''), '{\\pard\\par}', sep='')					
      #startParagraph(rtf)
      #eval(expr)
      rtf.add.row(rows=c(1:(st-1), 1:nline.body-1+st+nline.body*(i-1)))
      #endParagraph(rtf)		
      rtf$.rtf <- paste(rtf$.rtf, '{\\pard\\par}', ifelse(footer,'\\footer',''), tmp2, sep='')
    }	
    done(rtf)		
    
  }#end rtf.tab


  #--- produce to rtf file that as SAS output. It can be used for single table or table with multiple pages
  rtf_table_out_as_sas <- function(rtf, tb, cw=NULL, 
                                   colFormat=c(rep("L", 1), rep("C",1), rep("R",1)),
                                   cell1=2, cell2=1, nheader=1,
                                   varName=NULL,
                                   var.ul='', prd.status=' ', #'QA',
                                   titles="", footns="", 
                                   header=FALSE, footer=FALSE,
                                   nline.body=30, height=8.5, width=11, omi=c(1,1,1,1),
                                   addSpaceHeader=0, addSpaceFoot=0, page.disp=TRUE,
                                   ...) {
                                   
    # rtf: RTF object that is just needed to be declared once before calling the function
    # tb: table out
    # cw: column width
    # colFormat: alignment (left, center, or right) for each column
    # varName: variable name that want to be repeated when go to the next page
    # var.ul: pre-specified cell values that indicate the bottom line in a table
    # nline.body: number of lines per page
    # addSpaceFoot, addSpaceHeader: expand the width of header or footnotes
    # page.disp: whether diplay page number
    
    #--- generate blank pape (break page) if the data is more than a page
    if(nrow(tb) > nline.body) {
      idx <- seq(1, nrow(tb), by = nline.body)
      idx.n <- c(seq(nline.body, nrow(tb), by = nline.body), nrow(tb))
    }else {
      idx <- 1
      idx.n <- nrow(tb)
    }
    
    npage <- length(idx)
    
    for(k in 1:length(idx)) {
      subTable <- tb[idx[k]:idx.n[k], ]
      if(is.null(nrow(subTable))){
        subTable <- data.frame(` `=subTable)
      }
      #--- if nrow < nline.body, need adding some spaces to move footnotes to bottom of the page
      if(nrow(subTable) < nline.body) {
        addData <- data.frame(matrix("",ncol=ncol(subTable), nrow=nline.body-nrow(subTable)-1))
        names(addData) <- names(subTable)
        subTable <- rbind(subTable, addData)  
      }
      
      #-- get values that will be in the 1st row of next page
      rownames(subTable) <- 1:nrow(subTable)
      
      #-- check if the last row of previous page with varName not matched the 1st row of next page
      if(k==1)
        string.val <- NULL
      if(k!=1 & !all(unique(subTable[1,varName]) %in% "") & !all(unique(subTable[1,varName]) %in% string.val))
        string.val <- unique(subTable[1,varName])
      
      if(k==1 & all(tb[nrow(subTable)+1, varName] %in% "")) {
        for(m in 1:nrow(subTable)) 
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName]
      } else if(!all(tb[nrow(subTable)+1, varName] %in% "")) {
        string.val <- NULL
      } else {   
        #-- add last row of previous page to 1st row of next page    
        if(!is.null(string.val))
          subTable[1, varName] <- string.val
        
        for(m in 1:nrow(subTable)) 
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName] 
      }
      
      hd <- sapply(colnames(subTable), function(x) strsplit(x, '; ')[[1]])
      ret <- rbind(hd, as.matrix(subTable))
      colnames(ret) <- 1:ncol(ret)            
      rownames(ret) <- 1:nrow(ret)
      
      ret2 <- ret
      
      colnames(ret2) <- 1:ncol(ret2)            
      rownames(ret2) <- 1:nrow(ret2)
      attributes(ret2) <- list(dim = dim(ret2), dimnames = dimnames(ret2), `start cell` = c(cell1, cell2))
      
      col.just <- matrix(c(rep(colFormat, nrow(ret2) + nheader)), ncol=ncol(ret2), byrow=TRUE)
      
      dat <- ret2
      st <- attributes(dat)$'start cell'[1]
      hd <- 1:(st-1)
      if(is.null(cw)) cw <- c(2.5, rep(1.5,ncol(dat)-1))
      x <- dat
      cw2 <- matrix(rep(cw, nrow(x)), ncol=ncol(x), byrow=TRUE)
      if (st > 2) {
        y <- adj.width(x=dat[hd,],cw=cw, space=1)
        x[hd,] <- y$x
        cw2[hd,] <- y$cw    
      } 
      
      rtf.add.row <- function(rows=1:nrow(x)){
        ret <- "{\\pard\n"
        for (i in rows) if (i <= nrow(x)){
          sel <- which(cw2[i,]>0)
          ret <- paste(ret, 
            .add.table.row(
              col.data =     x[i,sel], 
              col.widths = cw2[i,sel], 
              col.justify = col.just[i,sel],
              font.size = fs,
              border.top = i %in% c(1), 
              border.bottom = if (i > st-2) i %in% c(st-1,nrow(x),max(rows)) else x[i,sel] %in% var.ul
              #x is the subtable with header
            ),  sep='') 
          if (i < st) if (!all(cw2[i,] == cw2[i+1,])) ret <- paste(ret,'{\\pard\\par}','', sep='')
        }
        ret <- paste(ret, "}\n\n", sep = "")
        rtf$.rtf <- paste(rtf$.rtf, ret, sep='')
      }
      
      rtf <- rtf
      fs <- rtf$.font.size
      rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)	# make "Courier New" as default
      rtf$.rtf <- gsub('field{\\fldinst{page}}', '', rtf$.rtf, fixed=TRUE)	# delete page number
      
      tmp2 <- .add.table.row(
        col.data =  paste(footns, collapse='\\line '), 
        col.widths = sum(cw)+addSpaceFoot, 
        col.justify = 'L',
        font.size = fs)	
      rtf.add.row(rows=c(1:(st-1), 1:nline.body-1+st))	
      rtf$.rtf <- paste(rtf$.rtf, '{\\pard\\par}', ifelse(footer,'\\footer',''), tmp2, sep='')
    } #end of the for loop	
    return(rtf)
  }#end rtf_table_out_as_sas  
 

}





#-------------Manually Convert R object into HTML code-------------#
df2html<-function( #convert a table into part of HTML code
  datF, #a data.frame for table output
  tabTit="",  #table title
  fnote=NULL,  #footnote
  ind=1
){
  datF<-as.data.frame(datF)
  dim1<-dim(datF)
  nr<-dim1[1]
  nc<-dim1[2]
  cnm<-colnames(datF)
  rnm<-rownames(datF) #currently the rownames would not be printed out.  
  out<-paste0('<a name=\"IDX',ind,'\"></a>')
  out<-c(out, '<div align=\"left\">')
  tabSum2<-paste0('<table class=\"Table\" cellspacing=\"1\" cellpadding=\"',
    nc+2, '\" rules=\"groups\" frame=\"box\" border=\"1\" bordercolor=\"#000000\" ',
    'summary=\"', tabTit, '\" >')  #style="table-layout: fixed; width: 100%"
  out<-c(out, tabSum2, '<colgroup>', rep('<col>',nc), '</colgroup>', '<thead>')
  #For table header
  tabHead<-c('<tr>', paste0('<td class="l NoteContent" colspan=\"', nc,
    '\">', tabTit, '<br>', paste(rep("-",138),collapse=''), '</td>'), '</tr>', '<tr>')
  for(i in 1:nc){
    tabHead<-c(tabHead, paste0('<th class=\"c b Header\" scope=\"col\">',
      cnm[i], '</th>'))
  }
  out<-c(out, tabHead, '</tr>', '</thead>', '<tbody>')
  #for table body
  tabBody<-NULL
  for(j in 1:nr){
    tabBody<-c(tabBody, '<tr>')
    for(i in 1:nc){
      tabBody<-c(tabBody, paste0('<td class=\"l Data\">', datF[j,i], '</td>'))
    }
    tabBody<-c(tabBody, '</tr>')
  }
  out<-c(out, tabBody, '</tbody>', '<tfoot>', '<tr>')
  out<-c(out, paste0('<td class=\"l NoteContent\" colspan=\"',nc,'\">',
    paste(rep("-",138),collapse=''), '<br>', fnote, '</td>'), '</tr>', '</tfoot>')
  out<-c(out, '</table>', '</div>', 
    '<p style="page-break-after: always;"><br></p><hr size="3">')
  return(out)  
}

fig2html<-function( #convert a png figure into part of HTML code
  figNm='plot.png', # the png file name
  figTit="", #figure title
  figH="480", figW="640",
  fnote=NULL,  #footnote
  ind=1
){
  out<-paste0('<a name=\"IDX',ind,'\"></a>')
  figTit1<-c('<div align=\"left\">', 
    paste0('<table class=\"Table\" cellspacing=\"1\" cellpadding=\"1',
      '\" rules=\"groups\" frame=\"box\" border=\"1\" bordercolor=\"#000000\" ',
      'summary=\"', figTit, '\" >'), '<colgroup>','<col>', '</colgroup>',
      '<thead>', '<tr>' ) 
  out<-c(out, figTit1, paste0('<td class=\"l NoteContent\" colspan=\"1\">', 
    figTit,  '</td>'), '</tr>', '</thead>', '<tbody>', '<tr>' )
  out<-c(out, '<td>', paste0('<img alt=\" \" src=\"', figNm,
      '\" style=\" height: ', figH, 'px; width: ', figW, 
      'px;\"  border=\"0\">'), '</td>', '</tr>', '</tbody>','<tfoot>','<tr>')
  out<-c(out, paste0('<td class=\"l NoteContent\" colspan=\"1\">',fnote,
    '</td>'), '</tr>', '</tfoot>')
  out<-c(out, '</table>', '</div>',
    '<p style="page-break-after: always;"><br></p><hr size="3">')
  return(out)  

}

cLink<-function( #generate a link in content table
  tit="tmplink", ind=1
){
  out<-'<li class=\"ContentItem\">'
  out<-c(out, '<span><b>&#183;</b><a href=\"',
    paste0('body.htm#IDX', ind,'\" target=\"body\">',tit,'</a>'),
    '</span><br></li>')
  return(out)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#revise a html table code
#make it into multi-level header
  toMultH <- function(
    tbc, # a vector of html table code
    split1=";"                
  ){
    wh.h <- grep('<th>', tbc)
    if(length(wh.h)>1){return(tbc)}
    tbc.h <- tbc[1:(wh.h-1)]
    if(wh.h==length(tbc)){return(tbc)}
    tbc.b <- tbc[(wh.h+1):length(tbc)]
    hh <- tbc[wh.h]
    hh <- gsub("<th>", "", hh, fixed=TRUE)
    hh <- gsub("<tr>", "", hh, fixed=TRUE)
    hh <- gsub("</tr>", "", hh, fixed=TRUE)
    hh <- strsplit(hh, split="</th>")[[1]]
    hh <- hh[-length(hh)] #remove the last empty element
    hh <- strsplit(hh, split=split1, fixed=TRUE)
    
    hh.l <- sapply(hh, length)
    short <- hh.l<max(hh.l)
    hh[short] <- lapply(hh[short], function(x){
      c(x, rep(" ", max(hh.l)-length(x)))})

    hh3 <-matrix(unlist(hh), nrow=max(hh.l))
    hh3 <- apply(hh3, 1, function(x){
      r1 <- "<tr> <th colspan=\'"
      colspan <- 1
      if(length(x)>1){
        for(i in 1:(length(x)-1)){#requre ncol for the table is 2+
          if( gsub(" ", "", x[i])==gsub(" ", "", x[i+1])){
            colspan <- colspan + 1
          }else {
            r1 <- paste(r1, colspan, "\'>", x[i], "</th> <th colspan=\'")
            colspan <- 1
          }
        }
      }
      r1 <- paste(r1, colspan, "\'>", x[length(x)], "</th> </tr>'")
      return(r1)
      #<th colspan='2'>      
    })
    return(c(tbc.h, hh3, tbc.b))
    
  }



#----------UI Functions------------------#


#select color function
selectInput_color <- function(id='wpColor', 
                              label='Background color', 
                              choices=colors(), 
                              selected='azure3',
                              HEX=HEXColors){
  code <- paste0("<div class=\"form-group shiny-input-container\">\n",
                 "<label class=\"control-label\" for=\"",
                 id, "\">", label, ":</label>\n<div>\n",
                 "<select id=\"", id, "\"><option value=\"",
                 selected, "\" selected>", selected, "</option>\n")
  for(i in 1:length(choices)){
    code <- paste0(code, 
                   "<option value=\"",
                   choices[i], "\" style=\"background-color: ",
                   HEX[choices[i]], "\">", choices[i], "</option>\n")
  }
  code <- paste0(code, "</select>",
                 "<script type=\"application/json\" data-for=\"",
                 id, "\" data-nonempty=\"\">{}</script> \n</div>\n</div> ")

  return(code)
}










#Function to create Table with checkboxes

checkboxTableInput <- function(df,table.id="checktable",id.name, 
  checked, check.name,labels=NULL, showSelectedOnly=FALSE
){
  table_head0<-capture.output(print(xtable(df[0,]), type="html", 
    include.rownames=F,include.colnames=T))
  table_head0[3]<-"<table border=1 class=\"table table-bordered\">"
  table_head <- paste(table_head0, collapse=" ")
  table_head <- gsub("</TABLE>", "", table_head)
  table_head <- gsub("</table>", "", table_head)
  table_head <- gsub("<table", paste0("<table id=\"",table.id,"\""), table_head)
  table_head<- gsub('<TR>','<tr>',table_head)
  table_head<- gsub('</TR>','</tr>',table_head)
  table_head <- gsub("<tr>", paste("<tr> <th> ", check.name, " </th>", sep=""), 
    table_head)
  table_body <- paste(capture.output(print(xtable(df), type="html", 
    content.only=T, include.rownames=F, include.colnames=F)), collapse=" ")
  table_body<- gsub('<TR>','<tr>',table_body)
  table_body<- gsub('</TR>','</tr>',table_body)
  table_body<- gsub('<TD>','<td>',table_body)
  table_body<- gsub('</TD>','</td>',table_body)
  table_body<- gsub('</tr>','',table_body)
  table_body<- gsub('</table>','',table_body)
  which.tag <- regexpr("<tr>", table_body)[1]
  table_body <- substr(table_body, which.tag, nchar(table_body))
  table_body2 <- unlist(strsplit(table_body, "<tr>", fixed=T))
# print(table_body2)
  table_body3 <- '' #table_body2[1]

  if(showSelectedOnly){
    for (i in 1:nrow(df)){
      if(checked[i]){
        check_tag <- paste("<tr>  <td align=left>", 
          as.character(checkboxInput(paste0(id.name,'_',i), "", checked[i])),
          " </td>", sep="")
        table_body3<-paste(table_body3, check_tag, table_body2[i+1], "</tr>",  sep="")
      }
    }
  }else{
    for (i in 1:nrow(df)){
      check_tag <- paste("<tr>  <TD align=left>", 
        as.character(checkboxInput(paste0(id.name,'_',i), "", checked[i])),
        " </td>", sep="")
      table_body3<-paste(table_body3, check_tag, table_body2[i+1], "</tr>", sep="")
    }
  }
  table_out <- paste(table_head, table_body3, "</table>", collapse=" ")
  if (!is.null(labels)){
    for (k in 1:length(labels)){
      table_out<-gsub(paste0('<th> ',colnames(df)[k],' </th>'),
        paste0('<th> ',labels[k],' </th>'), table_out)
    }
  }
  # print(table_out)

  return(table_out)
}

#-------------Create Tabs-------------#
tabs<-function(tab.id,tab.label,tab.value){
  tabset<-unique(cbind(tab.label,tab.value))
  tabcode1<-paste0('<div class=\"tabbable tabs-above\">\n',
    '<ul class=\"nav nav-tabs shiny-tab-input\" id=\"',
    tab.id,'\">\n<li class=\"active\">\n')
  tabcode2<-paste0('<a href=\"#tab-',tab.id,'-',1:nrow(tabset),
    '\" data-toggle=\"tab\" data-value=\"',
    tabset[,1],"\">",tabset[,2],"</a>\n</li>\n<li>\n")
  tabcode2[length(tabcode2)]<-gsub('<li>','</ul>',tabcode2[length(tabcode2)])
  tabcode3<-"<div class=\"tab-content\">"
  tabcode4<-paste0('<div class=\"tab-pane active\" data-value=\"',
    tabset[,1],'\" id=\"tab-',tab.id,'-',1:nrow(tabset),
    '\"></div>\n',collapse=" ")
  tabcode5<-'</div>\n</div>'
  tabcode<-paste0(tabcode1,paste0(tabcode2,collapse=" "),
    tabcode3,tabcode4,tabcode5,collapse=" ")  
  return(tabcode)
}

#-------------side panel data input-------------#
inlinetext<-function(text.id,text.label,text.value, note=NULL){
  if(is.null(note)){
    nt <- ""
  }else {
    nt <- paste0('<h6 style=\"width:50%\">', note, ' </h6>')
  }
  if(FALSE){ #using textInput widget
    code <- paste0(
      "<div class = \"form-group shiny-input-container\", style = \"width:100%\">",
      paste0("<label for=\"", text.id, "\">", text.label, "</label>"),
      paste0("<input id=\"", text.id, "\" type=\"text\" class=\"form-control\" value=\"", text.value, "\"/>"),
      "</div>",
      collapse='\n'
    )
  } else { #using traditional widget
    code<-paste0( '<div class=\"input-prepend\" style=\"width:100%\">',  
      paste0('<span class=\"add-on\" >',text.label,'</span>'),
      paste0('<input style=\"width:50%\" id=\"',text.id,
        '\" class=\"form-control\" value=\"',text.value,'\" type=\"text\" />\n'),
      nt,
      '</div>',
      collapse='\n')
  }
  return(code)
}

#-------------For output R code-------------#
runcode<-function(codelist, # merged dataset of loa and configuration file
                  datPath, # dataset path from input
                  outPath, # output path from input
                  libs,
                  params1,    #a vector of widgets name (require ordered)
                  paramLabs1, #a vector of widgets label(1:1 matched to params)
                  sourceCode, # source files from configuration file
                  subCode,
                  title0, # Main title with study id and tumor type
                  devpath,
                  outBcode=NULL
){
  
  codelist<-codelist[!(!is.na(codelist[,paramLabs1[1]]) & is.na(codelist[,params1[1]])),]
  sourceCode<-unique(sourceCode[!is.na(sourceCode)])
  sourceCode<-sourceCode[!grepl("rcode.r",sourceCode)]
  head1<-sop_head  #paste0('#/*soh',paste0(rep('*',73),collapse=''))
  nParam<-length(params1)

  if(FALSE){
  head2<-'if(FALSE){ # DESCRIPTION: Parameters for generating following results:'
  tmploa<-unique(codelist[,c('Request.Name',params1,paramLabs1)])
  tmploa2<-sapply(1:nrow(tmploa),function(y){
    paste0(tmploa$Request.Name[y],' ',
           ifelse(is.na(tmploa[y,paramLabs1[1]]),
                  '',
                  paste0('(',paste0(sapply(which(!is.na(tmploa[y,paramLabs1])),function(x){
                    paste0(tmploa[y,paramLabs1[x]],'=',
                           ifelse(is.na(tmploa[y,params1[x]]),
                                        'Parameters Needed',
                                        tmploa[y,params1[x]]))
                    }),collapse=';'),') \n')
                  )
           )})
  tmploa2<-gsub(";", ";\n    #", tmploa2)
  head3<-paste0('  #',1:length(tmploa2),'. ',tmploa2)  
  
  head4<-paste0('\n}\n#', paste0(rep("-",90),collapse='')) #end of DESCRIPTION
  } else {
     head2<-''
     head3<-''
     head4<-''
  }
  head5<-''# paste0('  #', gsub('\n', '\n  #  ', libs))
  head6<-'\nif(TRUE){ #------Read in CSV Dataset------'
  head7<-paste0( '  indataset<-list() \n',
    '  for(i in 1:length(inf)){ \n',
    '    indataset[[i]]<-read.csv_sas(inf[i],stringsAsFactors=FALSE,header=TRUE, encoding=inf.enc[i], comment.char=\"\") \n',
    '  } \n ',
    '  names(indataset)<-inf  \n',
    '  indataset.i<-indataset[[1]]  ')
  head8<-paste0('  ', subCode)
  head6.1<-'\n}\nif(TRUE){ #------Read in one RData------'
  head7.1<-paste0(
      'load(inf.R, indataR<-new.env())\n',
      'indataR<<-as.list(indataR)\n' )
  head9<-'\n}\n\nif (TRUE){ #------Source Packages and Function Files------'
  #connect default folder for reusable functions
  head10<-paste0(
    '  if(FALSE){ #Please firstly check whether all the required pacakges are installed.\n    ',
    '#install.packages(c(...))\n    ', 
    gsub('\n', '\n    ', libs),
    '\n  }',
    '\n  require(rtf) \n  require(haven) \n',
    '\n  read.csv_sas<<-function(file, header=TRUE, nrow=-1, comment.char="", ...){',
    '\n   is.sas<-grepl(\'.sas7bdat\', file, fixed=TRUE)',
    '\n   is.csv<-grepl(\'.csv\', file, fixed=TRUE)',
    '\n   if(is.sas){',
    '\n     ot<-haven::read_sas(file)',
    '\n     if(header & nrow==1){ot<-names(ot)}',
    '\n   }else if (is.csv){',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE, comment.char=comment.char, ...))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE,fileEncoding=\"latin1\", comment.char=comment.char))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-read.csv(file=file, h=header, nrow=nrow, na=c(\'NA\', \'\', \'.\'), stringsAsFactors=FALSE,fileEncoding=\"UTF-8\", comment.char=comment.char))',
    '\n     if(class(tm)==\'try-error\')',
    '\n     tm<-try(ot<-readLines(con=file))     ',
    '\n   }else{',
    '\n   ot<-NULL\n}',
    '\n   return(ot) \n  }',   
    "\n   try(source(SourceFile)) \n ",
    "\n  ", ifelse(is.null(eval(parse(text=outBcode))), " ", outBcode),
    "\n}")
  head11<-'\n\nif(OutputRTF){  #----- Generate RTF file ------'
  head12<-paste0('\n  pageW<-11 \n  pageH<-8.5 \n  figW<-9 \n  figH<-6 \n',
    '\n  rtf<-RTF(rtfFileName,width=pageW,height=pageH, font.size=8,omi=c(.5,.5,.5,.5))')
  r_out<-c(head1,head2,head3,head4,head5, head9,head10,head6,head7,head8,head6.1, head7.1, head11,head12)
  for (i in 1:nrow(codelist)){
    for(x in nParam:1){
      if(!is.na(codelist[i,params1[x]])){
        tmp0<-unlist(strsplit(as.character(codelist[i,params1[x]]),
	  ',',fixed=TRUE))
        tmp00 <- try( as.numeric(tmp0) )
        if( class(tmp00)[1]=='try-error' || any(is.na(as.numeric(tmp0))) )
            say1 <- TRUE
        else
          say1 <-FALSE
        tmp<-ifelse(say1,
	  paste0('c(\"',paste0(tmp0,collapse='\",\"'),'\")'),
          paste0('c(',paste0(codelist[i,params1[x]],collapse=','),')'))
        codelist$Title[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$Title[i],fixed=TRUE)
        codelist$PlotCode[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$PlotCode[i],fixed=TRUE)
        codelist$FootCode[i]<- gsub(paste0('input$',params1[x]),
	  tmp,codelist$FootCode[i],fixed=TRUE)
      }
    }
  
    #Add Header
    title<-codelist$Title[i]

    if(is.na(title) || length(title)<1) title <- " "
    if(grepl(title, 'paste', fixed=TRUE)) title<-eval(parse(text=title))
    addhead<-vector()
    addhead[1]<-paste0('\n\n  #------Add Header  ',codelist$order[i],' ------')
    addhead[2]<-paste0('  addHeader(rtf,title=\"',title0,'\",font.size=10)')
    addhead[3]<-paste0('  addHeader(rtf,title=\"',title,'\",font.size=10)')
    r_out<-c(r_out,addhead)
    
    #Add Plot
    if( (!is.na(codelist$Type.y[i]) & codelist$Type.y[i]=='Figure') || 
        (!is.na(codelist$Type.x[i]) & codelist$Type.x[i]=='Figure')  ){
      addplot<-vector()
      addplot[1]<-paste0('  #------Add Plot: ',codelist$Request.Name[i],' ------')
      addplot[2]<-paste0('  tmp<-',codelist$tmp[i])
      addplot[3]<-paste0('  pngtmp<-function(){',codelist$PlotCode[i],'}')
      addplot[4]<-paste0('  addPlot(rtf,pngtmp,width=',codelist$width[i]/160,',height=',codelist$height[i]/160,')')  
      r_out<-c(r_out,addplot)}
    
    #Add Table            
    if(  (!is.na(codelist$Type.y[i]) & codelist$Type.y[i]=='Table') || 
         (!is.na(codelist$Type.x[i]) & codelist$Type.x[i]=='Table')  ){
      addtab<-vector()
      addtab[1]<-paste0('  #------Add Table: ',codelist$Request.Name[i],' ------')
      addtab[2]<-paste0('  tmp<-',codelist$tmp[i])
      addtab[3]<-paste0('  outTable<-{',codelist$PlotCode[i],'}')
      addtab[4]<-paste0('  tWidth<-9\nnc<-nchar(as.character(outTable[,1]))\nc1W<-pmax(max(nc)*0.079/2, 20*0.079)\n',
                        '  caW<-nc*0.079\nlcW<-.711\nothW<-(tWidth-c1W-lcW)/(ncol(outTable)-2)\n',
                        '  centW<-rep(othW,ncol(outTable)-2)\ncWs<-c(c1W,centW,lcW)'
                        )
      addtab[5]<-paste0( '  addTable(rtf,outTable,col.widths=cWs,col.justify=rep(\'L\',length(outTable)))'
                       )
      r_out<-c(r_out,addtab)
    }
    
    #Add Footnote
    footnote<-codelist$FootCode[i]
    footnote<-gsub("\\\\n","\n",footnote)
    if(!is.na(footnote)&&gsub("", " ", footnote)!=''&&substr(footnote,1,5)=='paste')
      footnote<-eval(parse(text=footnote))
    addfoot<-vector()
    addfoot[1]<-'  addNewLine(rtf, n=2)'
    addfoot[2]<-paste0('  #------Add Footnote  ',codelist$order[i],' ------')
    addfoot[3]<-paste0('  addParagraph(rtf,\"',footnote,'\")')
    r_out<-c(r_out,addfoot)
    
    if(i!=nrow(codelist)){
      r_out<-c(r_out,'  addPageBreak(rtf,width=11,height=8.5,font.size=8,omi=c(.5,.5,.5,.5))')
    }else{
      r_out<-c(r_out,'\ndone(rtf)\n}  #------End of RTF------\n')
    }
  }
  return(paste0(r_out,collapse='\n'))
}

  checkSource<-function(fnm, keyS1='source('){#fnm must be a file under inpath2
    rF1<-NULL
    r1<-readLines(con=fnm)
    r2<-toupper(gsub(" ", '', r1))
    falseKey<-FALSE
    for(j in 1:length(r1)){
      if(grepl('IF(FALSE)', r2[j], fixed=TRUE)|grepl('IF(F)', r2[j], fixed=TRUE)){
        falseKey<-TRUE
        b1<-0
      }
      if(falseKey){
        if(gsub("IF(FALSE)","", r2[j],fixed=TRUE)=="[{]"|substr(r2[j],1,1)=="[{]") b1<-b1+1
        if(b1==0 & grepl("[{]", r2[j]))
          falseKey<-FALSE
        if(grepl("}", r2[j])) {
          b1<-b1-1
          if(b1==0) falseKey<-FALSE
        }
      }else{
        if(grepl(toupper(keyS1), r2[j], fixed=TRUE)){
          f3<-strsplit(r1[j],split='local.path1,', fixed=TRUE)[[1]][2]
          f3<-gsub("users/","",f3, fixed=TRUE)
          f3<-gsub("'","",f3, fixed=TRUE)
          f3<-gsub('"','',f3, fixed=TRUE)
          f3<-gsub('\"','',f3, fixed=TRUE)
          f3<-gsub(')','',f3, fixed=TRUE)
          f3<-gsub('(','',f3, fixed=TRUE)
          f3<-gsub(' ','',f3, fixed=TRUE)
          if(grepl(".r", tolower(f3), fixed=TRUE)){
            rF1<-c(rF1, f3)
          }
        }
      }
    }
    return(rF1)
  }




#-------------Creat Parameters-------------#
#use radiobutton, slidebar, checkbox, selectInput
widgets.code<-function(UInames, max.n){
  ind<-max.n!=0

  check.code<-lapply(max.n['check']:1,function(x){  
    ret<-'\n output$checkbox_xx<-renderUI({
    tmpN<-reactive(which(check.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if(is.na(check.param()[tmpN(),\'check.label_xx\'])
    ||class(try(default<-eval(parse(text=check.param()[tmpN(),\'check.default_xx\']))))==\"try-error\"
    ||class(try(choice<-eval(parse(text=check.param()[tmpN(),\'check.choice_xx\']))))==\"try-error\")return(NULL)
    if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'checkbox Options Are Not Available\')))
    if (is.na(default))default<-choice
    checkboxGroupInput(inputId=\'check_xx\',label=strong(check.param()[tmpN(),\'check.label_xx\']),choices=choice,selected=default)})\n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  check.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),check.code)
  
  radio.code<-lapply(max.n['radio']:1,function(x){
    ret<-'output$radiobutton_xx<-renderUI({
     tmpN<-reactive(which(radio.param()$Request.Name==input$analysis))
     if(length(tmpN())<1) return(NULL)
     try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
     if(is.na(radio.param()[tmpN(),\'radio.label_xx\'])
     ||class(try(default<-eval(parse(text=radio.param()[tmpN(),\'radio.default_xx\']))))==\"try-error\"
     ||class(try(choice<-eval(parse(text=radio.param()[tmpN(),\'radio.choice_xx\']))))==\"try-error\")return(NULL)
     if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'radiobutton Options Are Not Available\')))
     if (is.na(default))default<-choice[1]
    radioButtons(\'radio_xx\',label=strong(radio.param()[tmpN(),\'radio.label_xx\']),choices=choice,selected=default)})\n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  radio.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),radio.code)
  
  dropdown.code<-lapply(max.n['dropdown']:1,function(x){
    ret<-'output$dropdown_xx<-renderUI({   
    isolate({
      tmpN<-reactive(which(dropdown.param()$Request.Name==input$analysis))
      if(length(tmpN())<1) return(NULL)
      try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))   
    })
    if(is.null(dropdown.param()[tmpN(),\'dropdown.label_xx\'])) return(NULL)
    if (is.na(dropdown.param()[tmpN(),\'dropdown.label_xx\'])  
    ||class(try(default<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.default_xx\']))))==\"try-error\"
    ||class(try(default2<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.multiple_xx\']))))==\"try-error\"
    ||class(try(choice<-eval(parse(text=dropdown.param()[tmpN(),\'dropdown.choice_xx\']))))==\"try-error\")return(NULL)
    if (length(choice)==0)return(div(class=\"alert alert-error\",strong(\'dropdown Options Are Not Available\')))
    if (all(is.na(default)))default<-choice[1]
    if (all(is.na(default2)))default2<-TRUE
    selectInput(\'dropdown_xx\',label=strong(dropdown.param()[tmpN(),\'dropdown.label_xx\']),
      choices=choice,selected=default,multiple=default2,selectize=TRUE, width=\"100%\") }) \n\n  '
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })
  dropdown.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),dropdown.code)
     
  slide.code<-lapply(max.n['slide']:1,function(x){
    ret<-'output$slider_xx<-renderUI({
    tmpN<-reactive(which(slide.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if(is.null(slide.param()[tmpN(),\'slide.label_xx\'])) return(NULL)
    if (is.na(slide.param()[tmpN(),\'slide.label_xx\'])  
    ||class(try(min<-eval(parse(text=slide.param()[tmpN(),\'slide.min_xx\']))))[1]==\"try-error\"
    ||class(try(max<-eval(parse(text=slide.param()[tmpN(),\'slide.max_xx\']))))[1]==\"try-error\"
    ||class(try(default<-eval(parse(text=slide.param()[tmpN(),\'slide.value_xx\']))))[1]==\"try-error\"
    ||class(try(by<-eval(parse(text=slide.param()[tmpN(),\'slide.by_xx\']))))[1]==\"try-error\")return(NULL)
    if (length(c(min,max,default,by))<4)return(div(class=\"alert alert-error\",strong(\'slider Options Are Not Available\')))
    if (is.na(default))default<-mean(min,max)
    sliderInput(\'slide_xx\',label=strong(slide.param()[tmpN(),\'slide.label_xx\']),min=min,max=max,value=default,step=by,animate = TRUE)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
    })  
  slide.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),slide.code)
  
  date.code<-lapply(max.n['date']:1,function(x){
    ret<-'output$date_xx<-renderUI({
    tmpN<-reactive(which(date.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(date.param()[tmpN(),\'date.label_xx\'])  
    ||class(try(default<-eval(parse(text="(date.param()[tmpN(),\'date.value_xx\'])"))))==\"try-error\")return(NULL)
    if (!is.null(default)&is.na(default)) return(div(class=\"alert alert-error\",strong(\'date Options Are Not Available\')))
    dateInput(\'date_xx\',label=strong(date.param()[tmpN(),\'date.label_xx\']),
      value=default, format = \"yyyy-mm-dd\")})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  date.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),date.code)

  dateR.code<-lapply(max.n['dateR']:1,function(x){
    ret<-'output$dateR_xx<-renderUI({
    tmpN<-reactive(which(dateR.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(dateR.param()[tmpN(),\'dateR.label_xx\'])  
    ||class(try(min<-eval(parse(text="(dateR.param()[tmpN(),\'dateR.start_xx\'])"))))==\"try-error\"
    ||class(try(max<-eval(parse(text="(dateR.param()[tmpN(),\'dateR.end_xx\'])"))))==\"try-error\") return(NULL)
    if ((!is.null(min)&is.null(max))&(is.na(min)&is.na(max)))
      return(div(class=\"alert alert-error\",strong(\'data range Options Are Not Available\')))
    dateRangeInput(\'dateR_xx\',label=strong(dateR.param()[tmpN(),\'dateR.label_xx\']),
       start=min, end=max, format = \"yyyy-mm-dd\")})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  dateR.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),dateR.code)

  num.code<-lapply(max.n['num']:1,function(x){
    ret<-'output$num_xx<-renderUI({
    tmpN<-reactive(which(num.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(num.param()[tmpN(),\'num.label_xx\'])  
    ||class(try(default<-eval(parse(text="num.param()[tmpN(),\'num.value_xx\']"))))==\"try-error\")return(NULL)
    if (is.na(default)) return(div(class=\"alert alert-error\",strong(\'numericInput Options Are Not Available\')))
    numericInput(\'num_xx\',label=strong(num.param()[tmpN(),\'num.label_xx\']),
      value=default)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  num.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),num.code)

  text.code<-lapply(max.n['text']:1,function(x){
    ret<-'output$text_xx<-renderUI({
    tmpN<-reactive(which(text.param()$Request.Name==input$analysis))
    if(length(tmpN())<1) return(NULL)
    try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
    if (is.na(text.param()[tmpN(),\'text.label_xx\'])  
    ||class(try(default<-eval(parse(text=text.param()[tmpN(),\'text.value_xx\']))))==\"try-error\")return(NULL)
    if (is.na(default)) return(div(class=\"alert alert-error\",strong(\'textInput Options Are Not Available\')))
    textInput(\'text_xx\',label=strong(text.param()[tmpN(),\'text.label_xx\']),
      value=default)})\n\n'
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  text.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),text.code)
  
  textbox.code<-lapply(max.n['textbox']:1,function(x){
    ret<-paste0('output$textbox_xx<-renderUI({
     tmpN<-reactive(which(textbox.param()$Request.Name==input$analysis))
     try(tmp<-eval(parse(text=Vdic()$tmp[AnalyC()])))
     if(is.na(textbox.param()[tmpN(),\'textbox.label_xx\']))return(NULL)
     cols<-eval(parse(text=textbox.param()[tmpN(),\'textbox.cols_xx\']))
     rows<-eval(parse(text=textbox.param()[tmpN(),\'textbox.rows_xx\']))
     default<-textbox.param()[tmpN(),\'textbox.default_xx\']
     label<-textbox.param()[tmpN(),\'textbox.label_xx\']
     if(is.na(cols)||is.null(cols))cols<-40
     if(is.na(rows)||is.null(rows))rows<-2
     texttmp<-paste0(\'<h6>\',label,\'</h6>\n<textarea name=\"\',textbox_xx,\'\" 
     rows=\"\',rows,\'\"  cols=\"\',cols,\'\">\',default,\'</textarea>\')
     return(shiny::HTML(texttmp))
     })\n\n')
    if (x==1){ret<-gsub('_xx','',ret,fixed=TRUE)}else{ret<-gsub('_xx',x,ret,fixed=TRUE)}
    ret
  })  
  textbox.codes<-Reduce(function(x,y)paste0(x,y,collapse='\n'),textbox.code)
  
     
  widgets.code<-paste0(c(check.codes,radio.codes,dropdown.codes,slide.codes,
    date.codes, dateR.codes, num.codes, text.codes, textbox.codes)[ind],
    collapse='\n\n')
  return(widgets.code)
}

widgets.order<<-function(analysis, UIdic1, UInames, ncol.widg=NULL){
#  print(UIdic1)
#  print(UInames)
#  print(analysis)
  
  if(is.null(ncol.widg)){ncol.widg<-1}
  input.names<-as.vector(t(UIdic1[UIdic1$Request.Name==analysis, UInames]))
  if(length(input.names)==0){
    widg.list=NULL
  }else{
    input.labs<-as.vector(t(UIdic1[UIdic1$Request.Name==analysis,
      gsub("uiInput", "uilab", UInames, fixed=TRUE)]))
    #names(input.names)<-1:length(input.names)
    s_1<-!is.na(input.names)
    input.names<-input.names[s_1]#unique(setdiff(as.vector(input.names),NA))
    input.labs<-input.labs[s_1]
    input.labs[is.na(input.labs)]<-""

    widg.list<-unlist(strsplit(input.names,'input$',fixed=TRUE))
    sel0<-widg.list!='' & !is.na(widg.list)
    input.names<-widg.list<-widg.list[sel0]
    #input.labs<-input.labs[sel0]

    widg.list<-gsub('check','checkbox',widg.list)
    widg.list<-gsub('radio','radiobutton',widg.list)
    widg.list<-gsub('dropdown','dropdown',widg.list)
    widg.list<-gsub('slide','slider',widg.list)
   # widg.list<-gsub('date','date',widg.list)
   # widg.list<-gsub('dateR','dateR',widg.list)
   # widg.list<-gsub('num','num',widg.list)
   # widg.list<-gsub('text','text',widg.list)
   # widg.list<-gsub('textbox','textbox',widg.list)
  }
  if(length(widg.list)==0){
    widg.code<-'return(NULL)';
    input.names<-input.labs<-NA
  }else{
    if(F){
    widg.code<-paste0('uiOutput(\'',widg.list,'\')',collapse=',')
    widg.code<-paste0('return(conditionalPanel(condition=\'true\', div(',
      widg.code,')))',collapse='')
    }else{
    widg.code<-paste0('uiOutput(\'',widg.list,'\')') #,collapse=','
    w.widg<-floor(12/ncol.widg)    #upto 4, column width
    nrow.widg<-ceiling(length(widg.code)/ncol.widg)#number of rows
    widg.code2<-NULL
    for(i in 1:nrow.widg){
      widg.code3<-NULL
      for(j in 1:ncol.widg){
        ij<-(i-1)*ncol.widg+j  
        if(ij <= length(widg.code)){
          wij<-paste0("column(", w.widg,", ", widg.code[ij], 
              ", offset=", 0, ")") #max(w.widg*(j-1)-2,0)
          widg.code3<-paste0(c(widg.code3, wij), collapse=",")
        }
      }
      widg.code2<-c(widg.code2, paste0("fluidRow(", widg.code3, ")"))
    }
    widg.code<-paste0(widg.code2, collapse=',')
    widg.code<-paste0('return(conditionalPanel(condition=\'true\', div(',
      widg.code,')))',collapse='')
    }
  }
  out1<-data.frame(code=widg.code,names=input.names,labs=input.labs)

  return(out1)
}

widgets.param<-function(uidic, wid, max.n){
  if(!wid %in% names(max.n)){
    return(NA)
  }else{
    temp<-lapply(setdiff(unique(uidic$Request.Name),NA),function(x){
      row_sel<-uidic$Request.Name==x
      ret<-unique(uidic[which(row_sel), grepl(wid,colnames(uidic))])
      ret$Request.Name<-x
      #dfnames is the widget name which default values not specified in CD
      wid2<-wid[!wid%in%c("num","text")]
      if(length(wid2)>0){
        dfnames<-c(paste0(wid2,'.default',c(max.n[wid2]:2, '')))
        dfnames<-dfnames[!dfnames%in%colnames(uidic)] 
        if (length(dfnames)>0){
          dfs<-matrix(NA,nrow=nrow(ret),ncol=length(dfnames))
          colnames(dfs)<-dfnames
          ret2<-data.frame(ret,dfs,row.names=NULL)
        }else{ ret2<-data.frame(ret,row.names=NULL)}
      }else{ ret2<-data.frame(ret,row.names=NULL)}
      #dfnames3 is the dropdown widget name which multiple not specified in CD
      wid3<-wid2[grepl("dropdown",wid2)]
      if(length(wid3)>0){
        dfnames3<-c(paste0(wid3,'.multiple',c(max.n[wid3]:2, '')))
        dfnames3<-dfnames3[!dfnames3%in%colnames(uidic)]
        if (length(dfnames3)>0){
          dfs<-matrix(NA,nrow=nrow(ret2),ncol=length(dfnames3))
          colnames(dfs)<-dfnames3
          ret3<-data.frame(ret2,dfs, row.names=NULL)
        }else{ ret3<-data.frame(ret2,row.names=NULL) }
      }else{ ret3<-data.frame(ret2,row.names=NULL) }
      return(ret3)
    })
    #convert the list object to data.frame
    ret0<-Reduce(function(x,y){unique(rbind(x,y))},temp)
    return(ret0)
  }
}



#-------------Functions for RTF output setup-------------#
if(TRUE){
  col_width <- function(tb){  #Function to Generate rtf table column widths
    cws <- NULL
    for(i in 1:length(names(tb))) {
      ncName <- nchar(names(tb)[i])
      ncString <- max(nchar(tb[,i]))
      nc <- max(ncName, ncString)
      cw <- nc*0.89/10                 # assume 10 characters per an inch
      cws <- c(cws, cw)
    }
    return(cws)
  }
  
  
  #--- convert factor to character colname
  convert_fac_to_char <- function(dat) {
    for(i in 1:ncol(dat)) {
      if(class(dat[, i]) == "factor")
        dat[, i] <- as.character(dat[, i])
      
    }
    return(dat)
  }

  .convert <- function (x) 
  {
    x <- gsub("\\n", " \\\\line ", x)
    x <- gsub("<=", "\\\\u8804\\\\3", x)
    x <- gsub(">=", "\\\\u8805\\\\3", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&Alpha;", "\\\\u0913\\\\3", x)
    x <- gsub("&Beta;", "\\\\u0914\\\\3", x)
    x <- gsub("&Gamma;", "\\\\u0915\\\\3", x)
    x <- gsub("&Delta;", "\\\\u0916\\\\3", x)
    x <- gsub("&Epsilon;", "\\\\u0917\\\\3", x)
    x <- gsub("&Zeta;", "\\\\u0918\\\\3", x)
    x <- gsub("&Eta;", "\\\\u0919\\\\3", x)
    x <- gsub("&Theta;", "\\\\u0920\\\\3", x)
    x <- gsub("&Iota;", "\\\\u0921\\\\3", x)
    x <- gsub("&Kappa;", "\\\\u0922\\\\3", x)
    x <- gsub("&Lambda;", "\\\\u0923\\\\3", x)
    x <- gsub("&Mu;", "\\\\u0924\\\\3", x)
    x <- gsub("&Nu;", "\\\\u0925\\\\3", x)
    x <- gsub("&Xi;", "\\\\u0926\\\\3", x)
    x <- gsub("&Omicron;", "\\\\u0927\\\\3", x)
    x <- gsub("&Pi;", "\\\\u0928\\\\3", x)
    x <- gsub("&Rho;", "\\\\u0929\\\\3", x)
    x <- gsub("&Sigma;", "\\\\u0931\\\\3", x)
    x <- gsub("&Tau;", "\\\\u0932\\\\3", x)
    x <- gsub("&Upsilon;", "\\\\u0933\\\\3", x)
    x <- gsub("&Phi;", "\\\\u0934\\\\3", x)
    x <- gsub("&Chi;", "\\\\u0935\\\\3", x)
    x <- gsub("&Psi;", "\\\\u0936\\\\3", x)
    x <- gsub("&Omega;", "\\\\u0937\\\\3", x)
    x <- gsub("&alpha;", "\\\\u0945\\\\3", x)
    x <- gsub("&beta;", "\\\\u0946\\\\3", x)
    x <- gsub("&gamma;", "\\\\u0947\\\\3", x)
    x <- gsub("&delta;", "\\\\u0948\\\\3", x)
    x <- gsub("&epsilon;", "\\\\u0949\\\\3", x)
    x <- gsub("&zeta;", "\\\\u0950\\\\3", x)
    x <- gsub("&eta;", "\\\\u0951\\\\3", x)
    x <- gsub("&theta;", "\\\\u0952\\\\3", x)
    x <- gsub("&iota;", "\\\\u0953\\\\3", x)
    x <- gsub("&kappa;", "\\\\u0954\\\\3", x)
    x <- gsub("&lambda;", "\\\\u0955\\\\3", x)
    x <- gsub("&mu;", "\\\\u0956\\\\3", x)
    x <- gsub("&nu;", "\\\\u0957\\\\3", x)
    x <- gsub("&xi;", "\\\\u0958\\\\3", x)
    x <- gsub("&omicron;", "\\\\u0959\\\\3", x)
    x <- gsub("&pi;", "\\\\u0960\\\\3", x)
    x <- gsub("&rho;", "\\\\u0961\\\\3", x)
    x <- gsub("&sigmaf;", "\\\\u0962\\\\3", x)
    x <- gsub("&sigma;", "\\\\u0963\\\\3", x)
    x <- gsub("&tau;", "\\\\u0964\\\\3", x)
    x <- gsub("&upsilon;", "\\\\u0965\\\\3", x)
    x <- gsub("&phi;", "\\\\u0966\\\\3", x)
    x <- gsub("&chi;", "\\\\u0967\\\\3", x)
    x <- gsub("&psi;", "\\\\u0968\\\\3", x)
    x <- gsub("&omega;", "\\\\u0969\\\\3", x)
    x <- gsub("TRUE", "Yes", x)
    x <- gsub("FALSE", "No", x)
    x
  }
  
  
  .get.space.before.after <- function (space.before = NULL, space.after = NULL) 
  {
    ret <- ""
    if (!is.null(space.before)) {
      ret <- paste(ret, "\\sb", (space.before * 1440), sep = "")
    }
    if (!is.null(space.after)) {
      ret <- paste(ret, "\\sa", (space.after * 1440), sep = "")
    }
    ret
  }
  
   
  .add.table.row <- function (col.data = c("c1", "c2", "c3"), 
    col.widths = c(1, 4.5, 1), 
    col.justify = NULL, font.size = 10, last.row = FALSE, 
    indent = 0, border.top = FALSE, border.bottom = FALSE, space.before = NULL, 
    space.after = NULL) 
  {
    header <- paste("\\trowd\\trgaph100\\trleft", indent, sep = "")
    if (length(col.data) != length(col.widths)) {
      stop(paste("The number of data columns (", length(col.data), 
                 ") doesn't match the column widths (", length(col.widths), 
                 ")!  Input data: ", col.data, sep = ""))
    }
    justify <- vector()
    justify["L"] <- "\\ql"
    justify["R"] <- "\\qr"
    justify["C"] <- "\\qc"
    justify["J"] <- "\\qj"
    justify.v <- rep(justify["L"], length(col.data))
    numeric.cols <- which(!is.na(suppressWarnings(as.numeric(col.data))))
    if (length(numeric.cols) > 0) {
      justify.v[numeric.cols] <- justify["R"]
    }
    if (!is.null(col.justify)) {
      if (length(col.justify) == 1) {
        if (col.justify %in% names(justify)) {
          justify.v <- rep(justify[col.justify], length(col.data))
        }
        else {
          stop(paste("col.justify parameter not recognized: ", 
                     col.justify, " (should be L, R, C, or J)", 
                     sep = ""))
        }
      }
      else if (length(col.justify) == length(col.data)) {
        justify.v <- justify[col.justify]
      }
      else {
        stop(paste("The number of data columns (", length(col.data), 
                   ") doesn't match the col.justify (", length(col.justify), 
                   ") parameter!  Input data: ", paste(col.data, 
                                                       sep = "", collapse = ", "), sep = ""))
      }
    }
    btop <- ""
    bbottom <- ""
    if (any(border.top))  #  
      btop <- sapply(border.top, ifelse, "\\clbrdrt\\brdrs\\brdrw15",'') # make border.top as a vector option
    if (last.row == TRUE | any(border.bottom))  # 
      bbottom <- sapply(border.bottom, ifelse, "\\clbrdrb\\brdrs\\brdrw15",'') #make border.bottom as a vector option
    cols.prefix <- paste("\\clvertalt\\clshdrawnil\\clwWidth", 
                         round(col.widths * 1440, 0), "\\clftsWidth3\\clheight260\\clpadl100\\clpadr100\\gaph", 
                         btop, bbottom, "\\cellx", c(1:length(col.widths)), "\n", 
                         sep = "", collapse = "")
    cols <- paste("\\pard", justify.v, .get.space.before.after(space.before, 
                                                               space.after), "\\widctlpar\\intbl\\fi0\\f2\\fs", font.size * 
                    2, " ", .convert(col.data), "\\cell\n", sep = "", collapse = "")
    end.row <- "\\widctlpar\\intbl\\row\n"
    paste(header, cols.prefix, cols, end.row, sep = "")
  }
  
  # add width basing on duplication
  adj.width <- function(x, cw, space=1){
    ret <- array(0, dim=dim(x))
    x2 <- x
    #dup <- t(apply(x,1, function(x) duplicated(x)& x!='')) 
    dup <- t(apply(x,1, function(x) c(FALSE, x[-1]==x[-length(x)])))		
    for (i in 1:nrow(x)) {
      for (j in ncol(x):1) {
        if (i > 1 & j <= ncol(x)) if (!dup[i-1,j]) dup[i,j] <- FALSE
        if (dup[i,j]) {
          ret[i,j-1] <- ret[i,j]+space*cw[j]
          ret[i,j  ] <-         -space*cw[j]		
          x2[i,j] <- ''		
        }		
      }
      ret[i,] <- ret[i,]+cw
    }
    list(x=x2, cw=ret)		
  }
  
  
  ###--- This one is added/modified a little bit from the original rtf.tab that Michael Man developed
  # It expanded the width for header or footnote
  rtf.tab <- function(dat,cw=NULL,titles,col.just=NULL,var.ul,prd.status='QA',footns='',header=FALSE,footer=FALSE,
                      nline.body=40, addSpaceHeader=0, addSpaceFoot=0, page.disp=TRUE,...) { # almost automated w/ modified .add.table.row		
    st <- attributes(dat)$'start cell'[1]
    hd <- 1:(st-1)
    if(is.null(cw)) cw <- c(1.5,rep(.7,ncol(dat)-1))
    if(is.null(col.just)) col.just <- rep('R', nrow(dat)) 
    if(is.null(dim(col.just))) col.just <- matrix(rep(col.just,ncol(dat)), nrow=nrow(dat), byrow=FALSE)
    x <- dat
    cw2 <- matrix(rep(cw, nrow(x)), ncol=ncol(x), byrow=TRUE)
    if (st > 2) {
      #undebug(adj.width)
      y <- adj.width(x=dat[hd,],cw=cw, space=1)
      x[hd,] <- y$x
      cw2[hd,] <- y$cw		
    } 
    
    #expr=expression({
    rtf.add.row <- function(rows=1:nrow(x)){
      ret <- "{\\pard\n"
      for (i in rows) if (i <= nrow(x)){
        sel <- which(cw2[i,]>0)
        ret <- paste(ret, 
                     .add.table.row(
                       col.data =     x[i,sel], 
                       col.widths = cw2[i,sel], 
                       col.justify = col.just[i,sel],
                       font.size = fs,
                       border.top = i %in% c(1), 
                       border.bottom = if (i > st-2) i %in% c(st-1,nrow(x),max(rows)) else x[i,sel] %in% var.ul
                     ),
                     sep='') 	
        if (i < st) if (!all(cw2[i,] == cw2[i+1,])) ret <- paste(ret,'{\\pard\\par}','', sep='')
      }	
      ret <- paste(ret, "}\n\n", sep = "")
      rtf$.rtf <- paste(rtf$.rtf, ret, sep='')				
    }
    #})
    
    rtf <- RTF(...)
    fs <- rtf$.font.size
    rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)	# make "Courier New" as default
    rtf$.rtf <- gsub('field{\\fldinst{page}}', '', rtf$.rtf, fixed=TRUE)	# delete page number
    #nline.body <- 2
    npage <- ceiling((nrow(x)-st+1)/nline.body)
    #addHeader(rtf,title=titles[[1]],subtitle=titles[[2]])
    #systems <- c('Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}', 
    systems <- c(ifelse(page.disp==TRUE, paste('Page \\chpgn  of ', npage, sep=''), ""), 
                 format(Sys.time(), "%H:%M %d%b%Y"),
                 prd.status)
    #systems <- c(paste('Page \\chpgn  of ', npage, sep=''), 
    #             format(Sys.time(), "%H:%M %d%b%Y"),
    #             prd.status)
    hd.m <- c(paste(titles,  collapse='\\line '), 
              paste(systems, collapse='\\line '))  # main header									
    tmp <- .add.table.row(
      col.data =  hd.m, 
      col.widths = c(sum(cw)-2+addSpaceHeader,2), 
      col.justify = c('L', 'R'),
      font.size = fs)
    tmp2 <- .add.table.row(
      col.data =  paste(footns, collapse='\\line '), 
      col.widths = sum(cw)+addSpaceFoot, 
      col.justify = 'L',
      font.size = fs)							
    if (FALSE) rtf$.rtf <- paste(rtf$.rtf, "{\\header\\pard", "\\fi0\\f2\\fs", fs*2, 
                                 "\\qr Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}\\par}", sep='')
    for (i in 1:npage){						
      rtf$.rtf <- paste(rtf$.rtf, ifelse(i==1,'','\\page'), '{\\pard\\par}', 
                        ifelse(header,'{\\header',''), tmp, 
                        ifelse(header,'}',''), '{\\pard\\par}', sep='')					
      #startParagraph(rtf)
      #eval(expr)
      rtf.add.row(rows=c(1:(st-1), 1:nline.body-1+st+nline.body*(i-1)))
      #endParagraph(rtf)		
      rtf$.rtf <- paste(rtf$.rtf, '{\\pard\\par}', ifelse(footer,'\\footer',''), tmp2, sep='')
    }	
    done(rtf)		
    
  }#end rtf.tab


  #--- produce to rtf file that as SAS output. It can be used for single table or table with multiple pages
  rtf_table_out_as_sas <- function(rtf, tb, cw=NULL, 
                                   colFormat=c(rep("L", 1), rep("C",1), rep("R",1)),
                                   cell1=2, cell2=1, nheader=1,
                                   varName=NULL,
                                   var.ul='', prd.status=' ', #'QA',
                                   titles="", footns="", 
                                   header=FALSE, footer=FALSE,
                                   nline.body=30, height=8.5, width=11, omi=c(1,1,1,1),
                                   addSpaceHeader=0, addSpaceFoot=0, page.disp=TRUE,
                                   ...) {
                                   
    # rtf: RTF object that is just needed to be declared once before calling the function
    # tb: table out
    # cw: column width
    # colFormat: alignment (left, center, or right) for each column
    # varName: variable name that want to be repeated when go to the next page
    # var.ul: pre-specified cell values that indicate the bottom line in a table
    # nline.body: number of lines per page
    # addSpaceFoot, addSpaceHeader: expand the width of header or footnotes
    # page.disp: whether diplay page number
    
    #--- generate blank pape (break page) if the data is more than a page
    if(nrow(tb) > nline.body) {
      idx <- seq(1, nrow(tb), by = nline.body)
      idx.n <- c(seq(nline.body, nrow(tb), by = nline.body), nrow(tb))
    }else {
      idx <- 1
      idx.n <- nrow(tb)
    }
    
    npage <- length(idx)
    
    for(k in 1:length(idx)) {
      subTable <- tb[idx[k]:idx.n[k], ]
      if(is.null(nrow(subTable))){
        subTable <- data.frame(` `=subTable)
      }
      #--- if nrow < nline.body, need adding some spaces to move footnotes to bottom of the page
      if(nrow(subTable) < nline.body) {
        addData <- data.frame(matrix("",ncol=ncol(subTable), nrow=nline.body-nrow(subTable)-1))
        names(addData) <- names(subTable)
        subTable <- rbind(subTable, addData)  
      }
      
      #-- get values that will be in the 1st row of next page
      rownames(subTable) <- 1:nrow(subTable)
      
      #-- check if the last row of previous page with varName not matched the 1st row of next page
      if(k==1)
        string.val <- NULL
      if(k!=1 & !all(unique(subTable[1,varName]) %in% "") & !all(unique(subTable[1,varName]) %in% string.val))
        string.val <- unique(subTable[1,varName])
      
      if(k==1 & all(tb[nrow(subTable)+1, varName] %in% "")) {
        for(m in 1:nrow(subTable)) 
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName]
      } else if(!all(tb[nrow(subTable)+1, varName] %in% "")) {
        string.val <- NULL
      } else {   
        #-- add last row of previous page to 1st row of next page    
        if(!is.null(string.val))
          subTable[1, varName] <- string.val
        
        for(m in 1:nrow(subTable)) 
          if(all(!subTable[m,varName] %in% ""))
            string.val <- subTable[m, varName] 
      }
      
      hd <- sapply(colnames(subTable), function(x) strsplit(x, '; ')[[1]])
      ret <- rbind(hd, as.matrix(subTable))
      colnames(ret) <- 1:ncol(ret)            
      rownames(ret) <- 1:nrow(ret)
      
      ret2 <- ret
      
      colnames(ret2) <- 1:ncol(ret2)            
      rownames(ret2) <- 1:nrow(ret2)
      attributes(ret2) <- list(dim = dim(ret2), dimnames = dimnames(ret2), `start cell` = c(cell1, cell2))
      
      col.just <- matrix(c(rep(colFormat, nrow(ret2) + nheader)), ncol=ncol(ret2), byrow=TRUE)
      
      dat <- ret2
      st <- attributes(dat)$'start cell'[1]
      hd <- 1:(st-1)
      if(is.null(cw)) cw <- c(2.5, rep(1.5,ncol(dat)-1))
      x <- dat
      cw2 <- matrix(rep(cw, nrow(x)), ncol=ncol(x), byrow=TRUE)
      if (st > 2) {
        y <- adj.width(x=dat[hd,],cw=cw, space=1)
        x[hd,] <- y$x
        cw2[hd,] <- y$cw    
      } 
      
      rtf.add.row <- function(rows=1:nrow(x)){
        ret <- "{\\pard\n"
        for (i in rows) if (i <= nrow(x)){
          sel <- which(cw2[i,]>0)
          ret <- paste(ret, 
            .add.table.row(
              col.data =     x[i,sel], 
              col.widths = cw2[i,sel], 
              col.justify = col.just[i,sel],
              font.size = fs,
              border.top = i %in% c(1), 
              border.bottom = if (i > st-2) i %in% c(st-1,nrow(x),max(rows)) else x[i,sel] %in% var.ul
              #x is the subtable with header
            ),  sep='') 
          if (i < st) if (!all(cw2[i,] == cw2[i+1,])) ret <- paste(ret,'{\\pard\\par}','', sep='')
        }
        ret <- paste(ret, "}\n\n", sep = "")
        rtf$.rtf <- paste(rtf$.rtf, ret, sep='')
      }
      
      rtf <- rtf
      fs <- rtf$.font.size
      rtf$.rtf <- gsub('Times New Roman', 'Courier New', rtf$.rtf)	# make "Courier New" as default
      rtf$.rtf <- gsub('field{\\fldinst{page}}', '', rtf$.rtf, fixed=TRUE)	# delete page number
      
      tmp2 <- .add.table.row(
        col.data =  paste(footns, collapse='\\line '), 
        col.widths = sum(cw)+addSpaceFoot, 
        col.justify = 'L',
        font.size = fs)	
      rtf.add.row(rows=c(1:(st-1), 1:nline.body-1+st))	
      rtf$.rtf <- paste(rtf$.rtf, '{\\pard\\par}', ifelse(footer,'\\footer',''), tmp2, sep='')
    } #end of the for loop	
    return(rtf)
  }#end rtf_table_out_as_sas  
 

}





#-------------Manually Convert R object into HTML code-------------#
df2html<-function( #convert a table into part of HTML code
  datF, #a data.frame for table output
  tabTit="",  #table title
  fnote=NULL,  #footnote
  ind=1
){
  datF<-as.data.frame(datF)
  dim1<-dim(datF)
  nr<-dim1[1]
  nc<-dim1[2]
  cnm<-colnames(datF)
  rnm<-rownames(datF) #currently the rownames would not be printed out.  
  out<-paste0('<a name=\"IDX',ind,'\"></a>')
  out<-c(out, '<div align=\"left\">')
  tabSum2<-paste0('<table class=\"Table\" cellspacing=\"1\" cellpadding=\"',
    nc+2, '\" rules=\"groups\" frame=\"box\" border=\"1\" bordercolor=\"#000000\" ',
    'summary=\"', tabTit, '\" >')  #style="table-layout: fixed; width: 100%"
  out<-c(out, tabSum2, '<colgroup>', rep('<col>',nc), '</colgroup>', '<thead>')
  #For table header
  tabHead<-c('<tr>', paste0('<td class="l NoteContent" colspan=\"', nc,
    '\">', tabTit, '<br>', paste(rep("-",138),collapse=''), '</td>'), '</tr>', '<tr>')
  for(i in 1:nc){
    tabHead<-c(tabHead, paste0('<th class=\"c b Header\" scope=\"col\">',
      cnm[i], '</th>'))
  }
  out<-c(out, tabHead, '</tr>', '</thead>', '<tbody>')
  #for table body
  tabBody<-NULL
  for(j in 1:nr){
    tabBody<-c(tabBody, '<tr>')
    for(i in 1:nc){
      tabBody<-c(tabBody, paste0('<td class=\"l Data\">', datF[j,i], '</td>'))
    }
    tabBody<-c(tabBody, '</tr>')
  }
  out<-c(out, tabBody, '</tbody>', '<tfoot>', '<tr>')
  out<-c(out, paste0('<td class=\"l NoteContent\" colspan=\"',nc,'\">',
    paste(rep("-",138),collapse=''), '<br>', fnote, '</td>'), '</tr>', '</tfoot>')
  out<-c(out, '</table>', '</div>', 
    '<p style="page-break-after: always;"><br></p><hr size="3">')
  return(out)  
}

fig2html<-function( #convert a png figure into part of HTML code
  figNm='plot.png', # the png file name
  figTit="", #figure title
  figH="480", figW="640",
  fnote=NULL,  #footnote
  ind=1
){
  out<-paste0('<a name=\"IDX',ind,'\"></a>')
  figTit1<-c('<div align=\"left\">', 
    paste0('<table class=\"Table\" cellspacing=\"1\" cellpadding=\"1',
      '\" rules=\"groups\" frame=\"box\" border=\"1\" bordercolor=\"#000000\" ',
      'summary=\"', figTit, '\" >'), '<colgroup>','<col>', '</colgroup>',
      '<thead>', '<tr>' ) 
  out<-c(out, figTit1, paste0('<td class=\"l NoteContent\" colspan=\"1\">', 
    figTit,  '</td>'), '</tr>', '</thead>', '<tbody>', '<tr>' )
  out<-c(out, '<td>', paste0('<img alt=\" \" src=\"', figNm,
      '\" style=\" height: ', figH, 'px; width: ', figW, 
      'px;\"  border=\"0\">'), '</td>', '</tr>', '</tbody>','<tfoot>','<tr>')
  out<-c(out, paste0('<td class=\"l NoteContent\" colspan=\"1\">',fnote,
    '</td>'), '</tr>', '</tfoot>')
  out<-c(out, '</table>', '</div>',
    '<p style="page-break-after: always;"><br></p><hr size="3">')
  return(out)  

}

cLink<-function( #generate a link in content table
  tit="tmplink", ind=1
){
  out<-'<li class=\"ContentItem\">'
  out<-c(out, '<span><b>&#183;</b><a href=\"',
    paste0('body.htm#IDX', ind,'\" target=\"body\">',tit,'</a>'),
    '</span><br></li>')
  return(out)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#revise a html table code
#make it into multi-level header
  toMultH <- function(
    tbc, # a vector of html table code
    split1=";"                
  ){
    wh.h <- grep('<th>', tbc)
    if(length(wh.h)>1){return(tbc)}
    tbc.h <- tbc[1:(wh.h-1)]
    if(wh.h==length(tbc)){return(tbc)}
    tbc.b <- tbc[(wh.h+1):length(tbc)]
    hh <- tbc[wh.h]
    hh <- gsub("<th>", "", hh, fixed=TRUE)
    hh <- gsub("<tr>", "", hh, fixed=TRUE)
    hh <- gsub("</tr>", "", hh, fixed=TRUE)
    hh <- strsplit(hh, split="</th>")[[1]]
    hh <- hh[-length(hh)] #remove the last empty element
    hh <- strsplit(hh, split=split1, fixed=TRUE)
    
    hh.l <- sapply(hh, length)
    short <- hh.l<max(hh.l)
    hh[short] <- lapply(hh[short], function(x){
      c(x, rep(" ", max(hh.l)-length(x)))})

    hh3 <-matrix(unlist(hh), nrow=max(hh.l))
    hh3 <- apply(hh3, 1, function(x){
      r1 <- "<tr> <th colspan=\'"
      colspan <- 1
      if(length(x)>1){
        for(i in 1:(length(x)-1)){#requre ncol for the table is 2+
          if( gsub(" ", "", x[i])==gsub(" ", "", x[i+1])){
            colspan <- colspan + 1
          }else {
            r1 <- paste(r1, colspan, "\'>", x[i], "</th> <th colspan=\'")
            colspan <- 1
          }
        }
      }
      r1 <- paste(r1, colspan, "\'>", x[length(x)], "</th> </tr>'")
      return(r1)
      #<th colspan='2'>      
    })
    return(c(tbc.h, hh3, tbc.b))
    
  }



