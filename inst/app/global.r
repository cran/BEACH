############################################
##Author @ Danni Yu
##Date   @ 2016-05-31
############################################

CleanEnvir <<- function(pattern = "tmp") {
  objs <- ls(pos = ".GlobalEnv")
  #print(objs)
  rm(list = objs[grep("tmp", objs)], pos = ".GlobalEnv")
}
#print('global')
CleanEnvir()

  inGithub <- FALSE
 folder  <-'BEACH'
 htmlPath <- ''


#---------Configuration File---------------#
options(stringsAsFactors=FALSE)
  local.path0 <- file.path('.','functions')
  local.path1 <- file.path('.','functions')
  local.path2 <- tempdir()
  local.path3 <- file.path(local.path2, 'www')  
  if(!'www'%in%dir(local.path2)) {dir.create(local.path3)}
  
  #assign a static url to 'images' 
  addResourcePath('images',local.path3)

  htmltem <- file.path('.', 'html_template')
  npath3<-nchar(local.path3)+2

#define the maximum number of widgets
max.n1<-c(check=100, radio=100, dropdown=100, slide=100, 
  date=100, dateR=100, num=100, text=100, textbox=100)

#global variables defintion
#the value can be changed under 'expert' textInput

#indicate whether all sources files are output or only R script names
allSourceIn1<<-FALSE 

#create a global variable for dynamic render plot
dynamicCode<<-paste0("plot(0~0, col='white',axes=F, xlab='', ylab='',xlim=c(-3,3));",
  " text(x=0, y=0, labels=\'dynamicData is not defiend yet.\')")
dynamicData<<-NULL
dynamicData.xvar<<-NULL
dynamicData.yvar<<-NULL


#to check whether there is any CD
cdpool<<-dir(local.path1)
cdpool<<-cdpool[grepl('configuration', cdpool) & grepl('.csv', cdpool)]
cdpool<<-c(' ', cdpool)
cdpool2<<-gsub('_configuration_', '', cdpool)
cdpool2<<-gsub('configuration_', '', cdpool2) 
cdpool2<<-gsub('_configuration', '', cdpool2)
cdpool2<<-gsub('configuration', '', cdpool2)

#define the split string for multi-level header
muliHead.split <<- ";"


#define colors
RGBColors <- col2rgb(colors()[1:length(colors())])
HEXColors <- rgb(RGBColors[1,]/255, RGBColors[2,]/255, RGBColors[3,]/255)
names(HEXColors) <- colors()



