conditionalPanel(
  condition="true",
  div(class='row'),
  div(class='span4', 
    textInput('datPath','Path for read in dataset.',
	value="")), #paste0("//lrlhps/data/ttx_projects/onc/reusable/mock_data",
	  #"/mock_merged_clin_FMI_Breast.csv"))),
  div(class='span4',
    textInput('outPath','Path for output RTF file.',
	value= tempfile(pattern='shiny',
	  tmpdir="", #//lrlhps/data/ttx_projects/onc/reusable/r_function/dev/output
	  fileext='.rtf'))),
  br(),
  div(class='row'),
  div(class='span11',
	tabsetPanel(
	  tabPanel('Run Code', 
	           downloadButton("save_rcode","Save Code"),
	           div(class="alert alert-info", strong("Note: "), r.lib.alert),
	           verbatimTextOutput("rcode")),
    tabPanel('Source Functions', 
             downloadButton("save_scode","Save Source"),
             verbatimTextOutput("scode"))))
)



