GTM <- "
<!-- Google Tag Manager -->
  <noscript><iframe src=\"//www.googletagmanager.com/ns.html?id=GTM-W3L6VW\"
height=\"0\" width=\"0\" style=\"display:none;visibility:hidden\"></iframe></noscript>
<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
'//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','GTM-W3L6VW');</script>
<!-- End Google Tag Manager -->
"  

require(shiny)
require(knitr)
require(shinyIncubator)

# loadingBar <- tags$div(class="progress progress-striped active",
#                        tags$div(class="bar", style="width: 100%;"))
# # Code for loading message
# loadingMsg <- tags$div(class="modal", tabindex="-1", role="dialog", 
#                        "aria-labelledby"="myModalLabel", "aria-hidden"="true",
#                        tags$div(class="modal-header",
#                                 tags$h3(id="myModalHeader", "Loading...")),
#                        tags$div(class="modal-footer",
#                                 loadingBar))
# The conditional panel to show when shiny is busy
# loadingPanel <- conditionalPanel("$('html').hasClass('shiny-busy')",
#                                  loadingMsg)
# loadingPanel <- conditionalPanel(paste("input.goButton > 0 &&", 
#                                        "$('html').hasClass('shiny-busy')"),
#                                  loadingMsg)

shinyUI(navbarPage(
  theme = "bootstrap.css",
  title=("ChaoEntropy Online"),
  tabPanel(("Shannon entropy"),
           h1("ChaoEntropy"),
           sidebarLayout(
             sidebarPanel(
               tags$head(
                 tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
                 tags$style(type="text/css", "select { max-width: 250px; }"),
                 tags$style(type="text/css", "input { max-width: 250px; }"),
                 tags$style(type="text/css", "textarea { max-width: 230px; }"),
                 tags$style(type='text/css', ".span4 { max-width: 300px; }")
               ),
               actionButton("goButton", "Run!"),
               p(h4("Data Setting")),
               wellPanel(
                 selectInput(inputId="datatype", label="Select data type:",
                             choices=c("Abundance data"="abu", "Incidence data"="inc")),
                 
                 radioButtons(inputId="source", "Choose one:", 
                              choices=c("Import data" = "import", "Upload data" = "upload")
                 ),
                 conditionalPanel(condition="input.source == 'upload'",
                                  fileInput("files", "Choose File (.csv)")
                 ),
                 
                 
                 uiOutput("choose_dataset"),
                 p(em("Using ctrl / command key to select multiple datasets you want")),
                 
                 conditionalPanel(condition="input.source == 'import'",
                                  p("Import data:"),
                                  conditionalPanel(
                                    condition="input.datatype == 'abu'",
                                    tags$textarea(id="copyAndPaste_abu", rows=5, 
                                                  "Spider 46 22 17 15 15  9  8  6  6  4  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1  \nBirds 752 276 194 126 121 97  95  83  72  44  39  0  16  15  0  13  9  9  9  8  7  4  0  0  2  2  1  1  1")
                                  ),
                                  conditionalPanel(
                                    condition="input.datatype == 'inc'",
                                    tags$textarea(id="copyAndPaste_inc", rows=5, 
                                                  "Ant 62  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  5  5  5  5  6  6  7  9  9  9  9 10 10 12 13 14 14 14 15 15 16 18 19 19 20 29 \nSeedlings 121  61  47  52  43  43   9  24   5  24  11  11  13  17   6  10   3   8   9   9  3   6   6   1   7   4   6   6   4   3   4   2   2   1   1")
                                  ),          
                                  p(em("Refer to user guide for importing data"))
                 )
                 
               ),
               
               p(h4("General Setting")),
               wellPanel(
                 conditionalPanel(
                   condition = "input.datatype == 'abu'",
                   checkboxGroupInput(inputId="method1", label="Select the methods:",
                                      choices=c("Chao", "ChaoShen", "Grassberger", "Jackknife", "Zhang", "Observed"),
                                      selected=c("Chao", "ChaoShen", "Grassberger", "Jackknife", "Zhang", "Observed"))
                 ),
                 
                 conditionalPanel(
                   condition = "input.datatype == 'inc'",
                   checkboxGroupInput(inputId="method2", label="Select the methods:",
                                      choices=c("Chao", "Observed"), selected=c("Chao", "Observed"))
                 ),
                 numericInput(inputId="nboot", label="Number of bootstraps", value=100, min=1, max=1000, step=1),
                 numericInput(inputId="conf", label="Confidence level", value=0.95, min=0, max=1, step=0.01)
               )
               
             ),
             mainPanel(
               progressInit(),
               tabsetPanel(
                 tabPanel("Data Summary", h3("Basic data information"),
#                           loadingPanel,
                          htmlOutput("data_summary")
                 ),
                 tabPanel("Estimation", h3("Estimation of entropy"), 
#                           loadingPanel,
                          htmlOutput('est'),
                          downloadLink("dlest", "Download as csv file"),
                          conditionalPanel(
                            condition="input.datatype == 'abu'",
                            includeMarkdown("man/estimator_abu.md")),
                          conditionalPanel(
                            condition="input.datatype == 'inc'",
                            includeMarkdown("man/estimator_inc.md"))
                 ),
                 
                 tabPanel("Visualization", h3("Comparison with different methods"), 
                          p("Note: Please wait a moment!"),
#                           loadingPanel,
                          plotOutput("visualization", width="900px", height="600px")
                          
                 ),
                 tabPanel("User Guide", includeMarkdown("man/user.md")),
                 tabPanel("R code", includeMarkdown("man/[R]code.md"))
               )
             )
           )
  ),
  tabPanel(
    title=("Mutual Information "),
    h3("Coming soon :) ", style="text-align: center")
  )
  
  
))
