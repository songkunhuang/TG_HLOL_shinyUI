library(shinythemes)
library(leaflet)

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop")

navbarPage("配变重过载预警",theme=shinytheme("cerulean"),
    tabPanel("预警提交",
             verticalLayout(
             flowLayout(
               dateInput('date2',
                         label = paste('预警时间'),
                         value = as.character(Sys.Date()),
                         min = Sys.Date() - 5, max = Sys.Date() + 5,
                         format = "yyyy-mm-dd",
                         startview = 'month', language = 'zh-CN', weekstart = 1),
               selectInput("city", "地市:", 
                           choices = c("福州", "厦门")),
               selectInput("area", "区县:", 
                           choices = c(dataTableOutput("area"))),
               selectInput("type", "预警类型:", 
                           choices = c("重载", "过载")),
               submitButton("预警提交")
             ))),
    
    tabPanel("预警清单",
             fluidPage(
              sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Choose a dataset:", 
                             choices = c("predict", "loess", "extend")),
                 radioButtons("filetype", "File type:",
                              choices = c("csv", "tsv")),
                 downloadButton('downloadData', 'Download'),
                 submitButton("显示数据")
               ),
               mainPanel(
                 tableOutput('table')
               )
             )
                )
             ),

   tabPanel("实际清单",mainPanel(
     tabsetPanel(
       tabPanel("Tab 1",
                h4("Table"),
 #               tableOutput("table"),
                h4("Verbatim text output"),
 #               verbatimTextOutput("txtout"),
                h1("Header 1"),
                h2("Header 2"),
                h3("Header 3"),
                h4("Header 4"),
                h5("Header 5")
       ),
       tabPanel("Tab 2"),
       tabPanel("Tab 3")
     )
   )),
   tabPanel("可视化展示",mainPanel(

   
   navbarPage("Superzip", id="nav",
              
              tabPanel("Interactive map",
                       div(class="outer",
                           
                           tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css"),
                             includeScript("gomap.js")
                           ),
                           
                           leafletOutput("map", width="100%", height="100%"),
                           
                           # Shiny versions prior to 0.11 should use class="modal" instead.
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                         width = 330, height = "auto",
                                         
                                         h2("ZIP explorer"),
                                         
                                         selectInput("color", "Color", vars),
                                         selectInput("size", "Size", vars, selected = "adultpop"),
                                         conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                          # Only prompt for threshold when coloring or sizing by superzip
                                                          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                         ),
                                         
                                         plotOutput("histCentile", height = 200),
                                         plotOutput("scatterCollegeIncome", height = 250)
                           ),
                           
                           tags$div(id="cite",
                                    'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                           )
                       )
              ),
              
              tabPanel("Data explorer",
                       fluidRow(
                         column(3,
                                selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                         ),
                         column(3,
                                conditionalPanel("input.states",
                                                 selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                )
                         ),
                         column(3,
                                conditionalPanel("input.states",
                                                 selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                )
                         )
                       ),
                       fluidRow(
                         column(1,
                                numericInput("minScore", "Min score", min=0, max=100, value=0)
                         ),
                         column(1,
                                numericInput("maxScore", "Max score", min=0, max=100, value=100)
                         )
                       ),
                       hr(),
                       DT::dataTableOutput("ziptable")
              ),
              
              conditionalPanel("false", icon("crosshair"))
   ))
            )
  
   
)




