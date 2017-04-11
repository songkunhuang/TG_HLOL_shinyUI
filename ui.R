library(shinythemes)

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
                           choices = c(textOutput("area"))),
               selectInput("type", "预警类型:", 
                           choices = c("重载", "过载"))
             ))),
    
    tabPanel("预警清单",
             titlePanel('File download'),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Choose a dataset:", 
                             choices = c("predict", "loessnum", "extend")),
                 radioButtons("filetype", "File type:",
                              choices = c("csv", "tsv")),
                 downloadButton('downloadData', 'Download')
               ),
               mainPanel(
                 tableOutput("table")
               )
             )
                ),

   tabPanel("实际清单",mainPanel(
     tabsetPanel(
       tabPanel("Tab 1",
                h4("Table"),
                tableOutput("table"),
                h4("Verbatim text output"),
                verbatimTextOutput("txtout"),
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
   tabPanel("可视化展示")
  
   
)




