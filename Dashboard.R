library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(janitor)
library(ggplot2)
library(dplyr)
library(stringr)
library(fontawesome)


Data<-read.csv(file.choose())
Data1<-read.csv(file.choose(),T)
data <- data.frame(group = rep(c("Nungambakkam", "Saidapet", "Meenambakkam"), each = 365),values = c(Data1$Nungambakkam,Data1$Saidapet,Data1$Meenambakkam))
model <- aov(values~group, data=data)

title<-tags$a(
  href='https://mausam.imd.gov.in/chennai/',
  tags$img(
    src="https://newsonair.gov.in/writereaddata/News_Pictures/NAT/2018/Nov/NPIC-2018112482925.jpg", 
    height = '50', 
    width = '50'),
    target="_blank"
          )

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = title,
    titleWidth = 70
                 ),
  dashboardSidebar(
    tags$style(HTML("
      .main-sidebar{
        width:250px;
      }
    ")),
           sidebarMenu(
                style="position:adaptive;width:250px;",
                menuItem("",tabName = "home"),
                menuItem("About",tabName="about",icon=icon("fa-circle",class="fa-solid fa-circle-exclamation",verify_fa=F)),
                menuItem("Choose",tabName = "role",icon=icon("fa-calculator",class="fa-solid fa-calculator",verify_fa=F),
                selectInput("District","District",choices=Data$District),
                selectizeInput("Station","Station",choices="",selected=""),
                dateRangeInput(
                        inputId = "Daterange",
                        label = "Select the date Range",
                        start = "2020-01-01",
                        end = "2020-12-31" 
                              ),
                tableOutput("Table")),
                menuItem("Data",tabName = "render",icon=icon("fa-table",class="fa-solid fa-table-list",verify_fa=F)),
                menuItem("Analysis",tabName = "analysis",icon=icon("fa-chart",class="fa-solid fa-chart-simple",verify_fa=F)))),

  dashboardBody(
   theme=theme("readable"),
    tabItems(
      tabItem(tabName = "home",
              h1("INDIAN METEOROLOGICAL DEPARTMENT",align="center"),
              h2("Regional Meteorological Centre,chennai",align="center")),
      tabItem(tabName = "about",
              h2("Indian Meteorological Department",align="center"),
              h3("Regional Meteorology Centre, Chennai",align="center"),
              tags$img(src="https://gumlet.assettype.com/dtnext/import/Articles/2022/Jan/202201260703384736_Southern-districts-to-get-rain-for-next-48-hours-RMC_SECVPF.gif?w=1200&h=675&auto=format%2Ccompress&fit=max",
                       style="display: block; margin-left: auto; margin-right:auto;width:350px;"),
h4("          Regional Meteorology Centre (RMC), Chennai was established on 1st April 1945. As of now, it is one of the six such RMCs of Indian Meteorological Department, headed by an officer of the rank of Scientist-F and Deputy Director General of Meteorology. The Meteorological observatory functioning at Nungambakkam, Chennai which was formerly known as Madras (13 04'N/80 15'E).",align="center"),
                     
h4("          The Regional Meteorological Centre (RMC), Chennai one of the six regional centres of India Meteorological Department (IMD), located at No. 6 (Old No.50), College Road, between Good
Shepherd School and Women's Christian College. IMD is the National weather service provider for India.",align="center"),

h4("          The three Meteorological Centres in South India function at Hyderabad, Amaravati, Bengaluru and Thiruvananthapuram serving the states of Telangana, Andhra Pradesh, Karnataka, and Kerala &Lakshadweep respectively, under the technical and administrative control of the Regional Meteorological Centre, Chennai.",align="center"),

h4("          There are also Port Meteorological offices at Chennai, Kochi and Visakhapatnam, which interact with masters of ships and shipping companies and other marine interests. 
More than 1,400 personnel including 300 officers work in various offices under Regional Meteorological Centre, Chennai which includes 4 meteorological centres, 1 Area Cyclone warning centre, 2 cyclone warning centres, 6Doppler Weather Radar stations and 17 Aviation Meteorological Offices (AMOs).",align="center"),
       
),
      tabItem(tabName = "role"),
      tabItem(tabName = "render",h2("Regional Meteorology Centre, Chennai",align="center"),
            fluidRow( 
                tabBox(
                  width=12,
                  side="left",
                  height = "1000px",
            tabPanel("Station Data",tableOutput("GivenData")),
            tabPanel("Station's Monthly Average Rainfall",tableOutput("Good")),
            tabPanel("Plot",plotOutput("Line")),
         
    ))),
    tabItem(tabName = "analysis",
            h2("Analysis", align="center"),
            fluidRow(
            tabBox(
              tabPanel("ANOVA",verbatimTextOutput("summary1")
            ),
              tabPanel("Post Hoc Test",verbatimTextOutput("summary2")
            )))
            )),
    tags$footer("@copyright  IMD  Intership project by M.Sc.Statistics , Presidency College ,Chennai", align = "center")
  ))
    
server=shinyServer(function(session,input,output)
{
  
  observeEvent(
    input$District,
    updateSelectInput(session,"Station","Station",
                      choices=Data$Station[Data$District==input$District],selected = "")
  )
  
  
  output$GivenData<-renderTable({
    Data %>% 
    {subset(Data,Data$Station==input$Station,select=c(DATE,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC))} %>%
      filter(DATE >= 1 & DATE <= 31)    
  },bordered=T,striped = T)
  
  output$Table<-renderTable({
    doc<-Data %>%
          {subset(Data,Data$Station==input$Station,select=c(Date,Rain))}  %>%
             filter(Date >= input$Daterange[1] & Date <= input$Daterange[2]) %>%
               adorn_totals("row")
    doc1<-mean(replace(doc$Rain,doc$Rain==(sum(doc$Rain)/2),NA),na.rm=TRUE)
    doc<-rbind(doc,doc1)
  })
  
  output$Line<-renderPlot({
    Data %>%
      {subset(Data,Data$Station==input$Station,select=c(Date,Rain))}  %>%
      filter(Date >= input$Daterange[1] & Date <= input$Daterange[2]) %>%
      ggplot(Date=factor(Date, levels= Date),aes(x=Date,y=Rain))+
      geom_col(aes(x=Date,y=Rain),width = 0.25)+
      geom_point()+xlab("Dates")+ylab("Rainfall(mm)")+
      theme_dark()
  })
  
  output$Good<-renderTable({
    Data %>%
      {subset(Data,Data$Station==input$Station,select = c(Months,Ave_Rainfall))} %>%
      filter(str_detect(Months,"JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC"))
      
  })

  output$summary1<-renderPrint({
    model <- aov(values~group, data=data)
    print(model)
    print(summary(model))
    br()
    br()
  })
  
  output$summary2<-renderPrint({
    pht<-TukeyHSD(model, conf.level=.95)
    print(pht)
    br()
    br()
    })
})

shinyApp(ui,server)

