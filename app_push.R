#Check packages installed
# install.packages("pacman")
# 
# pacman::p_load(shiny,networkD3,tidyverse,DT,plotly,shinydashboard,shinyBS,shinyWidgets,data.table,purrr)

#load libraries 
library(shiny)
library(networkD3)
library(tidyverse)
library(DT)
library(purrr)
library(plotly)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)



sankey_data <- readRDS("final_destination_longV0.rds") # no zero hs gpa
#course_data <- readRDS("DB_crs_grade_FTIC1521V0.rds")
#sankey_updata <- readRDS("flow_DATA_updated202208V2.rds") #upto 202208
#new_sankeydata <- readRDS("sankey_new_data.rds")
course_data_v1 <- readRDS("DB_crs_grade_FTIC1521V1.rds")  
new_sankeydata_v1 <- readRDS("sankey_new_data_2015to2021.rds")

CIP_college <- unique(sankey_data$ENTRY_COLLEGE)   %>% sort()
CIP_depart <- unique(sankey_data$ENTRY_DEPARTMENT) %>% sort()
CIP_choice <- unique(sankey_data$UWFFall1CIPTitle) %>% sort()
cohort_choice <- sort(unique(sankey_data$Cohort), decreasing = F)  
maxhsgpa <- max(new_sankeydata_v1$GPA_HIGHSCHOOL)
minhsgpa <- min(new_sankeydata_v1$GPA_HIGHSCHOOL)
metriccolchoice <- CIP_college 
barchartmetric <- c("APR","RETENTION","GRADUATIONin4YRS")
 

###start-ui
ui <- dashboardPage( skin = "green",
                     dashboardHeader(title="From Entry To End Program", titleWidth = 300),
                     dashboardSidebar( width = 300,
                     sidebarMenu(id = "Menu...",                  
                        menuItem( text= "Overview FTIC", startExpanded = T,
                            menuSubItem(text = "APR/Graduation/Summary", tabName = "metricapr",     icon = icon("fas fa-chart-line")),
                            menuSubItem(text = "Course Grades Data", tabName = "coursegrades", icon = icon( "fas fa-align-right"))
                             
                            ),      
                        menuItem( text= "Migration FTIC", startExpanded = T,
                            menuSubItem(text = "Entry or Exit Flow Graph",  tabName = "flowchart", icon = icon("fas fa-stream"))

                        )# menuitem
                       
                            )
                     ), # sidebar menu
                     
        dashboardBody(
                         tags$head( 
                             tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 15
                         ),
                         tabItems(
                             # courses tab
                             tabItem(tabName = "coursegrades",
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, status = "info", background = "black",
                                             selectInput(inputId = "courseprogram", label = "Select a Program",
                                                         choices = sort(unique(course_data_v1$ENTRY_PROGRAM)),
                                                         selected = "Cybersecurity"
                                                         ),
                                             selectInput(inputId = "couresecohort", label = "Select a Cohort",
                                                         choices = sort(unique(course_data_v1$Cohort)),
                                                         selected = "2020"
                                                         ),
                                             "Choosing the high school GPA thresholds",br(),
                                             "will update passed/failed rates and counts",br(),
                                             "(Replaced NAs with 0.00)",
                                             numericInput(inputId = "crsgparange1",
                                                          label = "Minimum Allowed High School GPA Thresholds (>= 2.00)", step = 0.01, #dragRange = F,
                                                          value = 0.00,  min=minhsgpa, max = maxhsgpa ), 
                                             numericInput(inputId = "crsgparange2",
                                                          label = "Maximum Allowed High School GPA Thresholds (<= 5.04)",step = 0.01, #dragRange = F,
                                                          value = 5.04, min=minhsgpa, max = maxhsgpa ),
                                             selectInput(inputId = "topcrsselectterms", label = "Select a Term",
                                                         choices = NULL,
                                                         selected = NULL) 
                                             
                                             ), 
                                         box(width = 10, title = "DWF With The Most Frequently Enrolled 1st Year Courses by Program", solidHeader = T, 
                                             status = "info", actionButton("frerqCRS", label = "Show Updated Info."),
                                             plotly::plotlyOutput("bar_course_pass_plot"))), #1st row
                                     
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, background = "black",status = "warning",
                                             selectInput(inputId = "coursename", label = "Select a Course",
                                                         choices = NULL,
                                                         selected = NULL),br(),
                                             "Included Grades are A to F range, NF (Non-attending/Fail), U (Unsatisfactory), and W (Withdrawn).",
                                             br(),
                                             selectInput(inputId = "courseterms", label = "Select a Term",
                                                         choices = NULL,
                                                         selected = NULL)
                                             ),
                                         
                                         box(width = 10, title = "APR by 1st Year Course Grades by Program (included multiple attempts)", 
                                                     solidHeader = T, status = "warning", 
                                                     actionButton("aprgradeCRS",label = "Show Updated Info."),
                                                     plotly::plotlyOutput("bar_course_apr_plot"))
                                                 ) # 2nd row
                                     ), #course tabname/2nd row
                             # sankey tab
                             tabItem(tabName= "flowchart", 
                                     fluidRow(
                                                column(2,selectInput("movecohort","Choose a Cohort", choices = sort(unique(new_sankeydata_v1$Cohort)), selected = NULL )),
                                                column(2,selectInput("movelevel","Choose a Category", choices =c("College","Department","Program"), selected = NULL, multiple = FALSE )),
                                                column(2,selectInput("choosedirection", label = "Choose a Direction", choices = c("Forward","Backward"), selected = NULL)),
                                                column(2,selectInput("moveentry","Choose an Entry/Exit", choices = NULL , selected = NULL )),
                                                column(2,numericRangeInput(inputId = "sankeydirectionhsgpa", label = "Choose High School GPA", value = c(0.00,5.04), separator = " to ", step=0.01, width = "100%", min=0.00, max = 5.04 )), 
                                                column(2,"Click to refresh", br(), actionButton("entrysankey", label = "Start Flow Graph Update", class = "btn-lg btn-success")),
                                                bsTooltip(id="choosedirection", title = "Forward: entry-to-exit, Backward: exit-to-entry",
                                                placement = "right",  trigger = "hover",options = list(delay = list(show=1000, hide=3000 )))
                                                ), #1st flow
                                      #sankey updated: 1st row is filters and description. 2nd row is plot
                                     fluidRow(
                                         column(8,  box(width = 12,#title = "Moving In/Out", solidHeader = T, status = "success",
                                          networkD3::sankeyNetworkOutput("sankey_moving", width = "100%",  height = "800px")
                                           )),
                                         column(4,box(width = 12, title= "Flow graph Details:",status = "warning",solidHeader = T,# background = "black",
                                                      p("The path from entry to degree time such as direction of switching colleges, departments, or programs by choosing a category", style = "font-family: 'times'; font-si16pt"),
                                                      p("The importance of factors including between entry to 2nd Fall such as 1st term GPA or APR, high school GPA, and gender", style = "font-family: 'times'; font-si16pt"),
                                                      p("The path from entry to graduation time by choosing the Forward direction", style = "font-family: 'times'; font-si16pt"),
                                                      p("Previous paths leading to a final category by choosing the Backward direction", style = "font-family: 'times'; font-si16pt"),
                                                      p("How categories are linked with each other by counts and proportion", style = "font-family: 'times'; font-si16pt"),
                                                      br(),
                                                      strong("Terms:"),
                                                      p(span("Entry:", style= "color:blue"), "name of college, department, or program for the", strong(" first Fall"),style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Exit:", style= "color:blue"), "name of college, department, or program at", strong("graduation"),style = "font-family: 'times'; font-si16pt"),
                                                      p(span("2nd to 4th Fall:", style= "color:blue"), "continuing in each consecutive following Fall",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Degree:", style= "color:blue"), "name of degree for graduating college, department, or program",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Graduation Time:", style= "color:blue"), "graduated within 4-year, 5-years, 6-years, or no degree by Fall 2021",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Nodes:", style= "color:blue"), "starts entry groups and splits into different groups between entry to graduation time",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Links:", style= "color:blue"), "the thinkness of each line is proportional to the number students between nodes",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("No Degree:", style= "color:blue"), "no bachelor's degree by Fall 2021",style = "font-family: 'times'; font-si16pt"),
                                                      p(span("Stopout:", style= "color:blue"), "did not enroll in that Fall (either of the following)",style = "font-family: 'times'; font-si16pt"),
                                                      p(strong("Stopout to Degree name:"), "early graduation"),
                                                      p(strong("Stopout to NoDegree:"), "dropped out without degree"),
                                                      p(span("NA:", style="color:blue"), "no available information after withdrawing from all courses that semester/academic year",style = "font-family: 'times'; font-si16pt"),
                                                      br(),
                                                      em("Note: some numbers won't exactly match other sources since the time the data was downloaded",style = "font-family: 'times'; font-si16pt")
                                                      
                                                      )
                                         )
                                         ), # sanley plot and discription box
                                     #sankey original to replaced  with migration
                                     fluidRow(
                                         strong("Flow Graph Data filterd by Cohort and Entry"),
                                         box(width = NULL,title = "Entry to Exit", solidHeader = T,  
                                              DT::DTOutput("flowtable",  width = "100%",  height = "800px"))
                                     )
                                     ),  #program sankey tab
                                     
 
                             # summary tab
                             tabItem(tabName = "metricapr", 
                                      
                                  fluidRow( 
                                      column(width = 2, 
                                             
                                             box( title="Entry College",status="primary", width = NULL, solidHeader = T,background = "black",
                                                 selectizeInput(inputId = "metriccollege", 
                                                                label = "ENTRY COLLEGE or UWF", 
                                                                choices = c("UWF", sort(unique(sankey_data$ENTRY_COLLEGE))),
                                                                selected = "HMCSE", multiple=F ),
                                                 #high school gpa numeric input                  
                                                 numericInput(inputId = "barcharthsgpa1",
                                                             label = "Minimum Allowed High School GPA Thresholds (>= 2.00)", step = 0.01, #dragRange = F,
                                                             value = 0.00,  min=minhsgpa, max = maxhsgpa ), 
                                                 numericInput(inputId = "barcharthsgpa2",
                                                                      label = "Maximum Allowed High School GPA Thresholds (<= 5.04)",step = 0.01, #dragRange = F,
                                                             value = 5.04, min=minhsgpa, max = maxhsgpa ),
                                                 "Choosing the high school GPA thresholds",br(),
                                                 "will update performance rates and counts",br(),
                                                 "(Replaced NAs with 0.00)"
                                                 
                                                 )),                                      
                                      
                                      column(width = 10, 
                                              box(width = NULL, title = "Performance by Cohort", status = "primary", solidHeader = T,
                                                    actionButton("uwftable", label = "Show Updated Performance Metrics"),
                                                  DT::DTOutput("cohortaprtable"),
                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                           )
                                    #radar
                                    # column(width = 6,
                                    #             box(width = NULL, title = "Comparison Metrics", background = "black",
                                    #              selectInput(inputId = "radarcohort",  
                                    #                            label = "Select Cohort",  
                                    #                            choices = c(2015:2020), selected = "2015"),
                                    #                actionButton("radarchart", label = "COLLEGE RADAR"),
                                    #                plotly::plotlyOutput("radarapr1", height = "300px")))
                                    #            
                                    #        
                                        
                                        ),
                                    
                                     
                                 #entry department metrics           
                                 fluidRow( 
                                      box(width = 2, title= "Entry Departments and Metrics", solidHeader = T, status = "primary", background = "black",
                                            "Please Select a College from ENTRY COLLEGE Control",
                                            selectInput(inputId = "bardepartment", 
                                                        label ="SELECT DEPARTMENT", 
                                                        choices = NULL),
                                            selectInput(inputId = "barchartmetric", 
                                                        label ="SELECT METRIC", 
                                                        choices = c("RETENTION","APR","GRADUATIONin4YRS"))
                                  ),
                                      box(width = 10, title = "Performance by Entry Department",solidHeader = T, status = "primary",
                                             actionButton("barchartrall", label = "Show Updated Department Metrics"),
                                                plotly::plotlyOutput("barchartMETRIC"))
                                             
                                    ), # 2nd row
                                 #program chart by cohort
                                 fluidRow(
                                     box(width = 2, title= "Entry Programs by Cohort", solidHeader = T, status = "primary", background = "black",
                                         "Please Select a Metric from SELECT METRIC Control",
                                         selectInput(inputId = "cipcohort", 
                                                     label ="SELECT COHORT", 
                                                     choices = 2015:2020, selected = 2015),
                                         br(),
                                         "Summary Table by CIP with updated High School GPA Thresholds",
                                         br(),
                                         actionButton("summaryMetric", "Summary Table")
                                         # selectInput(inputId = "barchartmetric", 
                                         #             label ="SELECT METRIC", 
                                         #             choices = c("RETENTION","APR","GRADUATIONin4YRS"))
                                     ),
                                     box(width = 10, title = "Performance by Entry Program",solidHeader = T, status = "primary",
                                         actionButton(inputId = "barchartcip", label = "Show Updated Metrics"),
                                         plotly::plotlyOutput("barchartCIPMETRIC")
                                            
                                            )
                                     ),# 3rd row
                                #program table for download
                                fluidRow(
                                    box(width = 12, #actionButton(inputId = "downloadsummaryMetric", label = "Show Updated CIP Metrics"),
                                        DT::DTOutput("summetrictable"),
                                        style = "height:500px; overflow-y: scroll;overflow-x: scroll;")) # 4th row
                             )#tabitem
                                    ) #tabitems  
                             
                         
                      )# dashboardbody

)
#end-ui



#start-server

server <- function(input, output, session) {
    
### Sankey category  and entry or final
    movedata <- reactive({
        
       m_long <-  new_sankeydata_v1 %>% select(College, Department, Program, Cohort) %>% #filter(Cohort != 2021) %>% 
            tidyr::pivot_longer(c("College","Department","Program"), names_to = "level", values_to = "entry") %>% 
           filter(level == input$movelevel) %>% 
           data.frame()
            
    })
     
     observeEvent(movedata(), {
         choice <-  sort(unique(movedata()$entry))
         shiny::updateSelectInput(inputId ="moveentry", choices = choice)
         #shiny::updateSelectInput(inputId ="movefinal", choices = choice)
       })

  
     move.HSGPA <- reactive({
        sankey.hsgpa.filtered <- new_sankeydata_v1 %>%
                                 filter(Cohort == input$movecohort) %>% 
                                 mutate(GPA_HIGHSCHOOL = ifelse(is.na(GPA_HIGHSCHOOL), 0, GPA_HIGHSCHOOL)) %>% 
                                 mutate(response = ifelse(((College == input$moveentry) |(Department == input$moveentry)|(Program == input$moveentry)), "Yes", "No")) %>% 
                                    filter(response == "Yes")
         })
     
     observeEvent(move.HSGPA(), {
         minrange = min(move.HSGPA()$GPA_HIGHSCHOOL, na.rm = T)
         maxrange = max(move.HSGPA()$GPA_HIGHSCHOOL, na.rm = T)
         shiny::updateNumericInput(inputId ="sankeydirectionhsgpa", min = minrange, max= maxrange)
     })
     
     
      sankey_1st_data <- reactive({
         # entry data
         move1 <- new_sankeydata_v1  %>%  #filter(Cohort != 2021) %>%
             arrange(Stu_UWFID, Fall_ID) %>%
             dplyr::select(  Stu_UWFID, FALL_UWF,  "level"=input$movelevel,
                             Stu_Gender,Cohort, GPA_HIGHSCHOOL, Gradu_Time, TermGPA,EnteringFallGPA,APR)
         # degree info
         mov1_deg <- new_sankeydata_v1  %>%  #filter(Cohort != 2021) %>%
             dplyr::select( Stu_UWFID,  contains("deg") ) %>% group_by(DEGREECIPTitle) %>% ungroup() %>%
             select(Stu_UWFID, "College"=Deg_College, "Department"=Deg_Department,"Program"=Deg_ProgramCIPDesc) %>%
             filter(!duplicated(Stu_UWFID)) %>% select(Stu_UWFID, input$movelevel)
         #merge two
         two_sankey_data <- merge(move1,mov1_deg, by="Stu_UWFID", all.x=T )

         #6854
         move2 <- two_sankey_data %>%   #remove level column
             filter(GPA_HIGHSCHOOL >= input$sankeydirectionhsgpa[1] &  GPA_HIGHSCHOOL <= input$sankeydirectionhsgpa[2]) %>% 
             tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(level ,TermGPA,EnteringFallGPA )) %>%
             mutate(Fall1GPA =  ifelse( TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 "),
                    Year1GPA = ifelse( EnteringFallGPA_UWFFALL2 >= 2.00, "Above2.00","Below2.00") ) %>% filter(!duplicated(Stu_UWFID)) %>%  #6851
             mutate_at(c(8:11), replace_na, "Stopout") %>% filter(!is.na(level_UWFFALL1))
         move3 <-  setNames(move2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)],
                            c("ID","GEN", "Cohort","HSGPA","GRD","APR", "DEG",
                              "VAR1","VAR2","VAR3","VAR4",
                              "GPA1", "GPA2", "GPA3", "GPA4",
                              "UWF1","UWF2","UWF3","UWF4", "GPA","UWF"
                            ))

                #print(move3)
     })
     
     sankey_2nd_data <- reactive({             
                #test_flow_choose <- sankey_filtering_DF()[(sankey_filtering_DF()$Cohort == input$movecohort) & (sankey_filtering_DF()$VAR1 == input$moveentry),]    
               if (input$choosedirection =="Forward") {
                   test_flow_choose <- sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$VAR1 == input$moveentry),] 
               } else{
                   test_flow_choose <- sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$DEG == input$moveentry),] 
               }
     })
    sankey_3rd_data <- reactive({
        
                test_flow_filtered <- sankey_2nd_data() %>% 
                        dplyr::mutate(   VAR1 =  paste0(VAR1, "_Y1", sep=""),
                                         VAR2 =  paste0(VAR2, "_Y2", sep=""),
                                         VAR3 =  paste0(VAR3, "_Y3", sep=""),
                                         VAR4 =  paste0(VAR4, "_Y4", sep=""),
                                         GPA  =  paste0(GPA,  "Tgp", sep=""),
                                         APR  =  paste0(APR,  "apr", sep=""),
                                         GEN  =  paste0(GEN,  "GEN", sep=""),
                                         UWF  =  paste0(UWF,  "Fgp", sep=""),
                                         DEG  =  paste0(DEG,  "DEG", sep=""),
                                         GRD  =  paste0(GRD,   "GRD",sep=""))
               
             
                col1 <- test_flow_filtered %>%  
                    group_by(VAR1, GEN ) %>%  dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
                
                col1_apr <- test_flow_filtered %>%  
                    group_by(GEN, GPA ) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
                
                col2 <- test_flow_filtered %>%   
                    group_by(GPA,  APR ) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
                
                col3 <- test_flow_filtered %>%  
                     group_by(APR, VAR2) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% na.omit() %>% 
                     arrange(-n) %>%  select( In= 1, Out= 2, 3)
                
                col4 <- test_flow_filtered %>%  
                    group_by(VAR2, VAR3)  %>% dplyr::summarise(n =n(), .groups ="drop") %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
                
                # create flow APRto2
                col5 <- test_flow_filtered %>% 
                    group_by(VAR3, VAR4) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
                
                #create flow2to3
                col6 <- test_flow_filtered %>% 
                    group_by(VAR4, DEG ) %>% dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
                
                col7 <- test_flow_filtered %>%
                    group_by(DEG, GRD) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>%  arrange(-n) %>%  select( In= 1, Out= 2, 3)
                
                #create data frame 
                up_flow_data <- rbind(col1, col1_apr, col2, col3, col4, col5, col6, col7 ) 
                print(up_flow_data)
   
        })

     sankey_plot <- eventReactive(input$entrysankey,{
         
                # create nodes and links
                #links
                nodes_FD <- sankey_3rd_data() %>%  select(In, Out) %>% 
                        pivot_longer(c("In","Out"), names_to = "col_name",
                        values_to = "name_match") %>% select(-1) %>% distinct() %>% 
                        mutate(name = str_sub( name_match, end=-4)) %>% as.data.frame()
                nodes_FD$group <-  as.factor( gsub(" ", "-",nodes_FD$name))
                # nodes
                plot_id_FD <- sankey_3rd_data() %>% 
                        mutate( IDIn = match( In, nodes_FD$name_match)-1,
                        IDOut = match(Out, nodes_FD$name_match)-1,
                        Freq =n ) %>% as.data.frame()
                
                #sankey chart
                upsankey <-  sankeyNetwork(
                        Links = plot_id_FD, Nodes = nodes_FD,
                        Source = "IDIn",
                        Target = "IDOut",
                        Value = "Freq",
                        NodeID = "name",
                        #colourScale = my_college_color, 
                        NodeGroup = "group",#LinkGroup = "group",
                        sinksRight = F, iterations = 0,
                        fontSize =10, fontFamily = "Arial",
                        nodeWidth = 40, nodePadding = 25
                )

                htmlwidgets::onRender(upsankey, 'function(el2) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Entry","Gender","1st Term GPA", "APR","2nd Fall","3rd Fall","4th Fall","Degree","Graduation Time"];
    cols_x.forEach((d, i) => {
      d3.select(el2).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 15)
        .text(labels[i]);})}')

      })
    output$sankey_moving <- renderSankeyNetwork(sankey_plot()) # need add error message
           
    #### migration data table
             
    flowdata <- reactive({
    flowdatatable <-  sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$VAR1 == input$moveentry),] 
    
    })
    # stop here to update the flow table
    output$flowtable <- renderDT({
        
        FlowTable <- flowdata() %>% 
            select(Cohort,"GPA_HIGHSCHOOL"=HSGPA,GPA1,"Entry"=VAR1,  "Year2"=VAR2,"Year3"=VAR3,
                   "Year4"=VAR4,"FinalDegree"=DEG,"GraduationTime"=GRD ) %>% 
            group_by(Cohort,Entry,Year2,Year3,Year4,FinalDegree,GraduationTime) %>% 
            mutate(GPA_HIGHSCHOOL = ifelse(GPA_HIGHSCHOOL == 0 , NA, GPA_HIGHSCHOOL)) %>% 
            mutate(GPA1 = ifelse(GPA1 == 0, NA, GPA1)) %>% 
            dplyr::summarise(  MinHSGPA=min(GPA_HIGHSCHOOL, na.rm=T),
                               MeanHSGPA= round(mean(GPA_HIGHSCHOOL, na.rm=T),2),
                               MaxHSGPA=max(GPA_HIGHSCHOOL, na.rm=T) , 
                               MeanUWFGPATerm1= round(mean(GPA1, na.rm=T),2),
                               Count = n(), .groups= "drop") %>% ungroup() %>% arrange(-Count) 
        FlowTable$TotalCount <-  rep(sum(FlowTable$Count))
        FlowTable$FinalStatus_Rate <- paste0(round(FlowTable$Count/FlowTable$TotalCount,4)*100, "%", sep="") 
        
        datatable(FlowTable,  extensions = "Buttons",
                  filter = "top",
                  options = list(dom="Blfrtip",
                                 buttons=c("copy","csv","excel","pdf","print"), 
                                 lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>% 
            formatStyle('FinalStatus_Rate',
                        color = styleInterval(50, c('gray', 'white')),
                        backgroundColor = styleInterval(50, c('whtie', 'gray')))
        
    })
    
    
    
  ### APR/Graduation/Summary tab ####
   
    cip_college <- reactive({
        sankey_data[(sankey_data$ENTRY_COLLEGE == input$metriccollege),] 
    })
    
    observeEvent(cip_college(), {
        choice <- sort(unique(cip_college()$Cohort))
        shiny::updateSelectInput(inputId ="cipcohort", choices = choice)
    })
    
    metricbycohort <- reactive({
        
        temp_df <- cip_college() %>%  
            dplyr::filter(ENTRY_COLLEGE == input$metriccollege) %>% 
            filter(Cohort == input$cipcohort )  %>% 
            filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
        
    })
    
    # Bar Chart for CIP Metrics
    CipMetricTablebyCohort <-  eventReactive(input$barchartcip,{
        
        notapr=c("Dropped","WithdrawnFTIC")
        m_bar_cip <-  metricbycohort() %>%  
            mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
            mutate(GRADUATIONin4YRS = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
            select("ENTRY_PROGRAM"=UWFFall1CIPTitle,  "Metrics"=input$barchartmetric) %>% 
            group_by( ENTRY_PROGRAM,  Metrics) %>%  
            dplyr::summarise(Count =n(), .groups = "drop") %>% # count() %>%  #group_by( Cohort,  Metrics) %>% #ENTRY_DEPARTMENT,
            tidyr::pivot_wider(names_from = Metrics, values_from = Count, values_fill = 0) %>% 
            mutate(MetricCount = Yes) %>% rowwise() %>% 
            mutate(CohortSize = sum(No, Yes)) %>%
            mutate_at(vars(2:3), ~(round(./CohortSize*100,2))) %>%
            mutate( MetricPercent = paste(Yes, "%", sep=" ")) %>% 
            select(ENTRY_PROGRAM, MetricCount,CohortSize,MetricPercent,Yes)
        #help#
        ytext <- m_bar_cip$CohortSize
        y1mp <- m_bar_cip$MetricCount
        m.ypp <- m_bar_cip$MetricPercent
        y1mpp <- round(m_bar_cip$MetricCount/m_bar_cip$CohortSize*100,2)# percentage digit 2
        per_y1 <-  list( tickfont = list(color = "red"), overlaying = "y1mpp", side="right", title = "<b> % of performance")
        
        #bar plot
        bar_cip <-  plotly::plot_ly(m_bar_cip, x= ~reorder(ENTRY_PROGRAM, -CohortSize), y = ~CohortSize, type = "bar",name = "Cohort Size", text= ytext,
                                       textposition="auto",
                                       marker = list(color = "#007A33" ) )
           
        bar_cip %>%  
            add_trace( y = ~MetricCount, text = y1mp, name = input$barchartmetric , marker = list(color = "#004C97")) %>%
            add_trace( y = ~y1mpp, text= m.ypp,  name = "Performance (%)", marker = list( color = "#009CDE")) %>% #, yaxis = "y2", mode = "line+markers", type ="bar"
            layout(title = "",
                   xaxis = list( title="", tickfont = list(size = 12, color = "darkgreen")),
                   yaxis = list( title= paste(input$barchartmetric, "(Count)", sep=" "),
                                 titlefont = list(size = 12, color="red"), tickfont = list(size= 12, color="blue")),
                   yaxis2 = per_y1 ,
                   legend = list(x = 1, y= 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                   barmode = "group", bargap =0.15) %>% 
            config(displaylogo = FALSE)
        

      
    })
    
     output$barchartCIPMETRIC <- renderPlotly(CipMetricTablebyCohort())
        
 ######### DOWNLOAD RAW METRIC TABLE ######
        SUMMetricTable <- reactive({
            
            all_temp_df <-  sankey_data  %>% 
                filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
        }) 
        

     #output$metrictable <- downloadHandler(
        metriciptable1 <-  eventReactive(input$summaryMetric,{
            
            notapr=c("Dropped","WithdrawnFTIC")
            #retention
            rent_cip <-  SUMMetricTable() %>%   
                mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, RETENTION) %>% count() %>% 
                tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
                replace(is.na(.), 0) %>% mutate(RETENTION = Yes)  %>% select(-No, -Yes)
            #apr
            apr_cip <-  SUMMetricTable() %>% 
                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, APR) %>% count() %>% 
                tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
                replace(is.na(.), 0) %>% mutate(APR = Yes) %>% select(-No,-Yes)
            apr_rent_cip <- merge(rent_cip,apr_cip, by=c("ENTRY_COLLEGE","ENTRY_DEPARTMENT","ENTRY_PROGRAM", "Cohort" ), all.x = T)
            #graduation 
            gradu_cip <-  SUMMetricTable() %>% 
                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, Gradu_Time) %>% count() %>% 
                tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
                replace(is.na(.), 0) %>% 
                mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 
            
            all_cip_df <- merge(apr_rent_cip, gradu_cip,  by=c("ENTRY_COLLEGE","ENTRY_DEPARTMENT","ENTRY_PROGRAM", "Cohort" ), all.x = T)
            
            all_cip_df1 <-   all_cip_df %>% data.frame() %>% 
                mutate(across(c(5:10), ~round(.x/CohortSize*100,2), .names = "{col}%")) %>%
                select(1:8,10,9,11:17) # check location NoDegree%
            

            DT::datatable(all_cip_df1[,c(1:4,11,5:10,12:17)],  extensions = "Buttons",
                          filter = "top",
                          options = list(dom="Blfrtip",
                                         buttons=c("copy","csv","excel","pdf","print"),
                                         lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25))
        })
        
        output$summetrictable  <- renderDT(metriciptable1())
        
            
        #     filename = function() {
        #         paste("data-", Sys.Date(), ".csv", sep="")
        #     },
        #     content = function(file){
        #         shiny::withProgress(
        #             message = paste0("Downloading"),
        #             value = 0,
        #             {
        #                 shiny::incProgress(1/10)
        #                 Sys.sleep(1)
        #                 shiny::incProgress(5/10)
        #         
        #         write_csv(summary_cip(), file, row.names = F)
        #     }
        #     )
        #     }
        # )
        
        
        # 
        # output$barcharCIPMETRIC <- eventReactive(input$barcharcip,{
        # 
        #     MetricTabledf() %>%
        #         filter(ENTRY_COLLEGE == input$metriccollege[-1] & Cohort == input$cipcohort )
        # 
        # 
        # })


    #     r <- purrr::map_dbl(collegename[, 2:7], ~.x)
    #     nms <- names(r)
    #     
    #     #code to plot the radar
    #     plot_ly(
    #         type = 'scatterpolar',
    #         r = r,
    #         theta = nms,
    #         fill = 'toself', fillcolor="#4169E1",opacity=0.6,
    #         mode = 'markers'
    #         )   %>%
    #         plotly::layout(  
    #             polar = list(
    #                 radialaxis = list(
    #                     visible = T,
    #                     range = c(0,max(r)))), showlegend = F)
    #     
     
 

   #output5
   # Bar chart for department in summary tab
   bar_college <- reactive({
       sankey_data[(sankey_data$ENTRY_COLLEGE == input$metriccollege),] 
   })
   
   observeEvent(bar_college(), {
       choice <-  unique(bar_college()$ENTRY_DEPARTMENT)
       shiny::updateSelectInput(inputId ="bardepartment", choices = choice)
   })

   metricdf_bar <- reactive({
       
       temp_df <- bar_college() %>%  
           filter(ENTRY_COLLEGE == input$metriccollege) %>% 
          filter(ENTRY_DEPARTMENT == input$bardepartment)  %>% 
           filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))

   })
  
    barchart_METRIC_test <- eventReactive(input$barchartrall,{

        notapr=c("Dropped","WithdrawnFTIC")
         bardf <-   metricdf_bar() %>%
               mutate(GRADUATIONin4YRS = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
               mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr, "No","Yes")) %>%
               select(Cohort,  "Metrics"=input$barchartmetric) %>% 
               group_by( Cohort,  Metrics) %>%  
               count() %>%   
               tidyr::pivot_wider(names_from = Metrics, values_from = n, values_fill = 0) %>% 
               arrange(Cohort) %>% 
               mutate(MetricCount = Yes) %>% rowwise() %>% 
               mutate(CohortSize = sum(No, Yes)) %>%
               mutate_at(vars(2:3), ~(round(./CohortSize*100,2))) %>%
               mutate( MetricPercent = paste(Yes, "%", sep=" ")) %>% 
               select(Cohort, MetricCount,CohortSize,MetricPercent) #check  
         ymp= round(bardf$MetricCount/bardf$CohortSize*100,2)
         ycs= bardf$CohortSize
         ymc= bardf$MetricCount
         ypp= bardf$MetricPercent
         per_y <-  list( tickfont = list(color = "red"), overlaying = "ymp", side="right", title = "<b> % of performance")
         #bar plot
        bar_depart <-  plotly::plot_ly(bardf, x= ~Cohort, y = ~CohortSize, type = "bar",name = "Cohort Size", text= ycs,
                         textposition="auto",
                         marker = list(color = "#007A33" )
                         )  
        bar_depart %>%  add_trace( y = ~MetricCount, text = ymc, name = input$barchartmetric , marker = list(color = "#004C97")) %>%
                        add_trace( y = ~ymp,text=ypp,  name = "Performance (%)", marker = list( color = "#009CDE")) %>% #, yaxis = "y2", mode = "line+markers", type ="bar"
                        layout(title = "",
                             xaxis = list( title="", tickfont = list(size = 12, color = "darkgreen")),
                             yaxis = list( title= paste(input$barchartmetric, "(Count)", sep=" "),
                                    titlefont = list(size = 12, color="red"), tickfont = list(size= 12, color="blue")),
                             yaxis2 = per_y ,
                            legend = list(x = 1, y= 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                            barmode = "group", bargap =0.15) %>% 
                        config(displaylogo = FALSE)
    })
 
   output$barchartMETRIC <- renderPlotly(barchart_METRIC_test())
   
    

   ### Table for colleges or uwf
   UWFtable_all <-  reactive({
      rows <- sankey_data %>% 
              mutate(test= (input$metriccollege == "UWF" | sankey_data$ENTRY_COLLEGE == input$metriccollege), .groups="drop" )
      apr_table <-  sankey_data[rows$test,,drop= FALSE]  
      apr_table_filter <- apr_table %>%  filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
    })

    
    UWF_Table <- eventReactive(input$uwftable,{
        
        notapr= c("Dropped","WithdrawnFTIC")
       rent_college <-  UWFtable_all() %>%   
           mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
           group_by( Cohort, RETENTION) %>% count() %>% 
           tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
           replace(is.na(.), 0) %>% mutate(RETENTION=Yes)  %>% 
           select( -Yes,-No)
       
       rapr_college <-  UWFtable_all() %>% 
           group_by( Cohort, APR) %>% count() %>% 
           tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
           replace(is.na(.), 0) %>% mutate(APR = Yes) %>% 
           select( -Yes,-No) 
       
       apr_rent <- merge(rent_college,rapr_college, by=c("Cohort" ), all.x = T)
       
       gradu_college <-  UWFtable_all() %>% 
           group_by( Cohort, Gradu_Time) %>% count() %>% 
           tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
           replace(is.na(.), 0) %>% rowwise() %>% 
           mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 
       
       all_df <- merge(apr_rent, gradu_college,  by=c("Cohort" ), all.x = T)
       
        gradu <-  all_df %>%  mutate(Grad5yrs = (Grad4yrs + Grad5yrs) , Grad6yrs  = (Grad5yrs + Grad6yrs))
  
        #final table
        final_metric_df <-   gradu %>%  rowwise() %>% 
            mutate(across(c(2:7), ~round(.x/CohortSize*100,2), .names = "{col}%")) %>% 
            select(1,8,2:7,9:14)
    
         datatable(final_metric_df) 
          
    })   
    
   output$cohortaprtable  <- renderDT(UWF_Table()) 
   
     ### COURSES TAB  
 
   course_apr <- reactive({
       
       crsapr_df <- course_data_v1[,-1] %>%
            filter(ENTRY_PROGRAM == input$courseprogram) %>%
            filter(Cohort == input$couresecohort)
   })
  
   observeEvent(course_apr(), {
       choice_crsname <-  sort(unique(course_apr()$CRSE_NAME))
       shiny::updateSelectInput(inputId = "coursename", choices = choice_crsname)
       choice_crsterm <-  sort(unique(course_apr()$crs_DEMO_TIME))
       shiny::updateSelectInput(inputId = "courseterms", choices = c("(All)",choice_crsterm))
       choice_topcrsterm <-  sort(unique(course_apr()$crs_DEMO_TIME))
       shiny::updateSelectInput(inputId = "topcrsselectterms", choices = c("(All)",choice_topcrsterm))
   })
     
    course_pass <- reactive({
    
    topcrs_df <- course_data_v1[,-1] %>%
    mutate(topcrsall = (input$topcrsselectterms == "(All)" |  course_data_v1$crs_DEMO_TIME == input$topcrsselectterms))
    
    topcrs_t <-  topcrs_df[topcrs_df$topcrsall,,drop= FALSE] %>%
        filter(Cohort == input$couresecohort)  %>%
        filter(ENTRY_PROGRAM == input$courseprogram) %>%
        filter((GPA_HIGHSCHOOL >= input$crsgparange1) & (GPA_HIGHSCHOOL <= input$crsgparange2))  
    
    })

   # output course pass plot
  bar_course_pass <- eventReactive(input$frerqCRS,{
    freqcrs_df <-  course_pass()  %>%  
      group_by(CRSE_NAME,CRS_PASS ) %>%  
        count() %>% 
      tidyr::pivot_wider(names_from =  CRS_PASS, values_from =  n) %>% 
      replace(is.na(.), 0) 
    
  freqcrs_df$CRSCount <-  rowSums(freqcrs_df[,2:3])
  
  freqcrs_df1 <-  freqcrs_df  %>% data.frame() %>%  
      arrange(-CRSCount) %>% slice(1:25) %>% 
      mutate_at(vars(2:3), list(Prop =  ~ ./CRSCount*100 )) %>% 
      mutate(across(where(is.numeric), round, 2)) 
 
  crs_y1 <- paste(freqcrs_df1$Passed_Prop, "%", sep= " ")
  crs_y2 <- paste(freqcrs_df1$Failed_Prop, "%", sep= " ")
  
  plot_ly(freqcrs_df1, x = ~reorder(CRSE_NAME,-CRSCount), y = ~Passed , name="Passed"  , 
                          text = crs_y1,
                          type="bar", marker = list(color="#669900"))  %>%   
          add_trace( y = ~Failed  ,  marker = list(color="#FF3399"), name ="Failed",
                 text = crs_y2 , 
                 textposition ="auto") %>% 
       layout( xaxis = list(title =paste(input$topcrsselectterm,"Courses", sep =""), titlefont = list(size = 12, color="blue")), # add input xaxis
              yaxis = list(title ="Enrolled (Count)",titlefont = list(size = 12, color="green")), barmode ="group")
      
  })
    
   output$bar_course_pass_plot <- renderPlotly(bar_course_pass())
   
### APR by course grades
   course_gradeapr <- reactive({
       crs_df <- course_data_v1 %>% 
           mutate(crsall = (input$courseterms == "(All)" |  course_data_v1$crs_DEMO_TIME == input$courseterms))
       crs_t <-  crs_df[crs_df$crsall,,drop= FALSE] %>%  
           filter(ENTRY_PROGRAM == input$courseprogram) %>% 
           filter(Cohort == input$couresecohort)  %>% 
           filter((GPA_HIGHSCHOOL >= input$crsgparange1) & (GPA_HIGHSCHOOL <= input$crsgparange2)) %>% 
           filter(CRSE_NAME == input$coursename)   
          
   }) 
 

bar_course_apr <- eventReactive(input$aprgradeCRS,{
    
    aprcrs_DF <-  course_gradeapr()  %>% mutate(APR = factor(APR)) %>%  
        group_by( GRADE_AWARDED, APR) %>% count() %>% 
        plot_ly(x = ~GRADE_AWARDED, y =~n, color = ~APR, type="bar",  colors = c("#DE3163","#004C97","#004C97") ) %>%  
            layout( xaxis = list(title ="Course Grades", titlefont = list(size = 12, color="blue")),
                    yaxis = list(title ="Enrolled (Count)",titlefont = list(size = 12, color="green")),
                    barmode ="group",
                    legend=(list(title=list(text='<b> APR </b>'))))  
        
 
})
 
  output$bar_course_apr_plot <- renderPlotly(bar_course_apr())   

  
  
      
} #end-sever

shinyApp(ui = ui, server = server)
