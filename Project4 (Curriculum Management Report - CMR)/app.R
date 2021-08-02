#  Author : Chewick (chethana Wickramasinghe)
#  Date : 6/26/2020
#  This app will help generate course and program level reports

# Making sure all the required packages are installed before runni ------------------------------


list_of_packages_required <- c("openxlsx","utils","dplyr","tidyverse","scales","plotly"
                               ,"DT","visNetwork","igraph","shiny","shinydashboard","shinyWidgets", "shinyTree", "shinythemes")
count=0
for (i in list_of_packages_required){
  if(! i %in% installed.packages()){
    count=count+1
    print(paste(i, " package is required and will be installed for the app to run successfully"))
    install.packages(i)
  }else{
    print(paste(i, " package was already installed"))
  }
}

if(count > 0){
  print("NOTE: Addition packages are installed")
}else{
  print("hurray!!! All the required packages are pre-installed. Good to go")
}

# Libraries used ----------------------------------------------------------

library(openxlsx)
library(utils)
library(dplyr)
library(tidyverse)
library(scales)
library(plotly)
library(DT)
library(visNetwork)
library(igraph)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyTree)
library(shinythemes)

# Extracting data from excel file ------------------------------------------------------------

# file <- "/srv/shiny-server/CMR/data/Database.xlsx"
# file <- file.choose()
file <- ".\\data\\Database.xlsx"

courses <- openxlsx::read.xlsx(file, "Courses", sep.names = " ")

course_programs <- openxlsx::read.xlsx(file, "Course_Programs", sep.names = " ")

course_outcomes <- openxlsx::read.xlsx(file, "CO", sep.names = " ")

co_po <- openxlsx::read.xlsx(file, "CO_PO", sep.names = " ")

program_outcomes <- openxlsx::read.xlsx(file, "PO", sep.names = " ")

po_set <- openxlsx::read.xlsx(file, "PO_Set", sep.names = " ")

course_prerequisites <- openxlsx::read.xlsx(file, "Course_Prerequisites", sep.names = " ")

# Cleaning, modifying and joining data -------------------------------------------------------

# changing courses data
courses$`Kuali CM Links` <- paste0("<a href='",courses$`Kuali CM Links`,"'>",courses$`Course ID`,"</a>")# creating kuali hyperlinks

# changing the course_program sheet data from wide to long format
course_programs <-  reshape(course_programs, 
                            varying = c(colnames(course_programs)[-1.]),
                            v.names = "Membership",
                            timevar = "Program Type",
                            times = c(colnames(course_programs)[-1.]),
                            direction = "long") %>% 
    select(-id) %>% 
    remove_rownames() %>%
    drop_na()

# changing the co_po sheet data from wide to long format
co_po[is.na(co_po)] <- 0

co_po <- co_po %>% 
    pivot_longer(-c(`Course ID`, `CO ID`, `PO Set`), names_to = "PO ID", values_to = "Relevance")

# joining data
join_data <- inner_join(courses, course_programs, by="Course ID") %>% # courses <-> course_programs
    full_join(. , course_outcomes, by="Course ID") %>% # (courses <-> course_programs) <-> course_outcomes
    #drop_na(CO_ID) %>% # drop observations with no CO_IDs
    rename( `CO Description` = Description) %>% # rename column name Description to avoid any confusions when joining
    inner_join(. , co_po , by = c("Course ID", "CO ID")) %>% #  ((courses <-> course_programs) <-> course_outcomes) +<->co_op
    inner_join(. , program_outcomes, by= c("PO ID", "PO Set")) %>% # (((courses <-> course_programs) <-> course_outcomes) <-> co_op) <-> program_outcomes
    rename ( `PO Description` =  Description) %>% # rename column name Description to avoid any confusions when joining
    inner_join(. , po_set, by= "PO Set") %>% # ((((courses <-> course_programs) <-> course_outcomes) <-> co_op) <-> program_outcomes) <-> po_set
    rename( `POSet Description` = Description) # rename column name Description to avoid any confusions when joining

#formatting prerequisite data from long to wide
pre_req <- course_prerequisites %>% pivot_wider(names_from = "Course" , values_from = "Prerequisites") 

# setting up data to feed into ui ---------------------------------------------------

program_type_list <- as.list(unique(join_data$`Program Type`))

rubric_list <- as.list(unique(join_data$Department))


# functions ------------------------------------------------------------------------

# result <- data.frame("Course"=c("AP/ITEC 3003"), "Description"=c("sample")) # dummy dataframe to use in "get" funtion below
result <- data.frame("Course"=NA, "Description"=NA) # create an empty dataframe

get <- function(course){
    n = length(pre_req[[`course`]][[1]]) # number of prerequisites for the course passed through the function
    if (n != 0){ # if there's prerequistes associated with the course
        count = n
        while (count > 0){ # loop until zero prerequisites
            tmp_prereq <- pre_req[[`course`]][[1]][[count]] # temporary store current prerequisite
            
            if (tmp_prereq == "ITEC-GP"){ # special case when prereq == "ITEC-GP" because it is a combination of multiple courses
              tmp_prereq_co_descriptions <- course_outcomes %>% filter (`Course ID` %in% c("ITEC1000", "ITEC1010", "ITEC1620", "ITEC2610", "ITEC2620", "MATH1190","MATH2320","MATH2565")) %>% select(`Course ID`, Description) %>% rename(Course = `Course ID`) # getting the Course Oucomes (CO) associated with each course
              tmp <- tmp_prereq_co_descriptions # tables with both Course ID and Co Descriptions of each course
            }else{
              tmp_prereq_co_descriptions <- course_outcomes %>% filter (`Course ID` == tmp_prereq) %>% select(Description) # getting the Course Oucomes (CO) associated with the course
              if (length(tmp_prereq_co_descriptions[[1]]) == 0 ){ # output "NONE" if no COs
                tmp_prereq_co_descriptions = "NONE"
              }
              tmp <- data.frame("Course"=c(tmp_prereq), "Description"=c(tmp_prereq_co_descriptions))
            }
            
            tmp2 <- tmp %>% group_by(Course) %>% summarise(Description=paste0("-",Description,collapse="<br/>")) # combining multiple COs in a course
            
            tmp2 <- rbind(result, tmp2) # combine "result" and new "tmp2" dataframes
            result <- rbind(tmp2, get(tmp_prereq)) # "result" is now equal to newly combined dataframe containing prerequistes found
            count = count -1
        }
    }
    
    
    result <- unique(result) %>% drop_na(Course) # remove duplicates if exists and remove NA rows
    

    return(result)
}



# UI -------------------------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Course Report", tabName = "Course_Report"),
        menuItem("Program Report", tabName = "Program_Report")
    )
) # end sidebar

body <- dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css",href ="custom.css")),
    
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="head_title"> Curriculum Management Reports </span>\');
      })
     ')),
    
    tabItems(
        tabItem(tabName = "Course_Report",
                fluidPage(
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }" 
                    ),# to remove error messages showing in the App due to data will be changing according to request
                    
                    titlePanel(h1("Course Report" , style = "color:#1e282c; font-weight: bold;")),# abbrv. -> cr
                    
                    h2("Course Information"),
                    
                    fluidRow(box( width = 4, title = "Select a Course ", status = "primary" ,solidHeader = T,
                                 column(12,sidebarPanel(width = 600,
                                            pickerInput(
                                                inputId = "crProgramType",
                                                label = "Program",
                                                choices = (program_type_list),
                                                selected = program_type_list[1],
                                                multiple =FALSE,
                                                options = list(
                                                    `actions-box` =TRUE,
                                                    size=100
                                                )),
                                            uiOutput("crPICourseList"),
                                            uiOutput("crPIPoSet")         
                                        ))
                                 ), # end of box1
                             box( width = 8, title = "Course Selected", status = "primary" ,height = "340", solidHeader = T,
                                 column(12, DT::dataTableOutput('coursetable'), style = "height:270px; overflow-y: scroll;overflow-x: scroll;")
                                  ) # end of box2
                             ), # end of first fluidRow 
                    # end of Course Information Section
                    
                    h2("Mapping and Learning Outcomes"),
                    
                    fluidRow(box( width = "600",textOutput('coursePlotTitle'),  status = "primary", solidHeader = T,
                                column(12,plotOutput("coursePlot"))
                                  ) # end of box1
                             ), # end of second fluidRow
                    fluidRow(tabBox( width ="1200", 
                                tabPanel("Course Outcomes", box( width = "600",title = "List of Course Outcomes", status = "primary" , solidHeader = T, collapsible = T, collapsed =  T,
                                column(12,DT::dataTableOutput("courseoutcometable"),style = "height:250px; overflow-y: scroll;overflow-x: scroll;")
                                )), # end of box1
                                tabPanel("Mapping", box( width = "600",title = "Mapping to program Learning Outcomes (Details)", status = "primary" , solidHeader = T, collapsible = T, collapsed =  T,
                                column(12,DT::dataTableOutput("coursetable2"),style = "height:250px; overflow-y: scroll;overflow-x: scroll;")
                                )) # end of box1
                              ) # end of tabBox
                            ),  # end of third fluidRow
                    # Mapping and Learning Outcomes Section
                    
                    h2("Prerequisites"),
                    
                    fluidRow(box( width = 12, title = "Prerequisites", status = "primary", solidHeader = T,
                                column(12, visNetworkOutput("crPrereqTree", height = "700" ), downloadLink('crPrereqTreeHtml', 'Download .html file'))
                                ), # end of box1
                            box( width = 12, title = "Prerequisite Learning Outcomes", status = "primary" , solidHeader = T,  collapsible = T, collapsed =  T, #height = "520",
                                column(12, DT::dataTableOutput('prereqtable'), style = " height:450px; overflow-y: scroll;overflow-x: scroll;")
                                )  # end of box2
                            ), # end of forth fluidRow
                    # end of Prerequisites Section
                    
                    
                ) # main first sub fluidPage
        ), # end tabItem -> "Courese_Report"
        
        tabItem(tabName = "Program_Report",
                fluidPage(
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),### to remove error messages showing in the App due to data will be changing according to request

                    titlePanel(h1("Program Report", style = "color:#1e282c; font-weight: bold;")),
                    
                    fluidRow(box( title = "Select/Deselect Courses", status = "primary",height = "400" ,solidHeader = T,
                                 column(12,sidebarPanel(width = 600,
                                                        pickerInput(
                                                            inputId = "prRubric",
                                                            label = "Department", 
                                                            choices = rubric_list,
                                                            selected = "ITEC",
                                                            multiple =FALSE,
                                                            options = list(
                                                                `actions-box` =TRUE,
                                                                size=100
                                                            )),
                                                        uiOutput("prPIProgramType"),
                                                        uiOutput("prPICourseList"),
                                                        uiOutput("prPIPoSet")
                                        ))
                                ), # end of box1
                             box( title = "Courses Selected", status = "primary" , height = "400", solidHeader = T,
                                 column(12, DT::dataTableOutput('programtable'), style = "height:330px; overflow-y: scroll;overflow-x: scroll;")
                                 )# end of box2
                             ), # end of first fluidRow
                    fluidRow( tabBox( width ="1200", 
                                     tabPanel("Bar Graph", textOutput('programPlotTitle'),plotOutput("programPlot")), # tab1
                                     tabPanel("Heat Map", textOutput('programTilePlotTitle'),plotOutput("programtileplot")), # tab2
                                     tabPanel("Prerequisite Map", textOutput('prereqTitle'),  visNetworkOutput("prPrereqTree"), downloadLink('prPrereqTreeHtml', 'Download .html file')) # tab2
                                    ) # end of tabBox
                             ), # end of second fluidRow
                    fluidRow(box( width = "600",  title = "Mapping to program Learning Outcomes (Details)", status = "primary" ,solidHeader = T, collapsible = T,  collapsed =  T,
                                  column(12, DT::dataTableOutput("programtable2"),style = "height:250px; overflow-y: scroll;overflow-x: scroll;")
                                  ) # end of box1
                             )  # end of third fluidRow
                ) # main second sub fluidPage
        ) # end tabItem -> "Program_Report"
    ) # tabItems
) # end body


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "", titleWidth =10),
    sidebar,
    body
)


# SERVER -----------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ########################################### <Course Report Tab> ######################################
  
    output$crPICourseList <- renderUI({
        req(input$crProgramType)
      
        filter_Course_list <- join_data %>%
            filter (`Program Type` %in% req(input$crProgramType)) # filter data according to the Program Type selected

        course_list <- unique(as.list(paste0(filter_Course_list$`Course ID`,"-",filter_Course_list$`Course Description`))) # get the list of courses ("Course_ID-Course_Description") included in the Program Type
        
        pickerInput(
            inputId = "crCourseList",
            label = "Courses in the Program",
            choices = course_list,
            multiple = FALSE,
            options = list(
                `actions-box` =TRUE,
                size=100
            ))
    })
    
    output$crPIPoSet <- renderUI({
        req(input$crCourseList)
        req(input$crProgramType)
      
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course_ID
        list <- map(extract_Code,1) # extract only the Course_Id
        
        filter_Course_list <- join_data %>%
            filter (`Program Type` %in% input$crProgramType & `Course ID` %in% list) # filter data according to Program Type and Course Selected
        
        po_set_list <- unique(as.list(filter_Course_list$`PO Set`)) # get the list of PO_Sets available to choose from
        
        pickerInput(
            inputId = "crPoSet",
            label = "Program Learning outcome Set",
            choices = po_set_list,
            multiple =FALSE,
            options = list(
                `actions-box` =TRUE,
                size=100
            ))
    })
    
    get_data <- reactive ({ # extract data to plot learning outcome map
      
        req(input$crCourseList) # reason for adding this is to avoid warnings due to the outputs that depends on an input that is yet defined. Ref: https://stackoverflow.com/questions/55117519/hide-warnings-from-r-console
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course_ID
        list <- map(extract_Code,1)
        
        req(input$crProgramType)
        req(input$crPoSet)
        
        tmp <- join_data %>% 
            filter (`Program Type` == input$crProgramType & `Course ID` == list & `PO Set` == input$crPoSet) %>% # filter data according to the "Program Type", "Course", and "PO_Set" selected
            group_by(`PO ID`) %>% summarize(Contribution = mean(`Relevance`)) %>% # group data by the "PO_ID" and get the mean "Relevance" for each Po_ID
            arrange(`PO ID`) # arrange "PO_ID" in an order
        
        join <- right_join(tmp,program_outcomes %>% filter(`PO Set` == input$crPoSet)) # join filtered data with the "Program_Outcomes" data and further filter by the "PO_Set" selected
        join
    })
    
    get_display_data <- reactive ({ # extract the data to display in the "Course Selected" table
        req(input$crCourseList)
      
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course_ID
        list <- map(extract_Code,1)

        out <-  courses %>%
            filter (courses$`Course ID` %in% list) # filter the "courses" table to get the relative information about the course selected
        out
    })
    
    get_contibution_data <- reactive ({ # extract the data to display in the "Course Outcome and Program outcomes" table
        req(input$crCourseList)
        req(input$crProgramType)
      
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course ID
        list <- map(extract_Code,1)
        
        out <-  join_data %>%
            filter (`Course ID` %in% list & Relevance != 0 & `Program Type` == input$crProgramType) %>% # filter the data according to the "Program Type" and Course selected. Further only include data with Relavance not equal 0
            select (-`Kuali CM Links`, -Department, -`PO Set`, -`Short Description`, -`POSet Description`) # remove some of the columns

        unique(out) 
    })
    
    output$coursetable <- renderDataTable({ # output table contaning the information of the course selected
      
        output_data <- get_display_data() # get data
        
        names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
        
        output_data <- as.data.frame(t(output_data)) # transpose data to displat data vertically (easy to scroll vertically)

        datatable(output_data, colnames="", options = list(paging = FALSE, searching = FALSE), escape=F)
    })
    
    output$coursePlotTitle <- renderText({ # Change Plot Title according to the Course Selected
        
        req(input$crCourseList)
    })
    
    output$coursePlot <- renderPlot({ # create a bar graph to show the mapping of learning outcome
      req(input$crPoSet)
      
      join <- get_data() %>%
        arrange(`PO ID`) # obtain data and arrange/sort by "PO_ID"
      
      list <- as.list(unique(join$`PO ID`)) # get the list of unique "PO_ID"s available
      
      g <- ggplot(data=join, aes(x=factor(`PO ID` , level = rev(list)), y= `Contribution`, fill=`Contribution`, text=`Description`)) +
        geom_bar(stat="identity", fill="#92BAD3")+
        geom_text(aes(label=`Short Description`, y=1), hjust = 0, nudge_y = -1, size = 6)+
        labs(
          x = paste0(input$crPoSet," Program Outcomes"),
          y = "Contirbution")+ 
        theme(
          plot.caption = element_text(hjust = 0, size = 6),
          plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
          axis.title.x = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.title.y = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_blank()
        )+
        coord_flip()
      g
      
      # g<-ggplotly(g,tooltip = c("text"))
      g
    })
    
    output$coursetable2 <- renderDataTable({
      
      output_data <- get_contibution_data() # get the data to display in the "Course Outcome and Program outcomes" table
      
      names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
      
      # to fix Warning in processWidget(instance) 
      result = data.frame(output_data) 
      names(result) <- gsub(x = names(result),
                            pattern = "\\.",
                            replacement = " ")
      
      datatable(result,extensions = 'Buttons', 
                options = list(autoWidth = TRUE, paging = FALSE, dom = 'Blfrtip', buttons= list(list(extend = 'excel', filename = "download")), columnDefs = list(list(targets = c(which(colnames(output_data)=="PO ID"), which(colnames(output_data)=="Relevance")),visible = FALSE)) 
                ), escape=F)
      
    })
    
    output$courseoutcometable <- renderDataTable({
      
      output_data <- get_contibution_data() %>% group_by(`CO ID`) %>% summarise(CO_Description = `CO Description` ) # get the data to display in the "Course Outcome and Program outcomes" table
      names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
      
      # to fix Warning in processWidget(instance) 
      result = data.frame(output_data) 
      names(result) <- gsub(x = names(result),
                              pattern = "\\.",
                              replacement = " ")

      datatable(result,extensions = 'Buttons',
                options = list(autoWidth = TRUE, paging = FALSE, dom = 'Blfrtip', buttons = list(list(extend='excel',filename="download")), columnDefs = list(list(targets = c(which(colnames(output_data)=="PO ID"), which(colnames(output_data)=="Relevance")),visible = FALSE))
                ), escape=F)
    })
    
    
    crgetPrereqMap <- reactive({ # generate Prerequisite Map
        req(input$crCourseList)
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course ID
        course_selected <- map(extract_Code,1)
        
        g <- graph_from_data_frame(course_prerequisites, directed=TRUE) # create a graph structure using the "course_prerequisites" data
        
        data <- toVisNetworkData(g) # convert into a VisNetworkData
        
        data$nodes <- data$nodes %>% # focus only on the nodes dataframe
          mutate(group = case_when(str_extract(id, "[0-9]") == 1 ~ "1000level",
                                   str_extract(id, "[0-9]") == 2 ~ "2000level",
                                   str_extract(id, "[0-9]") == 3 ~ "3000level",
                                   str_extract(id, "[0-9]") == 4 ~ "4000level",
                                   id == "ITEC-GP" ~ "ITEC-GP",
                                   TRUE ~ "red"),
                 level = case_when(group == "1000level" ~ 1,
                                   group == "2000level" ~ 2,
                                   group == "3000level" ~ 4,
                                   group == "4000level" ~ 5,
                                   group == "ITEC-GP" ~  3,
                                   TRUE ~ 6)) %>% # create another column called "group" to group courses according to levels of study
          arrange(group) # arrange/sort by the group column
        
        
        g<-visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
          visEdges(arrows = "from") %>% 
          visNodes(borderWidth = 3, borderWidthSelected = 6) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = list(enabled=TRUE,selected=course_selected, style = "font-weight: bold;")) %>% # to highlight selected course and nearest nodes
          visExport() %>% # to export as png
          visPhysics(stabilization = TRUE) %>% 
          visIgraphLayout() %>%
          visLegend(zoom= FALSE) %>% 
          visHierarchicalLayout(direction = "UD", levelSeparation = 300, sortMethod = "directed") # UD: UP-Down is the orientation of the graph
        
        g
    })
    
    output$crPrereqTree <- renderVisNetwork({ # display the Prerequisite Tree
      
        crgetPrereqMap()
    })
    
    output$crPrereqTreeHtml <- downloadHandler( # download the Prerequisite map as a html file
        filename = function() {
          paste('Prerequisite_Map' ,'.html', sep='')
        },
        content = function(con) {
          crgetPrereqMap() %>% visSave(con)
        }
    )
    
    get_prereq_data <- reactive ({ # extract the Course prerequisites and their Course Outcomes
        req(input$crCourseList)
      
        extract_Code <- strsplit(as.character(input$crCourseList),"-") # extract only the Course ID
        course <- map(extract_Code,1)

        if (is.element(as.character(course), colnames(pre_req))){
            out <- get(as.character(course)) # call the funtion "get" to get the prereq information
            out <- out %>%  mutate(group = case_when(str_extract(Course, "[0-9]") == 2 ~ "2000level",
                                                   str_extract(Course, "[0-9]") == 3 ~ "3000level",
                                                   str_extract(Course, "[0-9]") == 4 ~ "4000level",
                                                   str_extract(Course, "[a-zA-Zl]{4}") == "High" ~ "0000level",
                                                   TRUE ~ "1000level")) %>% arrange(desc(group)) %>% select(-group)
            out
        }
    })
    
    output$prereqtable <- renderDataTable({
      
        output_data <- get_prereq_data() # get Course prerequisites and their Course Outcomes
        
        names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
        
        # to fix Warning in processWidget(instance) 
        result = data.frame(output_data) 
        names(result) <- gsub(x = names(result),
                              pattern = "\\.",
                              replacement = " ")
        
        datatable(result,extensions = 'Buttons', options = list(paging = FALSE, dom = 'Blfrtip', buttons = list(list(extend='excel',filename="download"))), escape=F, rownames = FALSE)
    })
    
    ########################################### <Program Report Tab> ######################################
    
    output$prPIProgramType <- renderUI({
        req(input$prRubric)
      
        filtered_list <- join_data %>%
            filter (Department == input$prRubric) # filter data according to Department Selected
        
        program_type_list <- unique(as.list(filtered_list$`Program Type`)) # get the types of programs under the "Department" selected
        
        pickerInput(
            inputId = "prProgramType",
            label = "Program",
            choices = program_type_list,
            selected = program_type_list[1],
            multiple = FALSE,
            options = list(
                `actions-box` =TRUE,
                size=100
            ))
    })
    
    output$prPICourseList <- renderUI({
        req(input$prRubric)
        req(input$prProgramType)
      
        filtered_list <- join_data %>%
            filter (Department == input$prRubric & `Program Type` %in% input$prProgramType) # filter data according to the "Department" and "Program Type" selected

        core_courses <-filtered_list %>% 
            filter (Membership == "Core") # filter out "core" courses
        
        non_core_courses <-filtered_list %>% 
            filter (Membership != "Core") # filter out "non-core" courses
        
        core_course_id_set <- unique(as.list(paste0(core_courses$`Course ID`,"-",core_courses$`Course Description`))) # get the list of core courses
        noncore_course_id_set <- unique(as.list(paste0(non_core_courses$`Course ID`,"-",non_core_courses$`Course Description`))) # get the list of non-core courses
        
        pickerInput(
            inputId = "prCourseList",
            label = "Courses in the Program",
            choices = list(
                `Core Courses` = core_course_id_set,
                `Non Core Courses` = noncore_course_id_set
            ),
            selected = core_course_id_set,
            multiple =TRUE,
            options = list(
                `actions-box` =TRUE,
                size=100,
                `deselect-all-text` = "Deselect All",
                `select-all-text` = "Select All"
            ))
    })
    
    output$prPIPoSet <- renderUI({
        req(input$prCourseList)
        req(input$prRubric)
        req(input$prProgramType)
      
        extract_Code <- strsplit(as.character(input$prCourseList),"-") # extract only the "Course ID"
        list <- map(extract_Code,1)
        
        filtered_list <- join_data %>%
            filter (Department == input$prRubric & `Program Type` %in% input$prProgramType & `Course ID` %in% list) # filter data according to the "Department", "Program Type", and the list of courses selected
        
        po_set_list <- as.list(unique(filtered_list$`PO Set`)) # get all the possible "PO_Set"s under above filters
        
        pickerInput(
            inputId = "prPoSet",
            label = "Program Learning Outcome Set",
            choices = po_set_list,
            multiple = FALSE
        )  
    })
    
    get_data2 <- reactive ({ # extract data to plot learning outcome map (Bar graph)
        req(input$prCourseList)
        req(input$prRubric)
        req(input$prProgramType)
        req(input$prPoSet)
        
        extract_Code <- strsplit(as.character(input$prCourseList),"-") # extract only the "Course ID"
        list <- map(extract_Code,1)

        tmp <- join_data %>%
            filter (Department == input$prRubric & `Program Type` == input$prProgramType & `Course ID` %in% list & `PO Set` == input$prPoSet) %>% # filter data according to the "Department", "Program Type", "PO_Set", and the list of courses selected
            group_by(`PO ID`) %>% summarize(Contribution = mean(`Relevance`)) # group data by the "PO_ID" and get the mean "Relevance" for each Po_ID

        filter_program_outcome <- program_outcomes %>%
            filter(`PO Set` == input$prPoSet) # filter "program_outcomes" dataframe by the "PO_Set" selected
         
        join <- full_join(tmp,filter_program_outcome) # join user filters data and filtered "program_outcome" data
        join
    })
    
    get_display_data2 <- reactive ({ # extract the data to display in the "Courses Selected" table
        req(input$prCourseList)
        req(input$prRubric)
        req(input$prProgramType)
        req(input$prPoSet)
        
        extract_Code <- strsplit(as.character(input$prCourseList),"-") # extract only the "Course ID"
        list <- map(extract_Code,1)
        
        out <-  join_data %>%
          filter(Department == input$prRubric & `Program Type` == input$prProgramType & `Course ID` %in% list & `PO Set` == input$prPoSet) %>% # filter data according to the "Department", "Program Type", "PO_Set", and the list of courses selected
          select(colnames(courses)) # select column that are in the "courses" dataframe
        
        tmp_url_example <- "?_inputs_&prRubric=%22ITEC%22&coursetable_rows_selected=null&coursetable_rows_all=%5B1%2C2%2C3%2C4%5D&coursetable_search=%22%22&sidebarItemExpanded=null&coursetable_state=null&crPrereqTree_highlight_color_id=%5B%22ITEC2210%22%2C%22WRIT2201%22%2C%22ITEC-GP%22%2C%22ITEC1000%22%5D&crPrereqTree_initialized=true&sidebarCollapsed=false&coursetable_rows_current=%5B1%2C2%2C3%2C4%5D&coursetable_cell_clicked=%7B%7D&crProgramType=%22ITEC-Ordinary%22&crCourseList=%22ITEC1000-Introduction%20to%20Information%20Technologies%22&crPoSet=%22ITEC%22&crPrereqTree_selected=%22ITEC1000%22&crPrereqTree_highlight_label_id=%5B%22ITEC1620%22%2C%22ITEC1010%22%2C%22ITEC3010%22%2C%22ITEC3020%22%2C%22ITEC3030%22%2C%22ITEC3210%22%2C%22ITEC3220%22%2C%22ITEC3230%22%2C%22ITEC2610%22%2C%22ITEC2620%22%2C%22MATH1190%22%2C%22MATH2320%22%2C%22MATH2565%22%2C%22MATH%201510%2FMATH%201710%2FAdvanced%20Functions%20(MHF4U)%2F%22%2C%22High%20school%20MATH%2011U%22%2C%22High%20school%20MATH%2011U%2FC%22%2C%22ITEC3500%22%2C%22ITEC3505%22%5D"
        split_tmp_url_example <- strsplit(tmp_url_example, ".crCourseList=%22ITEC1000")
        
        hostname <- session$clientData$url_hostname
        port <- session$clientData$url_port
        pathname <- session$clientData$url_pathname
        part1 <- split_tmp_url_example[[1]][1]
        part2 <- strsplit(split_tmp_url_example[[1]][2],".%22&crPoSet")[[1]][2]
        
        out$`Course Description tmp` <- gsub(x = out$`Course Description`, pattern = " ", replacement = "%20")
        
        out <- out %>% mutate(`CMR Link` = NA) # creating CMR hyperlinks to the course_report
        out <- out %>% mutate(`CMR Link` = paste0("http://", hostname, ":", port, pathname, part1,"&crCourseList=%22", `Course ID`, "-", `Course Description tmp`, "%22&crPoSet", part2)) %>%
          select(-`Course Description tmp`)
        
        out$`CMR Link` <- paste0("<a href='",out$`CMR Link`,"'>",out$`Course ID`,"</a>")
        
        out[,c(1,2,3,5,4)]
    })

    get_contibution_data2 <- reactive ({ # extract the data to display in the "Course Outcome and Program outcomes" table
        req(input$prCourseList)
        req(input$prRubric)
        req(input$prProgramType)
        req(input$prPoSet)
        
        extract_Code <- strsplit(as.character(input$prCourseList),"-") # extract only the "Course ID"
        list <- map(extract_Code,1)
        
        out <-  join_data %>%
            filter (Department == input$prRubric & `Program Type` == input$prProgramType & `Course ID` %in% list & `PO Set` == input$prPoSet& Relevance != 0 ) %>% # filter data according to the "Department", "Program Type", "PO_Set", and the list of courses selected. Further filter only data which "Relevance" is not equal to zero
            select (-`Kuali CM Links`, -Department, -`PO Set`, -`Short Description`, -`POSet Description`) # remove some of the columns
        
        out
    })
    
    output$programtable <- renderDataTable({ # output table contaning the information of the courses selected (plural). Default: all the core courses in the program selected
      
        output_data <- get_display_data2() # get data
        
        names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
        
        datatable(unique(output_data),options = list(paging = FALSE), rownames = FALSE, escape=F)
    })
    
    output$programPlotTitle <- renderText({ # Change Plot Title according to the Program Type selected (Bar graph)
      
        req(input$prProgramType)
    })
    
    output$programPlot <- renderPlot({ # create a bar graph to show the mapping of learning outcomes in a program level
        
        req(input$prPoSet)
        
        join <- get_data2() # get data
        
        list <- as.list(unique(join$`PO ID`))
        
        g <- ggplot(data=join, aes(x=factor(`PO ID`, level =rev(list)), y= `Contribution`, fill=`Contribution`)) +
            geom_bar(stat="identity", fill="#92BAD3")+
            geom_text(aes(label=`Short Description`, y=1), hjust = 0, nudge_y = -1, size = 6)+
            labs(#title = input$prProgramType,
                x = paste0(input$prPoSet, " Program Outcomes"),
                y = "Contirbution")+
            theme(
                plot.caption = element_text(hjust = 0, size = 6),
                # plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
                axis.title.x = element_text(hjust = 0.5, size = 15, face = "bold"),
                axis.title.y = element_text(hjust = 0.5, size = 15, face = "bold"),
                axis.text = element_text(size = 15),
                axis.text.x = element_blank()
            )+
            coord_flip()
        g
    })
    
    output$programTilePlotTitle <- renderText({ # Change Plot Title according to the Program Type selected (Heat map)

        req(input$prProgramType)
    })
    
    output$programtileplot <- renderPlot({ # create a heat map to show the mapping of learning outcomes in a program level
        req(input$prCourseList)
        req(input$prPoSet)
        
        extract_Code <- strsplit(as.character(input$prCourseList),"-") # extract only the "Course ID"
        list <- map(extract_Code,1)
        # list <- c("ITEC1000", "ITEC1010")
        
        new <- join_data %>%
            filter (`Course ID` %in% list ) %>% # filter by the courses selected. Default: all core courses in a program
            pivot_wider(names_from = `PO ID`, values_from = `Relevance`) %>% # convert to wide format
            group_by(`Course ID`) %>% # group by course ID
            mutate_if(is.numeric , ~replace_na(.,0)) %>% summarize_if(is.numeric, mean, na.rm=TRUE) %>% # get the mean of the relevance of each PO_ID on each course
            pivot_longer(-`Course ID`, names_to = "PO ID", values_to = "Relevance") # convert to long format to pass data into ggplot

        tmp <- program_outcomes %>%
            filter(`PO Set` == input$prPoSet ) # filter "program_outcomes" dataframe by the "PO_Set" selected
        
        new <- inner_join(new,tmp)
        
        list <- as.list(unique(paste0(new$`PO ID` , "-" , new$`Short Description`)))
        
        g <- ggplot(data = new, aes(x=`Course ID`, y=factor(paste0(`PO ID`, "-", `Short Description`), rev(list)), fill=`Relevance`)) + 
            geom_raster()+
            scale_fill_distiller( direction = 1)+
            labs(
                x = "Course ID",
                y = "Program Outcomes")+
            theme(
                plot.caption = element_text(hjust = 0, size = 6),
                axis.title.x = element_text(hjust = 0.5, size = 17, face = "bold"),
                axis.title.y = element_text(hjust = 0.5, size = 17, face = "bold"),
                axis.text = element_text(size = 10),
                axis.text.x = element_text(hjust = 1, angle = 45, size =15),
                axis.text.y = element_text(size =15)
            )
        g
    })
    
    output$prereqTitle <- renderText({ # Change Plot Title according to the Program Type selected (Prerequisite map)
      
        req(input$prProgramType)
    })
    
    prgetPrereqMap <- reactive({ # generate a tree map with the prerequisite data available
        g <- graph_from_data_frame(course_prerequisites, directed=TRUE) # create a graph structure using the "course_prerequisite" data
        
        data <- toVisNetworkData(g) # convert to VisNetweokData
        
        data$nodes <- data$nodes %>% # focus only on nodes dataframe
          mutate(group = case_when(str_extract(id, "[0-9]") == 1 ~ "1000level",
                                   str_extract(id, "[0-9]") == 2 ~ "2000level",
                                   str_extract(id, "[0-9]") == 3 ~ "3000level",
                                   str_extract(id, "[0-9]") == 4 ~ "4000level",
                                   id == "ITEC-GP" ~ "ITEC-GP",
                                   TRUE ~ "red"),
                 level = case_when(group == "1000level" ~ 1,
                                   group == "2000level" ~ 2,
                                   group == "3000level" ~ 4,
                                   group == "4000level" ~ 5,
                                   group == "ITEC-GP" ~  3,
                                   TRUE ~ 6)) %>% # create another column called "group" to group courses according to levels of study
          arrange(group) # arrange/sort by group column
        
        
        
        g<-visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
          visEdges(arrows = "from") %>% 
          visNodes(borderWidth = 3, borderWidthSelected = 6) %>%
          visOptions(highlightNearest = TRUE) %>%
          visExport() %>% # export as png
          visPhysics(stabilization = TRUE) %>%
          visIgraphLayout() %>%
          visLegend(zoom=FALSE) %>%
          visHierarchicalLayout(direction = "UD", levelSeparation = 300) # UD: Up-Down is the orientation of the grah
        g
    })
    
    output$prPrereqTree <- renderVisNetwork({ # create a prerequisite map to show the mapping of learning outcomes in a program level
        
        prgetPrereqMap()
    })
    
    output$prPrereqTreeHtml <- downloadHandler(
        filename = function() {
          paste('Prerequisite_Map' ,'.html', sep='')
        },
        content = function(con) {
          prgetPrereqMap() %>% visSave(con)
        }
    )
    
    output$programtable2 <- renderDataTable(bordered = TRUE,{
      
        output_data <- get_contibution_data2() # get data
        
        names(output_data) <- gsub(x = names(output_data), pattern = "_", replacement = " ") # replace "_"s in column names with space for display purposes
        
        # to fix Warning in processWidget(instance) 
        result = data.frame(output_data) 
        names(result) <- gsub(x = names(result),
                              pattern = "\\.",
                              replacement = " ")
        
        datatable(result, extensions = 'Buttons', 
                  options = list(paging = FALSE, dom = 'Blfrtip', buttons= list(list(extend = 'excel', filename = "download")), columnDefs = list(list(targets = c(which(colnames(output_data)=="PO ID"), which(colnames(output_data)=="Relevance")),visible = FALSE)) 
                  ), escape=F)
    },sanitize.text.function=identity)
    
    observe({ # for applying bookmarks. Doing so we will be able to get a shareable link
      
        reactiveValuesToList(input) # get all the input values
        session$doBookmark() # add bookmark to the session
    })
    
    onBookmarked(updateQueryString) # update url for each input update
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
