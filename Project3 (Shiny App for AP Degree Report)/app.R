#
# Author: chethana Wickramasinghe
# Date: April 27th 2020
# This App gets the data from an excel sheet containing Academic Program Report and help user visualize the data under selected options
#


library(xlsx)
library(openxlsx)
library(reshape2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(shiny)
library(memisc)
library(plotly)
library(shinyjs)
library(shinythemes)
library(shinyjs)

### get the data from the excel file  ##################### "./Integrated_Academic_Program_Report_modified.xlsx"
# file <-  "/srv/shiny-server/j907n34ycs3fw3/data/Integrated_Academic_Program_Report_modified.xlsx" 
file <- choose.files()
  
num_sheets <- length(getSheetNames(file)) # get number of excelsheets in the workbook

### initialize the data structures to store the data taken from the excel file
array_storing_excel_sheets <- vector(mode = "list")  # to store data in long format for easy access and use for the App
sections <- list() # to store the list of sections in the excel workbook i.e store the A1 cell information as the identification name of each sheet
scopes <- vector(mode = "list")
dataout <- iris

for(i in 1:num_sheets) {
  
  sheet <- read.xlsx(file, i, check.names=FALSE)
  names(sheet) <- gsub(x=names(sheet),pattern="\\.", replacement = " ") # all heading names are replacing '.' with space  
  
  cn <- colnames(sheet) # store the column names in each sheet
  
  #store cell A1 of all sheets in each iteration
  sections <- append(sections, cn[1]) # use as the list for the UI selectInput widget and as a identification for each sheet in excel
  
  tmp <- sheet
  scopes[[colnames(sheet[1])]] <-unique(select(tmp,"Scope")) %>% as.list(scopes) # to store the types of scope for each section/sheet
  
  array_storing_excel_sheets[[colnames(sheet[1])]] <- list(sheet) # storing the excel sheets in an array to reference back easily
}


# shiny App #####

shinyApp(
  ui <- fluidPage(theme = shinytheme("flatly"), shinyjs::useShinyjs(),#other themes:darkly, cerulean
                  
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }" 
                  ),### to remove error messages showing in the App due to data will be changing according to request
                  
                  titlePanel("Academic Program Report - Direct Visualization Tool"),
                  h4("Select below options to create graphs as needed and download data"),
                  fluidRow(
                    column(4,
                           sidebarPanel(width = 600,
                                        
                                        strong("CATEGORY 1" ,style = "color:#F9736B"),
                                        selectInput("Section", "Section:", choices = sections, width = 600, selected = NULL),
                                        uiOutput("Scopes1"),
                                        uiOutput("Codes1"),
                                        textOutput("text_out1"),
                                        br(),
                                        
                                        checkboxInput("category2", label=strong("CATEGORY 2" , style = "color:#06BCC3"), value = FALSE),
                                        uiOutput("Sections2"),
                                        # selectInput("Section2", "Section:", choices = sections, width = 600),
                                        uiOutput("Scopes2"),
                                        uiOutput("Codes2"),
                                        textOutput("text_out2"),
                                        br(),
                                        
                                        strong("Additional Options"),
                                        br(),
                                        radioButtons("rd",
                                                     label="Style:",
                                                     choices=list("Bars", "Lines"),
                                                     selected="Bars"),
                                        
                                        uiOutput("yearslider"),
                                        uiOutput("ratiocheck"),
                                        uiOutput("facetcheck")
                                        
                                        ,br(),
                                        downloadButton("downloadData", "Download Data")
                           ),
                           
                    ),
                    column(8, plotOutput("plot", height = "700px"),
                           fluidRow(column(10,offset = 3,
                                           tableOutput('table')))
                           
                    ),
                    
                  )
  ),
  
  server = function(input, output, session) {
    
    ### for style options Bars/Lines
    v2 <- reactiveValues(doPlot = "Bars")
    
    observeEvent(input$rd,{
      v2$doPlot <- input$rd # rd:radio button
    })
    
    ############### SECTION1 ########################
    output$Scopes1 <- renderUI({
      selectInput("Scope", "Scope:", choices = scopes[[input$Section]], width = 600)
    })
    
    output$Codes1 <- renderUI({
      l_df <-array_storing_excel_sheets[[input$Section]][[1]] %>% filter (Scope == input$Scope) # filter data by the scope selected
      out <- select(l_df, input$Section) # select column containing the Codes
      selectInput("Code", "Code:", choices =out, width = 600)
    })
    
    output$text_out1 <- renderText({
      l_df<-array_storing_excel_sheets[[input$Section]][[1]] %>% filter (Scope == input$Scope) %>% select(input$Section, Description) # filter using scope and only select column containing the codes and respective descriptive column
      rownames(l_df) <- l_df[,1] # make the column containing codes the rownames
      l_df[,1] <- NULL
      out<- paste0("Description: ", l_df[input$Code, "Description"])# output the cell in Description column that intersecting the selected Code
    })
    
    
    ############### SECTION2 ########################
    ### check if category 2 selected or not
    output$Sections2 <- renderUI({
      if(input$category2 == TRUE){
        selectInput("Section2", "Section:", choices = sections, width = 600)
      }
    })
    
    output$Scopes2 <- renderUI({
      if(input$category2 == TRUE){
        selectInput("Scope2", "Scope:", choices = scopes[[input$Section2]], width = 600)
      }
    })
    
    output$Codes2 <- renderUI({
      if(input$category2 == TRUE){
        l_df <-array_storing_excel_sheets[[input$Section2]][[1]] %>% filter (Scope == input$Scope2) # filter data by the scope selected
        out <- select(l_df, input$Section2) # select column containing the Codes
        selectInput("Code2", "Code:", choices = out, width = 600)
      }
    })
    
    output$text_out2 <- renderText({
      if(input$category2 == TRUE){
        l_df<-array_storing_excel_sheets[[input$Section2]][[1]] %>% filter (Scope == input$Scope2) %>% select(input$Section2, Description) #  filter using scope and only select column containing the codes and respective descriptive column
        rownames(l_df) <- l_df[,1] # make the column containing codes the rownames
        l_df[,1] <- NULL
        out<- paste0("Description: ", l_df[input$Code2, "Description"]) # output the cell in Description column that intersecting the selected Code
      }
    })
    
    output$ratiocheck <- renderUI(
      if (input$category2 == TRUE){
        checkboxInput("check_box", label="Ratio (CATEGORY1/CATEGORY2)", value = FALSE)
      }
    )
    
    output$facetcheck <- renderUI(
      if (input$category2 ==TRUE) {
        checkboxInput("facet_check_box", label="Enable Facet Comparison", value = FALSE)
      }
    )
    
    
    ############# GENERATING DATA ########################
    transform_dataset <- reactive ({
      
      df1<-array_storing_excel_sheets[[input$Section]][[1]] %>% filter (Scope == input$Scope) %>% select(-Description, -Scope) # filter using the scope and remove the Description and Scope column for simplification purposes and an easy transope
      df1[is.na(df1)] <- 0
      tmp_df1 <- as.data.frame(t(df1))  # transpose the data
      colnames(tmp_df1) <- tmp_df1[1,] # get the first row as headings (1st row of transposed data was the 1st column containing codes in original data)
      tmp_df1 <- tmp_df1[-1,] # remove the first row after it has been made the headings of the new transposed data set
      tmp2_df1 <-cbind(Year=rownames(tmp_df1),tmp_df1, row.names = NULL) # making the row names a column because the year row became the row name of the transposed data
      tmp2_df1[1:ncol(tmp2_df1)] <- lapply(tmp2_df1[1:ncol(tmp2_df1)], as.integer) # making Year column and # of students the type Integer
      
      combo <- list(category1 = tmp2_df1)
      
      if (input$category2 == TRUE){
        df2<-array_storing_excel_sheets[[input$Section2]][[1]] %>% filter (Scope == input$Scope2) %>% select(-Description, -Scope) # filter using the scope and remove the Description and Scope column for simplification purposes and an easy transope
        df2[is.na(df2)] <- 0
        tmp_df2 <- as.data.frame(t(df2))  # transpose the data 
        colnames(tmp_df2) <- tmp_df2[1,] # get the first row as headings (1st row of transposed data was the 1st column containing codes in original data)
        tmp_df2 <- tmp_df2[-1,] # remove the first row after it has been made the headings of the new transposed data set
        tmp2_df2 <-cbind(Year=rownames(tmp_df2),tmp_df2, row.names = NULL)  # making the row names a column because the year row became the row name of the transposed data
        tmp2_df2[1:ncol(tmp2_df2)] <- lapply(tmp2_df2[1:ncol(tmp2_df2)], as.integer) # making Year column and # of students the type Integer
        
        combo <- list(category1 = tmp2_df1, category2 =tmp2_df2) # store the new transformsed data frames in a combo list
      }
      combo
      
    })
    
    dataset <- reactive({
      if (input$category2 == TRUE){
        combo <- transform_dataset() # get the transformsed data
        
        df1<- combo$category1
        df1 <- df1[,c("Year",input$Code)] # select only the Year and Column respective to the code selected
        
        df2<- combo$category2
        df2 <- df2[,c("Year",input$Code2)] # select only the Year and Column respective to the code selected
        
        df1[is.na(df1)] <- 0 # make any NA to 0 value
        df1$Year <- as.numeric(as.character(df1$Year))
        df2[is.na(df2)] <- 0 # make any NA to 0 value
        df2$Year <- as.numeric(as.character(df2$Year))
        
        join<- full_join(df1, df2, by="Year") # do a full join to df1 and df2 using 'Year' column
        join[is.na(join)] <- 0 # make any NA to 0 value
        
        join
      }else{
        combo <- transform_dataset()
        df1<- combo$category1
        df1 <- df1[,c("Year",input$Code)] # select only the Year and Column respective to the code selected
        df1[is.na(df1)] <- 0 # make any NA to 0 value
        df1$Year <- as.numeric(as.character(df1$Year))
        df1long <- gather(df1, key="measure", value="value", c(colnames(df1)[2]))
        df1long
      }
    })
    
    ratio_dataset <- reactive({ # calculating the ratio of the 2 data sets selected
      tmp <- dataset()
      join_with_ratio <- tmp %>% mutate(ratio= tmp[[2]]/tmp[[3]]) %>% rename(Ratio = ratio)
      join_with_ratio
      
    })
    
    start <- reactive({
      data <- dataset()
      minout = min(data$Year)
      minout
    })
    
    end <- reactive({
      data <- dataset()
      maxout = max(data$Year)
      maxout
    })
    
    output$yearslider <- renderUI({
      data <- dataset()
      sliderInput("range", "Range of the Year:", min = min(data$Year), max = max(data$Year), value = c(min(data$Year),max(data$Year)), sep = "", step = 1)
      # sliderInput("range", "Range of the Year:", min = 2015, max = 2019, value = c(min,max), sep = "", step = 1)
    })
    
    
  # plot --------------------------------------------------------------------
    
    
    ### output plot
    output$plot <- renderPlot({
      filtered_data <- dataset() %>% filter(Year >= input$range[1] & Year <= input$range[2])
      
      if (input$category2 == TRUE) { # if category 2 is also selected
        
        l_join <- filtered_data # get dataset joining both the sections

# bar ---------------------------------------------------------------------

        
        if (v2$doPlot == "Bars"){ # if Bars option selected 
          
          if (input$check_box == FALSE){ # if looking for raw data
            
            if (input$facet_check_box == FALSE) { # if doesn't need facet comparison
              isolate ({
                
                l_join <- melt(l_join,id.vars = 1)
                names(l_join)[1] <- "Year"
                
                g <- ggplot(l_join,aes(x = factor(Year),y = value, fill=variable)) + 
                  geom_bar(stat = "identity",position = "dodge") + #dodge/stack
                  # scale_y_log10() 
                  geom_text(aes(label=value), vjust=-0.7, hjust =0.5, color="black",
                            position = position_dodge(0.9), size=4)+
                  labs(#title = "BAR GRAPH",
                    x = "Year",
                    y = "") +
                  scale_fill_manual(values = c("#F9736B", "#06BCC3"),
                                    labels = c(paste0(input$Section,"\n",input$Scope , "_",input$Code), paste0(input$Section2,"\n", input$Scope2, "_",input$Code2)), 
                                    name="")+
                  theme(
                    plot.caption = element_text(hjust = 0, size = 6),
                    legend.position = "bottom",
                    legend.justification = "center",
                    legend.title = element_text(size = 12),
                    plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                    axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
                    axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
                    axis.text = element_text(size = 12),
                    legend.text = element_text(size = 12)
                  )
                
                g
              })
            }else{ # if need facet comparison
              isolate ({
                
                # updateCheckboxInput(session, "check_box", value = FALSE) # ratio check box should be unselected because ratio is not presented as facets
                
                tmp <- l_join %>% rename(!!paste0("Category 1: ", input$Section,"\n",input$Scope , "_",input$Code) :=  colnames(l_join)[2],
                                         !!paste0("Category 2: ", input$Section2,"\n",input$Scope2 , "_",input$Code2) :=  colnames(l_join)[3])
                
                joinlong <- gather(tmp, key="measure", value="value", c(colnames(tmp)[2], colnames(tmp)[3]))
                
                g <- ggplot(joinlong, aes(x=factor(Year), y=value))+
                  geom_bar(stat='identity', fill="forest green")+
                  labs(
                    x = "Year",
                    y = "")+ 
                  facet_wrap(~measure, ncol=1, as.table = TRUE, scales ="free")+
                  theme(
                    strip.text = element_text(size = 15, face = "bold"),
                    axis.text = element_text(size = 12)
                  )
                g
              })
            }
          }else{ # if looking for ratio
            isolate ({
              
              # updateCheckboxInput(session, "facet_check_box", value = FALSE) # facet check box should be unselected because ratio is not presented as facets
              
              l_join_with_ratio <- ratio_dataset() %>% filter(Year >= input$range[1] & Year <= input$range[2])# get dataset joining both sections and with the ratio
              
              g <- ggplot(data=l_join_with_ratio, aes(x=factor(Year), y=Ratio)) +
                geom_bar(stat="identity", fill="#92BAD3")+
                geom_text(aes(label=round(Ratio,3)), vjust=-1, hjust =0.5, color="black",
                          position = position_dodge(0.9), size=4)+
                labs(#title = "BAR GRAPH",
                  x = "Year",
                  y = "")+ 
                theme(
                  plot.caption = element_text(hjust = 0, size = 6),
                  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                  axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
                  axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
                  axis.text = element_text(size = 12)
                )
              g
            })
          }

# line --------------------------------------------------------------------

          
        }else{ # if Lines option selected
          if (input$check_box == FALSE) # if looking for raw data
            if (input$facet_check_box == FALSE) { # if doesn't need facet comparison
              isolate ({
                
                modified_join <- l_join %>% pivot_longer(colnames(l_join)[-1], names_to = "Code", values_to = "Values")
                
                if (paste0(input$Section,"\n", input$Scope,"_",input$Code) == paste0(input$Section2,"\n",input$Scope,"_",input$Code2)){
                  new2 <- modified_join %>% mutate(Color = case_when(Code == paste0(input$Code, ".x") ~ "red",
                                                                     Code == paste0(input$Code2, ".y") ~ "blue"))
                }else{
                  new2 <- modified_join %>% mutate(Color = case_when(Code == input$Code ~ "red",
                                                                     Code == input$Code2 ~ "blue"))
                }
                
                # view(new2)
                # view(class(joinn))
                modified_join$Year <- as.numeric(as.character(modified_join$Year))
                # view(class(modified_join$Year))
                
                g <- ggplot(data = new2, aes(x=Year, y=Values)) + geom_line(aes(colour=Color), size=1) + 
                  geom_text(aes(label=Values),vjust = 1, hjust = 1) + geom_point(aes(x=Year, y=Values)) +  labs(y = "") +
                  scale_x_continuous()+
                  scale_color_manual(values =  c('red' = "#F9736B",
                                                         'blue' = "#06BCC3"),
                                     labels = c('red' = paste0(input$Section,"\n", input$Scope,"_",input$Code),'blue'= paste0(input$Section2,"\n",input$Scope,"_",input$Code2)), 
                                     name="")+
                  theme(
                    plot.caption = element_text(hjust = 0, size = 6),
                    legend.position = "bottom",
                    legend.justification = "center",
                    legend.title = element_text(size = 12),
                    plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                    axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
                    axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
                    axis.text = element_text(size = 12),
                    legend.text = element_text(size = 12)
                  )
                g
              })
            }else{ # if need facet comparison
              
              # updateCheckboxInput(session, "check_box", value = FALSE) # ratio check box should be unselected because ratio is not presented as facets
              
              tmp <- l_join %>% rename(!!paste0("Category 1: ", input$Section,"\n",input$Scope , "_",input$Code) :=  colnames(l_join)[2],
                                       !!paste0("Category 2: ", input$Section2,"\n",input$Scope2 , "_",input$Code2) :=  colnames(l_join)[3])
              
              joinlong <- gather(tmp, key="measure", value="value", c(colnames(tmp)[2], colnames(tmp)[3]))
              
              g <- ggplot(joinlong, aes(x=factor(Year), y=value, group =1))+
                geom_line(color='forest green')+
                labs(
                  x = "Year",
                  y = "")+ 
                facet_wrap(~measure, ncol=1, scales = "free")+
                theme(
                  strip.text.x = element_text(size = 15, face = "bold"),
                  axis.text = element_text(size = 12)
                )
              g
            }
          else # if looking for ratio
            isolate ({
              
              l_join_with_ratio <- ratio_dataset() %>% filter(Year >= input$range[1] & Year <= input$range[2])# get dataset joining both sections and with the ratio
              
              g <- ggplot(data = l_join_with_ratio, aes(x = Year, y = Ratio, group=1))  +geom_line(colour= "#92BAD3", size=2)+
                geom_text(aes(label=round(Ratio,3)),vjust = 1, hjust = 1) + geom_point(aes(x=Year, y=Ratio))+ labs(y = "") +
                theme(
                  plot.caption = element_text(hjust = 0, size = 6),
                  plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
                  axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
                  axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
                  axis.text = element_text(size = 12)
                )
              g
            })
        }
      }else{ # if category 2 not selected
        data <- filtered_data
        if (v2$doPlot == "Bars"){ # if bar is selected as style
          
          ggplot(data,aes(x = factor(Year),y = value)) + 
            geom_bar(aes( fill = "#F9736B"), stat = "identity",position = "dodge") +
            geom_text(aes(label=value), vjust=-0.7, hjust =0.5, color="black",
                      position = position_dodge(0.9), size=4)+
            labs(#title = "BAR GRAPH",
              x = "Year",
              y = "") +
            scale_fill_manual(values = c("#F9736B"),
                              labels = c(paste0(input$Section,"\n",input$Scope , "_",input$Code)), 
                              name="")+
            theme(
              plot.caption = element_text(hjust = 0, size = 6),
              plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
              axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
              axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
              axis.text = element_text(size = 12),
              legend.position = "bottom",
              legend.justification = "center",
              legend.text = element_text(size = 12)
            )
          
        }else{ # if line is selected as style
          ggplot(data,aes(x =Year,y = value)) + 
            geom_line(aes(color='#F9736B')) + 
            geom_point(color='#F9736B')+
            geom_text(aes(label = value),vjust = -0.5, hjust = 1) +
            labs(y = "") +
            scale_color_manual(values = c("#F9736B"),
                                             labels = c('#F9736B'= paste0(input$Section,"\n", input$Scope,"_",input$Code)), 
                                             name="")+
            theme(
              plot.caption = element_text(hjust = 0, size = 6),
              legend.position = "bottom",
              legend.justification = "center",
              legend.title = element_text(size = 12),
              plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
              axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
              axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
              axis.text = element_text(size = 12),
              legend.text = element_text(size = 12)
            )
        }
      }
    })
    
    # observeEvent(!input$category2, shinyjs::disable("check_box"))
    
    download_data <- reactive({ # get the correct data to download
      if (input$check_box == FALSE){ # if check box for ratio not selected
        data<- dataset()
      }
      else{
        data<- ratio_dataset()
      }
      
      names(data)[2] <- paste(input$Section,input$Scope,input$Code,sep = "<br>")
      names(data)[3] <- paste(input$Section2,input$Scope2,input$Code2, sep = "<br>")
      
      # data <- data %>% filter(Year >= input$range[1] & Year <= input$range[2])
      data
    })
    
    output$table <- renderTable(bordered = TRUE,{
      if ( input$category2 == TRUE){
        out_put_table <- download_data()
        out_put_table$Year <- as.integer(as.character(out_put_table$Year))
        out_put_table
      }
    },sanitize.text.function=identity)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("input_dataset", ".csv", sep = "")
      },
      content = function(file) {
        if (input$category2 == FALSE){
          write.csv(dataset() %>% select(1,3), file, row.names = FALSE)
        }else{
          write.csv(download_data(), file, row.names = FALSE)
        }
      })
    
    
  }
)


