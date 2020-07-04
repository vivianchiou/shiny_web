library(edgebundleR)
library(dplyr)
library(htmlwidgets)
relate_df <- readxl::read_excel(path ='case_relation_in_waiwan.xlsx')
# relate_df <- readxl::read_excel(path ='C:\\Users\\user\\Documents\\GitHub\\my_code\\shiny_web\\case_relation_in_waiwan.xlsx')
detail_cases <- readxl::read_excel(path ='part_detail_cases.xlsx')
# detail_cases <- readxl::read_excel(path ='C:\\Users\\user\\Desktop\\part_detail_cases.xlsx')
detail_cases <- detail_cases %>% 
  mutate(select_gender = ifelse(detail_cases$gender=="女性", "F", "M"),
         select_source = ifelse(detail_cases$imported=="本土", "L", ifelse(detail_cases$imported=="境外", "I", "S")))
# nodes <- data.frame(case = 1:nrow(detail_cases))
View(detail_cases)

x <- graph.data.frame(relate_df , directed = F, vertices = detail_cases)
clr <- as.factor(V(x)$select_source)
levels(clr) <- c("cyan5", "red", "darkorange")# 1L I S
V(x)$color <- as.character(clr)
p <- edgebundle(x, tension = 0.5,fontsize = 4, nodesize = c(1, 1), width = NULL, padding = 50, directed = F, cutoff = 0.5)
saveWidget(p, file="plot_edgebundle.html")


#####
shinyApp(
  ui=fluidPage(
    titlePanel(""),
    fluidRow(
      column(3,
             wellPanel(
               sliderInput("tension", "Tension", 0.5,min=0,max=1,step = 0.01),
               sliderInput("fontsize","Font size",12,min=2,max=10),
               sliderInput("width","Width and height",600,min=200,max=1200),
               sliderInput("padding","Padding",100,min=0,max=300)
             )
      ),
      column(9,
             verbatimTextOutput("type"),
             uiOutput("circplot")
      )
    )
  ),
  shinyServer(function(input, output) {
    
    output$circplot <- renderUI({
      edgebundleOutput("eb", width = input$width, height=input$width)
    })
    
    # output$type=reactive({V(a)$select_source})
    # outputOptions(output, 'type', suspendWhenHidden=FALSE)
    
    output$eb <- renderEdgebundle({
      edgebundle(x,tension=input$tension,cutoff=input$cutoff,
                 fontsize=input$fontsize,padding=input$padding)
    })
    
  })
)

