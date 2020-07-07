library(shiny)
library(shinydashboard)
library(supercaliheatmapwidget)

set.seed(2019-01-02)
data.frame(
  day = seq(as.Date("2018-01-01"), as.Date("2018-12-31"), "1 day"),
  val = sample(160, 365, replace=TRUE)
) -> calendar_data


# tw_calendar_data
names(tw_calendar_data) = c("day","val")
calendar_data = tw_calendar_data

supercal(
  calendar_data, 
  datetime_col = day, value_col = val,
  label = cal_label("top", "center"),
  cell_size = 40, cell_radius = 40,
  tooltip_item_name = cal_names("incident"),
  range = 4,
  height="100%"
) %>% 
  cal_legend(
    color = cal_colors("#deebf7", "#084594"),
    breaks = 4,
    cell_size = 20,
    hjust = "center"
  )  %>% 
  label_cells(js_body = "return(value);") %>% 
  cal_style(
    graph_label = "font-family: 'Muli Black'; fill: black; font-size: 14pt;",
    subdomain_text = "font-family: 'Muli'; fill: white; font-size:10pt"
  ) %>% 
  htmlwidgets::prependContent(
    htmltools::tags$style("@import url('https://fonts.googleapis.com/css?family=Muli:400,400i,700');")
  )


# =========================================================
ui <- navbarPage(title = "Covid-19 dashboard!",
                 header = tagList(
                   useShinydashboard()
                 ),
                 tabPanel(title = tags$p(icon("calendar"),"test"),
                          setBackgroundColor("#808080"),
                          box(width = 10,solidHeader = TRUE,# height = 100,
                              htmlOutput("inc")
                          )
                 )
                 
)

server <- function(input, output, session) {
  getPage<-function() {
    return(includeHTML("tw_daily_con_calender.html"))
  }
  output$inc<-renderUI({getPage()})
}
shinyApp(ui, server)
