library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(reactable)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyjs)
library(sf)
library(gt)
library(shinyalert)
library(zip)
library(openxlsx)
library(fpc)
library(osmdata)
library(htmltools)
library(DT)

readRenviron(".Renviron")

####Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port=8080)
###########################
ou<-read.csv("code_list.csv")

tree<-create_tree(
  ou, c("National","Province", "District", "Municipality"),
  levels_id = c("National","Province", "District", "pcode"))

#####HEADER###############
header<-dashboardHeader(
  titleWidth = 200,
  title = tags$img(src = "ne-flag.png", height = "30px"),
  leftUi = tagList(
    dropdownBlock(
      id="ymrange",
      title="Patient Cohort",
      icon=icon("calendar"),
      badgeStatus = NULL,
      fluidRow(
        column(6,
               div(
                 style = "text-align: left; margin-left: 5px;",
                 h5("Date Range: From")
               ),
               div(
                 style = "display: flex; align-items: center;",
                 div(
                   style = "width: 100%; margin-right: 5px;",
                   selectInput(
                     inputId = "start_year",
                     label = "Year",
                     choices = 2000:2100
                   )
                 ),
                 div(
                   style = "width: 100%;",
                   selectInput(
                     inputId = "start_month",
                     label = "Month",
                     choices = c("Baishakh"="01",
                                 "Jestha"="02",
                                 "Ashadh"="03",
                                 "Shrawan"="04",
                                 "Bhadra"="05",
                                 "Ashwin"="06",
                                 "Kartik"="07",
                                 "Mangsir"="08",
                                 "Poush"="09",
                                 "Magh"="10",
                                 "Falgun"="11",
                                 "Chaitra"="12")
                   )
                 )
               )
        )
      ),
      fluidRow(
        column(6,
               div(
                 style = "text-align: left; margin-left: 5px;",
                 h5("Date Range: To")
               ),
               div(
                 style = "display: flex; align-items: center;",
                 div(
                   style = "width: 100%; margin-right: 5px;",
                   selectInput(
                     inputId = "end_year",
                     label = "Year",
                     choices = 2000:2100
                   )
                 ),
                 div(
                   style = "width: 100%;",
                   selectInput(
                     inputId = "end_month",
                     label = "Month",
                     choices = c("Baishakh"="01",
                                 "Jestha"="02",
                                 "Ashadh"="03",
                                 "Shrawan"="04",
                                 "Bhadra"="05",
                                 "Ashwin"="06",
                                 "Kartik"="07",
                                 "Mangsir"="08",
                                 "Poush"="09",
                                 "Magh"="10",
                                 "Falgun"="11",
                                 "Chaitra"="12")
                   )
                 )
               )
        )
      )
    ),
    dropdownBlock(
      id="locations",
      title = "Location Selection",
      icon=icon("location-dot",lib = "font-awesome"),
      badgeStatus = NULL,
      treeInput(
        inputId = "ous",
        label = "Administrive Divisions:",
        choices = tree,
        closeDepth = 1,
        returnValue = "id",
        selected="Nepal"
        #levels = 1,
        #width = "100%",
        #borders = T,
        #return_value = "id"
      ),
      tags$style(HTML("#ous {color: black !important;}"))
    )
  )
)

#####SIDEBAR###############
sidebar<-dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Follow-up List", tabName = "list", icon = icon("tablets", lib = "font-awesome")),
    menuItem("Program Indicators", icon = icon("list-ol", lib = "font-awesome"),
             menuSubItem("Monthly Report",tabName = "report", icon = icon("square-poll-horizontal", lib = "font-awesome")),
             menuSubItem("DHIS2 Import",tabName = "dhis2",icon=icon("server"))
    ),
    menuItem("Case Clustering", tabName = "cluster", icon = icon("magnifying-glass-location", lib = "font-awesome"))
  )
)

#####BODY###############
body<-dashboardBody(
  useShinyjs(),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/nepali-date-converter/dist/nepali-date-converter.umd.js")
  ),
  tags$script(HTML('
  function convertToNepaliDate(dates, id) {
   const nepaliDates = dates.map(dateString => {
      if (dateString === "NA" || !dateString) {
        return "NA";  // Return "NA" for missing values
      }
      
      const date = new Date(dateString);
      if (isNaN(date)) {
        return "NA";  // Handle invalid dates as "NA"
      }
      
      const nepaliDate = new NepaliDate(date);
      return nepaliDate.format("YYYY/MM/DD");
    });

    // Pass the result back to Shiny
    Shiny.setInputValue(id, nepaliDates);
  }'
  )),
  tags$head(
    tags$style(HTML("
        .scroll-box {
          height: 700px;
          overflow-y: scroll;
        }
        .btn.btn-circle {
        border-radius: 10px;
        font-size: 15px;
        height: 19px;
        line-height: 1;
        padding: 0px 0;
        text-align: center;
        width: 19px;
        }
        .button-container div {
        display: block;
        width: 100%;
        margin-bottom: 1px;
      }
      .button-container button {
        width: 100%;
      }
      .equal-height .box {
        display: flex;
        flex-direction: column;
        height: 100%;
      }
      .equal-height .box .box-body {
        flex: 1;
        height: 100%;
      }
      
      .tag {
      display: inline-block;
      padding: 0.125rem 0.75rem;
      border-radius: 15px;
      font-weight: 600;
      font-size: 1.2rem;
      }
      
      .num-low {
      background: #d8f5d6;
      color: #2f532d;
      }
      
      .num-med {
      background: #d4daf7;
      color: #2a366f;
      }
      
      .num-high {
      background: #f7d4da;
      color: #6f2a36;
      }
      
      .center-action-button {
        display: flex;
        justify-content: center;
      }
      
      .custom-button {
      margin-RIGHT: 15px;
      }
      
      /* Make the control bar background transparent */
      .control-sidebar-light,
      .control-sidebar-light+.control-sidebar-bg {
      background-color: transparent !important;
      border-left: none !important;  /* Remove the left border */
      }
      
      "
    ))
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              column(width = 12,
                     valueBoxOutput(outputId = "newrecord", width = 2),
                     valueBoxOutput(outputId = "newlept", width = 2),
                     valueBoxOutput(outputId = "newleg2d", width = 2),
                     valueBoxOutput(outputId = "newlech", width = 2),
                     valueBoxOutput(outputId = "txfu", width = 2),
                     valueBoxOutput(outputId = "cont", width = 2)
              )
            ),
            fluidRow(
              class = "equal-height",
              box(width = 8, height = "100%",
                  title = "New Detections",
                  leafletOutput("newmap"),
                  id="dash-newmap",
                  collapsible = T,
                  closable = F),
              box(width = 4,height = "100%",
                  title = tagList(
                    actionButton("expand_button", "Expand/Collapse Province", style = "padding: 2px 8px; font-size: 11px;")
                  ),
                  extendShinyjs(text = "
                  shinyjs.toggleAllRows = function() {
                  Reactable.toggleAllRowsExpanded('cars-expansion-table');
                  }", functions = c("toggleAllRows")),
                  #actionButton("expand_button", "Expand/Collapse All"),
                  reactableOutput("newtable"),
                  id="dash-newtable",
                  collapsible = T,
                  closable = F)
            ),
            fluidRow(
              box(
                title = "Map of Cases to Follow up", status = "danger", solidHeader = T,
                leafletOutput("fumap"),width = 12,
                id="dash-fumap",
                collapsible = T,
                closable = F
              )
            ),
            fluidRow(
              box(
                title = "Leprosy Case Detection (last 5 years)", background = "teal", solidHeader = T,
                plotlyOutput("lepcases"),
                id="dash-lepcases",
                collapsible = T,
                closable = F
              ),
              box(
                title = "Trend of Cases among New (last 5 years)", background = "teal", solidHeader = T,
                plotlyOutput("amongn"),
                id="dash-amongn",
                collapsible = T,
                closable = F
              ))
            
    ),
    tabItem(tabName = "list",
            fluidRow(
              column(width=3,
                     selectizeInput(inputId = "futype",label = "Type of Follow-up Cases:",
                                    choices = c("All"=0,
                                                "Contact Not done + Outcome Overdue"=1,
                                                "Contact Not done + Outcome Complete"=2,
                                                "Contact Done + Outcome Overdue"=3, 
                                                "Contact Not done + Under Treatment"=4, 
                                                "Contact Done + Under Treatment"=5),
                                    selected = 0,
                                    multiple=T,
                                    options = list(
                                      plugins = list("remove_button"),
                                      create = TRUE,
                                      persist = F # keep created choices in dropdown
                                    ))
              ),
              column(width = 3,
                     actionBttn(inputId = "fugen",label = "Generate")
              ),
              column(width = 2,
                     downloadBttn(outputId = "fuload",label = "Table")
              ),
              column(width = 2,
                     downloadBttn(outputId = "fugpx",label = "GPX")
              ),
              column(width = 2,
                     actionBttn(inputId = "clearlist",label = "Clear")
              ),
              column(width = 12,
                     uiOutput(outputId = "futable"))
              #verbatimTextOutput("dates"),
            )),
    tabItem(tabName = "report",
            fluidRow(
              column(width = 3,
                     selectizeInput(
                       inputId = "retype", 
                       label = "Select the report periods:",
                       choices = NULL,
                       multiple=T,
                       options =list(
                         plugins=list("remove_button"),
                         persist=T
                       ))
              ),
              column(width = 3,
                     awesomeRadio(
                       inputId = "replevel",
                       label = "Select Data Aggregate Level:", 
                       choices = c("Province"=1, "District"=2, "Municipality"=3),
                       selected = 3,
                       inline = TRUE
                     )
              ),
              column(width = 2,
                     actionBttn(inputId = "regen",label = "",icon=icon("table",lib="font-awesome"))
              ),
              column(width = 2,
                     downloadBttn(outputId = "reload",label = "")
              ),
              column(width = 2,
                     actionBttn(inputId = "clearreport",label = "",icon=icon("trash-can",lib="font-awesome"))  
              )),
            fluidRow(
              column(width = 3, offset = 9,
                     selectizeInput(
                       inputId = "munichoice", 
                       label = "Filter report view by your locations:","",
                       multiple=T,options = list(
                         maxItems = 5,
                         placeholder="maximum: 5",
                         plugins = list("remove_button"),
                         persist = TRUE
                       )
                     )
              )),
            fluidRow(
              column(width = 12,
                     div(class = "scroll-box",htmlOutput(outputId = "retable"))))
    ),
    tabItem(tabName = "dhis2",
            fluidRow(
              column(width = 4,
                     accordion(
                       id = "d2_conf",
                       accordionItem(
                         title = "Authorization to DHIS2:",
                         status = "primary",
                         collapsed = FALSE,
                         textInput(inputId = "d2_user", label = "DHIS2 Username:"),
                         passwordInput(inputId ="d2_pw", label="DHIS2 Password:"),
                         div(class = "center-action-button", 
                             actionBttn(inputId = "d2_login", label = "Login")
                         )
                       ),
                       accordionItem(
                         title = "Action Box:",
                         status = "warning",
                         collapsed = TRUE,
                         div(
                           style = "text-align: left; margin-left: 5px;",
                           h5("Dataset Period:")
                         ),
                         div(
                           style = "display: flex; align-items: center;",
                           div(
                             style = "width: 100%; margin-right: 5px;",
                             selectInput(
                               inputId = "d2_year",
                               label = "Year",
                               choices = 2000:2100
                             )
                           ),
                           div(
                             style = "width: 100%;",
                             selectInput(
                               inputId = "d2_month",
                               label = "Month",
                               choices = c("Baishakh"="01",
                                           "Jestha"="02",
                                           "Ashadh"="03",
                                           "Shrawan"="04",
                                           "Bhadra"="05",
                                           "Ashwin"="06",
                                           "Kartik"="07",
                                           "Mangsir"="08",
                                           "Poush"="09",
                                           "Magh"="10",
                                           "Falgun"="11",
                                           "Chaitra"="12")
                             )
                           )
                         ),
                         div(class = "center-action-button", 
                             div(class = "custom-button",actionBttn(inputId = "d2_table", label = "Preview")),
                             div(class = "custom-button",actionBttn(inputId = "d2_import", label = "Send"))
                         )
                       )
                     )
              ),
              column(width = 8,
                     box(width = NULL,
                         title = "Instruction",
                         status = "info",
                         solidHeader = T,
                         id="d2_info",
                         collapsible = T,
                         closable = F,
                         p(strong("1. Log in to HMIS/DHIS2 here")),
                         p(em("We will not retain any sensitive or HMIS credential information on this platform.")),
                         p(strong("2. Select your working municipatities from the above dropdown [Select Locations] (mulitiples are allowed).")),
                         p(code("Please select the same municipalities STRICTLY according to your access permission in HMIS/DHIS2.")),
                         p(strong("3. Select the monthly period from the [Actin Box].")),
                         p(em("Only one-month period is allowed.")),
                         p(strong("3.Click [Preview] to view the data values and then click [Send] to send to HMIS/DHIS2 server."))
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     uiOutput("municipality_filter_ui"),
                     reactableOutput(outputId = "d2preview")    
                         
              )
            )
    ),
    tabItem(tabName = "cluster",
            fluidRow(
              column(width = 6,
                     box(
                       title = "Defining a Cluster:",
                       status = "info",
                       solidHeader = T,
                       width = NULL,
                       align = "left",
                       div(
                         style = "position: relative;",
                         numericInput(inputId = "clusternum", label = "Minimum cases as a cluster:", value = 3),
                         numericInput(inputId = "clusterdist", label = "Detection radius (meter):", value = 300),
                         div(
                           style = "position: absolute; top: 0px; right: 0px;", 
                           dropdownButton(
                             tags$h4("Method of Cluster Detection:"),
                             tags$p("DBSCAN (Density-Based Spatial Clustering of Applications with Noise) is a clustering algorithm used to identify groups of cases based on their spatial proximity."),
                             tags$p("The algorithm works by defining clusters as areas where a minimum number of cases (points) are within a specified distance (radius) from each other. This means that a cluster is formed if there are enough cases close to each other within the defined radius."),
                             tags$p("This method is particularly useful for detecting clusters of varying shapes and sizes in spatial data and can also identify outliers, or noise, which are points that do not belong to any cluster."),
                             circle = TRUE,
                             status = "primary",
                             icon = icon("question", lib = "font-awesome", class = "fa-sm"),
                             width = "300px",
                             inline = TRUE,
                             tooltip = tooltipOptions(title = "Click to see method of cluster detection!")
                           )
                         )
                       )
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Defining Contact Tracing Zone (radius of cases):",
                         status = "info",
                         solidHeader = T,
                         numericInput(inputId = "clusterbuffer", label = "Clustered cases (meter):", value = 300),
                         numericInput(inputId = "nonclusterdist", label = "Non-clustered cases (meter):", value = 150)
                     )
              ),
              
            ),
            fluidRow(
              column(width = 10,
                     box(width = NULL,
                         collapsible = F,background = "green",
                         title = "Map of Case Clustering and Contact Tracing Zone",
                         leafletOutput("dbscanmap")
                     )
              ),
              column(width = 2,
                     box(width = NULL,
                         title = "Action",
                         collapsible = F,
                         tags$div(
                           class = "button-container",
                           tags$div(actionBttn(inputId = "dbscan", label = "Start")),
                           tags$div(downloadBttn(outputId = "casegpx", label = "Cases.GPX")),
                           tags$div(downloadBttn(outputId = "zonegpx", label = "Zone.GPX")),
                           tags$div(actionBttn(inputId = "cluclear", label = "Clear"))
                         )
                     )
              )),
            fluidRow(  
              box(width = 10,
                  collapsible = F,
                  title = "Clustering Summary",
                  tableOutput("summary")
                  
              )
            )
    )
  )
)

#####CONTROL###############
controlbar <- dashboardControlbar(
  skin = "light",
  width = 100,
  
  # Custom div with flexbox layout and no margins for the button
  div(style = "
      display: flex; 
      justify-content: center; 
      align-items: center;
      flex-direction: column;
      height: 100%; 
      padding: 0;
      margin: 0;",
      actionBttn("logout", "Logout", icon = icon("sign-out-alt"),
                 style = "fill", color="warning",size="sm",
                 width = "100px",
                 block=T),
      
      uiOutput("conditionalButtonUI")
  )
)

ui<-dashboardPage(header,sidebar,body,controlbar,skin = "blue-light")

#########################
###SERVER################
#########################

server <- function(input, output, session) {
  
  credentials<-readRDS("users.RDS")
  # Reactive value to track if the user is logged in
  user_logged_in <- reactiveVal(FALSE)
  
  user_re<-reactiveVal(NULL)
  
  # Function to handle login
  login_prompt <- function() {
    showModal(modalDialog(
      title = "Login",
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 200px; flex-direction: column;",
        textInput("username", "Username:"),
        passwordInput("password", "Password:"),
        actionButton("login", "Login", style = "margin-top: 15px; align-self: center;")
      ),
      easyClose = FALSE,  # Disable closing the modal without interaction
      footer = NULL  # No footer, no cancel button
    ))
  }
  
  # Call login_prompt on startup
  login_prompt()
  
  # Observe the login button click inside the modal
  observeEvent(input$login, {
    user <- input$username
    pass <- input$password
    if (nrow(credentials[credentials$username == user & credentials$password == pass, ]) > 0) {
      user_re(user)
      removeModal()  # Close the modal only on successful login
      shinyalert("Login successful!", type = "success")
      user_logged_in(TRUE)  # Set user_logged_in to TRUE on successful login
    } else {
      shinyalert("Login failed. Try again.", type = "error")
    }
  })
  
  # Continuously check if the user is logged in, and if not, trigger the login modal
  observe({
    if (!user_logged_in()) {
      login_prompt()  # Re-trigger the login modal if the user is not logged in
    }
  })
  
  observeEvent(input$logout, {
    shinyalert("You have been logged out.", type = "info")
    session$reload()
    
  })
  
  rv <- reactiveValues(
    nepaliDatesNowTriggered = FALSE,
    nepaliDatesRegTriggered = FALSE,
    nepaliDatesOutTriggered = FALSE,
    nepaliDatesExpTriggered = FALSE
  )
  
  observe({
    current_date <- format(Sys.Date(), "%Y/%m/%d")
    #Trigger the JavaScript conversion using the current date
    #runjs(sprintf("convertToNepaliDate([%s])", jsonlite::toJSON(current_date)))
    runjs(sprintf("convertToNepaliDate(%s, 'nepaliDatesNow')", jsonlite::toJSON(current_date)))
  })
  
  now <- reactiveValues(
    date_y = NULL,
    date_m = NULL,
    today = NULL 
  )
  
  observeEvent(input$nepaliDatesNow, {
    if (!rv$nepaliDatesNowTriggered) {
      rv$nepaliDatesNowTriggered <- TRUE
      now$date_y<-str_split(input$nepaliDatesNow, "/")[[1]][1] %>% as.integer()
      now$date_m<-str_split(input$nepaliDatesNow, "/")[[1]][2]
      
      now$today<-input$nepaliDatesNow
      
      isolate({
        updateSelectInput(session, 
                          "start_year",
                          #choices = 2000:date_y,
                          selected =now$date_y)
        updateSelectInput(session, 
                          "start_month", 
                          selected=now$date_m)
        updateSelectInput(session, 
                          "end_year", 
                          #choices = date_y:2100,
                          selected=now$date_y)
        updateSelectInput(session, 
                          "end_month", 
                          selected=now$date_m)
        updateSelectInput(session, 
                          "d2_year", 
                          #choices = date_y:2100,
                          selected=now$date_y)
        updateSelectInput(session, 
                          "d2_month", 
                          selected=now$date_m)
        
        thisp<-format(seq.Date(from = floor_date(as.Date(now$today), "year"), to = as.Date(now$today), by = "month"),"%Y/%m")
        
        updateSelectInput(session,
                          'retype',
                          choices=thisp,
                          selected=thisp)
        
      })
      rv$nepaliDatesNowTriggered <- FALSE
    }
  })
  
  observeEvent(input$start_year, {
    updateSelectInput(session,
                      inputId = "end_year", 
                      choices = input$start_year:2100,
                      selected = now$date_y)
    
  })
  
  observeEvent(input$end_year,{
    req(now$date_m)
    choicem<-c("Baishakh"="01",
               "Jestha"="02",
               "Ashadh"="03",
               "Shrawan"="04",
               "Bhadra"="05",
               "Ashwin"="06",
               "Kartik"="07",
               "Mangsir"="08",
               "Poush"="09",
               "Magh"="10",
               "Falgun"="11",
               "Chaitra"="12")
    
    if(input$start_year==now$date_y & input$start_year==input$end_year){
      updateSelectInput(session,
                        inputId = "start_month",
                        choices = choicem[seq(1,which(choicem=="06",1))],
                        selected = now$date_m)
    }else if(input$start_year==input$end_year){
      updateSelectInput(session,
                        inputId = "start_month",
                        choices = choicem[seq(1,which(choicem=="06",1))])
    }
  })
  
  fetch_kobotoolbox_data <- function() {
    # Replace with your API details
    api_url <- "https://kf.kobotoolbox.org/api/v2/assets/"
    username <- Sys.getenv("KOBO_USERNAME")
    password <- Sys.getenv("KOBO_PASSWORD")
    form_id <- Sys.getenv("KOBO_FORM_1")
    
    # Build authentication header
    url <- paste0(api_url, form_id, "/data/")
    req <- httr::GET(url = url, authenticate(username, password),timeout(60))
    
    # Send request and handle response
    response <- httr::content(req, "text", encoding = "UTF-8")
    httr::stop_for_status(req)  # Stop if request fails
    
    # Parse JSON response with jsonlite
    data <- jsonlite::fromJSON(response)
    return(dplyr::select(data$results, c(-`_attachments`, -`_tags`, -`_notes`, -`_validation_status`, -`_geolocation`)))
  }
  
  fetch_outcome_data <- function() {
    # Replace with your API details
    api_url <- "https://kf.kobotoolbox.org/api/v2/assets/"
    username <- Sys.getenv("KOBO_USERNAME")
    password <- Sys.getenv("KOBO_PASSWORD")
    form_id <- Sys.getenv("KOBO_FORM_2")
    
    # Build authentication header
    url <- paste0(api_url, form_id, "/data/")
    req <- httr::GET(url = url, authenticate(username, password))
    
    # Send request and handle response
    response <- httr::content(req, "text", encoding = "UTF-8")
    httr::stop_for_status(req)  # Stop if request fails
    
    # Parse JSON response with jsonlite
    data <- jsonlite::fromJSON(response)
    if (length(data$results) == 0) {
      return(as.data.frame(data$results))
    } else {
      return(dplyr::select(data$results, c(-`_attachments`, -`_tags`, -`_notes`, -`_validation_status`, -`_geolocation`)))
    }
  }
  
  kbdata <- reactiveVal(NULL)
  
  kbupdate <- function() {
    data <- fetch_kobotoolbox_data()
    ou_dt <- fetch_outcome_data()
    if (nrow(ou_dt) != 0) {
      ou_dt <- filter(ou_dt, skip_logic != 'Yes')
    }
    if (nrow(ou_dt) != 0) {
      merged <- left_join(data, ou_dt, join_by("uid" == "C_uid", "leprosyclass" == "C_leprosyclass"))
      data$treatment_outcome <- ifelse(!is.na(merged$C_treatment_outcome), merged$C_treatment_outcome, data$treatment_outcome)
      data$contact_pep <- ifelse(!is.na(merged$C_contact_pep), merged$C_contact_pep, data$contact_pep)
      data$contact_num <- ifelse(!is.na(merged$C_contact_num), merged$C_contact_num, data$contact_num)
      data$outcome_date <- ifelse(!is.na(merged$C_outcome_date), merged$C_outcome_date, data$outcome_date)
    }
    kbdata(data)
  }
  
  # Handle login and data update
  observeEvent(input$login_button, {
    user_logged_in(TRUE)  # Set login status to TRUE after successful login
    kbupdate()  # Immediately trigger data update after login
  })
  
  # Also observe changes in login status to trigger data update
  refresh_timer <- reactiveTimer(30 * 60 * 1000)
  observeEvent(user_logged_in(), {
    if (user_logged_in()) {
      refresh_timer()
      kbupdate()  # Trigger data update when login status becomes TRUE
    }
  })
  
 
  ######
  
  observe({
    req(kbdata())  # Ensure kbdata is not NULL
    
    kbdt <- kbdata()
    reg_date <- kbdt$registration_date
    out_date <- kbdt$outcome_date
    
    reg_date <- ifelse(is.na(reg_date), "NA", reg_date)
    out_date <- ifelse(is.na(out_date), "NA", out_date)
    
    # Trigger JavaScript after UI is fully rendered
    session$onFlushed(function() {
      runjs(sprintf("
      convertToNepaliDate(%s, 'nepaliRegDates');
      convertToNepaliDate(%s, 'nepaliOutDates');
    ", jsonlite::toJSON(reg_date), jsonlite::toJSON(out_date)))
    }, once = TRUE)  # Ensure the code runs only once per flush
  })
  
  
  observeEvent(input$nepaliRegDates, {
    if (!rv$nepaliDatesRegTriggered) {
      rv$nepaliDatesRegTriggered <- TRUE
      kbdt <- kbdata()
      kbdt$reg_nedate<-input$nepaliRegDates
      kbdt$reg_neym<-substring(kbdt$reg_nedate,1,7)
      kbdata(kbdt)
      rv$nepaliDatesRegTriggered <- FALSE
    }
  })
  
  observeEvent(input$nepaliOutDates, {
    if (!rv$nepaliDatesOutTriggered) {
      rv$nepaliDatesOutTriggered <- TRUE
      kbdt <- kbdata()
      kbdt$out_nedate<-input$nepaliOutDates
      kbdt$out_neym<-substring(kbdt$out_nedate,1,7)
      kbdata(kbdt)
      rv$nepaliDatesOutTriggered <- FALSE
    }
  })
  

  ## Data for dashboard analysis ##################################################
  
  data<-reactive({
    #print(input$ous)
    #print(head(kbdata()))
    req(kbdata()$reg_nedate, kbdata()$out_nedate)
    if (is.null(input$ous) || length(input$ous) == 0) {
      kbdt <- kbdata() %>% 
        mutate(registration_date = as.Date(registration_date),
               outcome_date = as.Date(outcome_date),
               age = as.integer(age),
               reg_year = year(registration_date),
               reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
               out_mth = format_ISO8601(ymd(outcome_date), precision = "ym")) %>%
        mutate(lat = as.numeric(stringr::str_split_i(gps, " ", 1)),
               lng = as.numeric(stringr::str_split_i(gps, " ", 2))) %>%
        mutate(exp = ifelse(leprosyclass == "1", registration_date + months(12), registration_date + months(6))) %>%
        mutate(exp = as.Date(exp)) %>%
        mutate(fu = ifelse(exp <= Sys.Date() & treatment_outcome == "4", 1, 0)) %>%
        mutate(leprosyclass = ifelse(leprosyclass == "1", "MB", "PB")) %>%
        filter(is.na(training_practice)) %>%
        mutate(outcome_la = case_when(
          treatment_outcome == "1" ~ "RFT",
          treatment_outcome == "2" ~ "T-OUT",
          treatment_outcome == "3" ~ "Defaulter",
          treatment_outcome == "4" ~ "Under Treatment",
          treatment_outcome == "5" ~ "Loss of Follow-up",
          treatment_outcome == "6" ~ "Other",
          treatment_outcome == "RFT" ~ "RFT")) %>%
        mutate(popup = paste("Type:", leprosyclass, "<br>Register Date:", reg_nedate,
                             "<br>Recored Outcome Date:", out_nedate,
                             "<br>Outcome:", outcome_la))
      
      #if (length(input$range) == 2) {
      #  start <- ymd(input$range[1])
      #  end <- ymd(input$range[2])
      #  date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y-%m")
      #} else {
      #  date_range <- format(input$range, "%Y-%m")
      #}
      start<-ym(paste0(input$start_year,"/",input$start_month))
      end<-ym(paste0(input$end_year,"/",input$end_month))
      date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y/%m")
      
      #dt <- kbdt %>%
      #  filter(reg_mth %in% date_range)
      
      dt <- kbdt %>%
        filter(reg_neym %in% date_range)
      
      dt 
    } else{
      
      kbdt <- kbdata()%>%
        filter(muni_code%in%input$ous) %>% 
        mutate(registration_date = as.Date(registration_date),
               outcome_date = as.Date(outcome_date),
               age = as.integer(age),
               reg_year = year(registration_date),
               reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
               out_mth = format_ISO8601(ymd(outcome_date), precision = "ym")) %>%
        mutate(lat = as.numeric(stringr::str_split_i(gps, " ", 1)),
               lng = as.numeric(stringr::str_split_i(gps, " ", 2))) %>%
        mutate(exp = ifelse(leprosyclass == "1", registration_date + months(12), registration_date + months(6))) %>%
        mutate(exp = as.Date(exp)) %>%
        mutate(fu = ifelse(exp <= Sys.Date() & treatment_outcome == "4", 1, 0)) %>%
        mutate(leprosyclass = ifelse(leprosyclass == "1", "MB", "PB")) %>%
        filter(is.na(training_practice)) %>%
        mutate(outcome_la = case_when(
          treatment_outcome == "1" ~ "RFT",
          treatment_outcome == "2" ~ "T-OUT",
          treatment_outcome == "3" ~ "Defaulter",
          treatment_outcome == "4" ~ "Under Treatment",
          treatment_outcome == "5" ~ "Loss of Follow-up",
          treatment_outcome == "6" ~ "Other",
          treatment_outcome == "RFT" ~ "RFT")) %>%
        mutate(popup = paste("Type:", leprosyclass, "<br>Register Date:", reg_nedate,
                             "<br>Recored Outcome Date:", out_nedate,
                             "<br>Outcome:", outcome_la))
      
      #if (length(input$range) == 2) {
      #  start <- ymd(input$range[1])
      #  end <- ymd(input$range[2])
      #  date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y-%m")
      #} else {
      #  date_range <- format(input$range, "%Y-%m")
      #}
      start<-ym(paste0(input$start_year,"/",input$start_month))
      end<-ym(paste0(input$end_year,"/",input$end_month))
      date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y/%m")
      
      #dt <- kbdt %>%
      #  filter(reg_mth %in% date_range)
      
      dt <- kbdt %>%
        filter(reg_neym %in% date_range)
      
      dt}
    
  })
  
  ################################################################################
  
  valuebox_dt <- reactive({
    dat <- data()
    if (!is.null(dat)) {
      newcases <- sum(dat$registered_as == "1", na.rm = TRUE)
      newg2d <- sum(dat$registered_as == "1" & dat$i_who_disability == "2", na.rm = TRUE)
      newch <- sum(dat$registered_as == "1" & dat$age <= 14, na.rm = TRUE)
      
      se_ous <- if (length(input$ous) > 0) as.character(input$ous) else NULL
      dt <- kbdata()
      if (!is.null(dt)) {
        if (is.null(se_ous)) {
          txfu <- sum(dt$treatment_outcome == "4", na.rm = TRUE)
          cofu <- sum(dt$contact_pep == "0", na.rm = TRUE)
        } else {
          txfu <- sum(dt$treatment_outcome == "4" & dt$muni_code %in% se_ous, na.rm = TRUE)
          cofu <- sum(dt$contact_pep == "0" & dt$muni_code %in% se_ous, na.rm = TRUE)
        }
        
        analysis <- data.frame(nd = nrow(dat), nc = newcases, n2 = newg2d, nch = newch, tx = txfu, co = cofu)
        return(analysis)
      }
    }else{
      return(data.frame(nd = 0, nc = 0, n2 = 0, nch = 0, tx = 0, co = 0))
    }
  })
  
  output$newrecord <- renderValueBox({
    valueBox(
      valuebox_dt()$nd, "Total Detected", icon = icon("cloud-arrow-up", lib = "font-awesome"),
      color = "aqua")
  })
  
  output$newlept <- renderValueBox({
    valueBox(
      valuebox_dt()$nc, "New Cases", icon = icon("hospital-user", lib = "font-awesome"),
      color = "aqua")
  })
  
  output$newleg2d <- renderValueBox({
    valueBox(
      valuebox_dt()$n2, "New G2D Cases", icon = icon("person-dots-from-line", lib = "font-awesome"),
      color = "aqua")
  })
  
  output$newlech <- renderValueBox({
    valueBox(
      valuebox_dt()$nch, "New Child Cases", icon = icon("children", lib = "font-awesome"),
      color = "aqua")
  })
  
  output$txfu <- renderValueBox({
    valueBox(
      valuebox_dt()$tx, "Treatment Follow-up", icon = icon("tablets", lib = "font-awesome"),
      color = "maroon")
  })
  
  output$cont <- renderValueBox({
    valueBox(
      valuebox_dt()$co, "Contact Not Done", icon = icon("circle-nodes", lib = "font-awesome"),
      color = "maroon")
  })
  
  ################################################################################################
  output$newmap <- renderLeaflet({
    
    dt<-data()
    
    # Render the leaflet map
    l <- leaflet() %>%
      setView(lng = 84.1240, lat = 28.3949, zoom = 7) %>%
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery")%>% 
      addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"))
    
    if (nrow(dt) > 0) {
      l <- l %>%
        addCircleMarkers(
          data = dt,
          lng = ~lng, lat = ~lat,
          label = ~uid, popup = ~popup, color = "red", stroke = FALSE, fillOpacity = 0.5, radius = 6,
          clusterOptions = markerClusterOptions()
        )
    }
    
    l
  })
  
  summary<-reactiveVal(NULL)
  
  observe({
    
    dt<-data()
    
    summary_dt <- dt %>%
      select(province, district, municipality) %>%
      group_by(province, district, municipality) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      ungroup()
    
    summary(summary_dt)
  })
  
  output$newtable <- renderReactable({
    
    req(summary())
    
    reactable(summary(),
              groupBy = "province", 
              columns = list(
                province = colDef(name = "Province"),
                district = colDef(name = "District"),
                municipality =colDef(name = "Municipality"),
                n = colDef(
                  name="Detected Cases",
                  aggregate = "sum",
                  cell = function(value) {
                    if(value>=3){
                      classes <-"tag num-high"
                    }else if (value==2){
                      classes <-"tag num-med"
                    }else{
                      classes <-"tag num-low"
                    }
                    span(class = classes, value)}
                )
              ),
              defaultExpanded = T,
              searchable = TRUE,
              paginationType = "simple",
              paginateSubRows = TRUE,
              resizable = TRUE,
              height = 400,
              elementId = "cars-expansion-table"
    )
    
  })
  
  observeEvent(input$expand_button, {
    js$toggleAllRows()
  })
  
  ################################################################################################
  
  ### Plot Leprosy Cases
  output$lepcases<-renderPlotly({
    req(kbdata()$reg_nedate, kbdata()$out_nedate)
    
    se_ous<-as.character(input$ous)
    if(length(input$ous)==0){
      dt<-kbdata()
    }else{
      dt<-kbdata() %>% 
        filter(muni_code %in% se_ous)
    }
    
    dt<-dt%>% 
      mutate(registration_date := as.Date(registration_date),
             outcome_date := as.Date(outcome_date),
             age := as.integer(age),
             reg_year = year(registration_date),
             reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
             out_mth = format_ISO8601(ymd(outcome_date), precision = "ym")) %>% 
      mutate(reg_ney=as.integer(substring(reg_nedate,1,4)))
    
    #last5y <- format(seq.Date(Sys.Date() - years(5), Sys.Date(), by = "month"), "%Y-%m")
    
    netoday<-as.Date(now$today)
    
    last5y <- format(seq.Date(netoday - years(5), netoday, by = "month"), "%Y/%m")
    
    p_dt<-dt %>%
      filter(reg_neym %in% last5y) %>% 
      group_by(reg_ney) %>% 
      summarise(new=sum(registered_as=="1",na.rm = T),
                child=sum(age<=14,na.rm = T),
                cpro=ifelse(new==0,0,(sum(age<=14,na.rm = T)/sum(registered_as=="1",na.rm = T)*100)),
                g2d=sum(i_who_disability=="2",na.rm = T),
                g2pro=ifelse(new==0,0,sum(i_who_disability=="1",na.rm = T)/sum(registered_as=="1",na.rm = T)*100))
    
    if(nrow(p_dt)==0){maxn<-100}else{maxn<-max(p_dt$new)}
    
    if(nrow(p_dt)==0){
      minx<-year(netoday)-5
      maxx<-year(netoday)
    }else{
      maxx<-max(p_dt$reg_ney)
      minx<-maxx-5
    }
    
    all_years <- tibble(reg_ney = seq(minx, maxx, by = 1))
    p_dt <- all_years %>%
      left_join(p_dt, by = "reg_ney") %>%
      replace_na(list(new = 0, child = 0, cpro = 0, g2d = 0, g2pro = 0))
    
    p <- ggplot(p_dt, aes(x = reg_ney, y = new)) +
      geom_bar(stat = "identity", fill = "#6495ED") +
      labs(
        x = "Registered Year",
        y = "New Case Numbers"
      ) +
      scale_y_continuous(limits = c(0, max(p_dt$new) + 1)) +
      scale_x_continuous(breaks = seq(min(p_dt$reg_ney), max(p_dt$reg_ney), by = 1)) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)  
    
  })
  
  
  
  ##############################################################################
  
  ### Plot among new cases
  output$amongn<-renderPlotly({
    req(kbdata()$reg_nedate, kbdata()$out_nedate)
    
    se_ous<-as.character(input$ous)
    
    if(length(input$ous)==0){
      dt<-kbdata()
    }else{
      dt<-kbdata() %>% 
        filter(muni_code %in% se_ous)
    }
    
    dt<-dt%>% 
      mutate(registration_date := as.Date(registration_date),
             outcome_date := as.Date(outcome_date),
             age := as.integer(age),
             reg_year = year(registration_date),
             reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
             out_mth = format_ISO8601(ymd(outcome_date), precision = "ym")) %>%  
      mutate(reg_ney=as.integer(substring(reg_nedate,1,4)))
    
    #last5y <- format(seq.Date(Sys.Date() - years(5), Sys.Date(), by = "month"), "%Y-%m")
    
    netoday<-as.Date(now$today)
    
    last5y <- format(seq.Date(netoday - years(5), netoday, by = "month"), "%Y/%m")
    
    p_dt<-dt %>%
      filter(reg_neym %in% last5y) %>% 
      group_by(reg_ney) %>% 
      summarise(new=sum(registered_as=="1",na.rm = T),
                child=sum(age<=14,na.rm = T),
                cpro=ifelse(new==0,0,(sum(age<=14,na.rm = T)/sum(registered_as=="1",na.rm = T)*100)),
                g2d=sum(i_who_disability=="2",na.rm = T),
                g2pro=ifelse(new==0,0,sum(i_who_disability=="1",na.rm = T)/sum(registered_as=="1",na.rm = T)*100))
    
    #if(nrow(p_dt)==0){maxn<-100}else{maxn<-max(p_dt$cpro)}
    
    if(nrow(p_dt)==0){
      minx<-year(netoday)-5
      maxx<-year(netoday)
    }else{
      maxx<-max(p_dt$reg_ney)
      minx<-maxx-5
    }
    
    all_years <- tibble(reg_ney = seq(minx, maxx, by = 1))
    p_dt <- all_years %>%
      left_join(p_dt, by = "reg_ney") %>%
      replace_na(list(new = 0, child = 0, cpro = 0, g2d = 0, g2pro = 0))
    
    p_dt_long <- pivot_longer(p_dt, cols = c(cpro, g2pro),
                              names_to = "Indicatiors", values_to = "value")
    p_dt_long<-p_dt_long |> 
      mutate(Indicatiors:=ifelse(Indicatiors=="cpro","Child %","G2D %"))
    
    # Create ggplot object
    p<-ggplot(p_dt_long, aes(x = reg_ney, y = value, color = Indicatiors, group = Indicatiors)) +
      geom_line() +
      labs(
        x = "Registered Year",
        y = "Percentage (%)"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(
        legend.position = "left",
        legend.justification = c("left", "top")
      )
    ggplotly(p)
  })
  
  #########################################################################
  ### Follow-up Map########################################################
  #########################################################################
  fudt<-reactiveVal(NULL)
  
  output$fumap <- renderLeaflet({
    req(kbdata()$reg_nedate, kbdata()$out_nedate)
    
    se_ous<-as.character(input$ous)
    
    kbdt <- kbdata()
    
    if (length(se_ous) > 0) {
      kbdt <- kbdt %>%
        filter(muni_code %in% se_ous)
    }
    
    if (nrow(kbdt)==0){
        leaflet() %>%
        setView(lng = 84.1240, lat = 28.3949, zoom = 7) %>% 
        addTiles()
      } else {
          
          kbdt<-kbdt %>% 
            mutate(registration_date := as.Date(registration_date),
                   outcome_date := as.Date(outcome_date),
                   age := as.integer(age),
                   reg_year = year(registration_date),
                   reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
                   out_mth = format_ISO8601(ymd(outcome_date), precision = "ym")) %>% 
            mutate(lat=as.numeric(stringr::str_split_i(gps, " " , 1)),
                   lng=as.numeric(stringr::str_split_i(gps, " " , 2)))%>% 
            mutate(exp=ifelse(leprosyclass=="1",registration_date+months(12),registration_date+months(6))) %>% 
            mutate(exp :=as.Date(exp)) %>% 
            mutate(fu=ifelse(exp<=Sys.Date()&treatment_outcome=="4",1,0)) %>%
            mutate(leprosyclass:=ifelse(leprosyclass=="1","MB","PB")) %>% 
            filter(is.na(training_practice)) %>% 
            mutate(outcome_la = case_when(treatment_outcome=="1"~"RFT",
                                          treatment_outcome=="2"~"T-OUT",
                                          treatment_outcome=="3"~"Defaulter",
                                          treatment_outcome=="4"~"Under Treatment",
                                          treatment_outcome=="5"~"Loss of Follow-up",
                                          treatment_outcome=="6"~"Other",
                                          treatment_outcome=="RFT"~"RFT")) %>% 
            mutate(popup = paste("Type:", leprosyclass, "<br>Register Date:", reg_nedate,
                                 "<br>Recored Outcome Date:", out_nedate,
                                 "<br>Outcome:",outcome_la))
          
          fu<-kbdt %>% 
            mutate(cat=case_when(contact_pep==0 & treatment_outcome=="4" & fu==1 ~1,
                                 contact_pep==0 & treatment_outcome!="4" ~ 2,
                                 contact_pep==1 & treatment_outcome=="4" & fu==1 ~3,
                                 contact_pep==0 & treatment_outcome=="4" & fu==0 ~4,
                                 contact_pep==1 & treatment_outcome=="4" & fu==0 ~5,)) %>% 
            filter(!is.na(cat))
          
          fudt(fu)
          
          if(nrow(fu)==0){
            
            leaflet() %>%
              setView(lng = 84.1240, lat = 28.3949, zoom = 7) %>% 
              addTiles()
            
          }else{
            meanx<-mean(fu$lng)
            meany<-mean(fu$lat)
            #nepal_center <- c(28.3949, 84.1240)
            
            l <- leaflet() %>%
              setView(lng = meanx, lat = meany, zoom = 7) %>% 
              addTiles()
            
            getColor <- function(cat) {
              sapply(cat, function(x) {
                if (x == 1) {
                  return("red") 
                } else if (x ==2) {
                  return("pink")
                } else if (x == 3) {
                  return("purple")
                } else if (x == 4) {
                  return("orange")
                } else {
                  return("green")
                }
              })
            }
            
            icons <- awesomeIcons(
              icon = 'ios-close',
              iconColor = 'black',
              library = 'ion',
              markerColor = ~getColor(cat))
            
            cat.df <- split(fu,fu$cat)
            
            name<-matrix(nrow=5,ncol=2)
            name[,1]<-seq(1,5)
            name[,2]<-c("Contact Not done + Outcome Overdue",
                        "Contact Not done + Outcome Complete",
                        "Contact Done + Outcome Overdue", 
                        "Contact Not done + Under Treatment", 
                        "Contact Done + Under Treatment")
            
            names(cat.df)<-name[,2][name[,1]%in%unique(fu$cat)]
            
            names(cat.df) %>%
              purrr::walk( function(df) {
                l <<- l %>%
                  addAwesomeMarkers(
                    data=cat.df[[df]], lng=~lng, lat=~lat,
                    label=~uid,
                    popup = ~popup,
                    group=df,
                    icon=icons#,
                    #clusterOptions = markerClusterOptions()
                  )
              })
            
            category<-c("Contact Not done + Outcome Overdue",
                        "Contact Not done + Outcome Complete",
                        "Contact Done + Outcome Overdue", 
                        "Contact Not done + Under Treatment", 
                        "Contact Done + Under Treatment")
            counts<-c(sum(fu$cat==1),sum(fu$cat==2),sum(fu$cat==3),sum(fu$cat==4),sum(fu$cat==5))
            
            labels_with_counts <- paste0(category," (", counts, ")")
            
            
            l %>%
              addLayersControl(
                overlayGroups=names(cat.df),
                options = layersControlOptions(collapsed = T))%>%
              addLegend(
                position = "bottomleft",
                title = "Markers",
                colors = c("red", "pink","purple", "orange", "green"),
                labels = labels_with_counts
              )
          }}
  })
  
  
  ### Child among new
  
  #output$childmap <- renderLeaflet({
  #  nepal_center <- c(28.3949, 84.1240)
  #  leaflet() %>%
  #    setView(lng = nepal_center[2], lat = nepal_center[1], zoom = 7) %>% 
  #    addTiles()
  #})
  
  observe({
    req(fudt()$exp)
    fu <- fudt()
    exp_date <-as.character(fu$exp)
    exp_date <- ifelse(is.na(exp_date), "NA", exp_date)
    # Trigger the JavaScript conversion using the current date
    runjs(sprintf("convertToNepaliDate(%s, 'nepaliExpDates')", jsonlite::toJSON(exp_date)))
  })
  
  observeEvent(input$nepaliExpDates, {
    req(fudt()$exp)
    if (!rv$nepaliDatesExpTriggered) {
      rv$nepaliDatesExpTriggered <- TRUE
      fu <- fudt()
      fu$exp_nedate<-input$nepaliExpDates
      fudt(fu)
      rv$nepaliDatesExpTriggered <- FALSE
    }
  })
  
  
  ##################
  ### Follow-up List
  ##################
  fu_list <- reactiveVal(NULL)
  gpx<-reactiveVal(NULL)
  
  output$futable <- renderUI({
    if (is.null(fu_list())) {
      tagList(
        br(),
        h4("1. By default, the follow-up list includes all patients up to NOW; otherwise, select the cohort range on the left"),
        h4("2. Select locations from the above dropdown menu [Select Locations]"),
        h4("3. By default, ALL types of follow-up action are included"),
        h4("4. Click [Generate] to preview the table"),
        h4("5. Click [Table Download] to download an excel file"),
        h4("6. Click [GPX Download] to download a GPX file to load on GPX VIEWR for field work"),
        br()
      )
    } else {
      reactable(fu_list(),
                showPageInfo = TRUE,
                defaultPageSize = 20,
                filterable = TRUE,
                compact = TRUE,
                searchable = TRUE)
    }
  })
  
  observeEvent(input$tabs, {
    if(input$tabs=="list"){
      updateSelectInput(session, 
                        "start_year",
                        selected =now$date_y)
      updateSelectInput(session, 
                        "start_month", 
                        selected=now$date_m)
      updateSelectInput(session, 
                        "end_year", 
                        selected=now$date_y)
      updateSelectInput(session, 
                        "end_month", 
                        selected=now$date_m)
    }
  })
  
  fugenerate <- reactiveVal(FALSE)
  observeEvent(input$fugen, {
    fugenerate(FALSE)
    if (!fugenerate()) {
      fu <- fudt()
      
      start<-ym(paste0(input$start_year,"/",input$start_month))
      end<-ym(paste0(input$end_year,"/",input$end_month))
      
      date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y/%m")
      
      nowym<-paste0(now$date_y,"/",now$date_m)
      
      if(all(date_range==nowym)){
        fu<-fu
      }else{
        fu<-filter(fu,reg_neym %in% date_range)
      }
      
      if(any(input$futype==0)){type<-1:5}else{type<-input$futype}
      
      #print(class(input$futype))
      
      fu_slice <- fu %>% 
        filter(cat %in% type) %>% 
        select(registration_number, reg_nedate, province, district, municipality, muni_code, ward_no,
               leprosyclass, contact_pep, treatment_outcome, out_nedate, exp_nedate, cat,username, email) %>% 
        arrange(reg_nedate,cat) %>% 
        mutate(contact_pep:=ifelse(contact_pep=="1","Yes","No"),
               leprosyclass:=ifelse(leprosyclass=="1","MB","PB"),
               treatment_outcome:=case_when(treatment_outcome=="1"~"RFT",
                                            treatment_outcome=="2"~"T-OUT",
                                            treatment_outcome=="3"~"Defaulter",
                                            treatment_outcome=="4"~"Under Treatment",
                                            treatment_outcome=="5"~"Loss of Follow-up",
                                            treatment_outcome=="6"~"Other",
                                            treatment_outcome=="RFT"~"RFT"),
               Action=case_when(cat==1~"Contact Tracing + Outcome Follow-up",
                                cat==2~"Contact Tracing",
                                cat==3~"Outcome Follow-up",
                                cat==4~"Contact Tracing + Treatment Follow-up",
                                cat==5~"Treatment Follow-up"),
               cat:=case_when(cat==1~"Contact Not done + Outcome Overdue",
                              cat==2~"Contact Not done + Outcome Complete",
                              cat==3~"Contact Done + Outcome Overdue",
                              cat==4~"Contact Not done + Under Treatment",
                              cat==5~"Contact Done + Under Treatment"))
      
      colnames(fu_slice)<-
        c("Registration Number","Registered Date","Province","District","Municipality","muni_code","Ward","Leprosy Class",
          "Contact & PEP", "Treatment Outcome", "Outcome Date", "Expected RFT Date", "Current Status","Data Collector","Collector Email","Action")
      
      fu_slice<-fu_slice[,c("Registration Number","Registered Date","Province","District","Municipality","muni_code","Ward","Leprosy Class",
                            "Contact & PEP", "Treatment Outcome", "Outcome Date", "Expected RFT Date", "Current Status","Action","Data Collector","Collector Email")]
      
      se_ous<-as.character(input$ous)
      
      if(length(input$ous)==0){
        fu_slice<-fu_slice %>% 
          select(-muni_code)
        fu_list(fu_slice)
      }else{
        fu_slice<-fu_slice %>% 
          filter(muni_code %in% se_ous) %>% 
          select(-muni_code)
        fu_list(fu_slice)
      }
      
      
      fu_gpx <- fu %>% 
        filter(cat %in% type) %>% 
        mutate(contact_pep:=ifelse(contact_pep=="1","Yes","No"),
               leprosyclass:=ifelse(leprosyclass=="1","MB","PB"),
               treatment_outcome:=case_when(treatment_outcome=="1"~"RFT",
                                            treatment_outcome=="2"~"T-OUT",
                                            treatment_outcome=="3"~"Defaulter",
                                            treatment_outcome=="4"~"Under Treatment",
                                            treatment_outcome=="5"~"Loss of Follow-up",
                                            treatment_outcome=="6"~"Other",
                                            treatment_outcome=="RFT"~"RFT"),
               Action=case_when(cat==1~"Contact Tracing + Outcome Follow-up",
                                cat==2~"Contact Tracing",
                                cat==3~"Contact Tracing + Treatment Follow-up",
                                cat %in% c(4,5)~"Treatment Follow-up"))
      
      se_ous<-as.character(input$ous)
      
      if(length(input$ous)==0){
        fu_gpx
      }else{
        fu_gpx<-fu_gpx %>% 
          filter(muni_code %in% se_ous) 
      }
      
      fusf<-st_as_sf(fu_gpx,coords = c("lng","lat"))
      st_crs(fusf)<-4326
      
      gpx<-fusf %>% 
        mutate(name=paste("UID:",uid,"/","Name:",full_name_IC),
               desc=paste("Action:",Action),
               cmt=paste("Registered Date:",as.character(reg_nedate)),
               src=paste("")) %>% 
        select(name,desc,cmt,src)
      
      gpx(gpx)
      fugenerate(TRUE)}
  })
  
  
  fuclear<-reactiveVal(FALSE)
  observeEvent(input$clearlist, {
    fuclear(FALSE)
    if (!fuclear()) {
      fu_list(NULL)
      fuclear(TRUE)}
  })
  
  output$fuload <- downloadHandler(
    filename = function() {
      paste("Follow-up_List_", format(Sys.time(), "%Y-%m-%dT%H%M%S"),".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(fu_list(), file,sheetName ="List")
    }
  )
  
  output$fugpx <-downloadHandler(
    filename = function() {
      paste("Follow-up_GPX_", format(Sys.time(), "%Y-%m-%dT%H%M%S"),".gpx", sep = "")
    },
    content = function(file) {
      sf::st_write(gpx(),driver="GPX",file)
    }
  )
  
  
  ##################
  ### Report
  ##################
  
  ft_list_re<-reactiveVal(NULL)
  
  get_file_name <- function(type) {
    paste0(type, "_", session$token, ".html")
  }
  
  getPage <- function() {
    if (length(input$munichoice) == 0) {
      return(includeHTML(get_file_name("group")))
    } else {
      return(includeHTML(get_file_name("groupf")))
    }
  }
  
  #getPage<-function() {
  #  if (length(input$munichoice)==0){return(includeHTML("group.html"))}else{
  #    return(includeHTML("groupf.html"))
  #  }
  #}
  
  output$retable <- renderUI({
    if (is.null(ft_list_re())) {
      tagList(
        br(),
        h4("1. Monthly report of THIS YEAR is set by default"),
        h4("2. Select locations from the above dropdown menu [Select Locations]"),
        h5("#The data will be calculated at the Municipality level but you can select mulitiple Municipalities by different levels."),
        h4("3. Click [Generate] to view the table"),
        h4("4. Click [Download] to download an excel file"),
        br()
      )
    }else{
      getPage()
    }
  })
  
  period_re<-reactiveVal(NULL)
  unique_loc_re<-reactiveVal(NULL)
  
  repgenerate <- reactiveVal(FALSE)
  observeEvent(input$regen, {
    repgenerate(FALSE)
    if (!repgenerate()) {
      
      if (is.null(input$ous) || length(input$ous) == 0) {
        shinyalert("Error", "Please select at least one location.", type = "error")
      } else {
        
        
        intb<-read.csv("indtable.csv")
        ouls<-read.csv("code_list.csv")
        ouls$pcode<-as.character(ouls$pcode)
        ouls<-ouls %>% select(-National)
        
        
        kbdt <- kbdata() %>% 
          mutate(reg_date := as.Date(reg_nedate),
                 outcome_date := as.Date(out_nedate),
                 age := as.integer(age),
                 reg_year = year(reg_date),
                 reg_mth = format(reg_date,"%Y/%m"),
                 out_mth = format(outcome_date,"%Y/%m")) %>% 
          mutate(lat=as.numeric(stringr::str_split_i(gps, " " , 1)),
                 lng=as.numeric(stringr::str_split_i(gps, " " , 2)))%>% 
          mutate(exp=ifelse(leprosyclass=="1",reg_date+months(12),reg_date+months(6))) %>% 
          mutate(exp :=as.Date(exp)) %>% 
          mutate(fu=ifelse(exp<=Sys.Date()&treatment_outcome=="4",1,0)) %>%
          mutate(leprosyclass:=ifelse(leprosyclass=="1","MB","PB")) %>% 
          mutate(contact_num:=as.integer(contact_num))%>% 
          filter(is.na(training_practice))
        
        
        se_ous<-as.character(input$ous)
        
        kbdt<-kbdt %>% 
          filter(muni_code %in% se_ous)
        
        period<-input$retype %>% rev()
          
        period_re(period)
        
        #print(head(kbdt))
        #print(class(period))
        
        grouping_cols <- if (input$replevel == "1") {
          c("province", "class_gen_f")
        } else if (input$replevel == "2") {
          c("district", "class_gen_f")
        } else {
          c("muni_code", "class_gen_f")
        }
        #print(grouping_cols)
        
        if (input$replevel == "3") {
          skeleton <- expand.grid(
            muni_code = unique(se_ous)[unique(se_ous)%in%ouls$pcode],
            class_gen_f = c("MB/M", "MB/F", "PB/M", "PB/F"),
            period = period
          )
        } else if (input$replevel == "2") {
          skeleton <- expand.grid(
            district = unique(se_ous)[unique(se_ous)%in%ouls$District],
            class_gen_f = c("MB/M", "MB/F", "PB/M", "PB/F"),
            period = period
          )
        } else {
          skeleton <- expand.grid(
            province = unique(se_ous)[unique(se_ous)%in%ouls$Province],
            class_gen_f = c("MB/M", "MB/F", "PB/M", "PB/F"),
            period = period
          )
        }
        
        skeleton$period<-as.character(skeleton$period)
        
        in_list<-list()
        for (p in period){
          temp_df <- kbdt %>%
            mutate(
              gen_f = case_when(gender == "1" ~ "Male", gender == "2" ~ "Female"),
              class_gen = case_when(
                gender == "1" & leprosyclass == "MB" ~ "MB/M",
                gender == "2" & leprosyclass == "MB" ~ "MB/F",
                gender == "1" & leprosyclass == "PB" ~ "PB/M",
                gender == "2" & leprosyclass == "PB" ~ "PB/F"
              )
            ) %>% 
            mutate(class_gen_f = fct_relevel(class_gen, c("MB/M", "MB/F", "PB/M", "PB/F"))) %>% 
            group_by(!!!rlang::syms(grouping_cols)) %>% 
            summarise(
              ###
              i_13_1 = sum(reg_mth < p & treatment_outcome == "4", na.rm = TRUE),
              i_13_2 = sum(reg_mth == p & registered_as == "1", na.rm = TRUE), # NEW
              i_13_3 = sum(reg_mth == p & registered_as == "4", na.rm = TRUE), # Relapsed
              i_13_4 = sum(reg_mth == p & registered_as == "3", na.rm = TRUE), # Restart
              i_13_5 = sum(reg_mth == p & registered_as == "2", na.rm = TRUE), # in
              i_13_6 = sum(reg_mth == p & registered_as == "5", na.rm = TRUE), # other
              i_13_7 = i_13_1 + i_13_2 + i_13_3 + i_13_4 + i_13_5 + i_13_6,
              i_13_8 = i_13_7,
              ###
              i_13_9 = sum(out_mth == p & treatment_outcome == "1", na.rm = TRUE), # RFT
              i_13_10 = sum(out_mth == p & treatment_outcome == "2", na.rm = TRUE), # T-OUT
              i_13_11 = sum(out_mth == p & treatment_outcome == "5", na.rm = TRUE), # LossFU
              i_13_12 = sum(out_mth == p & treatment_outcome %in% c("3", "6"), na.rm = TRUE), # Other
              i_13_13 = i_13_9 + i_13_10 + i_13_11 + i_13_12, # TOTAL DEDUCTED
              i_13_14 = i_13_7 - i_13_13,
              ###
              i_13_15 = sum(reg_mth == p & age < 15 & registered_as == "1", na.rm = TRUE),
              i_13_16 = sum(reg_mth == p & age < 15 & treatment_outcome == "4", na.rm = TRUE),
              i_13_17 = sum(reg_mth == p & (skin_smear_test %in% c("1", "2")) & registered_as == "1", na.rm = TRUE),
              i_13_18 = sum(reg_mth == p & skin_smear_test == "1" & registered_as == "1", na.rm = TRUE),
              ###
              i_13_19_1 = sum(reg_mth == p & lepra_reaction == "1", na.rm = TRUE),
              i_13_19_2 = sum(reg_mth == p & lepra_reaction == "2", na.rm = TRUE),
              i_13_19_3 = sum(reg_mth == p & lepra_reaction == "3", na.rm = TRUE),
              ###
              i_13_21 = sum(reg_mth < p & contact_pep == "0", na.rm = TRUE),
              i_13_22 = sum(reg_mth == p & contact_pep == "1", na.rm = TRUE),
              i_13_23 = i_13_21 * 25,
              i_13_24 = sum(contact_num[reg_mth == p], na.rm = TRUE),
              i_13_25 = sum(reg_mth == p & (registered_as == "1" & case_detection_method %in% c("3", "5")), na.rm = TRUE),
              ###
              d_n_0 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "0"), na.rm = TRUE),
              d_n_1 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "1"), na.rm = TRUE),
              d_n_2 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "2"), na.rm = TRUE),
              d_n_x = sum(reg_mth == p & (registered_as == "1" & is.na(i_who_disability)), na.rm = TRUE),
              d_nc_0 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "0"), na.rm = TRUE),
              d_nc_1 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "1"), na.rm = TRUE),
              d_nc_2 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "2"), na.rm = TRUE),
              d_nc_x = sum(reg_mth == p & (registered_as == "1" & age < 15 & is.na(i_who_disability)), na.rm = TRUE),
              .groups = 'drop'
            )
          
          merged_df <- skeleton %>%
            filter(period == p) %>%
            left_join(temp_df, by = grouping_cols) %>%
            replace_na(list(
              i_13_1 = 0, i_13_2 = 0, i_13_3 = 0, i_13_4 = 0, i_13_5 = 0, 
              i_13_6 = 0, i_13_7 = 0, i_13_8 = 0, i_13_9 = 0,  i_13_10 = 0, i_13_11 = 0, 
              i_13_12 = 0, i_13_13 = 0, i_13_14 = 0, i_13_15 = 0, i_13_16 = 0, i_13_17 = 0, 
              i_13_18 = 0, i_13_19_1 = 0, i_13_19_2 = 0, i_13_19_3 = 0, i_13_21 = 0,
              i_13_22 = 0, i_13_23 = 0, i_13_24 = 0, i_13_25 = 0, d_n_0 = 0, d_n_1 = 0, 
              d_n_2 = 0, d_n_x = 0, d_nc_0 = 0, d_nc_1 = 0, d_nc_2 = 0, d_nc_x = 0
            )) |> 
            select(-period)
          
          if (input$replevel=="1"){
            merged_df<-merged_df %>% rename(location=province)
          }else if(input$replevel=="2"){
            merged_df<-merged_df %>% rename(location=district)
          }else{
            merged_df<-merged_df %>% rename(location=muni_code)
          }
          
          in_list[[p]] <- merged_df
        }
        
        #print(kbdt)
        #print(skeleton)
        #print(merged_df)
        
        ft_list<-gt_group()
        for(p in period){
          df<-in_list[[p]]
          dat<-pivot_longer(df,
                            cols = -c(location,class_gen_f),
                            values_to = "indicator",
                            names_to = "value")
          dat<-dat %>% 
            group_by(location,class_gen_f) %>% 
            ungroup()
          
          # Generate template data frame                                                                                                                                                                                                                                                                                                                     ame with all possible combinations of muni_code, class_gen_f, and indicator
          template <- expand.grid(
            location=unique(dat$location),
            class_gen_f = c("MB/M","MB/F","PB/M","PB/F"),
            value = intb$rcode
          )
          
          # Merge template with original data frame to fill missing values with 0
          filled_df <- full_join(dat,template, by = c("location","class_gen_f", "value"))
          filled_df[is.na(filled_df$indicator), "indicator"] <- 0
          
          filled_df$class_gen_f<-as.character(filled_df$class_gen_f)
          
          mb_sum <- filled_df %>%
            filter(grepl("^MB", class_gen_f)) %>%
            group_by(location,value) %>%
            summarize(MB = sum(indicator), .groups = 'drop')
          pb_sum <- filled_df %>%
            filter(grepl("^PB", class_gen_f)) %>%
            group_by(location,value) %>%
            summarize(PB = sum(indicator), .groups = 'drop')
          total_sum <- filled_df %>%
            group_by(location,value) %>%
            summarize(Total = sum(indicator), .groups = 'drop')
          
          summary <- mb_sum %>%
            full_join(pb_sum, by = c("location","value")) %>%
            full_join(total_sum, by = c("location","value"))
          
          summaryl<-pivot_longer(summary,
                                 -c(location,value),
                                 values_to = "indicator",
                                 names_to = "class_gen_f")
          
          filled_df<-bind_rows(filled_df,summaryl)
          
          unique_loc<-unique(filled_df$location)
          unique_loc_re(unique_loc)
          
          for (loc in unique_loc) {
            
            loc_df <- filled_df %>% filter(location == loc)
            
            loc_df <- loc_df %>%
              left_join(intb, by = c("value" = "rcode")) %>%
              mutate(value = ifelse(is.na(value), value, names))%>%
              select(-names)
            
            if(input$replevel=="3"){
              loc_df<-filter(loc_df,location == loc)
              loc_df <- loc_df %>%
                left_join(ouls, by = c("location" = "pcode")) %>%
                mutate(location = ifelse(is.na(value), location, Municipality))%>%
                select(-Province,-District,-Municipality)}
            
            wide<-loc_df|>
              pivot_wider(names_from= class_gen_f, values_from=indicator)
            
            gtb<-gt(wide)
            
            if(input$replevel=="3"){
              gtb <- gtb |>
                tab_spanner(
                  label = " MB ",
                  columns = c(MB,`MB/F`, `MB/M`),
                ) |>
                tab_spanner(
                  label = " PB ",
                  columns = c(PB,`PB/F`, `PB/M`)
                ) |> 
                cols_label(
                  location = md("**Municipality**"),
                  `MB/F` = "Female",
                  `MB/M` = "Male",
                  MB = md("**Total**"),
                  `PB/F` = "Female",
                  `PB/M` = "Male",
                  PB = md("**Total**"),
                  Total = md("**Total**"),
                  value=md("**Indicator**")
                ) 
            }else if(input$replevel=="2"){
              gtb <- gtb |>
                tab_spanner(
                  label = " MB ",
                  columns = c(MB,`MB/F`, `MB/M`),
                ) |>
                tab_spanner(
                  label = " PB ",
                  columns = c(PB,`PB/F`, `PB/M`)
                ) |> 
                cols_label(
                  location = md("**District**"),
                  `MB/F` = "Female",
                  `MB/M` = "Male",
                  MB = md("**Total**"),
                  `PB/F` = "Female",
                  `PB/M` = "Male",
                  PB = md("**Total**"),
                  Total = md("**Total**"),
                  value=md("**Indicator**")
                ) 
            }else{
              gtb <- gtb |>
                tab_spanner(
                  label = " MB ",
                  columns = c(MB,`MB/F`, `MB/M`),
                ) |>
                tab_spanner(
                  label = " PB ",
                  columns = c(PB,`PB/F`, `PB/M`)
                ) |> 
                cols_label(
                  location = md("**Province**"),
                  `MB/F` = "Female",
                  `MB/M` = "Male",
                  MB = md("**Total**"),
                  `PB/F` = "Female",
                  `PB/M` = "Male",
                  PB = md("**Total**"),
                  Total = md("**Total**"),
                  value=md("**Indicator**")
                ) 
            }
            
            gtb<-gtb |> 
              cols_align(
                align = c("center"),
                columns = c(MB,`MB/F`, `MB/M`,
                            PB,`PB/F`, `PB/M`,
                            Total)
              ) |> 
              cols_align(
                align = c("left"),
                columns = c(location,value)
              )|> 
              tab_style(
                style = list(
                  cell_borders(
                    sides="bottom",weight = px(2),color = "#999999")),
                locations = 
                  cells_body(
                    columns = c(value,MB,`MB/F`,`MB/M`,`PB/F`,`PB/M`,PB,Total),
                    rows = value %in% c( "13.7 Total patients (13.1 to 13.6)",
                                         "13.8 Total patients received services this month",
                                         "13.13 Total deduction (13.9 to 13.13)",
                                         "13.14 Patient at the end (13.7 - 13.13)",
                                         "13.16 Total children (new and old)",
                                         "13.18 Smear positive among tested",
                                         "13.18 Smear positive among tested",
                                         "13.19 Lepra reaction:Neuritis",
                                         "13.22 Number of index cases whose contact examination done",
                                         "13.25 New cases detected from contact examination",
                                         "Among new cases:Not Done")
                  )
              )
            
            gtb<-gtb|> 
              tab_header(
                title = paste("Report Month:",p),
                subtitle = paste("Generated/Updated on:",now$today))
            
            ft_list<-ft_list |> 
              grp_add(gtb)
          }
        }
        
        ft_list_re(ft_list)
        
        ouscode<-data.frame(location=c(unique(ouls$Province),unique(ouls$District),unique(ouls$Municipality)),
                            code=c(unique(ouls$Province),unique(ouls$District),ouls$pcode))
        ouscode<-ouscode |> 
          filter(code %in% unique_loc_re())
        
        updateSelectizeInput(session, "munichoice", choices = setNames(ouscode$code, ouscode$location))
        
        ft_list |> 
          gtsave(get_file_name("group"))
      }
    }
    
    repgenerate(TRUE)
  })
  
  observeEvent(input$munichoice, {
    if(length(input$munichoice)!=0){
      
      seq<-rep(unique_loc_re(),length(period_re()))
      index<-which(seq %in% input$munichoice)
      
      gt<-gt_group()
      for(i in index){
        gt<-gt |> 
          grp_add(grp_pull(ft_list_re(),i))
      }
      gt |> 
        gtsave(get_file_name("groupf"))
    }
  })
  
  repclear<-reactiveVal(FALSE)
  observeEvent(input$clearreport, {
    repclear(FALSE)
    if (!repclear()) {
      ft_list_re(NULL)
      repclear(TRUE)}
  })
  
  output$reload<-downloadHandler(
    
    filename = function() {
      paste("MonthlyReport_", year(Sys.Date()),".zip", sep = "")
    },
    content = function(fname) {
      
      shinyjs::disable("reload")
      showNotification("Gathering files...", type = "message", duration = NULL, id = "download_note")
      
      unique_muni<-unique_loc_re()
      period<-period_re()
      ft_list<-ft_list_re()
      
      seq<-rep(unique_muni,length(period))
      
      choicem <- c("Baishakh"="01",
                   "Jestha"="02",
                   "Ashadh"="03",
                   "Shrawan"="04",
                   "Bhadra"="05",
                   "Ashwin"="06",
                   "Kartik"="07",
                   "Mangsir"="08",
                   "Poush"="09",
                   "Magh"="10",
                   "Falgun"="11",
                   "Chaitra"="12")
      
      nemonth<-names(choicem)
      
      wbls<-list()
      for(muni in unique_muni){
        wb <- createWorkbook()
        for (mon in nemonth){
          addWorksheet(wb, mon)
        }
        
        for(i in 1:length(period)){
          sheet<-nemonth[month(ym(period[i]))]
          
          index<-which(seq %in% muni)[i]
          gt_df <- as.data.frame(grp_pull(ft_list,index))
          
          writeData(wb, sheet, gt_df, startRow = 5, startCol = 2, colNames = TRUE)
          mergeCells(wb, sheet, cols = 4:6, rows = 4)
          mergeCells(wb, sheet, cols = 7:9, rows = 4)
          mergeCells(wb, sheet, cols = 2, rows = 4:5)
          mergeCells(wb, sheet, cols = 3, rows = 4:5)
          mergeCells(wb, sheet, cols = 10, rows = 4:5)
          
          mergeCells(wb, sheet, cols = 2:4, rows = 2)
          txt<-paste("Generated/Updated on:",now$today)
          writeData(wb, sheet, txt, startRow = 2, startCol = 2)
          
          # Merging cells for Female, Male, and Total under MB and PB
          mergeCells(wb, sheet, cols = 4, rows = 5)
          mergeCells(wb, sheet, cols = 5, rows = 5)
          mergeCells(wb, sheet, cols = 6, rows = 5)
          mergeCells(wb, sheet, cols = 7, rows = 5)
          mergeCells(wb, sheet, cols = 8, rows = 5)
          mergeCells(wb, sheet, cols = 9, rows = 5)
          
          # Adding the spanner labels
          writeData(wb, sheet, "MB", startCol = 4, startRow = 4)
          writeData(wb, sheet, "PB", startCol = 7, startRow = 4)
          writeData(wb, sheet, "Female", startCol = 5, startRow = 5)
          writeData(wb, sheet, "Male", startCol = 6, startRow = 5)
          writeData(wb, sheet, "Total", startCol = 4, startRow = 5)
          writeData(wb, sheet, "Female", startCol = 8, startRow = 5)
          writeData(wb, sheet, "Male", startCol = 9, startRow = 5)
          writeData(wb, sheet, "Total", startCol = 7, startRow = 5)
          
          writeData(wb, sheet, "Municipality", startCol = 2, startRow = 4)
          writeData(wb, sheet, "Indicator", startCol = 3, startRow = 4)
          writeData(wb, sheet, "Total", startCol = 10, startRow = 4)
          
          cstyle<-createStyle(
            border = "bottom",
            borderColour = "#bcbcbc",
            borderStyle = "thick"
          )
          tstyle<-createStyle(
            border = "TopBottomLeftRight ",
            borderStyle = "thin",
            borderColour = "#000000"
          )
          
          addStyle(wb, sheet, cstyle, rows = 4:5, cols = 2:10, gridExpand = TRUE)
          addStyle(wb, sheet, tstyle, rows = 2, cols = 2:4, gridExpand = TRUE)
        }
        
        wbls[[muni]]<-wb
      }
      
      dir<-dir.create("xls_files", recursive = T)
      
      fs <- c()
      
      lapply(names(wbls), function(name) {
        path <- paste0("xls_files/",name, ".xlsx")
        fs <<- c(fs, path)
        saveWorkbook(wbls[[name]], file=path, overwrite = TRUE)
      })
      
      zip(zipfile=fname, files=fs)
      unlink("xls_files", recursive = TRUE)
      
      shinyjs::enable("reload")
      removeNotification("download_note")
    },
    contentType = "application/zip"
  )
  
  ###################
  ####CLEAN UP HTML
  ##################
  cleanup_files <- reactive({
    directory <- "."
    prefixes <- c("group_", "groupf_")
    days_old <- 1
    
    # List files in the directory
    files <- list.files(directory, pattern = "^(group_|groupf_).*\\.html$", full.names = TRUE)
    
    # Current time
    current_time <- Sys.time()
    
    # Loop through each file
    for (file in files) {
      # Get file information
      file_info <- file.info(file)
      
      # Calculate age in days
      age_in_days <- as.numeric(difftime(current_time, file_info$mtime, units = "days"))
      
      # Check if file is older than specified days_old
      if (age_in_days >= days_old) {
        # Delete the file
        file.remove(file)
        cat("Deleted file:", file, "\n")
      }
    }
    
    # Return a reactive value indicating cleanup status (optional)
    TRUE  # You can return whatever makes sense in your context
  })
  
  observe({
    cleanup_files()
  })
  
  ###############################
  ### Cluster
  ###############################
  
  dbscan_l<-reactiveVal(NULL) ###OUTPUT_LEAFLET
  summary_dt <-reactiveVal(NULL)###SUMMARY DATATABEL
  buffer_re<-reactiveVal(NULL)###CONTACT TRACING ZONE
  db_re<-reactiveVal(NULL) ###CLUSTER/NONCLUSTER OUTPUT
  
  
  cluclear_re<-reactiveVal(FALSE)
  observeEvent(input$cluclear, {
    cluclear_re(FALSE)
    if (!cluclear_re()) {
      
      dbscan_l(NULL)
      
      cluclear_re(TRUE)}
  })
  
  
  output$dbscanmap <- renderLeaflet({
    if (is.null(dbscan_l())){
      leaflet() %>%
        setView(lng = 84.1240, lat = 28.3949, zoom = 7) %>% 
        addTiles(group = "OpenStreetMap") %>% 
        addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
        addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"))
    }else{
      dbscan_l()
    }
  })
  
  
  observeEvent(input$dbscan, {
    shinyjs::disable("dbscan")
    showNotification("Conducting Cluster Detection...", type = "message", duration = NULL, id = "dbscan_note")
    
    if (is.null(input$ous) || length(input$ous) == 0) {
      shinyalert("Error", "Please select at least one location.", type = "error")
    } else {
      #print(head(kbdata()))
      kbdt <- kbdata() %>% 
        mutate(registration_date := as.Date(registration_date),
               outcome_date := as.Date(outcome_date),
               age := as.integer(age),
               reg_year = year(registration_date),
               reg_mth = format_ISO8601(ymd(registration_date), precision = "ym"),
               out_mth = format_ISO8601(ymd(outcome_date), precision = "ym"))%>% 
        mutate(lat=as.numeric(stringr::str_split_i(gps, " " , 1)),
               lng=as.numeric(stringr::str_split_i(gps, " " , 2)))%>% 
        mutate(exp=ifelse(leprosyclass=="1",registration_date+months(12),registration_date+months(6))) %>% 
        mutate(exp :=as.Date(exp)) %>% 
        mutate(fu=ifelse(exp<=Sys.Date()&treatment_outcome=="4",1,0)) %>%
        mutate(leprosyclass:=ifelse(leprosyclass=="1","MB","PB"))%>% 
        filter(is.na(training_practice)) %>% 
        mutate(cat=case_when(contact_pep==0 & treatment_outcome=="4" & fu==1 ~1,
                             contact_pep==0 & treatment_outcome!="4" ~ 2,
                             contact_pep==1 & treatment_outcome=="4" & fu==1 ~3,
                             contact_pep==0 & treatment_outcome=="4" & fu==0 ~4,
                             contact_pep==1 & treatment_outcome=="4" & fu==0 ~5,),
               Action=case_when(cat==1~"Contact Tracing + Outcome Follow-up",
                                cat==2~"Contact Tracing",
                                cat==3~"Outcome Follow-up",
                                cat==4~"Contact Tracing + Treatment Follow-up",
                                cat==5~"Treatment Follow-up",
                                TRUE ~ "Case Complete"),
               cat:=case_when(cat==1~"Contact Not done + Outcome Overdue",
                              cat==2~"Contact Not done + Outcome Complete",
                              cat==3~"Contact Done + Outcome Overdue",
                              cat==4~"Contact Not done + Under Treatment",
                              cat==5~"Contact Done + Under Treatment",
                              TRUE ~ "Case Complete")) %>% 
        mutate(outcome_la = case_when(treatment_outcome=="1"~"RFT",
                                      treatment_outcome=="2"~"T-OUT",
                                      treatment_outcome=="3"~"Defaulter",
                                      treatment_outcome=="4"~"Under Treatment",
                                      treatment_outcome=="5"~"Loss of Follow-up",
                                      treatment_outcome=="6"~"Other",
                                      treatment_outcome=="RFT"~"RFT")) %>%
        mutate(popup = paste("Type:", leprosyclass, "<br>Register Date:", reg_nedate,
                             "<br>Recored Outcome Date:", out_nedate,
                             "<br>Outcome:", outcome_la))
      se_ous<-as.character(input$ous)
      
      start<-ym(paste0(input$start_year,"/",input$start_month))
      end<-ym(paste0(input$end_year,"/",input$end_month))
      
      date_range <- format(seq.Date(from = start, to = end, by = "month"), "%Y/%m")
      
      #nowym<-paste0(now$date_y,"/",now$date_m)
      #print(head(kbdt))
      
      #if(all(date_range==nowym)){
      #  kbdt<-kbdt %>% 
      #    filter(muni_code %in% se_ous)
      #}else{
      #  kbdt<-kbdt %>% 
      #    filter(muni_code %in% se_ous) %>% 
      #    filter(reg_neym %in% date_range)
      #}
      
      kbdt<-kbdt %>%
        filter(muni_code %in% se_ous) %>% 
        filter(reg_neym %in% date_range)
      
      if (nrow(kbdt) == 0) {
        shinyalert("ZERO cases!", "There is no case currently detected/recored in the selected locations and period.", type = "error")
      } else {
        
        dtsf<-st_as_sf(kbdt,coords = c("lng","lat"),crs=4326)
        dtsf_t<-st_transform(dtsf,crs = 24345)
        
        radius<-input$clusterdist
        pts<-input$clusternum
        
        #st_coordinates(dtsf)
        set.seed(123)
        db <- fpc::dbscan(st_coordinates(dtsf_t), eps = radius, MinPts = pts)
        
        
        dtsf$CLUSTER_ID<-db$cluster
        
        dtsf<-dtsf %>% 
          group_by(CLUSTER_ID) %>% 
          mutate(CLUSTER_SIZE=ifelse(CLUSTER_ID==0,NA,n())) %>% 
          ungroup()
        dtsf<-dtsf %>% 
          mutate(CLUSTER_ID:=ifelse(CLUSTER_ID==0,NA,CLUSTER_ID))
        
        db_re(dtsf)
        
        db0<-filter(dtsf,is.na(CLUSTER_ID))
        db1<-filter(dtsf,!is.na(CLUSTER_ID))
        
        rand_colors<-c("red","darkred","lightred","orange", "beige",
                       "green", "darkgreen", "lightgreen", "blue", 
                       "darkblue", "lightblue", "purple", "darkpurple",
                       "pink", "cadetblue","gray", "lightgray", "black","white")
        
        
        colors <- as.data.frame(matrix(nrow = length(unique(db1$CLUSTER_ID)), ncol = 2))
        names(colors) <- c("CLUSTER_ID", "color")
        colors$CLUSTER_ID <- sort(unique(db1$CLUSTER_ID))
        colors$color <- c(rand_colors[sample(length(rand_colors),length(unique(db1$CLUSTER_ID)),replace = T)])
        db1 <- left_join(db1, colors,by="CLUSTER_ID")
        
        icons1 <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = db1$color
        )
        
        #db1_c<-db1 %>% 
        #  group_by(CLUSTER_ID) %>% 
        #  summarise(geom = st_union(geometry))
        
        min_lat <- min(kbdt$lat)
        max_lat <- max(kbdt$lat)
        min_lon <- min(kbdt$lng)
        max_lon <- max(kbdt$lng)
        
        bbox <- c(min_lon, min_lat, max_lon, max_lat)
        
        q <- opq(bbox = bbox , timeout = 300) %>%
          add_osm_feature(key = 'boundary', value = 'administrative') %>%
          add_osm_feature(key = 'admin_level', value = '7') %>%
          osmdata_sf()
        
        mu<-q$osm_multipolygons
        
        clu_buf<-input$clusterbuffer
        non_clu<-input$nonclusterdist
        
        buffer1<-st_buffer(db1,clu_buf) %>% select(`_id`)
        #print(head(buffer1))
        
        buffer0<-st_buffer(db0,non_clu) %>% select(`_id`)
        #print(head(buffer0))
        
        buffer_combined <- rbind(buffer1, buffer0)
        
        buffer_union <- st_union(buffer_combined)
        
        buffer_union <- st_collection_extract(buffer_union, "POLYGON")
        
        buffer <- st_sf(geometry = buffer_union) %>% st_make_valid()  
        
        buc<-st_intersection(buffer,mu)
        
        ind<-st_intersects(buffer,mu)
        
        bucn<-buc
        #print(names(bucn))
        #print(nrow(bucn))
        #print(length(unlist(ind)))
        rownames(bucn)<-mu$name[unlist(ind)]
        
        buffer_re(bucn)
        
        
        if(nrow(db1)>0){dt1<-st_drop_geometry(db1) %>% 
          bind_cols(st_coordinates(db1))}
        
        dt0<-st_drop_geometry(db0) %>% 
          bind_cols(st_coordinates(db0))
        
        
        ###SUMMARY DATATABLE
        su<-data.frame(clusters=length(unique(db1$CLUSTER_ID)),
                       cases=nrow(db1),
                       noises=nrow(db0))
        names(su)<-c("Clusters","Clustered Cases","Non-Clustered Cases")
        
        summary_dt(su)
        
        
        meanx <- mean(kbdt$lng, na.rm = TRUE)
        meany <- mean(kbdt$lat, na.rm = TRUE)
        
        library(htmltools)
        legendHtml <- HTML(paste(
          "<link rel='stylesheet' href='https://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css'>",
          "<div style='line-height: 1.5;'>",
          "<i class='ion ion-ios-close' style='color:black; font-size: 16px;'></i> Clustered Cases<br>",
          "<svg width='16' height='16'><circle cx='8' cy='8' r='6' fill='red' fill-opacity='0.5' /></svg> Non-clustered Cases<br>",
          "</div>"
        ))
        
        if(nrow(db1)>0){
          l<-leaflet() %>%
            setView(lng = meanx, lat = meany, zoom = 12) %>% 
            addTiles(group = "OpenStreetMap") %>% 
            addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
            addAwesomeMarkers(
              data = dt1, lng = ~X, lat = ~Y,
              label = ~uid,
              popup = ~popup,
              icon = icons1,
              group = ~CLUSTER_ID
            ) %>% 
            addCircleMarkers(
              data = dt0, lng = ~X, lat = ~Y,
              label = ~uid, popup= ~popup, color="red",stroke = F,fillOpacity = 0.5, radius = 6
            ) %>% 
            addPolygons(
              data = bucn, fillOpacity = 0, weight = 2, color = "deepskyblue", opacity = 1
            ) %>% 
            addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
            addControl(html = legendHtml, position = "bottomleft")
        }else{
          l<-leaflet() %>%
            setView(lng = meanx, lat = meany, zoom = 12) %>% 
            addTiles(group = "OpenStreetMap") %>% 
            addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
            addCircleMarkers(
              data = dt0, lng = ~X, lat = ~Y,
              label = ~uid, popup= ~popup, color="red",stroke = F,fillOpacity = 0.5, radius = 6
            ) %>% 
            addPolygons(
              data = bucn, fillOpacity = 0, weight = 2, color = "deepskyblue", opacity = 1
            ) %>% 
            addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
            addControl(html = legendHtml, position = "bottomleft")
        }
        
        dbscan_l(l)
        
        output$summary<-renderTable({
          summary_dt()
        })
        
      }}
    
    shinyjs::enable("dbscan")
    removeNotification("dbscan_note")
  })
  
  buffer<-reactive({
    bucn<-buffer_re() %>% st_geometry()
    n<-rownames(bucn)
    bt<-bucn %>% 
      st_boundary() %>% 
      st_cast("MULTILINESTRING") %>% 
      st_sf() %>% 
      mutate(name=n,
             desc="",
             cmt="",
             src=paste("Contact Zone Detected on:",Sys.Date()))
    bt})
  
  
  output$zonegpx<-downloadHandler(
    filename = function() {
      paste("contact_zone_", Sys.Date(),".gpx", sep = "")
    },
    content = function(file) {
      st_write(buffer(), driver = "GPX", file)
    })
  
  cases_re<-reactive({
    db_re() %>% 
      mutate(name=paste0(uid,"/",full_name_IC),
             desc=paste0(registration_date,"/",leprosyclass),
             cmt=paste0("Action: ",Action),
             src=paste("Clustering Detection on:",Sys.Date())) %>% 
      select(name,desc,cmt,src)
  })
  
  output$casegpx<-downloadHandler(
    filename = function() {
      paste("cases_", Sys.Date(),".gpx", sep = "")
    },
    content = function(file) {
      st_write(cases_re(), driver = "GPX", file)
    })
  
  ######################################
  ### DHIS2
  ######################################
  
  login_status <- reactiveVal(FALSE)
  filled_re <- reactiveVal(FALSE)
  
  observeEvent(input$d2_login, {
    
    loginDHIS2 <- function(baseurl, username, password) {
      url <- paste0(baseurl, "api/me")
      r <- httr::GET(url,httr::authenticate(username, password))
      # Return TRUE if status code is 200, otherwise FALSE
      return(r$status_code == 200L)
    }
    
    username <- input$d2_user
    password <- input$d2_pw
    # Call the login function
    baseurl <- "https://hmis.gov.np/hmis/"   #"https://ntd-watch.itg.be/leprosy/"  
    success <- loginDHIS2(baseurl, username, password)
    # Show a success or failure message
    if (success) {
      shinyalert(
        title = "Success",
        text = "Login successful!",
        type = "success"
      )
      
      login_status(TRUE)
      #updateBox("d2_auth", action = "toggle")
      updateAccordion(id = "d2_conf", selected = 2)
      
    } else {
      shinyalert(
        title = "Failure",
        text = "Login failed. Please check your credentials.",
        type = "error"
      )
      login_status(FALSE)
    }
  })
  
  #######################
  #### Data PREVIEW #####
  wide_re<-reactiveVal(NULL)
  
  observeEvent(input$d2_table, {
    req(login_status())
    
    if (login_status()) {
      
      kbdt <- kbdata() %>% 
        mutate(reg_date := as.Date(reg_nedate),
               outcome_date := as.Date(out_nedate),
               age := as.integer(age),
               reg_year = year(reg_date),
               reg_mth = format(reg_date,"%Y/%m"),
               out_mth = format(outcome_date,"%Y/%m")) %>% 
        mutate(lat=as.numeric(stringr::str_split_i(gps, " " , 1)),
               lng=as.numeric(stringr::str_split_i(gps, " " , 2)))%>% 
        mutate(exp=ifelse(leprosyclass=="1",reg_date+months(12),reg_date+months(6))) %>% 
        mutate(exp :=as.Date(exp)) %>% 
        mutate(fu=ifelse(exp<=Sys.Date()&treatment_outcome=="4",1,0)) %>%
        mutate(leprosyclass:=ifelse(leprosyclass=="1","MB","PB")) %>% 
        mutate(contact_num:=as.integer(contact_num))%>% 
        filter(is.na(training_practice))
      
      period<-paste0(input$d2_year,"/",input$d2_month)
      
      se_ous<-as.character(input$ous)
      
      intb<-read.csv("indtable.csv")
      ouls<-read.csv("code_list.csv")
      ouls$pcode<-as.character(ouls$pcode)
      ouls<-ouls %>% select(-National)
      
      p<-period
      
      skeleton <- expand.grid(
        muni_code = unique(se_ous)[unique(se_ous)%in%ouls$pcode],
        class_gen_f = c("MB/M", "MB/F", "PB/M", "PB/F"),
        period = period)
      
      temp_df <- kbdt %>%
        mutate(
          gen_f = case_when(gender == "1" ~ "Male", gender == "2" ~ "Female"),
          class_gen = case_when(
            gender == "1" & leprosyclass == "MB" ~ "MB/M",
            gender == "2" & leprosyclass == "MB" ~ "MB/F",
            gender == "1" & leprosyclass == "PB" ~ "PB/M",
            gender == "2" & leprosyclass == "PB" ~ "PB/F"
          )
        ) %>% 
        mutate(class_gen_f = fct_relevel(class_gen, c("MB/M", "MB/F", "PB/M", "PB/F"))) %>% 
        group_by(muni_code,class_gen_f) %>% 
        summarise(
          ###
          i_13_1 = sum(reg_mth < p & treatment_outcome == "4", na.rm = TRUE),
          i_13_2 = sum(reg_mth == p & registered_as == "1", na.rm = TRUE), # NEW
          i_13_3 = sum(reg_mth == p & registered_as == "4", na.rm = TRUE), # Relapsed
          i_13_4 = sum(reg_mth == p & registered_as == "3", na.rm = TRUE), # Restart
          i_13_5 = sum(reg_mth == p & registered_as == "2", na.rm = TRUE), # in
          i_13_6 = sum(reg_mth == p & registered_as == "5", na.rm = TRUE), # other
          i_13_7 = i_13_1 + i_13_2 + i_13_3 + i_13_4 + i_13_5 + i_13_6,
          i_13_8 = i_13_7,
          ###
          i_13_9 = sum(out_mth == p & treatment_outcome == "1", na.rm = TRUE), # RFT
          i_13_10 = sum(out_mth == p & treatment_outcome == "2", na.rm = TRUE), # T-OUT
          i_13_11 = sum(out_mth == p & treatment_outcome == "5", na.rm = TRUE), # LossFU
          i_13_12 = sum(out_mth == p & treatment_outcome %in% c("3", "6"), na.rm = TRUE), # Other
          i_13_13 = i_13_9 + i_13_10 + i_13_11 + i_13_12, # TOTAL DEDUCTED
          i_13_14 = i_13_7 - i_13_13,
          ###
          i_13_15 = sum(reg_mth == p & age < 15 & registered_as == "1", na.rm = TRUE),
          i_13_16 = sum(reg_mth == p & age < 15 & treatment_outcome == "4", na.rm = TRUE),
          i_13_17 = sum(reg_mth == p & (skin_smear_test %in% c("1", "2")) & registered_as == "1", na.rm = TRUE),
          i_13_18 = sum(reg_mth == p & skin_smear_test == "1" & registered_as == "1", na.rm = TRUE),
          ###
          i_13_19_1 = sum(reg_mth == p & lepra_reaction == "1", na.rm = TRUE),
          i_13_19_2 = sum(reg_mth == p & lepra_reaction == "2", na.rm = TRUE),
          i_13_19_3 = sum(reg_mth == p & lepra_reaction == "3", na.rm = TRUE),
          ###
          i_13_21 = sum(reg_mth < p & contact_pep == "0", na.rm = TRUE),
          i_13_22 = sum(reg_mth == p & contact_pep == "1", na.rm = TRUE),
          i_13_23 = i_13_21 * 25,
          i_13_24 = sum(contact_num[reg_mth == p], na.rm = TRUE),
          i_13_25 = sum(reg_mth == p & (registered_as == "1" & case_detection_method %in% c("3", "5")), na.rm = TRUE),
          ###
          d_n_0 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "0"), na.rm = TRUE),
          d_n_1 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "1"), na.rm = TRUE),
          d_n_2 = sum(reg_mth == p & (registered_as == "1" & i_who_disability == "2"), na.rm = TRUE),
          d_n_x = sum(reg_mth == p & (registered_as == "1" & is.na(i_who_disability)), na.rm = TRUE),
          d_nc_0 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "0"), na.rm = TRUE),
          d_nc_1 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "1"), na.rm = TRUE),
          d_nc_2 = sum(reg_mth == p & (registered_as == "1" & age < 15 & i_who_disability == "2"), na.rm = TRUE),
          d_nc_x = sum(reg_mth == p & (registered_as == "1" & age < 15 & is.na(i_who_disability)), na.rm = TRUE),
          .groups = 'drop')
      
      merged_df <- skeleton %>%
        filter(period == p) %>%
        left_join(temp_df, by = c("muni_code", "class_gen_f")) %>%
        replace_na(list(
          i_13_1 = 0, i_13_2 = 0, i_13_3 = 0, i_13_4 = 0, i_13_5 = 0, 
          i_13_6 = 0, i_13_7 = 0, i_13_8 = 0, i_13_9 = 0,  i_13_10 = 0, i_13_11 = 0, 
          i_13_12 = 0, i_13_13 = 0, i_13_14 = 0, i_13_15 = 0, i_13_16 = 0, i_13_17 = 0, 
          i_13_18 = 0, i_13_19_1 = 0, i_13_19_2 = 0, i_13_19_3 = 0, i_13_21 = 0,
          i_13_22 = 0, i_13_23 = 0, i_13_24 = 0, i_13_25 = 0, d_n_0 = 0, d_n_1 = 0, 
          d_n_2 = 0, d_n_x = 0, d_nc_0 = 0, d_nc_1 = 0, d_nc_2 = 0, d_nc_x = 0
        )) |> 
        select(-period) %>% 
        rename(location=muni_code)
      
      dat<-pivot_longer(merged_df,
                        cols = -c(location,class_gen_f),
                        values_to = "value",
                        names_to = "code")
      dat<-dat %>% 
        group_by(location,class_gen_f) %>% 
        ungroup()
      
      load("dval_ind.RData") ###load DHIS2 FORMATTING REFERENCE
      
      dval1<-filter(dval_index,class_gen_f!="default")
      dval2<-filter(dval_index,class_gen_f=="default")
      
      dat1<-filter(dat,startsWith(code,"i_"))
      dat2<-filter(dat,startsWith(code,"d_"))
      
      filled1<-left_join(dval1,dat1,by=c("code","class_gen_f"))
      
      dat2<-dat2 |> 
        group_by(code,location) |> 
        summarise(value=sum(value)) |> 
        ungroup() |> 
        mutate(class_gen_f="default")
      
      filled2<-left_join(dval2,dat2,by=c("code","class_gen_f"))
      
      filled<-bind_rows(filled1,filled2) ####FINAL DATASET
      filled_re(filled)
      
      muni_df <- filled %>% 
        select(location,names,class_gen_f,value.y)
      
      muni_df <- muni_df %>%
        left_join(ouls, by = c("location" = "pcode")) %>%
        mutate(location = ifelse(is.na(value.y), location, Municipality))%>%
        select(-Municipality)
      
      wide<-muni_df|>
        pivot_wider(names_from= class_gen_f, values_from=value.y)
      
      order<-c("Province","District","location","names","MB/F","MB/M","PB/F","PB/M","default")
      wide <- wide[, order] %>% 
        arrange(location)
      
      wide_re(wide)
      
      output$municipality_filter_ui <- renderUI({
        req(wide_re())
        div(
          div(tags$label("Filter Municipality", `for` = "municipality-filter")),
          tags$select(
            id = "municipality-filter",
            onchange = "Reactable.setFilter('d2preview-table', 'location', this.value)",
            tags$option("All", value = ""),
            lapply(unique(wide_re()$location), function(location) tags$option(location, value = location))
          )
        )
      })
      
      
      output$d2preview <- renderReactable({
        reactable(wide_re(),
                  columns = list(
                    location = colDef(name = "Municipality"),
                    names = colDef(name = "Indicators"),
                    `MB/F` = colDef(cell = function(value) {
                      if (is.na(value)) {
                        return("NA")
                      }
                      return(value)
                    }),
                    `MB/M` = colDef(cell = function(value) {
                      if (is.na(value)) {
                        return("NA")
                      }
                      return(value)
                    }),
                    `MB/M` = colDef(cell = function(value) {
                      if (is.na(value)) {
                        return("NA")
                      }
                      return(value)
                    }),            
                    `PB/M` = colDef(cell = function(value) {
                      if (is.na(value)) {
                        return("NA")
                      }
                      return(value)
                    }),            
                    `PB/F` = colDef(cell = function(value) {
                      if (is.na(value)) {
                        return("NA")
                      }
                      return(value)
                    }),
                    default = colDef(name = "Total",
                                     cell = function(value) {
                                       if (is.na(value)) {
                                         return("NA")
                                       }
                                       return(value)
                                     })
                  ),
                  filterable = TRUE, minRows = 28,
                  defaultPageSize = 28,
                  searchable = TRUE,
                  paginationType = "simple",
                  paginateSubRows = TRUE,
                  resizable = TRUE,
                  elementId = "d2preview-table" 
        )
      })
      
    } else {
      shinyalert(
        title = "Error",
        text = "You must be logged in to view the data tables.",
        type = "info"
      )
    }
    
  })
  
  
  ######################
  #### Data IMPORT ####
  observeEvent(input$d2_import, {
    req(login_status(),filled_re())
    
    if (login_status()) {
      
      baseurl<-"https://hmis.gov.np/hmis/"
      username <- input$d2_user
      password <- input$d2_pw
      
      loginDHIS2<-function(baseurl,username,password) {
        url<-paste0(baseurl,"api/me")
        r<-GET(url,authenticate(username,password))
        assertthat::assert_that(r$status_code == 200L) }
      
      loginDHIS2(baseurl,d2_username,d2_password)
      
      filled<-filled_re()
      
      dataSet<-"rcs5vhrMJrQ"
      qt<-paste0(input$d2_year,input$d2_month)
      output_list<-list()
      locs<-unique(filled$location)
      
      for(loc in locs){
        uid<-d2ou$id[d2ou$code==loc]
        
        url<-paste0(baseurl,
                    "api/dataValueSets.json?",
                    "dataSet=",dataSet,
                    "&period=",qt,
                    "&orgUnit=",uid)

        output_list[[loc]]<-filled %>% 
          filter(location==loc) %>% 
          select(dataElement,categoryOptionCombo,value.y) %>% 
          rename(value=value.y)
        
        httr::POST(url,body=jsonlite::toJSON(output_list[[loc]],auto_unbox = TRUE), httr::content_type_json())
        
      }
      
    } else {
      shinyalert(
        title = "Error",
        text = "You must be logged in to HMIS/DHIS2 to import data.",
        type = "info"
      )
    }
  })
  
  ###################
  ### Users Admin
  ###################
  is_admin <- function(user) {
    credentials$Role[credentials$username==user] %in% c("Admin", "admin","ADMIN")
  }
  
  
  output$conditionalButtonUI <- renderUI({
    req(user_re())
    if (is_admin(user_re())) {
      actionBttn("admin", "Users Admin", icon = icon("users"),
                 style = "fill", color = "success", size = "sm",
                 width = "100px", block = TRUE)
    }
  })
  
  observeEvent(input$admin, {
    showModal(modalDialog(
      title = "Edit User Credentials",
      DTOutput("editableTable"),
      footer = tagList(
        modalButton("Close"),
        actionButton("save", "Save Changes")
      ),
      size = "l"  # Large size modal to accommodate the table
    ))
  })
  
  table_data <- reactiveVal(credentials)
  
  # Render the editable table inside the modal
  output$editableTable <- renderDT({
    datatable(table_data(), editable = TRUE)
  })
  
  # Capture and apply edits to the table
  observeEvent(input$editableTable_cell_edit, {
    info <- input$editableTable_cell_edit
    current_data <- table_data()  # Get the current table data
    
    # Apply the edited value to the table
    current_data[info$row, info$col] <- info$value
    
    # Update the reactive table with the modified data
    table_data(current_data)
  })
  
  # Save changes to the credentials file when "Save Changes" button is clicked
  observeEvent(input$save, {
    # Update global credentials variable and save it
    credentials <<- table_data()  # Use <<- to update the global credentials
    saveRDS(credentials, "users.rds")  # Save updated credentials to file
    
    # Close the modal after saving
    removeModal()
  })
  
}

shinyApp(ui = ui, server = server)
