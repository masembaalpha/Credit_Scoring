# CREDIT SCRORING----

#LIBRARIES
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyauthr)  

#library(plotly)
library(tidyquant)
library(lubridate)

# 3.3 Explainer using LIME ----
library(parsnip)
library(h2o)
library(glue)
library(tidymodels)
library(tidyverse)
# library(rsample)
# library(workflows)


# File Sourcing---
source(file = "02_scripts/crud_operations_local.R")
source(file = "02_scripts/info_card.R")
source(file="02_scripts/credit_profile.R")
df <- read_rds("00_data/hmeq_clean_tbl.rds")

# h2o.init()
# h2o_model <- h2o.loadModel("01_model/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937/StackedEnsemble_AllModels_2_AutoML_1_20240109_90937")
#xg_boost <- readRDS("01_model/xg_boost_model.rds")
# UI ----
ui <- tagList(
    
    # CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cyborg")),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    shinythemes::themeSelector(),
    
    # JS ----
    shinyjs::useShinyjs(),
    
    # User Login ----
    # verbatimTextOutput(outputId = "creds"),
    shinyauthr::loginUI(
        id = "login", 
        title = tagList(h2(class = "text-center", "Credit Scoring"), 
                        p(class = "text-center", "Please Log In")),
        login_title = "Enter"
    ),
    
    # Dashboard ----
    uiOutput(outputId = "dashboard")
) 

# SERVER ----

server <- function(input, output, session){
    
    read_user_base()
    
    # 1.0 Credentials ----
    credentials <- callModule(
        module = shinyauthr::login,
        id       = "login",
        data     = user_base_tbl,
        user_col = user,
        pwd_col  = password,
        log_out  = reactive(logout_init())
    )
    
    logout_init <- callModule(
        module = shinyauthr::logout,
        id     = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    #  1.1 PROFILE ANALYSIS----
    
    # 1.2 SCORE ANALYSIS VISUALS----
    
     # 2.0 RENDER Dashboard ----
    
    output$dashboard <- renderUI({
        
        req(credentials()$user_auth)
        
        navbarPage(
            title = "CREDIT RISK ANALYSIS",
            inverse = FALSE,
            collapsible = TRUE,
            
            theme = shinytheme("cyborg"),
            
            header = div(
                class = "pull-right",
                style = "padding-right: 20px;",
                p("Welcome" )
            ),
            # 2.1 HOME PAGE----
            tabPanel(title = "Home",
                     
                     div (
                         class="container",
                         id ="readme",
                         credit_profile()
                     )
                
            ),
            tabPanel(title = "Credit Profile",
                     h3("Credit Portfolio", tags$small("Analysis")),
                     column(
                         width = 8,
                         fluidRow(
                             id="accounts_summary",
                             column(
                                 width = 2,
                                 div(
                                     info_card(title ="Accounts" ,main_icon = "address-book",
                                               value = df %>% count(BAD) %>%pull(n) %>%  sum())
                                 )
                             ),
                             column(
                                 width = 2,
                                 div(
                                     info_card(title ="Loanbook" ,main_icon = "university",
                                               value = ((df %>% pull(LOAN) %>%  sum())/1e6) %>% round(1) %>%
                                                   scales::dollar_format(suffix = " M")()
                                     )
                                 )
                             ),
                             column(
                                 width = 2,
                                 div(
                                     info_card(title ="Healthy" ,main_icon = "heartbeat", bg_color = "success",sub_text_color = "success",
                                               value = df %>% filter(BAD==0) %>% nrow()
                                     )
                                 )
                             ),
                             column(
                                 width = 2,
                                 div(
                                     info_card(title ="Risky" ,main_icon = "exclamation-triangle",bg_color = "danger",sub_text_color = "danger",
                                               value = df %>% filter(BAD==1) %>% nrow()
                                     )
                                 )
                             )
                         ),
                         fluidRow(
                             id= "payment_history",
                             p("Loan Summary"),
                             div(
                                renderDataTable(
                                     df %>% group_by(REASON) %>%
                                         summarise(Sum=sum(LOAN),
                                                   Count = n(),
                                                   Mean=mean(LOAN),
                                                   Median=median(LOAN),
                                                   SD  = sd(LOAN),
                                                   MIN =min(LOAN),
                                                   MAX =max(LOAN)) %>%
                                         rename(Type=REASON)
                                     
                                 )

                             )
                         )
                     ),
                     column(
                         width = 4
                         
                     )
                     
            ),
            # 2.2 SINGLE APPLICANT----

            tabPanel(
                title = "Single Applicant",
                
                ## 2.2.0 HEADER ----
                div(
                    class = "container",
                    id = "header",
                    h2(class = "page-header", "Individual Applicant", tags$small("analysis"))
                ),
                
                ## 2.2.1 APPLICATION UI -----
                div(
                    class = "container",
                    id = "application_ui",
                    
                    ## 2.2.1.0 DATA INPUT SINGLE ----
                    column(
                        width = 4, 
                        wellPanel(
                            
                            div(
                                shinyWidgets::pickerInput(
                                    inputId  = "loan_type", 
                                    label    = h4("Loan Type"), 
                                    choices  = unique(df$REASON), 
                                    selected = unique(df$REASON)), 
                                multiple = TRUE,
                                options  = list(
                                    `actions-box` = TRUE,
                                    size = 10,
                                    `selected-text-format` = "count > 3"
                                )
                            ),
                            div(
                                id="demographics",
                                numericInput(inputId = "yob",
                                             label   = "YOB",
                                             value   =YEAR(today()),
                                             min     = YEAR(today()),
                                             max     = YEAR(today())-70)
                            ),
                            div(
                                shinyWidgets::pickerInput(
                                    inputId  = "JOB", 
                                    label    = h4("Job Type"), 
                                    choices  = unique(df$JOB), 
                                    selected = unique(df$JOB)), 
                                multiple = TRUE,
                                options  = list(
                                    `actions-box` = TRUE,
                                    size = 10,
                                    `selected-text-format` = "count > 3"
                                )
                            ),
                            div(
                                id="income_settings",
                                numericInput(inputId = "income",
                                             label="Total Monthly Income",
                                             min = 1000,max = 10000000,value = 1000),
            
                                numericInput(inputId = "income",
                                             label="Monthly Household expenditure - Ex loans",
                                             min = 1000,max = 10000000,value = 1000),
                                numericInput(inputId = "debts",
                                             label="Total monthly loans premiums",
                                             min = 1000,max = 10000000,value = 1000),
                                sliderInput(inputId = "deliquent",
                                            label   = "Loans on default",
                                            value   = 0, min = 0, max = 10)
                            ),
                            
                            div(
                                id = "input_settings",
                                hr(),
                                sliderInput(inputId = "Loan Tenure",
                                            label   = "Loan Tenure(Months)",
                                            value   = 11,
                                            min     = 0,
                                            max     = 120),
                                actionButton(inputId = "apply", label = "Apply Settings", icon = icon("sync"))
                            ),# %>% hidden(),
                            
                            div(
                                id = "data_inputs",
                                selectInput(inputId = "data_variables",
                                            label = "Select variables",
                                            choices = c("Mortgage","debt_income R","Credit_line","Age"),
                                            selected =c("Mortgage","debt_income R"),
                                            multiple = TRUE
                                )
                            )
                        )
                    ),
                    
                    ## 2.2.1.1 PLOT PANEL ----
                    column(
                        width = 8, 
                        h3("Placeholder- Charts")
                    )
                ),
                
                ## 2.2.2 ANALYST COMMENTARY ----
                div(
                    class = "container",
                    id = "commentary",
                    column(
                        width = 12,
                        div(
                            class = "panel",
                            div(class = "panel-header", h4("Analysis Commentary")),
                            div(
                                class = "panel-body",
                                p("Placeholder- Comments on the outputs")
                            )
                        )
                    )
                )
            ),
            # 2.3 GROUP APPLICATION----
            tabPanel(title = "Multiple Applications",
                     p("Multiple Applications")
                     
            )
        )
    })
    
    
    h4("powered by",tags$small("Dalberg Research"))
}

# RUN APP ----
shinyApp(ui = ui, server = server)
