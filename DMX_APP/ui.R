###-------------------------
list.of.packages <- c("shiny","shinyWidgets","shinydashboard","shinydashboardPlus","shinyjs","taskscheduleR","sortable","DT","rhandsontable")
new.packages <- list.of.packages [!(list.of.packages %in% installed.packages()[,"Package"] )]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
rm(new.packages, list.of.packages)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(rhandsontable)
library(taskscheduleR)
library(sortable)
library(DT)
library(data.table)


header <- dashboardHeaderPlus(titleWidth =40)
sidebar <- dashboardSidebar(width = 30, sidebarMenu(id= "left_sidebar",menuItem("DMX",tabName = "Home",icon = icon("home")),
                                                    menuItem("HashingBoard",tabName = "HASHINGBOARD",icon = icon("hashtag")),
                                                    menuItem("FileType Defination",tabName = "FileType",icon = icon("file")),
                                                    menuItem("Driver UI",tabName='DriverUI',icon = icon("anchor")),
                                                   menuItem("Job Scheduler",tabName='Scheduler',icon = icon("algolia"))
                                                    ))

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #ffffff;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #ffffff;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-image: linear-gradient(-90deg,#0F4C81,#ffffff,#ffffff);
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #ffffff;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #6497b1;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #ffffff;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6497b1;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-image: linear-gradient(-90deg,#0F4C81,#ffffff,#ffffff);
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-image: linear-gradient(-90deg,#0F4C81,#ffffff,#ffffff);
                                }

                                '))),
  
  tags$head(tags$style(HTML('
  .skin-blue .left-side,  .skin-blue .wrapper {
    background-image: linear-gradient(-90deg,#0F4C81,#ffffff,#ffffff);
  }'))),
  ##sidebar toggle
  
  tags$head(tags$style(HTML('
  .main-header .sidebar-toggle {
  float: right;
  background-color: transparent;
  background-image: none;
  padding: 15px 15px;
  font-family: fontAwesome;
  }'))),
  
  ##button on flipping box
  
  tags$head(tags$style(HTML('.btn-primary {
    color: #fff;
      background-color: #0F4C81;
      border-color: #0F4C81;
  }
  .btn-primary.focus, .btn-primary:focus {
    color: #fff;
      background-color: #286090;
      border-color: #122b40;
      }
    .btn-primary:hover {
      color: #fff;
      background-color: #0F4C81;
      border-color: #253e29;
  }'))),
  
  ##buttons on fliping box and text colour in buttons
  
  tags$head(tags$style(HTML('.btn{
  border-radius: 12px;
  -webkit-box-shadow: none;
  box-shadow: 2px 6px 8px 4px rgba(0,0,0,0.5);
  border: 2px solid transparent;
  background-color: #fff;
  color: #0F4C81;
  }'))),
  
  
  #center box position and height
  
  tags$head(tags$style(HTML('/*@media all and (min-width:768px)*/
  .col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8,.col-sm-9
                            {
                            float: left;
                            left: 11%;
                            height: 350px;
                            }'))),
  
  tags$head(tags$style(HTML('/*@media all and (min-width:768px)*/
    .col-sm-9 {
      width: 75%;
    }'))),
  
  ##boxplus colour
  
  tags$head(tags$style(HTML('.box.box-solid.box-primary > .box-header
                            {
                            color: #F6D8AE;
      background-color: #0F4C81;
      border-color: #0F4C81;
                            }'))),
  
  # boxplus border and text color
  
  tags$head(tags$style(HTML('.box.box-solid.box-primary {
  border: 1px solid #0F4C81;
  color: #F6D8AE;
  }'))),
  
  
  ## close and minimize sign on flipping box
  
  tags$head(tags$style(HTML('.box.box-solid.box-primary > .box-header .btn, .box.box-solid.box-primary > .box-header a {
  background-color: #F6D8AE;
  border-color: #0F4C81;
  color:#0F4C81
  }'))),
  
  ##flip box header color  
  
  tags$head(tags$style(HTML(' body {
    font-size: 14px;
    line-height: 1.4285;
    color: #0F4C81;
     background-color: #0F4C81;
     
      }'))),
  
  tags$head(tags$style(HTML('.box-body {
  border-top-left-radius: 0px;
  border-top-right-radius: 0px;
  border-bottom-right-radius: 3px;
  border-bottom-left-radius: 10px;
  padding: 10px;
}'))),
  
  tags$head(tags$style(HTML('
.col-lg-1, .col-lg-10, .col-lg-11, .col-lg-12, .col-lg-2, .col-lg-3, .col-lg-4, .col-lg-5, .col-lg-6, .col-lg-7, .col-lg-8, .col-lg-9, .col-md-1, .col-md-10, .col-md-11, .col-md-12, .col-md-2, .col-md-3, .col-md-4, .col-md-5, .col-md-6, .col-md-7, .col-md-8, .col-md-9, .col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-xs-1, .col-xs-10, .col-xs-11, .col-xs-12, .col-xs-2, .col-xs-3, .col-xs-4, .col-xs-5, .col-xs-6, .col-xs-7, .col-xs-8, .col-xs-9 {
  position: left;
  min-height: 1px;
  padding-right: 15px;
  padding-left: 15px;
}'))),
  
  #box color inside and text color
  
  tags$head(tags$style(HTML('.box.box-solid.box-primary {
  border: 1px solid #0F4C81;
  color: #f6d8ae;
  background-color: #0F4C81;
  text-align: left;
}'))),
  
  
  ## header font style
  tags$head(tags$style(HTML('.h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
  font-family: "Impact, Charcoal, sans-serif";

}'))),
  
  #flipbox header title size
  tags$head(tags$style(HTML(' .h3, h3 { font-size: 32px;
}'))),
  
  ## header font size
  tags$head(tags$style(HTML(' .box-header .box-title, .box-header > .fa, .box-header > .glyphicon, .box-header > .ion {
  display: inline-block;
  font-size: 24px;
  margin: 0;
  line-height: 1;

}'))),
  
  tags$head(tags$style(HTML(' .box.box-solid.box-primary > .box-header {
  color: #f6d8ae;
    background-color: #0F4C81;
    border-color: #0F4C81;
      text-align: center;
}'))),
  
  
  
  
  
  tabItems(tabItem(tabName = "Home",uiOutput("DMX")),
           tabItem(tabName = "HASHINGBOARD",uiOutput("xx")),
           tabItem(tabName = "FileType",uiOutput("FT")),
           tabItem(tabName = "Scheduler",uiOutput("Scheduler")),
           tabItem(tabName = "DriverUI",uiOutput("DriverUI"))
           
  )
  
)

ui <- dashboardPagePlus(
  shinyjs::useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  title = 'DMX'
)
