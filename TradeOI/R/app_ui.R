#' Function ui to run TradeOi app
#'
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinyjs
#' @import shinyalert
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export


# Define UI

#Charging all the packages

# Define UI
app_ui = function(){
  shinyUI(dashboardPage(skin = "black",
                           dashboardHeader(title = "Detecting Outliers and Computing Trade Indices", titleWidth = 450,
                                           tags$li(a(href = 'http://www.intracen.org/',
                                                     img(src = 'img/logo.png',
                                                         title = "ITC Home", height = "30px"),
                                                     style = "padding-top:10px; padding-bottom:10px;"),
                                                   class = "dropdown"),

                                           tags$li(a(href = 'https://www.trademap.org/Index.aspx',
                                                     img(src = 'img/trademap.png',
                                                         title = "Trade Map", height = "30px"),
                                                     style = "padding-top:10px; padding-bottom:10px;"),
                                                   class = "dropdown"),

                                           tags$li(a(href = 'mailto:marketanalysis@intracen.org',
                                                     img(src = 'img/www/mail.png',
                                                         title = "Send us an email", height = "30px"),
                                                     style = "padding-top:10px; padding-bottom:10px;"),
                                                   class = "dropdown")),

                           #content of the sideBar
                           dashboardSidebar(
                             source("inst/Body/sidebar.R",local=T)$value),

                           #content of the body
                           dashboardBody(
                             source("inst/Body/body.R",local=T)$value),
                           useShinyalert()

                        ))
}
