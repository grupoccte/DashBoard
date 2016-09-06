library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
require(devtools)
require(rCharts)

dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parametros",tabName = "Parametros", icon = icon("dashboard"))
    ),
    uiOutput("radioApp"),
    uiOutput("seletorCurso"),
    uiOutput("seletorPeriodo"),
    uiOutput("seletorDisciplina")
    ),
  dashboardBody(
    #Visao Geral
    conditionalPanel("input.aplicacao == 1",
                     fluidRow(
                       column(
                         width=8,
                         tabBox(
                           side = "left", width = 12,
                           selected = "Geral",
                           tabPanel("Geral", ""),
                           tabPanel("Indicadores", ""),
                           tabPanel("Alunos", "")
                         )
                       ),
                       column(
                         width = 4,
                         box(
                           width = 12, title = "Indicadores",status = "primary",solidHeader = TRUE ,collapsible = TRUE,
                           dataTableOutput("indicadoresGeral")
                         ),
                         box(
                           width=12,title = "Alunos",status = "primary",solidHeader = TRUE ,collapsible = TRUE,
                           dataTableOutput("alunosGeral")
                         )
                       )
                     )
                     ),
    #Analise de desempenho
    conditionalPanel("input.aplicacao == 2",
                     fluidRow(
                       column(
                         width=8,
                         tabBox(
                           side = "left", width = 12,
                           selected = "Geral",
                           tabPanel("Geral", ""),
                           tabPanel("Indicadores", ""),
                           tabPanel("Alunos", "")
                         )
                       ),
                       column(
                         width = 4,
                         box(
                           width = 12, title = "Indicadores",status = "primary",solidHeader = TRUE ,collapsible = TRUE
                         ),
                         box(
                           width=12,title = "Alunos",status = "primary",solidHeader = TRUE ,collapsible = TRUE
                         )
                       )
                     )),
    #Analise de Evasao
    conditionalPanel("input.aplicacao == 3",
                     fluidRow(
                       column(
                         width=8,
                         tabBox(
                           side = "left", width = 12,
                           selected = "Geral",
                           tabPanel("Geral", ""),
                           tabPanel("Indicadores", ""),
                           tabPanel("Alunos", "")
                         )
                       ),
                       column(
                         width = 4,
                         box(
                           width = 12, title = "Indicadores",status = "primary",solidHeader = TRUE ,collapsible = TRUE
                         ),
                         box(
                           width=12,title = "Alunos",status = "primary",solidHeader = TRUE ,collapsible = TRUE
                         )
                       )
                     ))
  )
)