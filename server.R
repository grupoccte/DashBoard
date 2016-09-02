library(shiny)
library(shinydashboard)

shinyServer(function(input, output) {
  #Radio da aplicação
  output$radioApp <- renderUI({
    radioButtons("aplicacao","Seleciona a aplicação:", 
                 choices =c("Visão geral dos dados" = "geral",
                            "Análise de Desempenho" = "analiseDesempenho",
                            "Análise de Evasão" = "analiseEvasao"),selected = "geral")
  })
  
  #Select para Curso
  output$seletorCurso <- renderUI({
    selectInput("curso", "Escolha o Curso:", choices = (""))
  })
  #Select para o periodo a depender do curso escolhido
  output$seletorPeriodo <- renderUI({
    selectInput("periodo", "Escolha o Periodo:", choices = (""))
  })
  #Select da disciplina a depender do curso e do periodo escolhido
  output$seletorDisciplina <- renderUI({
    selectInput("disciplina","Escolha a Disciplina:",choices = (""))
  })

})
