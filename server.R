library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)


#Leitura base de dados gerais
baseGeral <- read.csv2(file = "data/BaseGeral/base_desempenho.csv", encoding = "UTF-8")
#Leitura base de dados evasao
baseEvasao <- read.csv2(file = "data/BaseEvasão/base_evasao.csv", encoding = "UTF-8")
dicionarioBaseEvasao <- read.csv(file = "data/BaseEvasão/dicionario_dadosEvasao.csv", encoding = "UTF-8")#Leitura base de dados desempenho
baseDesempenho <- read.csv2(file = "data/BaseDesempenho/base_desempenho.csv", encoding ="UTF-8")
dicionarioBaseDesempenho <- read.csv2(file = "data/BaseDesempenho/dicionario_dadosDesempenho.csv")

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
    cursos <- as.character(unique(baseGeral$Curso))
    selectInput("curso", "Escolha o Curso:", choices = (sort(cursos)))
  })
  #Select para o periodo a depender do curso escolhido
  output$seletorPeriodo <- renderUI({
    periodos <- filter(baseGeral, Curso == input$curso)
    showPeriodo <- sort(as.character(unique(periodos$Periodo)))
    names(showPeriodo)<- paste(showPeriodo,"º Periodo")
    selectInput("periodo", "Escolha o Periodo:", showPeriodo)
  })
  #Select da disciplina a depender do curso e do periodo escolhido
  output$seletorDisciplina <- renderUI({
    disciplinas <- filter(baseGeral,Curso == input$curso, Periodo == input$periodo)
    selectInput("disciplina","Escolha a Disciplina:",as.character(unique(disciplinas$Disciplina)))
  })
  
  ##Base de acordo com os parametros escolhidos
  baseFiltrada <- reactive({
    filter(baseGeral,Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
  }) 
  
  
  #Visao geral dos dados
  
  #retorna tabela indicadores alunos
  output$indicadoresGeral <- renderDataTable({
    listaIndicadores <- data.frame(dicionarioBaseEvasao$INDICADOR)
    colnames(listaIndicadores) <- c("Indicador")
    DT::datatable(
      listaIndicadores,options = list(paging = FALSE,searching = FALSE, 
                                      info = FALSE, scrollY = '300px')
    )
  })
  
  #Analise de desempenho
  
  #Analise de evasao

})
