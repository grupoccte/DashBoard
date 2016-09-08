library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
require(devtools)
require(rCharts)


#Leitura base de dados gerais
baseGeral <- read.csv2(file = "data/BaseGeral/base_desempenho.csv", encoding = "UTF-8")
#Leitura base de dados evasao
baseEvasao <- read.csv2(file = "data/BaseEvasão/base_evasao.csv", encoding = "UTF-8")
dicionarioBaseEvasao <- read.csv(file = "data/BaseEvasão/dicionario_dadosEvasao.csv", encoding = "UTF-8")#Leitura base de dados desempenho
baseDesempenho <- read.csv2(file = "data/BaseDesempenho/base_desempenho.csv", encoding ="UTF-8")
dicionarioBaseDesempenho <- read.csv2(file = "data/BaseDesempenho/dicionario_dadosDesempenho.csv")

#Calculo de médias, máximos e mínimos para análise geral

colVariaveis <- select(baseGeral, one_of(as.character(dicionarioBaseDesempenho$Variável))) #Seleciona somente as colunas das variáveis na base geral em função do dicionário
visGeralIndicadores <- data.frame(
  Indicador = paste("Ind", c(1:ncol(colVariaveis))),
  Min = sapply(select(baseGeral, c(5:41)), min), 
  Média = colMeans(select(baseGeral, c(5:41))), 
  Max = sapply(select(baseGeral, c(5:41)), max)
)


shinyServer(function(input, output) {
  #Radio da aplicação
  output$radioApp <- renderUI({
    radioButtons("aplicacao","Seleciona a aplicação:", 
                 choices =c("Visão geral dos dados" = 1,
                            "Análise de Desempenho" = 2,
                            "Análise de Evasão" = 3),selected = 1)
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
    a<- filter(baseGeral,Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
  })
  
  #Visao geral dos dados
  
  #retorna tabela indicadores gerais
  output$indicadoresGeral <- renderDataTable({
    listaVariaveis <- data.frame(dicionarioBaseDesempenho$Descrição.sobre.as.variáveis)
    colnames(listaVariaveis) <- c("Indicador")
    DT::datatable(
      listaVariaveis,options = list(paging = FALSE,searching = FALSE, 
                                      info = FALSE, scrollY = '300px')
    )
  })
  #retorna tabela alunos gerais
  output$alunosGeral <- renderDataTable({
    listaAlunos <- data.frame(baseFiltrada()$Aluno)
    colnames(listaAlunos) <- c("Nome")
    DT::datatable(
      listaAlunos,options = list(paging = FALSE,searching = FALSE, 
                                 info = FALSE, scrollY = '300px')
    )
  })
  
  output$graficoGeral <- renderChart2({
    indSelecionados <- input$indicadoresGeral_rows_selected
    if(is.null(indSelecionados)) {
      indSelecionados <- c(1:nrow(dicionarioBaseDesempenho))
    }
    g <- nPlot(Média ~ Indicador, data = visGeralIndicadores[indSelecionados,], type = 'multiBarHorizontalChart', width = 600)
    g$chart(showControls = F)
    g
  })
  
  #Analise de desempenho
  
  #Analise de evasao

})
