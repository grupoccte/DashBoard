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
  
  #Analise de desempenho
  baseFiltradaDese <- reactive({
    a<- filter(baseDesempenho,Curso == input$curso, Periodo == input$periodo, Nome.da.Disciplina == input$disciplina)
  })
  
  
  
  #retorna tabela indicadores Desempenho
  output$indicadoresDesempenho <- renderDataTable({
    listaVariaveis <- data.frame(dicionarioBaseDesempenho[,c("Descrição.sobre.as.variáveis", "Construto")])
    colnames(listaVariaveis) <- c("Indicador", "Construto")
    DT::datatable(
      listaVariaveis,options = list(paging = FALSE,searching = FALSE, 
                                    info = FALSE, scrollY = '300px', scrollX = '300px'),class = "compact",
      selection = list(target = 'row',mode="single",selected=c(1))
    )
  })

  #retorna tabela alunos Desempenho
  output$alunosDesempenho <- renderDataTable({
    listaAlunosDese <- data.frame(baseFiltradaDese()[,c("Nome.do.Aluno","DESEMPENHO_BINARIO")])
    listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
    listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório"
    #listaSelect <- listaAlunos[]
    colnames(listaAlunosDese) <- c("Nome", "Desempenho")
    DT::datatable(
      listaAlunosDese,options = list(paging = FALSE,searching = FALSE, 
                                 info = FALSE, scrollY = '300px'),class = "compact"
    )
  })
  
  #Analise de evasao

})
