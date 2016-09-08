library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
require(devtools)
require(rCharts)


#Leitura base de dados gerais
baseGeral <- read.csv2(file = "data/BaseGeral/base_desempenho.csv", encoding = "UTF-8")
#Leitura base de dados evasao
baseEvasao <- read.csv2(file = "data/BaseEvasão/base_evasao.csv", encoding = "latin1")
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
    if(input$aplicacao != 3){
      cursos <- as.character(unique(baseGeral$Curso))
      selectInput("curso", "Escolha o Curso:", choices = (sort(cursos)))  
    }else{
      cursos<- as.character(unique(baseEvasao$Curso))
      selectInput("curso","Escolha o Curso:", choices = (sort(cursos)))
    }
    
  })
  #Select para o periodo a depender do curso escolhido
  output$seletorPeriodo <- renderUI({
    if(input$aplicacao != 3){
      periodos <- filter(baseGeral, Curso == input$curso)
      showPeriodo <- sort(as.character(unique(periodos$Periodo)))
      names(showPeriodo)<- paste(showPeriodo,"º Periodo")
      selectInput("periodo", "Escolha o Periodo:", showPeriodo)
    }else{
      periodos<- filter(baseEvasao,Curso == input$curso)
      showPeriodo <- sort(as.character(unique(periodos$Periodo)))
      names(showPeriodo)<- paste(showPeriodo,"º Periodo")
      selectInput("periodo","Escolha o Periodo:",showPeriodo)
    }
  })
  #Select da disciplina a depender do curso e do periodo escolhido
  output$seletorDisciplina <- renderUI({
    if(input$aplicacao != 3){
      disciplinas <- filter(baseGeral,Curso == input$curso, Periodo == input$periodo)
      selectInput("disciplina","Escolha a Disciplina:",as.character(unique(disciplinas$Disciplina)))  
    }else{
      disciplinas <- filter(baseEvasao,Curso == input$curso, Periodo == input$periodo)
      selectInput("disciplina","Escolha a Disciplina:",as.character(unique(disciplinas$Disciplina)))  
    }
    
  })
  
  
  ##Base de acordo com os parametros escolhidos e de acordo com a base de dados
  baseFiltrada <- reactive({
    if(input$aplicacao == 1){
      filter(baseGeral,Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
    }else if(input$aplicacao == 2){
      filter(baseDesempenho,Curso == input$curso, Periodo == input$periodo, Nome.da.Disciplina == input$disciplina)
    }else{
      filter(baseEvasao,Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
    }
    
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
    listaAlunosDese <- data.frame(baseFiltrada()[,c("Nome.do.Aluno","DESEMPENHO_BINARIO")])
    listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
    listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório"
    colnames(listaAlunosDese) <- c("Nome", "Desempenho")
    DT::datatable(
      listaAlunosDese,options = list(paging = FALSE,searching = FALSE, 
                                     info = FALSE, scrollY = '300px'),class = "compact"
    )
  })
  
  #Analise de evasao
  
  #retorna tabela de indicadores evasao
  output$indicadoresEvasao <- renderDataTable({
    listaVariaveis <- data.frame(dicionarioBaseEvasao[,c("INDICADOR","CONSTRUTOS")])
    colnames(listaVariaveis) <- c("Indicador","Construto")
    DT::datatable(
      listaVariaveis,options = list(paging = FALSE,searching = FALSE, 
                                    info = FALSE, scrollY = '300px')
    )
  })
  #retorna tabela de alunos evasao
  output$alunosEvasao <- renderDataTable({
    listaAlunosEvasao <- data.frame(baseFiltrada()[,c("Aluno","EVASAO")])
    listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 0] <- "Baixo"
    listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 1] <- "Alto"
    colnames(listaAlunosEvasao) <- c("Nome", "Risco")
    DT::datatable(
      listaAlunosEvasao,options = list(paging = FALSE,searching = FALSE, 
                                     info = FALSE, scrollY = '300px'),class = "compact"
    )
  })
  
  #infoBoxes
  variavelClasseEvasao <- reactive({
    table(baseFiltrada()$EVASAO)
  })
  
  #Retorna a % Baixo Risco EVASAO = 0
  variavelBaixoRisco <- reactive({
    if(is.na(variavelClasseEvasao()[1])){
      0
    }else{
      round((variavelClasseEvasao()[1]/count(baseFiltrada()))*100,2) 
    }
  })
  #Retorna a % Alto Risco EVASAO = 1
  variavelAltoRisco <- reactive({
    if(is.na(variavelClasseEvasao()[2])){
      0
    }else{
      round((variavelClasseEvasao()[2]/count(baseFiltrada()))*100,2) 
    }
  })
  
  output$AltoRiscoBox <- renderValueBox({
    valueBox(
      paste0(variavelAltoRisco(), "%"), "Alto Risco", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  output$BaixoRiscoBox <- renderValueBox({
    valueBox(
      paste0(variavelBaixoRisco(), "%"), "Baixo Risco", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
})