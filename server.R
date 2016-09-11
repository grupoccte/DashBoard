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
dicionarioBaseEvasao <- dicionarioBaseEvasao[with(dicionarioBaseEvasao, order(CONSTRUTOS)), ] #ordenação pela coluna de construto para facilitar os checkboxes dinâmicos sem repetição
rownames(dicionarioBaseEvasao) <- NULL #Correção para o número das linhas após ordenação
baseDesempenho <- read.csv2(file = "data/BaseDesempenho/base_desempenho.csv", encoding ="UTF-8")
dicionarioBaseDesempenho <- read.csv2(file = "data/BaseDesempenho/dicionario_dadosDesempenho.csv")

#::Calculo de médias, máximos e mínimos para análise geral

colVariaveis <- select(baseGeral, one_of(as.character(dicionarioBaseDesempenho$Variável))) #Seleciona somente as colunas das variáveis na base geral em função do dicionário
visGeralIndicadores <- data.frame(
  Indicador = paste("Ind", c(1:ncol(colVariaveis))),
  Min = sapply(colVariaveis, min), 
  Média = colMeans(colVariaveis),
  Max = sapply(colVariaveis, max)
) 

#::Dados p/ análise de desempenho

#Construtos de variáveis, gera um dataframe associando um construto a uma range de variáveis (ex: Busca por ajuda - 6:12)

anterior <- ""
Construto <- c()
Varinicial <- c()
Varfinal <- c()

for(i in 1:nrow(dicionarioBaseDesempenho)) {
  atual <- as.character(dicionarioBaseDesempenho[i,"Construto"])
  if(atual != "" && atual != anterior) {
    Construto <- c(Construto, atual)
    anterior = atual
    Varinicial <- c(Varinicial, i)
    if(i != 1) {
      Varfinal <- c(Varfinal, i - 1)
    }
  }
}
Varfinal <- c(Varfinal, nrow(dicionarioBaseDesempenho))

DFconstrutosDesempenho <- data.frame(Construto, Varinicial, Varfinal)

#Tratamento de dados para plotagem de gráfico de desempenho
colVariaveisSat <- select(filter(baseDesempenho, DESEMPENHO_BINARIO == "0"), one_of(as.character(dicionarioBaseDesempenho$Variável)))
colVariaveisInsat <- select(filter(baseDesempenho, DESEMPENHO_BINARIO == "1"), one_of(as.character(dicionarioBaseDesempenho$Variável)))

dadosDesempenho <- data.frame(
  Indicador = c(
    paste("Ind", c(1:ncol(colVariaveisSat))), 
    paste("Ind", c(1:ncol(colVariaveisSat)))
  ),
  Min = c(
    Min = sapply(colVariaveisSat, min),
    Min = sapply(colVariaveisInsat, min)
  ),
  Média = c(
    colMeans(colVariaveisSat),
    colMeans(colVariaveisInsat)
  ),
  Max = c(
    sapply(colVariaveisSat, max),
    sapply(colVariaveisInsat, max)
  ),
  Desempenho = c(
    rep("Satisfatório", each = ncol(colVariaveisSat)),
    rep("Insatisfatório", each = ncol(colVariaveisInsat))
  )
)

#Tratamento de dados para plotagem de gráfico de evasão
colVariaveisBaixoRisco <- select(filter(baseEvasao, EVASAO == "0"), one_of(as.character(dicionarioBaseEvasao$ID)))
colVariaveisAltoRisco <- select(filter(baseEvasao, EVASAO == "1"), one_of(as.character(dicionarioBaseEvasao$ID)))

dadosEvasao <- data.frame(
  Indicador = c(
    paste("Ind", c(1:ncol(colVariaveisBaixoRisco))), 
    paste("Ind", c(1:ncol(colVariaveisBaixoRisco)))
  ),
  Min = c(
    Min = sapply(colVariaveisAltoRisco, min),
    Min = sapply(colVariaveisBaixoRisco, min)
  ),
  Média = c(
    colMeans(colVariaveisAltoRisco),
    colMeans(colVariaveisBaixoRisco)
  ),
  Max = c(
    sapply(colVariaveisAltoRisco, max),
    sapply(colVariaveisBaixoRisco, max)
  ),
  RiscoEvasao = c(
    rep("Alto risco", each = ncol(colVariaveisAltoRisco)),
    rep("Baixo risco", each = ncol(colVariaveisBaixoRisco))
  )
)

#Construtos de evasão

anterior <- ""
Construto <- c()
Varinicial <- c()
Varfinal <- c()

for(i in 1:nrow(dicionarioBaseEvasao)) {
  atual <- as.character(dicionarioBaseEvasao[i,"CONSTRUTOS"])
  if(atual != "" && atual != anterior) {
    Construto <- c(Construto, atual)
    anterior = atual
    Varinicial <- c(Varinicial, i)
    if(i != 1) {
      Varfinal <- c(Varfinal, i - 1)
    }
  }
}
Varfinal <- c(Varfinal, nrow(dicionarioBaseEvasao))

DFconstrutosEvasao <- data.frame(Construto, Varinicial, Varfinal)

shinyServer(function(input, output) {
  #Radio da aplicação
  output$radioApp <- renderUI({
    radioButtons("aplicacao","Seleciona a aplicação:", 
                 choices =c("Visão geral dos dados" = 1,
                            "Análise de Desempenho" = 2,
                            "Análise de Evasão" = 3),selected = 1)
  })
  
  #Base em função do radio
  baseInputs <- reactive({
    if(!is.null(input$aplicacao)) {
      if(input$aplicacao != 3) {
        baseGeral
      } else {
        baseEvasao
      }
    } else {
      NULL
    }
  })
  
  #Select para Curso
  output$seletorCurso <- renderUI({
    if(!is.null(input$aplicacao)) {
      cursos <- sort(as.character(unique(baseInputs()$Curso)))
    } else {
      cursos <- NULL
    }
    selectInput("curso", "Escolha o Curso:", cursos)
  })
  
  #Select para o periodo a depender do curso escolhido
  output$seletorPeriodo <- renderUI({
    if(!is.null(input$curso) && input$curso != "") {
      periodos<- filter(baseInputs(), Curso == input$curso)
      showPeriodo <- sort(as.character(unique(periodos$Periodo)))
      if(length(showPeriodo) > 0) {
        names(showPeriodo) <- paste(showPeriodo, "º Periodo")
      }
    } else {
      showPeriodo <- NULL
    }
    selectInput("periodo", "Escolha o Periodo:", showPeriodo)
  })
  
  #Select da disciplina a depender do curso e do periodo escolhido
  output$seletorDisciplina <- renderUI({
    if(!is.null(input$curso) && input$curso != "" && !is.null(input$periodo) && input$periodo != "") {
      disciplinas <- filter(baseInputs(), Curso == input$curso, Periodo == input$periodo)
    } else {
      disciplinas <- NULL
    }
    selectInput("disciplina","Escolha a Disciplina:",as.character(unique(disciplinas$Disciplina)))
  })
  
  
  ##Base de acordo com os parametros escolhidos e de acordo com a base de dados
  baseFiltrada <- reactive({
    if(!is.null(input$aplicacao)) {
      if(input$aplicacao == 1){
        filter(baseGeral, Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
      }else if(input$aplicacao == 2){
        filter(baseDesempenho, Curso == input$curso, Periodo == input$periodo, Nome.da.Disciplina == input$disciplina)
      }else{
        filter(baseEvasao, Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
      }
    } else {
      NULL
    }
  })
  
  #Visao geral dos dados
  
  #retorna tabela indicadores gerais
  output$indicadoresGeral <- renderDataTable({
    listaVariaveis <- data.frame(dicionarioBaseDesempenho$Descrição.sobre.as.variáveis)
    colnames(listaVariaveis) <- c("Indicador")
    DT::datatable(
      listaVariaveis,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = '300px'
      )
    )
  })
  
  #retorna tabela alunos gerais
  output$alunosGeral <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 1 && !is.null(baseFiltrada())) {
      listaAlunos <- select(baseFiltrada(), Aluno);
      colnames(listaAlunos) <- c("Nome")
    } else {
      listaAlunos <- NULL
    }
    DT::datatable(
      listaAlunos, 
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = '300px'
      )
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
  
  INcheckboxesDesempenho <- reactive({
    checkboxes <- grep("constdesemp_", names(input))
    qtd <- length(checkboxes)
    construtos <- c()
    for(i in 1:qtd) {
      if(!is.null(input[[c(paste("constdesemp_", i, sep = ""))]]) && input[[c(paste("constdesemp_", i, sep = ""))]] == TRUE) {
        construtos <- c(construtos, i)
      }
    }
    construtos
  })

  #retorna tabela indicadores Desempenho
  output$indicadoresDesempenho <- renderDataTable({

    construtosCheckBox <- INcheckboxesDesempenho()
    
    listaVariaveis <- data.frame(dicionarioBaseDesempenho[,c("Descrição.sobre.as.variáveis", "Construto")])
    colnames(listaVariaveis) <- c("Indicador", "Construto")
    
    if(!is.null(construtosCheckBox)) {
      filtroVariaveis <- c()
      for(i in construtosCheckBox) {
        filtroVariaveis <- c(filtroVariaveis, DFconstrutosDesempenho[i,]$Varinicial:DFconstrutosDesempenho[i,]$Varfinal)
      }
    } else {
      filtroVariaveis <- c(1:nrow(listaVariaveis))
    }
    
    DT::datatable(
      listaVariaveis[filtroVariaveis,],
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE, scrollY = '300px',
        scrollX = '300px'),
      class = "compact",
      selection = list(target = 'row', mode="multiple")
    )
  })
  
  #retorna tabela alunos Desempenho
  output$alunosDesempenho <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      listaAlunosDese <- data.frame(baseFiltrada()[,c("Nome.do.Aluno","DESEMPENHO_BINARIO")])
      listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
      listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório";
      colnames(listaAlunosDese) <- c("Nome", "Desempenho")
    } else {
      listaAlunosDese <- NULL
    }
    DT::datatable(
      listaAlunosDese,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = '300px'),
      class = "compact"
    )
  })
  
  #infoBoxes de desempenho
  
  #BoxDesempenho satisfatório
  output$SatisfatorioBox <- renderValueBox({
    desempenhoSatisfatorio <- 0
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      classesSat <- table(baseFiltrada()$DESEMPENHO_BINARIO)
      desempenhoSatisfatorio <-round((classesSat[1] / nrow(baseFiltrada())) * 100, 2)
    }
    valueBox(
      paste0(desempenhoSatisfatorio, "%"), "Satisfatório", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  #BoxDesempenho insatisfatório
  output$InsatisfatorioBox <- renderValueBox({
    desempenhoInsatisfatorio <- 0
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      classesSat <- table(baseFiltrada()$DESEMPENHO_BINARIO)
      desempenhoInsatisfatorio <-round((classesSat[2] / nrow(baseFiltrada())) * 100, 2)
    }
    valueBox(
      paste0(desempenhoInsatisfatorio, "%"), "Insatisfatório", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$graficoDesempenho <- renderChart2({
    indSelecionados <- input$indicadoresDesempenho_rows_selected
    if(is.null(indSelecionados)) {
      construtosCheckBox <- INcheckboxesDesempenho()
      if(!is.null(construtosCheckBox)) {
        indSelecionados <- c()
        for(i in construtosCheckBox) {
          indSelecionados <- c(indSelecionados, DFconstrutosDesempenho[i,]$Varinicial:DFconstrutosDesempenho[i,]$Varfinal)
        }
      } else {
        indSelecionados <- c(1:nrow(dicionarioBaseDesempenho))
      }
    }
    indSelecionados <- c(indSelecionados, indSelecionados + nrow(dicionarioBaseDesempenho))
    nPlot(Média ~ Indicador, data = dadosDesempenho[indSelecionados,], group = 'Desempenho', type = 'multiBarHorizontalChart', width = 600)
  })
  
  #Checkboxes p/ construtos de desempenho
  
  output$construtosDesempenho <- renderUI({
    colunas <- list() #lista de colunas
    colunas[[1]] <- list() #cada coluna tem uma lista de checkboxes
    colunas[[2]] <- list()
    colunas[[3]] <- list()
    totalconstrutos <- nrow(DFconstrutosDesempenho)
    construtosporcol <- round(totalconstrutos / 3, 0)
    
    colat <- 1
    nsel <- 1
    for(i in 1:nrow(DFconstrutosDesempenho)) {
      colunas[[colat]][[nsel]] <- checkboxInput(paste("constdesemp_", i, sep = ""), DFconstrutosDesempenho[i,]$Construto, FALSE)
      nsel <- nsel + 1
      if(nsel > construtosporcol) {
        colat <- colat + 1
        nsel <- 1
      }
    }
    
    fluidRow(
      column(
        width=4,
        colunas[[1]]
      ),
      column(
        width=4,
        colunas[[2]]
      ),
      column(
        width=4,
        colunas[[3]]
      )
    )
  })
  
  #Analise de evasao
  
  INcheckboxesEvasao <- reactive({
    checkboxes <- grep("constevas_", names(input))
    qtd <- length(checkboxes)
    construtos <- c()
    for(i in 1:qtd) {
      if(!is.null(input[[c(paste("constevas_", i, sep = ""))]]) && input[[c(paste("constevas_", i, sep = ""))]] == TRUE) {
        construtos <- c(construtos, i)
      }
    }
    construtos
  })
  
  #retorna tabela de indicadores evasao
  output$indicadoresEvasao <- renderDataTable({
    listaVariaveis <- data.frame(dicionarioBaseEvasao[,c("INDICADOR","CONSTRUTOS")])
    colnames(listaVariaveis) <- c("Indicador","Construto")
    
    construtosCheckBox <- INcheckboxesEvasao()
    
    if(!is.null(construtosCheckBox)) {
      filtroVariaveis <- c()
      for(i in construtosCheckBox) {
        filtroVariaveis <- c(filtroVariaveis, DFconstrutosEvasao[i,]$Varinicial:DFconstrutosEvasao[i,]$Varfinal)
      }
    } else {
      filtroVariaveis <- c(1:nrow(listaVariaveis))
    }
    
    DT::datatable(
      listaVariaveis[filtroVariaveis,],
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE, 
        scrollY = '300px'
      )
    )
  })
  
  #retorna tabela de alunos evasao
  output$alunosEvasao <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 3) {
      listaAlunosEvasao <- data.frame(baseFiltrada()[,c("Aluno","EVASAO")])
      listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 0] <- "Baixo"
      listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 1] <- "Alto"
      colnames(listaAlunosEvasao) <- c("Nome", "Risco")
    } else {
      listaAlunosEvasao <- NULL
    }
    DT::datatable(
      listaAlunosEvasao,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = '300px'),
      class = "compact"
    )
  })
  
  #infoBoxes de evasão
  
  #BoxEvasao baixo risco
  output$BaixoRiscoBox <- renderValueBox({
    baixoRisco <- 0 
    if(!is.null(input$aplicacao) && input$aplicacao == 3) {
      classesEvas <- table(baseFiltrada()$EVASAO)
      baixoRisco <- round((classesEvas[1] / nrow(baseFiltrada())) * 100, 2)
    }
    valueBox(
      paste0(baixoRisco, "%"), "Baixo Risco", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  #BoxEvasao alto risco
  output$AltoRiscoBox <- renderValueBox({ 
    altoRisco <- 0
    if(!is.null(input$aplicacao) && input$aplicacao == 3) {
      classesEvas <- table(baseFiltrada()$EVASAO)
      altoRisco <- round((classesEvas[2] / nrow(baseFiltrada())) * 100, 2)
    }
    valueBox(
      paste0(altoRisco, "%"), "Alto Risco", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #Gráfico de evasão
  
  output$graficoEvasao <- renderChart2({
    indSelecionados <- input$indicadoresEvasao_rows_selected
    
    if(is.null(indSelecionados)) {
      construtosCheckBox <- INcheckboxesEvasao()
      if(!is.null(construtosCheckBox)) {
        indSelecionados <- c()
        for(i in construtosCheckBox) {
          indSelecionados <- c(indSelecionados, DFconstrutosEvasao[i,]$Varinicial:DFconstrutosEvasao[i,]$Varfinal)
        }
      } else {
        indSelecionados <- c(1:nrow(dicionarioBaseEvasao))
      }
    }
    
    indSelecionados <- c(indSelecionados, indSelecionados + nrow(dicionarioBaseEvasao))
    g <- nPlot(Média ~ Indicador, data = dadosEvasao[indSelecionados,], group = "RiscoEvasao", type = 'multiBarHorizontalChart', width = 600)
  })
  
  #Checkboxes p/ construtos de evasão
  
  output$construtosEvasao <- renderUI({
    colunas <- list()
    colunas[[1]] <- list()
    colunas[[2]] <- list()
    colunas[[3]] <- list()
    totalconstrutos <- nrow(DFconstrutosEvasao)
    construtosporcol <- round(totalconstrutos / 3, 0)
    
    colat <- 1
    nsel <- 1
    for(i in 1:nrow(DFconstrutosEvasao)) {
      colunas[[colat]][[nsel]] <- checkboxInput(paste("constevas_", i, sep = ""), DFconstrutosEvasao[i,]$Construto, FALSE)
      nsel <- nsel + 1
      if(nsel > construtosporcol) {
        colat <- colat + 1
        nsel <- 1
      }
    }
    
    fluidRow(
      column(
        width=4,
        colunas[[1]]
      ),
      column(
        width=4,
        colunas[[2]]
      ),
      column(
        width=4,
        colunas[[3]]
      )
    )
  })
  
})