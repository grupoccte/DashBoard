library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(tidyr)
require(devtools)
require(rCharts)


#Leitura base de dados gerais
baseGeral <- read.csv2(file = "data/BaseGeral/base_geral.csv", encoding = "latin1")
#Leitura base de dados evasao
baseEvasao <- read.csv2(file = "data/BaseEvasão/base_evasao.csv", encoding = "latin1")
dicionarioBaseEvasao <- read.csv(file = "data/BaseEvasão/dicionario_dadosEvasao.csv", encoding = "UTF-8")#Leitura base de dados desempenho
dicionarioBaseEvasao <- dicionarioBaseEvasao[with(dicionarioBaseEvasao, order(CONSTRUTOS)), ] #ordenação pela coluna de construto para facilitar os checkboxes dinâmicos sem repetição
rownames(dicionarioBaseEvasao) <- NULL #Correção para o número das linhas após ordenação
baseDesempenho <- read.csv2(file = "data/BaseDesempenho/base_desempenho.csv", encoding ="UTF-8")
dicionarioBaseDesempenho <- read.csv2(file = "data/BaseDesempenho/dicionario_dadosDesempenho.csv", encoding = "UTF-8")
listaVariaveisDesempenho <- data.frame(dicionarioBaseDesempenho[,c("Variável","Descrição.sobre.as.variáveis")])
listaVariaveisGeral <- read.csv(file = "data/BaseGeral/Novo_dicionario_dadosGeral.csv", encoding = "UTF-8")
listaVariaveisEvasao <- data.frame(dicionarioBaseEvasao[,c("ID","INDICADOR")])
listaVariaveisEvasao <- listaVariaveisEvasao[with(listaVariaveisEvasao, order(ID)), ]
#::Calculo de médias, máximos e mínimos para análise geral

visGeralIndicadores <- function(base) {
  if(!is.null(base) && nrow(base) != 0) {
    colVariaveis <- select(base, one_of(as.character(listaVariaveisGeral$Variável))) #Seleciona somente as colunas das variáveis na base geral em função do dicionário
    data.frame(
      Indicador = c(1:ncol(colVariaveis)),
      Min = sapply(colVariaveis, min), 
      Média = colMeans(colVariaveis),
      Max = sapply(colVariaveis, max)
    )
  } else {
    NULL
  }
}

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

dadosDesempenho <- function(base) {
  if(!is.null(base)) {
    colVariaveisSat <- select(filter(base, DESEMPENHO_BINARIO == "0"), one_of(as.character(dicionarioBaseDesempenho$Variável)))
    colVariaveisInsat <- select(filter(base, DESEMPENHO_BINARIO == "1"), one_of(as.character(dicionarioBaseDesempenho$Variável)))
    
    #MIN
    if(nrow(colVariaveisSat) != 0) {
      minSat <- sapply(colVariaveisSat, min)
    } else {
      minSat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    if(nrow(colVariaveisInsat) != 0) {
      minInsat <- sapply(colVariaveisInsat, min)
    } else {
      minInsat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    #MÉDIA
    if(nrow(colVariaveisSat) != 0) {
      mediaSat <- colMeans(colVariaveisSat)
    } else {
      mediaSat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    if(nrow(colVariaveisInsat) != 0) {
      mediaInsat <- colMeans(colVariaveisInsat)
    } else {
      mediaInsat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    #MAX
    if(nrow(colVariaveisSat) != 0) {
      maxSat <- sapply(colVariaveisSat, max)
    } else {
      maxSat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    if(nrow(colVariaveisInsat) != 0) {
      maxInsat <- sapply(colVariaveisInsat, max)
    } else {
      maxInsat <- rep(0, each = ncol(colVariaveisSat))
    }
    
    data.frame(
      Indicador = c(
        c(1:ncol(colVariaveisSat)), 
        c(1:ncol(colVariaveisSat))
      ),
      Min = c(
        minSat,
        minInsat
      ),
      Média = c(
        mediaSat,
        mediaInsat
      ),
      Max = c(
        maxSat,
        maxInsat
      ),
      Desempenho = c(
        rep("Satisfatório", each = ncol(colVariaveisSat)),
        rep("Insatisfatório", each = ncol(colVariaveisInsat))
      )
    )
  } else {
    NULL
  }
}

#Complemento dos construtos de desempenho (variáveis que não estão associadas a nenhum construto no dataframe após leitura do dicionário)

Construtos <- c()
for(i in 1:nrow(DFconstrutosDesempenho)) {
  Construtos <- c(Construtos, rep(as.character(DFconstrutosDesempenho[i,]$Construto), (DFconstrutosDesempenho[i,]$Varfinal + 1) - DFconstrutosDesempenho[i,]$Varinicial))
}
dicionarioBaseDesempenho$Construto <- Construtos

#Calculo de correção para construtos de desempenho não selecionados

calcConstrutosDesempenho <- function(linhas, construtos) {
  if(!is.null(construtos)) {
    totaldiff <- 0
    diff <- c()
    ultimoanterior <- 0
    for(i in construtos) {
      totaldiff <- totaldiff + (DFconstrutosDesempenho[i,]$Varinicial - ultimoanterior - 1)
      ultimoanterior <- DFconstrutosDesempenho[i,]$Varfinal
      diff <- c(diff, rep(totaldiff, each = (DFconstrutosDesempenho[i,]$Varfinal - DFconstrutosDesempenho[i,]$Varinicial + 1)))
    }
    return(diff[linhas])
  } else {
    return(0)
  }
}

#Tratamento de dados para plotagem de gráfico de evasão

dadosEvasao <- function(base) {
  if(!is.null(base)) {
    colVariaveisBaixoRisco <- select(filter(base, EVASAO == "1"), one_of(as.character(dicionarioBaseEvasao$ID)))
    colVariaveisAltoRisco <- select(filter(base, EVASAO == "0"), one_of(as.character(dicionarioBaseEvasao$ID)))
    
    #MIN
    if(nrow(colVariaveisBaixoRisco) != 0) {
      minBaixo <- sapply(colVariaveisBaixoRisco, min)
    } else {
      minBaixo <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    if(nrow(colVariaveisAltoRisco) != 0) {
      minAlto <- sapply(colVariaveisAltoRisco, min)
    } else {
      minAlto <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    #MÉDIA
    if(nrow(colVariaveisBaixoRisco) != 0) {
      mediaBaixo <- colMeans(colVariaveisBaixoRisco)
    } else {
      mediaBaixo <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    if(nrow(colVariaveisAltoRisco) != 0) {
      mediaAlto <- colMeans(colVariaveisAltoRisco)
    } else {
      mediaAlto <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    #MAX
    if(nrow(colVariaveisBaixoRisco) != 0) {
      maxBaixo <- sapply(colVariaveisBaixoRisco, max)
    } else {
      maxBaixo <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    if(nrow(colVariaveisAltoRisco) != 0) {
      maxAlto <- sapply(colVariaveisAltoRisco, max)
    } else {
      maxAlto <- rep(0, each = ncol(colVariaveisBaixoRisco))
    }
    
    data.frame(
      Indicador = c(
        c(1:ncol(colVariaveisBaixoRisco)), 
        c(1:ncol(colVariaveisBaixoRisco))
      ),
      Min = c(
        minBaixo,
        minAlto
      ),
      Média = c(
        mediaBaixo,
        mediaAlto
      ),
      Max = c(
        maxBaixo,
        maxAlto
      ),
      RiscoEvasao = c(
        rep("Alto risco", each = ncol(colVariaveisAltoRisco)),
        rep("Baixo risco", each = ncol(colVariaveisBaixoRisco))
      )
    )
  } else {
    NULL
  }
}

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

#Calculo de correção para construtos de evasão não selecionados

calcConstrutosEvasao <- function(linhas, construtos) {
  if(!is.null(construtos)) {
    totaldiff <- 0
    diff <- c()
    ultimoanterior <- 0
    for(i in construtos) {
      totaldiff <- totaldiff + (DFconstrutosEvasao[i,]$Varinicial - ultimoanterior - 1)
      ultimoanterior <- DFconstrutosEvasao[i,]$Varfinal
      diff <- c(diff, rep(totaldiff, each = (DFconstrutosEvasao[i,]$Varfinal - DFconstrutosEvasao[i,]$Varinicial + 1)))
    }
    return(diff[linhas])
  } else {
    return(0)
  }
}

shinyServer(function(input, output) {
  #Radio da aplicação
  output$radioApp <- renderUI({
    radioButtons("aplicacao","Seleciona a aplicação:", 
                 choices =c("Visão geral dos dados" = 1,
                            "Análise de Desempenho" = 2,
                            "Análise de Evasão" = 3),selected = 1);
  })
  
  #Base em função do radio
  baseInputs <- reactive({
    if(!is.null(input$aplicacao)) {
      if(input$aplicacao == 1) {
        baseGeral
      } else if(input$aplicacao == 2) {
        baseDesempenho
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
    selectInput("disciplina","Escolha a Disciplina:", sort(as.character(unique(disciplinas$Disciplina))))
  })
  
  
  ##Base de acordo com os parametros escolhidos e de acordo com a base de dados
  baseFiltrada <- reactive({
    if(!is.null(input$aplicacao) && !is.null(input$curso) && input$curso != "" && !is.null(input$periodo) && input$periodo != "" && !is.null(input$disciplina) && input$disciplina != "") {
      if(input$aplicacao == 1){
        filter(baseGeral, Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
      }else if(input$aplicacao == 2){
        filter(baseDesempenho, Curso == input$curso, Periodo == input$periodo, Disciplina == input$disciplina)
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
    listaVariaveis <- data.frame(listaVariaveisGeral$Descrição.sobre.as.variáveis)
    listaVariaveis["N"] <- c(1:nrow(listaVariaveis)) 
    colnames(listaVariaveis) <- c("Descrição","Nº")
    listaVariaveis <- data.frame(listaVariaveis[,c("Nº", "Descrição")])
    if(input$tabGeral == "1" || input$tabGeral == "3" ){
      DT::datatable(
        listaVariaveis,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = '300px'
        )
      )
    }else{
      DT::datatable(
        listaVariaveis,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = '300px'
        ),
        selection = list(target = 'row',mode="single",selected=c(1))
      )
    }
  })
  
  #retorna tabela alunos gerais
  output$alunosGeral <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 1 && !is.null(baseFiltrada())) {
      listaAlunos <- select(baseFiltrada(), Aluno);
      colnames(listaAlunos) <- c("Nome")
    } else {
      listaAlunos <- NULL
    }
    if(input$tabGeral == "1"){
      DT::datatable(
        listaAlunos, 
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = '300px'
        )
      )
    }else if(input$tabGeral == "3"){
      DT::datatable(
        listaAlunos, 
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = '300px'
        ),
        selection = list(target = 'row',mode="single",selected=c(1))
      )
    }else{
      variaveis <- as.character(listaVariaveisGeral$Variável)
      varSelected <- variaveis[input$indicadoresGeral_rows_selected]
      base <- baseFiltrada()
      if(length(varSelected) == 0 || is.null(base)) {
        listaAlunos <- NULL
      } else{
        listaAlunos <- base[,c("Aluno",varSelected)] 
        colnames(listaAlunos) <- c("Nome","Valor")
    }
      DT::datatable(
        listaAlunos, 
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = '300px'
        )
      )
    }
  })
  
  #Gráfico "geral" da visão geral dos dados
  
  output$graficoGeral <- renderChart2({
    indSelecionados <- input$indicadoresGeral_rows_selected
    if(is.null(indSelecionados)) {
      indSelecionados <- c(1:nrow(listaVariaveisGeral))
    }
    indSelecionados <- sort(indSelecionados)
    if(!is.null(input$aplicacao) && input$aplicacao == 1) {
      base <- visGeralIndicadores(baseFiltrada())
    } else {
      base <- NULL
    }
    if(!is.null(base) && input$aplicacao == 1) {
      g <- nPlot(Média ~ Indicador, data = base[indSelecionados,], title = "Média dos indicadores geral", type = 'multiBarHorizontalChart', width = 600)
      g$chart(showControls = F)
      g
    } else {
      nPlot(a ~ b, data = data.frame(a = c(0), b = c(0)), type = 'multiBarHorizontalChart', width = 600)
    }
  })
  
  #Gráfico "indicadores" da visão geral dos dados
  
  output$graficoGeralIndicadores <- renderChart2({
    if(!is.null(input$aplicacao) && input$aplicacao == 1) {
      base <- visGeralIndicadores(baseFiltrada())
    } else {
      base <- NULL
    }
    indicador <- input$indicadoresGeral_rows_selected
    
    baseFil <- baseFiltrada()
    if(!is.null(indicador) && indicador != 0 && !is.null(input$aplicacao) && input$aplicacao == 1 && !is.null(base) && !is.null(base)) {
      listaAlunos <- select(baseFil, Aluno, one_of(as.character(listaVariaveisGeral[indicador,]$Variável)))
      colnames(listaAlunos) <- c("Nome", "Valor")
      listaAlunos["Aluno"] <- c(1:nrow(baseFil))
      names(listaAlunos$Nome) <- rep("nome", each = nrow(listaAlunos))
      
      min <- base[indicador,]$Min
      media <- round(base[indicador,]$Média, 1)
      max <- base[indicador,]$Max
      hit <- paste("#! function(){return 'Aluno: ' + this.point.nome + '<br />Valor: ' + this.point.y + '<br />Min: ", min, "<br />Média: ", media, "<br />Max: ", max, "';}!#", sep = "")
      
      descricao <- as.character(listaVariaveisGeral[indicador,]$Descrição.sobre.as.variáveis)
      h <- hPlot(Valor ~ Aluno, data = listaAlunos, type = "bubble", title = descricao, size = 1)
      h$tooltip(borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE, formatter = hit, width = 600)
      h$chart(zoomType = "xy");
      h
    } else {
      hPlot(b ~ a, data = data.frame(a = c(0), b = c(0)), type = "bubble", title = "", size = 1)
    }
  })
  
  #Grafico "Alunos" da visao geral dos dados
  output$graficoGeralAlunos <- plotly::renderPlotly({
    alunos <- sort(as.character(baseFiltrada()$Aluno))
    alunoSelect <- alunos[input$alunosGeral_rows_selected]
    aluno <- as.double(select(filter(baseFiltrada(), Aluno == alunoSelect),one_of(as.character(listaVariaveisGeral$Variável))))
    mediaGeral <- as.double(colMeans(select(baseFiltrada(), one_of(as.character(listaVariaveisGeral$Variável)))))
    #cria um data.frame com os indicadores, a freq do aluno e a media geral do indicador
    listaVariaveisGeral["N"] <- c(1:nrow(listaVariaveisGeral))
    dataGeral <- data.frame(listaVariaveisGeral$N,aluno,mediaGeral,listaVariaveisGeral$Descrição.sobre.as.variáveis)
    colnames(dataGeral) <- c("Var","Freq_Aluno","Media_Turma","Descricao")
    dataGeral <- dataGeral[with(dataGeral, order(Var)), ]
    g <- gather(dataGeral, var, value, Freq_Aluno, Media_Turma) %>%
      plot_ly(x = value, y = Var,mode = "markers",
              color = var, colors = c("pink", "blue")) %>%
      add_trace(x = value, y = Var,mode = "lines",
                group = Var, showlegend = F, line = list(color = "gray")) %>%
      layout(
        title = paste("Aluno:", alunoSelect),
        xaxis = list(title = "Frequencia por Indicador"),
        yaxis = list(title = "Indicador")
      )
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
    listaVariaveis["N"] <- c(1:nrow(listaVariaveis))
    colnames(listaVariaveis) <- c("Descrição", "Construto","Nº")
    listaVariaveis <- data.frame(listaVariaveis[,c("Nº","Descrição", "Construto")])    
    if(!is.null(construtosCheckBox)) {
      filtroVariaveis <- c()
      for(i in construtosCheckBox) {
        filtroVariaveis <- c(filtroVariaveis, DFconstrutosDesempenho[i,]$Varinicial:DFconstrutosDesempenho[i,]$Varfinal)
      }
    } else {
      filtroVariaveis <- c(1:nrow(listaVariaveis))
    }
    
    if(input$tabDesempenho == "1"){
      DT::datatable(
        listaVariaveis[filtroVariaveis,],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE, scrollY = '300px',
          scrollX = '300px'),
        class = "compact",
        selection = list(target = 'row', mode="multiple")
      ) 
    }else{
      DT::datatable(
        listaVariaveis[filtroVariaveis,],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE, scrollY = '300px',
          scrollX = '300px'),
        class = "compact",
        selection = list(target = 'row', mode="single",selected=c(1))
      )
    }
  })
  
  #retorna tabela alunos Desempenho
  output$alunosDesempenho <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      listaAlunosDese <- data.frame(baseFiltrada()[,c("Aluno","DESEMPENHO_BINARIO")])
      listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
      listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório";
      colnames(listaAlunosDese) <- c("Nome", "Desempenho")
      listaAlunosDese <- listaAlunosDese[with(listaAlunosDese, order(Nome)), ]
      rownames(listaAlunosDese) <- NULL
    } else {
      listaAlunosDese <- NULL
    }
    if(input$tabDesempenho == "1"){
      DT::datatable(
        listaAlunosDese,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact"
      ) 
    }else if(input$tabDesempenho == "3"){
      variaveis <- as.character(dicionarioBaseDesempenho$Variável)
      varSelected <- variaveis[input$indicadoresDesempenho_rows_selected]
      if(length(varSelected) == 0){
        listaAlunosDese <- NULL
      }else{
        listaAlunosDese <- baseFiltrada()[,c("Aluno",varSelected,"DESEMPENHO_BINARIO")]
        listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
        listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório"
        colnames(listaAlunosDese) <- c("Nome","Valor","Desempenho")
        listaAlunosDese <- listaAlunosDese[with(listaAlunosDese, order(Nome)), ]
      }
      DT::datatable(
        listaAlunosDese,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact",
        selection = list(target = 'row', mode="single",selected=c(1))
      )
    }else{
      variaveis <- as.character(dicionarioBaseDesempenho$Variável)
      varSelected <- variaveis[input$indicadoresDesempenho_rows_selected]
      if(length(varSelected) == 0 || input$aplicacao != 2){
        listaAlunosDese <- NULL
      }else{
        listaAlunosDese <- baseFiltrada()[,c("Aluno",varSelected,"DESEMPENHO_BINARIO")]
        listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 0] <- "Satisfatório"
        listaAlunosDese$DESEMPENHO_BINARIO[listaAlunosDese$DESEMPENHO_BINARIO == 1] <- "Insatisfatório"
        colnames(listaAlunosDese) <- c("Nome","Valor","Desempenho")
        listaAlunosDese <- listaAlunosDese[with(listaAlunosDese, order(Nome)), ]
      }
      DT::datatable(
        listaAlunosDese,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact"
      )
    }
  })
  
  #infoBoxes de desempenho
  
  #BoxDesempenho satisfatório
  output$SatisfatorioBox <- renderValueBox({
    desempenhoSatisfatorio <- 0
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      classesSat <- table(baseFiltrada()$DESEMPENHO_BINARIO)
      Sat <- if (!is.na(classesSat["0"])) classesSat["0"] else 0
      
      desempenhoSatisfatorio <-round((Sat / nrow(baseFiltrada())) * 100, 2)
      if(is.na(desempenhoSatisfatorio)) {
        desempenhoSatisfatorio <- 0
      }
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
      Insat <- if (!is.na(classesSat["1"])) classesSat["1"] else 0
      
      desempenhoInsatisfatorio <-round((Insat / nrow(baseFiltrada())) * 100, 2)
      if(is.na(desempenhoInsatisfatorio)) {
        desempenhoInsatisfatorio <- 0
      }
    }
    valueBox(
      paste0(desempenhoInsatisfatorio, "%"), "Insatisfatório", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #Gráfico geral de desempenho
  output$graficoDesempenhoGeral <- renderChart2({
    indSelecionados <- input$indicadoresDesempenho_rows_selected
    
    if(!is.null(indSelecionados)) {
      indSelecionados <- indSelecionados + calcConstrutosDesempenho(indSelecionados, INcheckboxesDesempenho())
    }
    
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
    indSelecionados <- sort(indSelecionados)
    if(!is.null(input$aplicacao) && input$aplicacao == 2) {
      base <- dadosDesempenho(baseFiltrada())
    } else {
      base <- NULL
    }
    if(!is.null(base)) {
      g <- nPlot(Média ~ Indicador, data = base[indSelecionados,], group = 'Desempenho', type = 'multiBarHorizontalChart', width = 600)
      g$chart(color = c('green', 'red'))
      g
    } else {
      nPlot(a ~ b, data = data.frame(a = c(0), b = c(0)), type = 'multiBarHorizontalChart', width = 600)
    }
  })
  
  #Gráfico de indicadores de desempenho
  output$graficoDesempenhoInd <- renderChart2({
    indicador <- input$indicadoresDesempenho_rows_selected
    base <- baseFiltrada()
    if(!is.null(indicador) && indicador != 0 && !is.null(base) && input$aplicacao == 2) {
      titulo <- as.character(dicionarioBaseDesempenho[indicador,]$Descrição.sobre.as.variáveis)
      subtitulo <- as.character(dicionarioBaseDesempenho[indicador,]$Construto)
      
      var <- as.character(dicionarioBaseDesempenho[indicador,]$Variável)
      alunos <- select(base, one_of(as.character(c("Aluno", var, "PROBABILIDADE", "DESEMPENHO_BINARIO"))))
      colnames(alunos) <- c("Nome", "Valor", "Probabilidade", "Desempenho")
      if(nrow(alunos) != 0) {
        alunos["Aluno"] <- c(1:nrow(alunos)) #tratamento de erro, if para não contar de 1 à 0 (coluna de 2 elementos) e inserir em um dataframe com 0 linhas
      } else {
        alunos <- data.frame(Nome = character(), Valor = integer(), Probabilidade = double(), Desempenho = double(), Aluno = integer())
      }
      
      alunos$Desempenho[alunos$Desempenho == "0"] <- "Satistatório"
      alunos$Desempenho[alunos$Desempenho == "1"] <- "Insatisfatório"
      
      h <- hPlot(Valor ~ Aluno, data = alunos, type = "bubble", title = titulo, subtitle = subtitulo, group = "Desempenho", size = "Probabilidade")
      h$colors('rgba(223, 63, 63, .5)', 'rgba(60, 199, 113,.5)')
      h$chart(zoomType = "xy")
      h$params$width <- 600
      h
    } else {
      hPlot(b ~ a, data = data.frame(a = c(0), b = c(0)), type = "bubble", title = "", size = 1)
    }
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
  
  #Grafico "indicadores" da analise de desempenho
  output$graficoDesempenhoIndicadores <- renderChart2({
    indicadorDesempenho <- input$indicadoresDesempenho_rows_selected
    if(!is.null(indicadorDesempenho) && indicadorDesempenho != 0 && !is.null(input$aplicacao) && input$aplicacao == 1 && !is.null(baseFiltrada())) {
      listaAlunoDese <- select(baseFiltrada(), Aluno, one_of(as.character(listaVariaveisGeral[indicadorDesempenho,]$Variável)))
      listaAlunoDese['Aluno'] <- c(1:nrow(baseFiltrada()))
      colnames(listaAlunos) <- c("Nome", "Valor")
      descricao <- as.character(listaVariaveisGeral[indicador,]$Descrição.sobre.as.variáveis)
      g <- hPlot(Aluno ~ VAR01, data = listaAlunoDese, type = "bubble", title = descricao,size = "PROBABILIDADE", group = "EVASAO")
      g$colors('green', 'red')
    } else {
      hPlot(b ~ a, data = data.frame(a = c(0), b = c(0)), type = "bubble", title = "", size = 1)
    }
  })
  
  #Grafico "Alunos" da analise de desempenho
  output$graficoDesempenhoAlunos <- plotly::renderPlotly({
    alunos <- sort(as.character(baseFiltrada()$Aluno))
    alunoSelect <- alunos[input$alunosDesempenho_rows_selected]
    aluno <- as.double(select(filter(baseFiltrada(), Aluno == alunoSelect),one_of(as.character(listaVariaveisDesempenho$Variável))))
    mediaSat <- as.double(colMeans(select(filter(baseFiltrada(), DESEMPENHO_BINARIO == "0"),one_of(as.character(listaVariaveisDesempenho$Variável)))))
    #cria um data.frame com os indicadores, a freq do aluno e a media geral do indicador
    listaVariaveisDesempenho["N"] <- c(1:nrow(listaVariaveisDesempenho))
    dataDesempenho <- data.frame(listaVariaveisDesempenho$N,mediaSat,aluno,listaVariaveisDesempenho$Descrição.sobre.as.variáveis)
    colnames(dataDesempenho) <- c("Var","Satisfatorio","Freq_Aluno","Descricao")
    dataDesempenho <- dataDesempenho[with(dataDesempenho, order(Var)), ]
    g <- gather(dataDesempenho, var, value, Freq_Aluno, Satisfatorio) %>%
      plot_ly(x = value, y = Var, mode = "markers",
              color = var, colors = c("orange", "green")) %>%
      add_trace(x = value, y = Var, mode = "lines",
                group = Var, showlegend = F, line = list(color = "gray")) %>%
      layout(
        title = paste("Aluno:", alunoSelect),
        xaxis = list(title = "Frequencia por Indicador"),
        yaxis = list(title = "Indicador")
      )
    g
    
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
    listaVariaveis["N"] <- c(1:nrow(listaVariaveis))
    colnames(listaVariaveis) <- c("Descrição","Construto","Nº")
    listaVariaveis <- data.frame(listaVariaveis[,c("Nº","Descrição","Construto")])
    
    construtosCheckBox <- INcheckboxesEvasao()
    
    if(!is.null(construtosCheckBox)) {
      filtroVariaveis <- c()
      for(i in construtosCheckBox) {
        filtroVariaveis <- c(filtroVariaveis, DFconstrutosEvasao[i,]$Varinicial:DFconstrutosEvasao[i,]$Varfinal)
      }
    } else {
      filtroVariaveis <- c(1:nrow(listaVariaveis))
    }
    
    if(input$tabEvasao == "1"){
      DT::datatable(
        listaVariaveis[filtroVariaveis,],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE, 
          scrollY = '300px'
        )
      )  
    }else{
      DT::datatable(
        listaVariaveis[filtroVariaveis,],
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE, 
          scrollY = '300px'
        ),
        selection = list(target = 'row', mode="single",selected=c(1))
      )
    }
    
    
  })
  
  #retorna tabela de alunos evasao
  output$alunosEvasao <- renderDataTable({
    if(!is.null(input$aplicacao) && input$aplicacao == 3 && !is.null(baseFiltrada())) {
      listaAlunosEvasao <- data.frame(baseFiltrada()[,c("Aluno","EVASAO")])
      listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 0] <- "Baixo"
      listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 1] <- "Alto"
      colnames(listaAlunosEvasao) <- c("Nome", "Risco")
    } else {
      listaAlunosEvasao <- NULL
    }
    if(input$tabEvasao == "1"){
      DT::datatable(
        listaAlunosEvasao,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact"
      )
    }else if(input$tabEvasao == "3"){
      variaveis <- as.character(listaVariaveisEvasao$ID)
      varSelected <- variaveis[input$indicadoresEvasao_rows_selected]
      if(length(varSelected) == 0){
        listaAlunosEvasao <- NULL
      }else{
        listaAlunosEvasao <- baseFiltrada()[,c("Aluno",varSelected,"EVASAO")]
        listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 0] <- "Baixo"
        listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 1] <- "Alto"
        colnames(listaAlunosEvasao) <- c("Nome","Valor","Risco")
      }
      DT::datatable(
        listaAlunosEvasao,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact",
        selection = list(target = 'row', mode="single",selected=c(1))
        
      )
    }else{
      variaveis <- as.character(listaVariaveisEvasao$ID)
      varSelected <- variaveis[input$indicadoresEvasao_rows_selected]
      base <- baseFiltrada()
      if(length(varSelected) == 0 || is.null(base) || input$aplicacao != 3){
        listaAlunosEvasao <- NULL
      }else{
        listaAlunosEvasao <- base[,c("Aluno",varSelected,"EVASAO")]
        listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 0] <- "Baixo"
        listaAlunosEvasao$EVASAO[listaAlunosEvasao$EVASAO == 1] <- "Alto"
        colnames(listaAlunosEvasao) <- c("Nome","Valor","Risco")
      }
      DT::datatable(
        listaAlunosEvasao,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          info = FALSE,
          scrollY = '300px'),
        class = "compact"
      )
    }
  })
  
  #infoBoxes de evasão
  
  #BoxEvasao baixo risco
  output$BaixoRiscoBox <- renderValueBox({
    baixoRisco <- 0 
    base <- baseFiltrada()
    if(!is.null(input$aplicacao) && input$aplicacao == 3 && !is.null(base)) {
      classesEvas <- table(base$EVASAO)
      Baixo <- if (!is.na(classesEvas["0"])) classesEvas["0"] else 0
      
      baixoRisco <- round((Baixo / nrow(base)) * 100, 2)
      if(is.na(baixoRisco)) {
        baixoRisco <- 0
      }
    }
    valueBox(
      paste0(baixoRisco, "%"), "Baixo Risco", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  #BoxEvasao alto risco
  output$AltoRiscoBox <- renderValueBox({ 
    altoRisco <- 0
    base <- baseFiltrada()
    if(!is.null(input$aplicacao) && input$aplicacao == 3 && !is.null(base)) {
      classesEvas <- table(base$EVASAO)
      Alto <- if (!is.na(classesEvas["1"])) classesEvas["1"] else 0
      
      altoRisco <- round((Alto / nrow(base)) * 100, 2)
      if(is.na(altoRisco)) {
        altoRisco <- 0
      }
    }
    valueBox(
      paste0(altoRisco, "%"), "Alto Risco", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #Gráfico de evasão
  
  output$graficoEvasao <- renderChart2({
    indSelecionados <- input$indicadoresEvasao_rows_selected
    
    if(!is.null(indSelecionados)) {
      indSelecionados <- indSelecionados + calcConstrutosEvasao(indSelecionados, INcheckboxesEvasao())
    }
    
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
    
    if(!is.null(input$aplicacao) && input$aplicacao == 3) {
      base <- dadosEvasao(baseFiltrada())
    } else {
      base <- NULL
    }
    if(!is.null(base)) {
      indSelecionados <- c(indSelecionados, indSelecionados + nrow(dicionarioBaseEvasao))
      indSelecionados <- sort(indSelecionados)
      g <- nPlot(Média ~ Indicador, data = base[indSelecionados,], group = "RiscoEvasao", type = 'multiBarHorizontalChart', width = 600)
      g$chart(color = c('red', 'green'))
      g
    } else {
      nPlot(a ~ b, data = data.frame(a = c(0), b = c(0)), type = 'multiBarHorizontalChart', width = 600)
    }
  })
  
  #Gráfico de indicadores de evasão
  output$graficoEvasaoInd <- renderChart2({
    indicador <- input$indicadoresEvasao_rows_selected
    base <- baseFiltrada()
    if(!is.null(indicador) && indicador != 0 && !is.null(base) && input$aplicacao == 3) {
      titulo <- as.character(dicionarioBaseEvasao[indicador,]$INDICADOR)
      subtitulo <- as.character(dicionarioBaseEvasao[indicador,]$CONSTRUTOS)
      var <- as.character(dicionarioBaseEvasao[indicador,]$ID)
      alunos <- select(base, one_of(as.character(c("Aluno", var, "PROBABILIDADE", "EVASAO"))))
      colnames(alunos) <- c("Nome", "Valor", "Probabilidade", "Evasao")
      
      if(nrow(alunos) != 0) {
        alunos["Aluno"] <- c(1:nrow(alunos))
      } else {
        alunos <- data.frame(Nome = character(), Valor = integer(), Probabilidade = double(), Desempenho = double(), Aluno = integer())
      }
      
      alunos$Evasao[alunos$Evasao == "0"] <- "Baixo Risco"
      alunos$Evasao[alunos$Evasao == "1"] <- "Alto Risco"
      h <- hPlot(Valor ~ Aluno, data = alunos, type = "bubble", title = titulo, subtitle = subtitulo, group = "Evasao", size = "Probabilidade")
      h$colors('rgba(223, 63, 63, .5)', 'rgba(60, 199, 113,.5)')
      h$chart(zoomType = "xy")
      h$params$width <- 600
      h
    } else {
      hPlot(b ~ a, data = data.frame(a = c(0), b = c(0)), type = "bubble", title = "", size = 1)
    }
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
  
  #Grafico "Alunos" da analise de evasao
  output$graficoEvasaoAlunos <- plotly::renderPlotly({
    alunos <- sort(as.character(baseFiltrada()$Aluno))
    alunoSelect <- alunos[input$alunosEvasao_rows_selected]
    aluno <- as.double(select(filter(baseFiltrada(), Aluno == alunoSelect),one_of(as.character(listaVariaveisEvasao$ID))))
    mediaBaixoRisco <- as.double(colMeans(select(filter(baseFiltrada(), EVASAO == "0"),one_of(as.character(listaVariaveisEvasao$ID)))))
    #cria um data.frame com os indicadores, a freq do aluno e a media geral do indicador
    # no eixo y ta aparecendo o indicador de 2 em 2
    listaVariaveisEvasao["N"] <- c(1:nrow(listaVariaveisEvasao))
    dataEvasao <- data.frame(listaVariaveisEvasao$N,mediaBaixoRisco,aluno,listaVariaveisEvasao$INDICADOR)
    colnames(dataEvasao) <- c("Var","Baixo_Risco","Freq_Aluno","Descricao")
    dataEvasao <- dataEvasao[with(dataEvasao, order(Var)), ]
    g <- gather(dataEvasao, var, value, Freq_Aluno, Baixo_Risco) %>%
      plot_ly(x = value, y = Var, mode = "markers",
              color = var, colors = c("orange", "green")) %>%
      add_trace(x = value, y = Var, mode = "lines",
                group = Var, showlegend = F, line = list(color = "gray")) %>%
      layout(
        title = paste("Aluno:", alunoSelect),
        xaxis = list(title = "Frequencia por Indicador"),
        yaxis = list(title = "Indicador")
      )
    g
  })
  
})