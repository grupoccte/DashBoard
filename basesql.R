require("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "casa")

getBaseGeral <- function() {
  baseGeral <- dbGetQuery(con, "SELECT curso, periodo, disciplina, aluno, var01, var04, var05, var11, var12, var14, var16, var17, var18, var19, var22, var31, var39 from geral")
  colnames(baseGeral) <- c("Curso", "Periodo", "Disciplina", "Aluno", "VAR01", "VAR04", "VAR05", "VAR11", "VAR12", "VAR14", "VAR16", "VAR17", "VAR18", "VAR19", "VAR22", "VAR31", "VAR39")
  return(baseGeral)
}

getDicionarioGeral <- function() {
  dicionarioGeral <- dbGetQuery(con, "SELECT variavel, descricao, construto from dicionario_geral")
  colnames(dicionarioGeral) <- c("Variável", "Descrição", "Construto")
  return(dicionarioGeral)
}

getBaseDesempenho <- function() {
  baseDesempenho <- dbGetQuery(con, "SELECT curso, periodo, disciplina, aluno, var01, var02, var03, var04, var05, var10, var12, var16, var18, var20, var21, var22, var24, var25, var28, var31, var31b, var31c, var32a, var32b, var32c, var33, var34 from desempenho")
  colnames(baseDesempenho) <- c("Curso", "Periodo", "Disciplina", "Aluno", "VAR01", "VAR02", "VAR03", "VAR04", "VAR05", "VAR10", "VAR12", "VAR16", "VAR18", "VAR20", "VAR21", "VAR22", "VAR24", "VAR25", "VAR28", "VAR31", "VAR31b", "VAR31c", "VAR32a", "VAR32b", "VAR32c", "VAR33", "VAR34")
  return(baseDesempenho)
}

getDicionarioDesempenho <- function() {
  dicionarioDesempenho <- dbGetQuery(con, "SELECT variavel, descricao, construto from dicionario_desempenho")
  colnames(dicionarioDesempenho) <- c("Variável", "Descrição", "Construto")
  return(dicionarioDesempenho)
}

getBaseEvasao <- function() {
  baseEvasao <- dbGetQuery(con,"SELECT curso, periodo, disciplina, aluno, var01, var02, var03, var10, var11, var12, var13, var16, var17, var18, var19, var20, var21, var26, var27, var29 from evasao")
  colnames(baseEvasao) <- c("Curso", "Periodo", "Disciplina", "Aluno", "var01", "var02", "var03", "var10", "var11", "var12", "var13", "var16", "var17", "var18", "var19", "var20", "var21", "var26", "var27", "var29")
  return(baseEvasao)
}

getDicionarioEvasao <- function() {
  dicionarioEvasao <- dbGetQuery(con, "SELECT variavel, descricao, construto from dicionario_evasao")
  colnames(dicionarioEvasao) <- c("Variável", "Descrição", "Construto")
  dicionarioEvasao$Variável <- tolower(dicionarioEvasao$Variável)
  return(dicionarioEvasao)
}

fechaConexao <- function() {
  dbDisconnect(con)
  dbUnloadDriver(drv)
}