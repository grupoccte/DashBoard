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

getBaseEvasao <- function() {
  baseEvasao <- dbGetQuery(con,"SELECT curso, periodo, disciplina, aluno, var01, var02, var03, var10, var11, var12, var13, var16, var17, var18, var19, var20, var21, var26, var27, var29 from evasao")
  colnames(baseEvasao) <- c("Curso", "Periodo", "Disciplina", "Aluno", "var01", "var02", "var03", "var10", "var11", "var12", "var13", "var16", "var17", "var18", "var19", "var20", "var21", "var26", "var27", "var29")
  return(baseEvasao)
}

getDicionarioEvasao <- function() {
  dicionarioEvasao <- dbGetQuery(con, "SELECT variavel, descricao, construto from dicionario_evasao")
  colnames(dicionarioEvasao) <- c("Variável", "Descrição", "Construto")
  return(dicionarioEvasao)
}

fechaConexao <- function() {
  dbDisconnect(con)
  dbUnloadDriver(drv)
}