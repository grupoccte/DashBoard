#Instalação do dashboard

##Instalação comum

* Shiny-server (versão 1.4 ou superior é recomendada)

As seguintes bibliotecas são necessárias

* ShinyDashboard
* DT
* dplyr
* stringr
* devtools (para download e instalação do rcharts)
* rCharts
* reshape2
* Plotly

##Instalação com Docker (Linux)

Além do Docker são necessários:

* GIT
* Docker-compose

Tendo isto instalado basta executar o script de instalação:

```
sudo instalacao.sh
```

O script irá baixar o projeto no github, criar uma imagem com o shiny-server e as bibliotecas necessárias executando após a conclusão na porta 80.
Para execuções futuras basta abrir a linha de comando e navegar até a pasta com os arquivos do docker e executar o seguinte comando para subir novamente todos os containers:

```
Docker-compose up -d
```

E para parar todos os serviços:

```
Docker-compose stop
```

