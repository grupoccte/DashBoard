#Instalação do dashboard

##Instalação comum

* Shiny-server (versão 1.4 ou superior é recomendada)

As seguintes bibliotecas também são necessárias

* ShinyDashboard
* DT
* dplyr
* stringr
* devtools (para download e instalação do rcharts)
* rCharts
* reshape2
* Plotly

Detalhes sobre a instalação do shiny-server podem ser encontrados no [site](https://www.rstudio.com/products/shiny/download-server/) oficial.

Obs: O shiny-server é disponibilizado apenas para o sistema operacional linux.

##Instalação com Docker

Além do Docker também é necessária a instalação do docker-compose em versão superior à 1.7

Salve o seguinte conteudo em um arquivo chamado "docker-compose.yml"

```
version: '2'
services:
  shinyserver:
    image: tafm/dashboard
    command: shiny-server "/srv/shiny-server/shiny-server.conf"
    ports:
      - "80:3838"
```

Agora é necessário navegar até a pasta onde se encontra o arquivo do docker-compose e executar:

```
Docker-compose up -d
```
Caso seja a primeira execução o composer irá baixar a imagem configurada com a aplicação. Concluído o processo acesse http://localhost e a aplicação deverá estar disponível.

Para parar todos os serviços, vá até a pasta e digite o comando:

```
Docker-compose stop
```

