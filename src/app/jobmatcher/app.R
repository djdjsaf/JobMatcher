#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(shiny)
library(DT)
library(readxl)

# Import du dataframe. Attention, il doit être dans le même répertoire.
df <- read_xlsx("data_jobs.xlsx")
df = df[-c(1)] 

# On crée une colonne score remplie de 0.
score <- rep(0, dim(df)[1])
df <- cbind(df, score)



# On crée des colonnes binaires en fonction de l'apparition de la caractéristique dans la description
df$python <- ifelse(grepl("python", tolower(df$descr)), 1, 0)
df$sql <- ifelse(grepl("sql", tolower(df$descr)), 1, 0)
df$cdi <- ifelse(grepl("cdi", tolower(df$descr)), 1, 0)
df$stage <- ifelse(grepl("stage", tolower(df$intitule)), 1,0)
df$alternance <- ifelse(grepl("alternance", tolower(df$intitule)), 1,0) 
df[,"télétravail"] <- ifelse(grepl("télétravail", tolower(df$descr)), 1, 0)


# Partie UI de l'application
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel('Job Matcher Analyste de données'),
  
  sidebarLayout(
    sidebarPanel(
      # On crée les slider de préférence pour les différentes caractéristiques
      sliderInput('attr1', "Python:",  min = -1, max = 1, value = 0, step = .25),
      sliderInput('attr2', "SQL:",  min = -1, max = 1, value = 0, step = .25),
      sliderInput('attr3', "CDI:",  min = -1, max = 1, value = 0, step = .25),
      sliderInput('attr4', "Alternance:",  min = -1, max = 1, value = 0, step = .25),
      sliderInput('attr5', "Stage:",  min = -1, max = 1, value = 0, step = .25),
      sliderInput('attr6', "Télétravail:",  min = -1, max = 1, value = 0, step = .25),
      textOutput('text')
    ),
    mainPanel(
      # Premier onglet représentant le top6 des jobs
      tabsetPanel(
        tabPanel(
          'Les Jobs qui vous correspondent le plus',
          dataTableOutput('table')
        ),
        tabPanel(
          # Deuxième onglet
          'Disparité score DA/DS',
          plotly::plotlyOutput('plot')
        )
      )
      
    )
  )
  
)



# Partie serveur de l'application
server <- function(input, output) {
  
  output$text <- renderText({
    paste('Choisissez vos préférences !')
  })
  
  # On permet au df d'être editable
  output$data <- DT::renderDT (df, editable = TRUE, server = FALSE)
  
  # On référence l'enemble des attributs à "écouter" pour permettre le changement du df
  toListen <- reactive({
    list(input$attr1,input$attr2,input$attr3,input$attr4,input$attr5,input$attr6)
  })
  
  # Si on observe un changement d'attribut, on crée une nouvelle variable de score
  observeEvent(toListen(), {
    var <- input$attr1*df$python + input$attr2*df$sql + input$attr3*df$cdi + 
      input$attr4*df$alternance + input$attr5*df$stage + input$attr6*df[,"télétravail"]
    var <- 1/(1+exp(x = -var))
    df[, "score"] <<- var
    # On modifie l'output table en conséquence
    output$table <- renderDataTable(head(df[order(-df$score),][, c("entreprise", "note", "intitule", "ville", "score")]))
    
    # Création du graphique de score en fonction du poste
    output$plot <- plotly::renderPlotly({
      df %>%
        na.omit() %>%
        ggplot(aes(y=score, x=poste, fill =poste )) + 
        geom_boxplot() + 
        scale_fill_brewer(palette = "Paired") 
      
      
    })
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
