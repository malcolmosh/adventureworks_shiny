library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(ggthemes)
#library(magrittr)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(shinyWidgets)
library(stringr)
library(lubridate)
library(scales)

#for sql connection
library(DBI)
library(odbc)
library(config)

# langue FR
#Sys.setlocale("LC_TIME", "fr_CA")
#Sys.setlocale("LC_ALL","French")
#https://portal.azure.com/?quickstart=true#@hec.ca/resource/subscriptions/83098db9-6d52-4d4b-808a-eab274203759/resourceGroups/resourcegroup/providers/Microsoft.Sql/servers/tiaadventureworks2019/firewall
#https://joetorres531.medium.com/build-an-enterprise-ready-r-shiny-app-with-azure-and-docker-a87c73028d97
#https://www.r-bloggers.com/2018/02/r-azure-sql-server-and-mac-os-x/

### Connexion serveur ----
dw <- get("datawarehouse")

con <- dbConnect(drv = odbc::odbc(),   
                 Driver = dw$driver,    
                 Server = dw$server,    
                 UID    = dw$uid,    
                 PWD    = dw$pwd,    
                 Database = dw$database 
)

DBI::dbExecute(con, "SET QUOTED_IDENTIFIER ON")


### Variables persistantes  --------

#infos générales
infos_gen <- dbGetQuery(con, 'SELECT * FROM dbo.BuildVersion')

#tables de schémas
tables_totales <- dbGetQuery(con, 'SELECT * FROM INFORMATION_SCHEMA.TABLES')

#nombre d'enregistrements à analyser
fractions <- list(1,5,20,40,60,80,100)
fractions_pourc <- list("1%", "5%", "20%","40%", "60%", "80%", "100%")

#liste des tables individuelles - générées par adventureworks
choix_tables= tables_totales %>%
  filter(TABLE_SCHEMA =='SalesLT') %>%
  select(TABLE_NAME) %>%
  pull()

#value box pour créer boîte info sans shiny dashboard

valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(subtitle),
                      div(style = ("font-size: 22px; font-weight: bold;"),
                          textOutput(value)
                      )
                  )
              )
          )
      )
  )
}

# App ui -----
ui <- (navbarPage("Explorateur AdventureWorksLT",
                   
### Tab intro ----
# introduction splash
tabPanel("Intro",
                h1("La base de données en bref"),
         br(),
         br(),
         fluidRow(
           
          
          # nbr d'entreprises incubées
           valueBox(value="boite_tottables",subtitle="Tables totales",icon="cog",color="red"
           ), 
           
          # nbr de candidatures reçues 
           valueBox(value="boitetot_modif",subtitle="Dernière modification",icon="tachometer",color="green"
           ),
          
          # nbr de compagnies qui ont soumis
          valueBox(value="boite_version",subtitle="Version",icon="building",color="blue"
          )
          
          )

),

  # tab filtrer ----
  tabPanel("Profilage",
           
   sidebarPanel(
     pickerInput("table_select", 
                 choices=choix_tables, selected = "", multiple = TRUE, label = "Choisir une table", options = pickerOptions(maxOptions = 1)
     ),
     br(),
     pickerInput("nbr_enregistrements", label = "Choisir un pourcentage d'enregistrements",
                 choices=fractions_pourc, selected=NULL),
     br(),
     materialSwitch(inputId = "afficher", label = "Afficher la table"),
     br(),
     submitButton("GO"),
     width = 2
   ),
  
  ### menus déroulants ----         
  mainPanel(
    tableOutput("infos_table_selection"),
    br(),
    hr(),
    tableOutput("infos_colonnes_selection"),
    br(),
    hr(),
    span(textOutput("texte_affichage"), style="color:red"),
    hr(),
    DTOutput("table_selection")
  )
            
           ),

  
# tab ventes agrégées -----
tabPanel("Rapport des ventes",
         
         sidebarPanel(
           radioGroupButtons(
             inputId = "format_agreg",
             label = "Format de la table", 
             choices = c("Simple", "Détaillée"),
             status = "primary"),
           submitButton("Rafraîchir", icon("refresh")),
           h5("Rapport des ventes en format html"),
           downloadButton("report", "Télécharger"),
           width=2
         ),
         mainPanel(
         
         fluidRow(
           #graphique industrie
           box(
             title="Ventes",
             shinycssloaders::withSpinner(
             plotOutput("plot2"))
           )
           
          #graphique techno
           ,box(
             title="Nombre de ventes par produit",
             shinycssloaders::withSpinner(
             plotOutput("plot1"))
           )
         )
         ,fluidRow(
           br(),
           hr(),
          h3("Tableau des ventes agrégées"),
             shinycssloaders::withSpinner(
               DTOutput("table_agregee"))
         ))
      

),

# tab infos ----
tabPanel("Info",
         p("Preuve de concept construite pour le cours Technologies de l'intelligence d'affaires offert à l'hiver 2022 (TECH60701), enseigné par Mehdi Lahlou."), 
        p("Équipe :"),
         p(
           tags$ul(
             tags$li("Olivier Simard-Hanley"),
             tags$li("Joseph Navar Garcia"),
             tags$li("Walid Khaïli")
           ))
         
)
                                      
# close the UI definition
))

# app server ----
server <- function(input, output) {

  # informations générales sur la table sélectionnée par le menu déroulant
  infos_table_selection_reactive <- reactive ({
    dbGetQuery(con,paste0("EXEC sp_spaceused '[SalesLT].[",input$table_select,"]'"))
  })
  
  # information sur les colonnes de la table sélectionnée

  infos_colonnes_selection_reactive <- reactive ({
    dbGetQuery(con, paste0("SELECT c.name,
             c.max_length,
             c.is_nullable,
             t.name
             FROM sys.columns c
             JOIN sys.types   t
             ON c.user_type_id = t.user_type_id
             WHERE c.object_id    = Object_id('SalesLT.",input$table_select,"')"))
  })
  
  #sortir les colonnes  de la table en format datatable
  output$infos_colonnes_selection<- renderTable({ 
  
    #n'afficher la table que si une sélection est faite dans le menu déroulant des tables
    validate(
      need(input$table_select, "")
    )
    
    infos_colonnes_selection_reactive <- infos_colonnes_selection_reactive()
    
    data.frame(infos_colonnes_selection_reactive) %>% 
      rename(`Colonne` = name, LongueurMax = max_length, `Null_possible` = is_nullable, `Type` = name.1)
    
  })
  
  #sortir les infos générales de la table en format datatable
  output$infos_table_selection<- renderTable({ 
    
    #n'afficher la table que si une sélection est faite dans le menu déroulant des tables
    validate(
      need(input$table_select, "Effectuer une sélection")
    )
    
    infos_table_selection_reactive <- infos_table_selection_reactive()
    
    data.frame(infos_table_selection_reactive) %>% 
    rename(`Nom de la table` = name, Rangées = rows, `Espace réservé` = reserved, `Espace utilisé` = data, `Taille de l'index`=index_size, `Espace inutilisé` = unused)

  })
  
  
  # pourcentage de lignes sélectionné
  pourcentage_enregistrements_reactive <- reactive ({
  
    #aller cherche le nombre de rangées dans la table sélectionnée
    #d'abord, chercher l'équvalent entre le string sélectionné et le nombre associé (10 pour "10%", par ex.)
    index <- match(input$nbr_enregistrements, fractions_pourc)
    index <- fractions[index]
    #déterminer le pourcentage d'enregistrements à sélectionner
    (as.numeric(index) * 0.01)
    
  })

  #nombre de rangées sélectionnées - objet à utiliser
  nombre_rangees <- reactive({ 
    pourcentage_enregistrements_reactive <- pourcentage_enregistrements_reactive()
    infos_table_selection_reactive <- infos_table_selection_reactive()
    
    #chercher le nombre de rangées (string) et le nettoyer, puis convertir en nombre
    nbr_rangees <- infos_table_selection_reactive$rows
    nbr_rangees <- str_replace_all(string=nbr_rangees, pattern=" ", repl="")
    resultat <- as.numeric(nbr_rangees) * pourcentage_enregistrements_reactive
    floor(resultat)
  })
  

  
  #texte à afficher avec le nombre de rangées sélectionnées
  output$texte_affichage<- renderText({ 
    
    #n'afficher le texte que si une sélection est faite dans le menu déroulant des tables
    validate(need(input$table_select, ""))
    
    if (input$afficher==TRUE){
    
    nombre_rangees <- nombre_rangees()
    paste(nombre_rangees, "rangées sélectionnées")
    }
  })
  
  # table SQL sélectionnée selon le menu déroulant et le pourcentage de lignes à afficher 
  table_selection <- reactive ({
    
    nombre_rangees <- nombre_rangees()
    dbGetQuery(con,paste0('
            SELECT TOP ', nombre_rangees,' *
            FROM SalesLT.',input$table_select,';')
    )
  })
  
  # conversion de la table sélectionnée en format datatable
  output$table_selection <- renderDataTable({
    
    #n'afficher la table que si une sélection est faite dans le menu déroulant des tables
    validate(need(input$table_select, ""))
    
    if (input$afficher==TRUE){
    
    table_selection <- table_selection()
    
    datatable(table_selection,
              options = list(paging = TRUE,    ## paginate the output
                             pageLength = 25,  ## number of rows to output for each page
                             scrollX = TRUE,   ## enable scrolling on X axis
                             scrollY = TRUE,   ## enable scrolling on Y axis
                             autoWidth = TRUE, ## use smart column width handling
                             server = FALSE,   ## use client-side processing
                             dom = 'Bfrtip',
                             buttons = c('csv', 'excel')
              ),
              extensions = 'Buttons',
              selection = 'single', ## enable selection of a single row
              rownames = FALSE                ## don't show row numbers/names
    )
    }
  })
  
  #boîte dernière modif de la BD
  output$boitetot_modif<- renderText({ 
    format(as.Date(infos_gen$ModifiedDate, format="%d/%m/%Y"),"%a %d %b %Y")
  })
  
  #boîte nombre de tables dans la BD
  output$boite_tottables<- renderText({ 
    dbGetQuery(con, 'SELECT COUNT(*) FROM sys.tables')[[1]]
    
  })
  
  #boîte version détaillée
  output$boite_version<- renderText({ 
    infos_gen$`Database Version`
  })
  
  # agrégation de la table des ventes
  table_ventes_agreg <- reactive ({
    
    dbGetQuery(con,paste0('
            SELECT TOP 100 SOH.SalesOrderID, SOH.OrderDate, SOH.OnlineOrderFlag, SOH.ShipMethod, SOH.SubTotal,
            SOD.ProductID, SOD.OrderQty, SOD.LineTotal, P.Name, P.ProductNumber
            FROM SalesLT.SalesOrderHeader as SOH
            inner join SalesLT.SalesOrderDetail as SOD on SOH.SalesOrderID=SOD.SalesOrderID
            inner join SalesLT.Product as P on P.ProductID=SOD.ProductID;'))          
  })
  
  # filtre de la table des ventes
  reactive_format_table <- reactive({
    
    table_ventes_agreg <- table_ventes_agreg()
    
    #ajouter des données synthétiques 
    table_ventes_agreg <- 
      table_ventes_agreg %>% 
      add_row(`OrderDate` = as.Date("2008-06-02",format="%Y-%m-%d"), `SubTotal` = 792123.12) %>% 
      add_row(`OrderDate` = as.Date("2008-06-03",format="%Y-%m-%d"), `SubTotal` = 682123.14) 
    
    if (input$format_agreg =="Simple"){
      table_ventes_agreg %>% 
        group_by(`OrderDate`) %>% 
        summarize(SubTotal=sum(SubTotal))
    }
    else
    {table_ventes_agreg}
    
  })
  
  # table agregée en format datatable
  output$table_agregee <- renderDataTable({
    
    reactive_format_table <- reactive_format_table()
  
    
    datatable(reactive_format_table,
              options = list(                             
                buttons = c('csv', 'excel')
              ),
              extensions = 'Buttons'
    )
  })
  
  
  #graphique nbr produits vendus
  output$plot1 <- renderPlot({
    table_ventes_agreg <- table_ventes_agreg()
    
    #plot produits
    table_ventes_agreg %>% 
      group_by(Name) %>% 
      summarize(total=n()) %>% 
      #arrange(desc(percent)) %>% 
      ggplot(aes(y=total,x=reorder(Name,total)))+
      scale_y_continuous(breaks=c(2,4,6,8,10,12))+
      geom_col()+
      geom_label(aes(label=total))+
      coord_flip() +
      labs(title="",x="",y="Total d'unités vendues")+
      theme_gdocs()
    
  })
  
  #graphique ventes par jour
  output$plot2 <- renderPlot({
    reactive_format_table <- reactive_format_table()
    
    if (input$format_agreg =="Simple"){
      reactive_format_table %>% 
        group_by(month = floor_date(`OrderDate`, unit = "month")) %>% 
        summarize(SubTotal=sum(SubTotal)) %>% 
        ggplot(aes(x=month,y=SubTotal))+
        geom_col(aes(fill = factor(month)))+
        scale_y_continuous(labels = scales::comma)+
        scale_x_datetime(labels = date_format("%b"))+
        labs(x="Mois",y="Ventes totales en dollars")+
        geom_label(aes(label=paste(round(SubTotal,2),"$")))+
        guides(fill=FALSE)+
        theme_gdocs()
    }
    else
    {
    reactive_format_table %>% 
      # add_row(`OrderDate` = as.Date("2008-06-02",format="%Y-%m-%d"), `SubTotal` = 792123.12) %>% 
      # add_row(`OrderDate` = as.Date("2008-06-03",format="%Y-%m-%d"), `SubTotal` = 682123.14) %>% 
      group_by(`OrderDate`) %>% 
      summarize(SubTotal=sum(SubTotal)) %>% 
      ggplot(aes(x=OrderDate,y=SubTotal))+
      geom_col(aes(fill = factor(OrderDate)))+
      scale_y_continuous(labels = scales::comma)+
      labs(x="Jour",y="Ventes totales en dollars")+
      geom_label(aes(label=paste(round(SubTotal,2),"$")))+
      guides(fill="none")+
      theme_gdocs()
    }
    
  })
  
  #créer le graphe 1 à exporter manuellement
  graphe_ventes <- 
    dbGetQuery(con,paste0('
            SELECT TOP 100 SOH.SalesOrderID, SOH.OrderDate, SOH.OnlineOrderFlag, SOH.ShipMethod, SOH.SubTotal,
            SOD.ProductID, SOD.OrderQty, SOD.LineTotal, P.Name, P.ProductNumber
            FROM SalesLT.SalesOrderHeader as SOH
            inner join SalesLT.SalesOrderDetail as SOD on SOH.SalesOrderID=SOD.SalesOrderID
            inner join SalesLT.Product as P on P.ProductID=SOD.ProductID;')) %>% 
      #ajouter des données synthétiques 
      add_row(`OrderDate` = as.Date("2008-06-02",format="%Y-%m-%d"), `SubTotal` = 792123.12) %>% 
      add_row(`OrderDate` = as.Date("2008-06-03",format="%Y-%m-%d"), `SubTotal` = 682123.14) %>% 
      group_by(`OrderDate`) %>% 
      summarize(SubTotal=sum(SubTotal)) %>% 
      ggplot(aes(x=OrderDate,y=SubTotal))+
      geom_col(aes(fill = factor(OrderDate)))+
      scale_y_continuous(labels = scales::comma)+
      labs(x="Jour",y="Ventes totales en dollars")+
      geom_label(aes(label=paste(round(SubTotal,2),"$")))+
      guides(fill="none")+
      theme_gdocs()
    
  #créer le graphe 2 à exporter manuellement
  #plot produits
  
  graphe_produits <- 
    dbGetQuery(con,paste0('
            SELECT TOP 100 SOH.SalesOrderID, SOH.OrderDate, SOH.OnlineOrderFlag, SOH.ShipMethod, SOH.SubTotal,
            SOD.ProductID, SOD.OrderQty, SOD.LineTotal, P.Name, P.ProductNumber
            FROM SalesLT.SalesOrderHeader as SOH
            inner join SalesLT.SalesOrderDetail as SOD on SOH.SalesOrderID=SOD.SalesOrderID
            inner join SalesLT.Product as P on P.ProductID=SOD.ProductID;')) %>% 
    group_by(Name) %>% 
    summarize(total=n()) %>% 
    #arrange(desc(percent)) %>% 
    ggplot(aes(y=total,x=reorder(Name,total)))+
    scale_y_continuous(expand=c(0,0), breaks=c(2,4,6,8,10,12))+
    geom_col()+
    geom_label(aes(label=total))+
    coord_flip() +
    labs(title="",x="",y="Total d'unités vendues")+
    theme_gdocs()
  
  
  #section rmarkdown ----
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "adventureworks_report.Rmd")
      file.copy("adventureworks_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(table_selec = input$table_select,
                     datejour = format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%A %d %B %Y"),
                     graphe1 = graphe_ventes,
                     graphe2 = graphe_produits
                     )

      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  

  
}

shinyApp(ui = ui, server = server)



