# =====================================================================
# DASHBOARD ACP INTERACTIF PAR ANNÉE + ACP PAR PROFESSION
# =====================================================================

library(shiny)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(sf)
library(shinydashboard)
library(DT)

# ---------------------------------------------------------------------
# Chargement de la base
# ---------------------------------------------------------------------

database <- readRDS("./datafinal/database.rds")

variables_quanti <- c(
  "effectif_total",
  "densite_moyenne",
  "taux_couv_hosp",
  "ratio_ambu_hosp",
  "diversite_offre"
)

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "ACP Santé - Dashboard Interactif"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Données initiales", tabName = "raw", icon = icon("table")),
      menuItem("Données transformées", tabName = "agg", icon = icon("database")),
      menuItem("ACP Territoires", tabName = "acp", icon = icon("chart-line")),
      menuItem("ACP par profession (Territoires)", tabName = "acp_prof_terr",
               icon = icon("stethoscope"))
      
    ),
    
    selectInput("annee", "Année :", 
                choices = sort(unique(database$annee)), 
                selected = max(database$annee))
    
   
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------------------
      # DONNÉES INITIALES
      # ---------------------------
      tabItem(
        tabName = "raw",
        h2("Données initiales (année filtrée)"),
        DTOutput("table_raw")
      ),
      
      # ---------------------------
      # DONNÉES TRANSFORMÉES
      # ---------------------------
      tabItem(
        tabName = "agg",
        h2("Données transformées (agrégées par département)"),
        DTOutput("table_transformed")
      ),
      
      # ---------------------------
      # ACP TERRITOIRES
      # ---------------------------
      tabItem(
        tabName = "acp",
        h2("ACP : Comparaison des territoires (départements)"),
        fluidRow(
          box(width = 6, plotOutput("eigPlot")),
          box(width = 6, plotOutput("varCorr"))
        ),
        fluidRow(
          box(width = 6, plotOutput("indPlot")),
          box(width = 6, plotOutput("biplot"))
        ),
        fluidRow(
          box(width = 6, plotOutput("contribPC1")),
          box(width = 6, plotOutput("contribPC2"))
        )
      ),
      # ---------------------------
      # ACP PAR PROFESSION (SUR TOUS LES TERRITOIRES)
      # ---------------------------
      tabItem(
        tabName = "acp_prof_terr",
        h2("ACP par profession - Comparaison des territoires"),
        
        fluidRow(
          box(width = 4,
              selectInput("profession_select", "Profession :", 
                          choices = sort(unique(database$profession_sante)),
                          selected = unique(database$profession_sante)[1])
          )
        ),
        
        h3("Données filtrées"),
        DTOutput("table_prof_terr"),
        
        fluidRow(
          box(width = 6, plotOutput("eigProfTerr")),
          box(width = 6, plotOutput("corrProfTerr"))
        ),
        fluidRow(
          box(width = 6, plotOutput("indProfTerr")),
          box(width = 6, plotOutput("biplotProfTerr"))
        ),
        fluidRow(
          box(width = 6, plotOutput("contribProfTerr1")),
          box(width = 6, plotOutput("contribProfTerr2"))
        )
      )
      
    )
  )
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ---------------------------
  # DONNÉES INITIALES
  # ---------------------------
  
  data_raw <- reactive({
    database %>% filter(annee == input$annee)
  })
  
  output$table_raw <- renderDT({
    datatable(
      data_raw() %>% st_drop_geometry(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---------------------------
  # DONNÉES AGRÉGÉES TERRITOIRE
  # ---------------------------
  
  data_dept <- reactive({
    
    database %>%
      filter(annee == input$annee) %>%
      group_by(departement, nom) %>%
      summarise(
        effectif_total = sum(effectif_total, na.rm = TRUE),
        densite_moyenne = mean(densite_moyenne, na.rm = TRUE),
        nb_etablissements = first(nb_etablissements),
        nb_hopitaux = first(nb_hopitaux),
        nb_cliniques = first(nb_cliniques),
        nb_msp = first(nb_msp),
        nb_lva = first(nb_lva),
        taux_couv_hosp = mean(taux_couv_hosp, na.rm = TRUE),
        ratio_ambu_hosp = mean(ratio_ambu_hosp, na.rm = TRUE),
        diversite_offre = mean(diversite_offre, na.rm = TRUE),
        geometry = first(geometry),
        .groups = "drop"
      )
  })
  
  output$table_transformed <- renderDT({
    datatable(
      data_dept() %>% st_drop_geometry(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---------------------------
  # ACP TERRITOIRES
  # ---------------------------
  
  acp_matrix <- reactive({
    
    df <- data_dept() %>% st_drop_geometry()
    
    M <- df %>%
      column_to_rownames("departement") %>%
      select(
        effectif_total, densite_moyenne, nb_etablissements,
        nb_hopitaux, nb_cliniques, nb_msp, nb_lva,
        taux_couv_hosp, ratio_ambu_hosp, diversite_offre
      )
    
    M[is.na(M)] <- 0
    M
  })
  
  res_pca <- reactive({
    PCA(acp_matrix(), scale.unit = TRUE, ncp = 5, graph = FALSE)
  })
  
  output$eigPlot <- renderPlot({
    fviz_eig(res_pca(), addlabels = TRUE)
  })
  
  output$varCorr <- renderPlot({
    fviz_pca_var(res_pca(), col.var = "contrib", repel = TRUE)
  })
  
  output$indPlot <- renderPlot({
    fviz_pca_ind(res_pca(), col.ind = "cos2", repel = TRUE)
  })
  
  output$biplot <- renderPlot({
    fviz_pca_biplot(res_pca(), repel = TRUE)
  })
  
  output$contribPC1 <- renderPlot({
    fviz_contrib(res_pca(), choice="var", axes=1, top=10)
  })
  
  output$contribPC2 <- renderPlot({
    fviz_contrib(res_pca(), choice="var", axes=2, top=10)
  })
  
  # =====================================================================
  #        ACP PAR PROFESSION SUR LES TERRITOIRES — OPTION B
  # =====================================================================
  
  # ---- 1. Données filtrées par profession + année ----
  data_prof_territoires <- reactive({
    database %>%
      filter(annee == input$annee,
             profession_sante == input$profession_select) %>%
      select(departement, profession_sante, all_of(variables_quanti)) %>%
      distinct()
  })
  
  output$table_prof_terr <- renderDT({
    datatable(
      data_prof_territoires(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---- 2. Matrice ACP ----
  acp_prof_terr_matrix <- reactive({
    df <- data_prof_territoires()
    M <- df %>%
      column_to_rownames("departement") %>%
      select(all_of(variables_quanti))
    
    M[is.na(M)] <- 0
    M
  })
  
  # ---- 3. Calcul ACP ----
  res_pca_prof_terr <- reactive({
    PCA(acp_prof_terr_matrix(), scale.unit = TRUE, ncp = 5, graph = FALSE)
  })
  
  # ---- 4. Graphiques ----
  
  output$eigProfTerr <- renderPlot({
    fviz_eig(res_pca_prof_terr(), addlabels = TRUE, ylim = c(0, 40),
             main = paste("Éboulis -", input$profession_select))
  })
  
  output$corrProfTerr <- renderPlot({
    fviz_pca_var(res_pca_prof_terr(),
                 col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE,
                 title = paste("Cercle des corrélations -", input$profession_select))
  })
  
  output$indProfTerr <- renderPlot({
    fviz_pca_ind(res_pca_prof_terr(),
                 col.ind = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE,
                 title = paste("Départements -", input$profession_select))
  })
  
  output$biplotProfTerr <- renderPlot({
    fviz_pca_biplot(res_pca_prof_terr(),
                    repel = TRUE,
                    col.var = "#2E9FDF",
                    col.ind = "#696969",
                    title = paste("Biplot -", input$profession_select))
  })
  
  output$contribProfTerr1 <- renderPlot({
    fviz_contrib(res_pca_prof_terr(),
                 choice = "var", axes = 1, top = 10,
                 title = paste("Contributions à PC1 -", input$profession_select))
  })
  
  output$contribProfTerr2 <- renderPlot({
    fviz_contrib(res_pca_prof_terr(),
                 choice = "var", axes = 2, top = 10,
                 title = paste("Contributions à PC2 -", input$profession_select))
  })
  
}

# ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
