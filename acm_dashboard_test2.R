# =====================================================================
# DASHBOARD ACM - AGRÉGÉ PAR DÉPARTEMENT + ACM PAR PROFESSION
# =====================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(DT)
library(sf)

# ---------------------------------------------------------------------
# Chargement des données
# ---------------------------------------------------------------------

database <- readRDS("./datafinal/database.rds")

# Variables qualitatives utilisées dans l'ACM
vars_acm <- c(
  "profession_sante", "densite_classe", "effectif_classe",
  "etabl_classe", "presence_hopital", "presence_clinique",
  "msp_classe", "region_type", "presence_lva",
  "offre_dominante", "niveau_equipement", "densite_pro_classe",
  "profil_infra", "type_territoire"
)

# Petite fonction "mode" pour résumer les variables catégorielles
mode_cat <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# =====================================================================
# UI
# =====================================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard ACM"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("ACM Territoires", tabName = "acm_terr", icon = icon("globe")),
      menuItem("ACM par profession", tabName = "acm_prof", icon = icon("user-md")),
      menuItem("Données", tabName = "data", icon = icon("table"))
    ),
    
    selectInput("annee", "Année :", 
                choices = sort(unique(database$annee)),
                selected = max(database$annee)),
    
    checkboxInput(
      "exclude_outremer",
      "Exclure les DOM-TOM",
      value = TRUE
    )
    
  ),
  
  dashboardBody(
    tabItems(
      
      # ================================================================
      # 1. ACM AGRÉGÉE PAR DÉPARTEMENT
      # ================================================================
      tabItem(
        tabName = "acm_terr",
        h2("ACM agrégée par département"),
        
        fluidRow(
          box(
            width = 4,
            numericInput(
              "top_contrib",
              "Top N modalités contributives :",
              min = 5, max = 50, value = 20, step = 5
            )
          ),
          box(
            width = 4,
            selectInput(
              "famille_vars",
              "Famille de variables à afficher :",
              choices = c(
                "Toutes",
                "Type de territoire",
                "Région",
                "Densité / effectif",
                "Équipement",
                "Présence d’infrastructures",
                "Professions",
                "Offre dominante",
                "Profil d'infrastructure"
              ),
              selected = "Toutes"
            )
          )
        ),
        
        fluidRow(
          box(width = 6, plotOutput("eigPlotTerr")),
          box(width = 6, plotOutput("varPlotTerr"))
        ),
        fluidRow(
          box(width = 6, plotOutput("indPlotTerr")),
          box(width = 6, plotOutput("contribTerrDim1"))
        ),
        fluidRow(
          box(width = 6, plotOutput("contribTerrDim2"))
        )
      ),
      
      # ================================================================
      # 2. ACM PAR PROFESSION
      # ================================================================
      tabItem(
        tabName = "acm_prof",
        h2("ACM par profession"),
        
        fluidRow(
          box(
            width = 4,
            selectInput(
              "profession_select",
              "Profession :",
              choices = sort(unique(database$profession_sante))
            )
          ),
          box(
            width = 4,
            checkboxInput(
              "exclude_outremer",
              "Exclure les Dom-Tom",
              value = TRUE
            )
          )
        ),
        
        
        h3("Données agrégées (départements)"),
        DTOutput("table_prof"),
        
        fluidRow(
          box(width = 6, plotOutput("eigPlotProf")),
          box(width = 6, plotOutput("varPlotProf"))
        ),
        fluidRow(
          box(width = 6, plotOutput("indPlotProf")),
          box(width = 6, plotOutput("contribProfDim1"))
        ),
        fluidRow(
          box(width = 6, plotOutput("contribProfDim2"))
        )
      ),
      
      # ================================================================
      # 3. DONNÉES
      # ================================================================
      tabItem(
        tabName = "data",
        h2("Données filtrées (année sélectionnée)"),
        DTOutput("table_data")
      )
    )
  )
)

# =====================================================================
# SERVER
# =====================================================================

server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  # 1. ACM TERRITOIRES : données agrégées par département
  # -------------------------------------------------------------------
  data_acm_terr <- reactive({
    
    df <- database %>%
      filter(annee == input$annee)
    
    # --- Filtre DOM-TOM global ---
    if (isTRUE(input$exclude_outremer)) {
      df <- df %>% filter(!est_domtom)
    }
    
    df <- df %>%
      group_by(departement) %>%
      summarise(
        across(all_of(vars_acm), mode_cat),
        .groups = "drop"
      )
    
    rownames(df) <- df$departement
    df
  })
  
  
  res_acm_terr <- reactive({
    df <- data_acm_terr()
    # On passe uniquement les variables de l'ACM, en data.frame de base
    mat <- df[, vars_acm, drop = FALSE]
    MCA(mat, graph = FALSE)
  })
  
  
  # Filtrage des modalités selon famille + contribution
  filtrer_modalites <- function(res_mca, famille, topN) {
    
    contrib <- res_mca$var$contrib
    if (nrow(contrib) == 0) return(character(0))
    
    noms <- rownames(contrib)
    # Catégorie = nom de variable avant le dernier "_"
    categories <- sub("_[^_]*$", "", noms)
    
    # Filtre par famille
    if (famille != "Toutes") {
      keep <- switch(
        famille,
        "Type de territoire"         = categories %in% c("type_territoire"),
        "Région"                     = categories %in% c("region_type"),
        "Densité / effectif"         = categories %in% c("densite_classe", "densite_pro_classe", "effectif_classe"),
        "Équipement"                 = categories %in% c("etabl_classe", "msp_classe", "niveau_equipement"),
        "Présence d’infrastructures" = categories %in% c("presence_hopital", "presence_clinique", "presence_lva"),
        "Professions"                = categories %in% c("profession_sante"),
        "Offre dominante"            = categories %in% c("offre_dominante"),
        "Profil d'infrastructure"    = categories %in% c("profil_infra"),
        rep(TRUE, length(categories))   # fallback
      )
      contrib <- contrib[keep, , drop = FALSE]
    }
    
    if (nrow(contrib) == 0) return(character(0))
    
    score <- rowSums(contrib[, 1:2, drop = FALSE])  # contribution Dim1 + Dim2
    n <- min(topN, length(score))
    ord <- order(score, decreasing = TRUE)[1:n]
    rownames(contrib)[ord]
  }
  
  
  # ----- Graphiques ACM Territoires -----
  
  output$eigPlotTerr <- renderPlot({
    fviz_screeplot(res_acm_terr(), addlabels = TRUE) +
      ggtitle(paste("Éboulis des valeurs propres -", input$annee))
  })
  
  output$varPlotTerr <- renderPlot({
    selected <- filtrer_modalites(res_acm_terr(), input$famille_vars, input$top_contrib)
    
    if (length(selected) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune modalité à afficher pour cette famille", cex = 1.2)
      return()
    }
    
    fviz_mca_var(
      res_acm_terr(),
      select.var = list(name = selected),
      col.var = "contrib",
      repel = TRUE
    ) +
      ggtitle(paste(
        "Variables (ACM) -", input$annee,
        "\nFamille :", input$famille_vars,
        "| Top", length(selected)
      ))
  })
  
  output$indPlotTerr <- renderPlot({
    fviz_mca_ind(res_acm_terr(), col.ind = "cos2", repel = TRUE) +
      ggtitle(paste("Départements (ACM) -", input$annee))
  })
  
  output$contribTerrDim1 <- renderPlot({
    fviz_contrib(res_acm_terr(), choice = "var", axes = 1, top = 20) +
      ggtitle(paste("Contributions à l'axe 1 -", input$annee))
  })
  
  output$contribTerrDim2 <- renderPlot({
    fviz_contrib(res_acm_terr(), choice = "var", axes = 2, top = 20) +
      ggtitle(paste("Contributions à l'axe 2 -", input$annee))
  })
  
  # -------------------------------------------------------------------
  # 2. ACM PAR PROFESSION
  # -------------------------------------------------------------------
  
  data_acm_prof <- reactive({
    
    df <- database %>%
      filter(
        annee == input$annee,
        profession_sante == input$profession_select
      )
    
    # --- Filtre DOM-TOM global ---
    if (isTRUE(input$exclude_outremer)) {
      df <- df %>% filter(!est_domtom)
    }
    
    df <- df %>%
      group_by(departement) %>%
      summarise(
        across(all_of(vars_acm), mode_cat),
        .groups = "drop"
      )
    
    rownames(df) <- df$departement
    df
  })
  
  
  
  res_acm_prof <- reactive({
    df <- data_acm_prof()
    if (nrow(df) < 3) return(NULL)
    mat <- df[, vars_acm, drop = FALSE]
    MCA(mat, graph = FALSE)
  })
  
  
  
  
  output$table_prof <- renderDT({
    datatable(
      data_acm_prof(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  
  output$eigPlotProf <- renderPlot({
    req(res_acm_prof())
    fviz_screeplot(res_acm_prof(), addlabels = TRUE) +
      ggtitle(paste("Éboulis -", input$profession_select, "-", input$annee))
  })
  
  output$varPlotProf <- renderPlot({
    req(res_acm_prof())
    # Top 20 toutes familles confondues pour cette profession
    selected <- filtrer_modalites(res_acm_prof(), "Toutes", 20)
    
    if (length(selected) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune modalité à afficher", cex = 1.2)
      return()
    }
    
    fviz_mca_var(
      res_acm_prof(),
      select.var = list(name = selected),
      col.var = "contrib",
      repel = TRUE
    ) +
      ggtitle(paste("Variables (ACM) -", input$profession_select, "-", input$annee))
  })
  
  output$indPlotProf <- renderPlot({
    req(res_acm_prof())
    fviz_mca_ind(res_acm_prof(), col.ind = "cos2", repel = TRUE) +
      ggtitle(paste("Départements -", input$profession_select, "-", input$annee))
  })
  
  output$contribProfDim1 <- renderPlot({
    req(res_acm_prof())
    fviz_contrib(res_acm_prof(), choice = "var", axes = 1, top = 20)
  })
  
  output$contribProfDim2 <- renderPlot({
    req(res_acm_prof())
    fviz_contrib(res_acm_prof(), choice = "var", axes = 2, top = 20)
  })
  
  # -------------------------------------------------------------------
  # 3. DONNÉES
  # -------------------------------------------------------------------
  
  output$table_data <- renderDT({
    
    df <- database %>% filter(annee == input$annee)
    
    # --- Filtre DOM-TOM global ---
    if (isTRUE(input$exclude_outremer)) {
      df <- df %>% filter(!est_domtom)
    }
    
    datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
}

# =====================================================================
shinyApp(ui = ui, server = server)
