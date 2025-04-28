library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

# -----------------------------------------------------------------------------
# Globale Funktionen

# Extrahiere den numerischen Methodencode aus einem String in der Spalte "Methode"
getMethodNumber <- function(methodStr) {
  if (grepl("Ufer", methodStr, ignore.case = TRUE)) {
    return(0)
  }
  # Extrahiere die erste im String vorkommende Zahl
  num <- as.numeric(gsub("([^0-9]*)([0-9]+)(.*)", "\\2", methodStr))
  return(num)
}

# Berechne die mittlere Fließgeschwindigkeit (vm)
# Bei Methode 2 wird – abhängig vom Parameter useKreps – entweder die normale Zweipunkt-Formel oder
# die Kreps-Variante verwendet.
calcMeanFlowVelocity <- function(row, useKreps = FALSE) {
  row <- as.list(row)
  method <- getMethodNumber(row[["Methode"]])
  if(is.na(method) || method == 0) return(NA_real_)
  
  # Methode 2 – Zweipunktmethode: Hier berücksichtigen wir, dass die zweite Messung
  # entweder in der Spalte "0,6 (m/s)" oder falls diese NA ist in "0,8 (m/s)" vorhanden sein kann.
  if(method == 2) {
    v0_2 <- as.numeric(row[["0,2 (m/s)"]])
    v0_6 <- as.numeric(row[["0,6 (m/s)"]])
    if(is.na(v0_6)) {
      v0_6 <- as.numeric(row[["0,8 (m/s)"]])
    }
    return(0.5 * (v0_2 + v0_6))
  } else if(method == 1) {
    return(as.numeric(row[["0,6 (m/s)"]]))
  } else if(method == 3) {
    return(0.25 * (as.numeric(row[["0,2 (m/s)"]]) +
                     2 * as.numeric(row[["0,6 (m/s)"]]) +
                     as.numeric(row[["0,8 (m/s)"]])))
  } else if(method == 5) {
    return(0.1 * (as.numeric(row[["Oberfl. (m/s)"]]) +
                    3 * as.numeric(row[["0,2 (m/s)"]]) +
                    3 * as.numeric(row[["0,6 (m/s)"]]) +
                    2 * as.numeric(row[["0,8 (m/s)"]]) +
                    as.numeric(row[["Sohle (m/s)"]])))
  } else if(method == 6) {
    return(0.1 * (as.numeric(row[["Oberfl. (m/s)"]]) +
                    2 * as.numeric(row[["0,2 (m/s)"]]) +
                    2 * as.numeric(row[["0,4 (m/s)"]]) +
                    2 * as.numeric(row[["0,6 (m/s)"]]) +
                    2 * as.numeric(row[["0,8 (m/s)"]]) +
                    as.numeric(row[["Sohle (m/s)"]])))
  } else {
    return(NA_real_)
  }
}


# Erzeuge einen Default-Datensatz (wird verwendet, falls keine Datei hochgeladen wird)
createDefaultData <- function() {
  df <- data.frame(
    "Lotrechte" = 1:10,
    "Lage (m)" = round(seq(0.5, 4.5, length.out = 10), 2),
    "Methode" = c("linkes Ufer", "1 Punkt", "2 Punkt", "3 Punkt", "3 Punkt", 
                  "3 Punkt", "3 Punkt", "3 Punkt", "3 Punkt", "3 Punkt"),
    "Tiefe (m)" = round(rep(0.25, 10), 3),
    "Oberfl. (m/s)" = rep(0.2, 10),
    "Oberfl. (m):" = rep(0.1, 10),
    "0,2 (m/s)" = round(seq(0.300, 0.390, length.out = 10), 3),
    "0,2 (m):" = round(seq(0.5, 1.4, length.out = 10), 3),
    "0,4 (m/s)" = round(seq(0.350, 0.440, length.out = 10), 3),
    "0,4 (m):" = round(seq(0.8, 1.7, length.out = 10), 3),
    "0,5 (m/s)" = round(seq(0.360, 0.450, length.out = 10), 3),
    "0,5 (m):" = round(seq(1.0, 1.9, length.out = 10), 3),
    "0,6 (m/s)" = round(seq(0.400, 0.490, length.out = 10), 3),
    "0,6 (m):" = round(seq(1.2, 2.1, length.out = 10), 3),
    "0,62 (m/s)" = round(seq(0.410, 0.500, length.out = 10), 3),
    "0,62 (m):" = round(seq(1.3, 2.2, length.out = 10), 3),
    "0,7 (m/s)" = round(seq(0.420, 0.510, length.out = 10), 3),
    "0,7 (m):" = round(seq(1.4, 2.3, length.out = 10), 3),
    "0,8 (m/s)" = round(seq(0.450, 0.540, length.out = 10), 3),
    "0,8 (m):" = round(seq(1.6, 2.5, length.out = 10), 3),
    "0,9 (m/s)" = round(seq(0.470, 0.560, length.out = 10), 3),
    "0,9 (m):" = round(seq(1.8, 2.7, length.out = 10), 3),
    "Sohle (m/s)" = rep(0.1, 10),
    "Sohle (m):" = rep(3.0, 10),
    "mittlere Geschw. (m/s)" = NA,
    "Flaeche (m^2)" = NA,
    "Durchfl. (m^3/s)" = NA,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for(i in 1:nrow(df)) {
    df[i, "mittlere Geschw. (m/s)"] <- calcMeanFlowVelocity(as.list(df[i, ]), useKreps = FALSE)
  }
  return(df)
}

# -----------------------------------------------------------------------------
# Funktion: Standardisiere den eingelesenen Datensatz
# Erzeugt zwei DataFrames:
# 1. df_line: Eine Zeile pro Lotrechte inkl. berechneter vm und der Hilfsspalte "adjustedMethod".
#    Falls useKreps TRUE ist und Methode 2 vorliegt, wird adjustedMethod auf "2_kreps" gesetzt,
#    ansonsten wird der numerische Code (als Zeichen) übernommen.
# 2. df_points: Long-Format-Datensatz für Tiefen- und Geschwindigkeitswerte.
standardizeDataset <- function(df_original, useKreps = FALSE) {
  new_names <- c(
    "Lage (m)" = "Lage..m.",
    "Tiefe (m)" = "Tiefe..m.",
    "Eisdicke (m)" = "Eisdicke..m.",
    "Referenzwasserstand (m)" = "Referenzwasserstand..m.",
    "Randfakt" = "Randfakt.",
    "Oberfl. (m/s)" = "Oberfl...m.s.",
    "Oberfl. (m):" = "Oberfl...m..",
    "0,2 (m/s)" = "X0.2..m.s.",
    "0,2 (m):" = "X0.2..m..",
    "0,4 (m/s)" = "X0.4..m.s.",
    "0,4 (m):" = "X0.4..m..",
    "0,5 (m/s)" = "X0.5..m.s.",
    "0,5 (m):" = "X0.5..m..",
    "0,6 (m/s)" = "X0.6..m.s.",
    "0,6 (m):" = "X0.6..m..",
    "0,62 (m/s)" = "X0.62..m.s.",
    "0,62 (m):" = "X0.62..m..",
    "0,7 (m/s)" = "X0.7..m.s.",
    "0,7 (m):" = "X0.7..m..",
    "0,8 (m/s)" = "X0.8..m.s.",
    "0,8 (m):" = "X0.8..m..",
    "0,9 (m/s)" = "X0.9..m.s.",
    "0,9 (m):" = "X0.9..m..",
    "Sohle (m/s)" = "Sohle..m.s.",
    "Sohle (m):" = "Sohle..m..",
    "mittlere Geschw. (m/s)" = "mittlere.Geschw...m.s.",
    "Flaeche (m^2)" = "Flaeche..m.2.",
    "Durchfl. (m^3/s)" = "Durchfl...m.3.s."
  )
  for(newName in names(new_names)) {
    oldName <- new_names[[newName]]
    if (oldName %in% colnames(df_original)) {
      df_original <- df_original %>% rename(!!newName := all_of(oldName))
    }
  }
  
  df_line <- df_original %>% 
    select(any_of(c("Lotrechte", "Lage (m)", "Tiefe (m)",
                    "Oberfl. (m/s)", "0,2 (m/s)", "0,4 (m/s)", "0,5 (m/s)", "0,6 (m/s)",
                    "0,62 (m/s)", "0,7 (m/s)", "0,8 (m/s)", "0,9 (m/s)",
                    "Sohle (m/s)", "mittlere Geschw. (m/s)", "Flaeche (m^2)",
                    "Durchfl. (m^3/s)", "Methode", "adjustedMethod", "vm")))
  
  # Erzeuge die Spalte "Method" als numerischen Code basierend auf "Methode"
  df_line <- df_line %>% mutate(Method = sapply(Methode, getMethodNumber))
  
  # Erzeuge adjustedMethod und berechne vm
  df_line <- df_line %>%
    mutate(adjustedMethod = if_else(useKreps & Method == 2, "2_kreps", as.character(Method))) %>%
    rowwise() %>%
    mutate(vm = calcMeanFlowVelocity(as.list(cur_data()), useKreps = useKreps)) %>% 
    ungroup()
  
  depthCols <- c("Oberfl. (m):", "0,2 (m):", "0,4 (m):", "0,5 (m):", "0,6 (m):",
                 "0,62 (m):", "0,7 (m):", "0,8 (m):", "0,9 (m):", "Sohle (m):")
  speedCols <- c("Oberfl. (m/s)", "0,2 (m/s)", "0,4 (m/s)", "0,5 (m/s)", "0,6 (m/s)",
                 "0,62 (m/s)", "0,7 (m/s)", "0,8 (m/s)", "0,9 (m/s)", "Sohle (m/s)")
  
  for(col in depthCols) {
    if(!(col %in% names(df_original))) df_original[[col]] <- NA
  }
  for(col in speedCols) {
    if(!(col %in% names(df_original))) df_original[[col]] <- NA
  }
  
  for(col in depthCols) {
    df_original[[col]] <- ifelse(df_original[[col]] == 0, NA, df_original[[col]])
  }
  
  df_depth <- df_original %>% pivot_longer(
    cols = all_of(depthCols),
    names_to = "DepthVar",
    values_to = "x_m"
  ) %>% mutate(Messpunkt = case_when(
    DepthVar == "Oberfl. (m):" ~ "Oberfl.",
    DepthVar == "Sohle (m):" ~ "Sohle",
    TRUE ~ gsub(" \\(m\\):$", "", DepthVar)
  ))
  
  df_speed <- df_original %>% pivot_longer(
    cols = all_of(speedCols),
    names_to = "SpeedVar",
    values_to = "v_ms"
  ) %>% mutate(Messpunkt = case_when(
    SpeedVar == "Oberfl. (m/s)" ~ "Oberfl.",
    SpeedVar == "Sohle (m/s)" ~ "Sohle",
    TRUE ~ gsub(" \\(m/s\\)$", "", SpeedVar)
  ))
  
  df_points <- left_join(df_depth, df_speed, by = c("Lotrechte", "Messpunkt", "Lage (m)", "Tiefe (m)"))
  df_points <- df_points %>% filter(!is.na(x_m) & x_m != 0)
  
  return(list(df_line = df_line, df_points = df_points))
}


my_theme <- bs_theme(
  version = 5,
  primary = "#B0C4DE",
  base_font = font_google("Roboto")
)



# -----------------------------------------------------------------------------
# UI-Definition
ui <- page_sidebar(
  theme = my_theme,
  # title = "Shiny-Anwendung zur Bearbeitung von Durchflussmessungen",
  sidebar = sidebar(
    width = "385px",
    navset_card_underline(
      title = "MENÜ",
      accordion(
        accordion_panel(
          title = "Daten Upload",
          card(
            card_header("Datei hochladen"),
            card_body(
              fileInput("file", label = "Lade .tsv Datei", accept = ".tsv"),
              checkboxInput("zweipunkt_kreps", "Für Zweipunkt-Methode Kreps-Berechnung verwenden", value = FALSE),
              selectInput("flussVerfahren", "Durchflussverfahren wählen:",
                          choices = c("Querschnittsmitte" = "mitte", "Mittlerer Querschnitt" = "mittel"),
                          selected = "mitte")
            )
          )
        ),
        accordion_panel(
          title = "Messung bearbeiten",
          card(
            card_header("Lotrechten entfernen"),
            card_body(
              helpText("Klicke auf einen Punkt im Plot oder nutze die Buttons, um Lotrechten zu entfernen."),
              actionButton("remove_every_2", "Jede 2. Lotrechte entfernen", class = "btn btn-sm"),
              actionButton("remove_every_3", "Jede 3. Lotrechte entfernen", class = "btn btn-sm"),
              actionButton("remove_every_4", "Jede 4. Lotrechte entfernen", class = "btn btn-sm"),
              numericInput("remove_interval", "Entfernungsintervall festlegen:", value = 2, min = 2, step = 1),
              actionButton("remove_custom", "Benutzerdefinierte Lotrechten entfernen", class = "btn btn-sm"),
              checkboxInput("remove_speed_only", "Peeilung bei Lotrechte entfernen", value = FALSE),
              checkboxInput("remove_individual", "Einzelne Messpunkte entfernen", value = FALSE),
              actionButton("reset", "Reset Lotrechten"),
              verbatimTextOutput("removed_Lot")
            )
          )
        ),
        accordion_panel(
          title = "Werte anzeigen",
          card(
            card_header("Durchfluss Q"),
            card_body(
              tableOutput("durchfluss")
            )
          ),
          card(
            card_header("Querschnittsfläche A"),
            card_body(
              tableOutput("querschnittsflaeche")
            )
          ),
          card(
            card_header("Werte in Vergleichsplot hinzufügen"),
            card_body(
              actionButton("add_values", "Werte hinzufügen"),
              actionButton("reset_values", "Reset Vergleichsplot")
            )
          )
        )
      )
    )
  ),
  div(style = "display: flex; align-items: center; gap: 20px; padding: 10px;",
      tags$img(src = "hydroedit.png", height = "95px", style = "margin-right: 10px;"),
      tags$h3("HydroEdit - Bearbeitung von Durchflussmessungen", style = "margin: 10; color: #2c3e50")
      ),
  navset_card_underline(
    title = "ANZEIGE",
    nav_panel("Plot Querschnitt", 
              plotlyOutput("plotOutput_querschnitt"),
              card_body(downloadButton("download_querschnittsplot", "Download PNG"))
    ),
    nav_panel("Data Querschnitt", 
              tableOutput("DataTable"),
              card_body(downloadButton("download_data_querschnitt", "Download CSV"))
    ),
    nav_panel("Plot Vergleich", 
              plotlyOutput("plotOutput_vergleich"),
              card_body(downloadButton("download_vergleichsplot", "Download PNG"))
    ),
    nav_panel("Data Vergleich", 
              tableOutput("DataTableVergleich"),
              card_body(downloadButton("download_data_vergleich", "Download CSV"))
    ),
    nav_panel("Metadaten", 
              tableOutput("Metadaten"))
  )
)

# -----------------------------------------------------------------------------
# Server-Definition
server <- function(input, output, session) {
  
  # Reaktive Werte
  original_data <- reactiveVal()
  active_data_line <- reactiveVal()
  active_data_points <- reactiveVal()
  meta_data <- reactiveVal()
  pegel_info <- reactiveVal()
  original_data_vergleich <- reactiveVal()
  active_data_vergleich <- reactiveVal()
  rv_removed_Lot <- reactiveVal(integer(0))
  
  # Aktualisiere Standardisierung, wenn sich der Kreps‑Modus ändert
  observeEvent(input$zweipunkt_kreps, {
    req(original_data())
    standard <- standardizeDataset(original_data(), useKreps = input$zweipunkt_kreps)
    active_data_line(standard$df_line)
    active_data_points(standard$df_points)
  })
  
  # Datei einlesen
  observeEvent(input$file, {
    req(input$file)
    df_raw <- read.table(input$file$datapath, skip = 29, header = TRUE,
                         sep = "\t", dec = ",", stringsAsFactors = FALSE)
    df_raw <- df_raw %>% select(-X)
    new_names <- c(
      "Lage (m)" = "Lage..m.",
      "Tiefe (m)" = "Tiefe..m.",
      "Eisdicke (m)" = "Eisdicke..m.",
      "Referenzwasserstand (m)" = "Referenzwasserstand..m.",
      "Randfakt" = "Randfakt.",
      "Oberfl. (m/s)" = "Oberfl...m.s.",
      "Oberfl. (m):" = "Oberfl...m..",
      "0,2 (m/s)" = "X0.2..m.s.",
      "0,2 (m):" = "X0.2..m..",
      "0,4 (m/s)" = "X0.4..m.s.",
      "0,4 (m):" = "X0.4..m..",
      "0,5 (m/s)" = "X0.5..m.s.",
      "0,5 (m):" = "X0.5..m..",
      "0,6 (m/s)" = "X0.6..m.s.",
      "0,6 (m):" = "X0.6..m..",
      "0,62 (m/s)" = "X0.62..m.s.",
      "0,62 (m):" = "X0.62..m..",
      "0,7 (m/s)" = "X0.7..m.s.",
      "0,7 (m):" = "X0.7..m..",
      "0,8 (m/s)" = "X0.8..m.s.",
      "0,8 (m):" = "X0.8..m..",
      "0,9 (m/s)" = "X0.9..m.s.",
      "0,9 (m):" = "X0.9..m..",
      "Sohle (m/s)" = "Sohle..m.s.",
      "Sohle (m):" = "Sohle..m..",
      "mittlere Geschw. (m/s)" = "mittlere.Geschw...m.s.",
      "Flaeche (m^2)" = "Flaeche..m.2.",
      "Durchfl. (m^3/s)" = "Durchfl...m.3.s."
    )
    for(newName in names(new_names)) {
      oldName <- new_names[[newName]]
      if(oldName %in% colnames(df_raw)) {
        df_raw <- df_raw %>% rename(!!newName := all_of(oldName))
      }
    }
    depthCols <- c("X0.2..m..", "X0.4..m..", "X0.5..m..", "X0.6..m..", 
                   "X0.62..m..", "X0.7..m..", "X0.8..m..", "X0.9..m..", 
                   "Oberfl. (m):", "Sohle (m):")
    for(cc in depthCols) {
      if(cc %in% colnames(df_raw)) {
        df_raw[[cc]] <- ifelse(df_raw[[cc]] == 0, NA, df_raw[[cc]])
      }
    }
    
    # Wenn die Spalte "Methode" nicht vorhanden ist oder alle Werte NA oder leer sind,
    # dann verwenden wir den Wert aus "Methode.1".
    if (!"Methode" %in% names(df_raw) || all(is.na(df_raw$Methode)) || all(df_raw$Methode == "")) {
      if ("Methode.1" %in% names(df_raw)) {
        df_raw$Methode <- df_raw$`Methode.1`
      } else {
        df_raw$Methode <- NA
      }
    }
    
    original_data(df_raw)
    standard <- standardizeDataset(df_raw, useKreps = input$zweipunkt_kreps)
    active_data_line(standard$df_line)
    active_data_points(standard$df_points)
    
    meta_pegel <- read.table(input$file$datapath, skip = 21, nrows = 6, 
                             sep = ":", dec = ",", stringsAsFactors = FALSE) %>%
      rename(Information = V1, Wert = V2) %>%
      mutate(Einheit = c("-","m","m³/s","m²","m","m/s"))
    meta_pegel[,2] <- gsub("[^0-9,.]", "", meta_pegel[,2])
    meta_pegel[,2] <- gsub(",", ".", meta_pegel[,2]) %>% as.numeric() %>% round(digits = 3)
    meta_data(meta_pegel)
    
    pegelinfo <- read.table(input$file$datapath, nrows = 3, 
                            sep = "", stringsAsFactors = FALSE)
    pegel_info(pegelinfo)
    
    vergleich_data_start <- data.frame(
      Anzahl_Lotrechten = meta_data()[1,2],
      Q = meta_data()[3,2],
      A = meta_data()[4,2],
      stringsAsFactors = FALSE
    )
    vergleich_data_start$Anzahl_Lotrechten <- factor(vergleich_data_start$Anzahl_Lotrechten)
    original_data_vergleich(vergleich_data_start)
    active_data_vergleich(vergleich_data_start)
  })
  
  observe({
    if(is.null(input$file)) {
      defaultDF <- createDefaultData()
      standard <- standardizeDataset(defaultDF, useKreps = input$zweipunkt_kreps)
      original_data(defaultDF)
      active_data_line(standard$df_line)
      active_data_points(standard$df_points)
      
      meta_data(data.frame(
        Info = c("nLot", "Pegel", "Qursprung", "Aursprung"),
        Wert = c(nrow(defaultDF), "Pegel XY", 1.234, 5.678),
        stringsAsFactors = FALSE
      ))
      pegel_info(data.frame(
        Dummy = c("Pegel XY", "Fluss", "2025-02-14"),
        stringsAsFactors = FALSE
      ))
      
      vergleich_data_start <- data.frame(
        Anzahl_Lotrechten = nrow(defaultDF),
        Q = 1.234,
        A = 5.678,
        stringsAsFactors = FALSE
      )
      vergleich_data_start$Anzahl_Lotrechten <- factor(vergleich_data_start$Anzahl_Lotrechten)
      original_data_vergleich(vergleich_data_start)
      active_data_vergleich(vergleich_data_start)
    }
  })
  
  output$DataTable <- renderTable({
    req(active_data_line())
    # Entferne Zeilen, in denen die mittlere Geschwindigkeit NA ist,
    # was darauf hinweist, dass die Lotrechte (völlig) entfernt wurde.
    df <- active_data_line() %>% filter(!is.na(`mittlere Geschw. (m/s)`))
    
    # Definiere lesbare Bezeichnungen für numerische Messmethoden:
    methodLabels <- c("Peilung", "1 Punkt", "2 Punkt", "3 Punkt", 
                      "2 Punkt nach Kreps", "5 Punkt", "6 Punkt")
    
    # Erzeuge eine Hilfsspalte für die anzuzeigende Methode:
    df <- df %>% mutate(MethodeText = 
                          if_else(!is.na(adjustedMethod) & adjustedMethod == "2_kreps",
                                  "2 Punkt nach Kreps",
                                  if_else(!is.na(suppressWarnings(as.numeric(Methode))),
                                          methodLabels[as.numeric(Methode) + 1],
                                          Methode)))
    
    # Wähle die zu zeigenden Spalten:
    df_display <- df %>% select(
      Lotrechte,
      `Lage (m)`,
      `Tiefe (m)`,
      `Oberfl. (m/s)`,
      `0,2 (m/s)`,
      `0,4 (m/s)`,
      `0,5 (m/s)`,
      `0,6 (m/s)`,
      `0,62 (m/s)`,
      `0,7 (m/s)`,
      `0,8 (m/s)`,
      `0,9 (m/s)`,
      `Sohle (m/s)`,
      `mittlere Geschw. (m/s)`,
      MethodeText,
      `Durchfl. (m^3/s)`,
      `Flaeche (m^2)`
    )
    
    # Setze die Spaltenüberschriften so, dass die Spalte mit vm "mittlere Geschw. (m/s)" und
    # die Methode-Spalte "Methode" heißt.
    colnames(df_display)[14:15] <- c("mittlere Geschw. (m/s)", "Methode")
    
    df_display
  })
  
  
  # -----------------------------------------------------------------------------
  # Entfernung von Lotrechten
  removeLotrechteByCheckbox <- function(lot) {
    dfL <- active_data_line()
    dfP <- active_data_points()
    
    rv_removed_Lot(unique(c(rv_removed_Lot(), lot)))
    
    if (input$remove_speed_only) {
      idxL <- which(as.character(dfL$Lotrechte) == as.character(lot))
      if (length(idxL) > 0) {
        dfL$"mittlere Geschw. (m/s)"[idxL] <- NA
        dfL$"Durchfl. (m^3/s)"[idxL] <- NA
        dfL$"Flaeche (m^2)"[idxL] <- NA
      }
      idxP <- which(as.character(dfP$Lotrechte) == as.character(lot))
      if (length(idxP) > 0) {
        dfP$x_m[idxP] <- NA
        dfP$v_ms[idxP] <- NA
      }
      active_data_line(dfL)
      active_data_points(dfP)
      # Hier löschen wir den Vergleichsdatensatz NICHT, damit alte Vergleiche erhalten bleiben.
    } else {
      if ("Lotrechte" %in% names(dfL)) {
        dfL <- dfL[as.character(dfL$Lotrechte) != as.character(lot), ]
      }
      if ("Lotrechte" %in% names(dfP)) {
        dfP <- dfP[as.character(dfP$Lotrechte) != as.character(lot), ]
      }
      active_data_line(dfL)
      active_data_points(dfP)
      
      # ENTFERNE NICHT den Vergleichsdatensatz – kommentar diesen Teil:
      # current <- active_data_vergleich()
      # if (!is.null(current) && nrow(current) > 0) {
      #   current <- current[ as.character(current$Anzahl_Lotrechten) != as.character(lot), ]
      #   active_data_vergleich(current)
      # }
    }
  }
  
  observeEvent(input$remove_every_2, {
    req(active_data_line())
    dfL <- active_data_line()
    removed <- dfL$Lotrechte[seq(2, nrow(dfL), 2)]
    for (lot in removed) {
      removeLotrechteByCheckbox(lot)
    }
  })
  
  observeEvent(input$remove_every_3, {
    req(active_data_line())
    dfL <- active_data_line()
    removed <- dfL$Lotrechte[seq(3, nrow(dfL), 3)]
    for (lot in removed) {
      removeLotrechteByCheckbox(lot)
    }
  })
  
  observeEvent(input$remove_every_4, {
    req(active_data_line())
    dfL <- active_data_line()
    removed <- dfL$Lotrechte[seq(4, nrow(dfL), 4)]
    for (lot in removed) {
      removeLotrechteByCheckbox(lot)
    }
  })
  
  observeEvent(input$remove_custom, {
    req(active_data_line())
    dfL <- active_data_line()
    interval <- input$remove_interval
    removed <- dfL$Lotrechte[seq(interval, nrow(dfL), interval)]
    for (lot in removed) {
      removeLotrechteByCheckbox(lot)
    }
  })
  
  update_vm_for_lotrechte <- function(lotrechte_val) {
    # Hole aktuelle aggregierte Daten (df_line) und den Long-Datensatz (df_points)
    dfL <- active_data_line()         # Aggregierter Datensatz: Eine Zeile pro Lotrechte
    dfP <- active_data_points()         # Long-Format: Einzelmesspunkte
    
    # Filtere alle noch vorhandenen Messpunkte für diese Lotrechte
    subsetP <- dfP %>% filter(Lotrechte == lotrechte_val)
    
    # Wenn keine Messpunkte mehr vorhanden sind, entferne die Lotrechte komplett
    if(nrow(subsetP) == 0) {
      dfL <- dfL %>% filter(Lotrechte != lotrechte_val)
      active_data_line(dfL)
      return()
    }
    
    # Ermittele den Zeilenindex in dfL für die betroffene Lotrechte
    row_idx <- which(dfL$Lotrechte == lotrechte_val)
    if(length(row_idx) == 0) return()
    
    # Erzeuge ein neues Aggregat (new_row) aus der vorhandenen Zeile als Basis
    new_row <- dfL[row_idx, , drop = FALSE]
    
    # Lokale Funktion, die basierend auf den aktuell vorhandenen Messpunkten
    # den neuen numerischen Methodencode bestimmt.
    determineNewMethod <- function(avail, useKreps, orig) {
      # Falls alle für die Sechspunkt-Methode nötigen Messpunkte vorhanden sind:
      if(all(c("Oberfl.", "0,2", "0,4", "0,6", "0,8", "Sohle") %in% avail)) {
        return(6)
      }
      # Falls alle für die Fünfpunkt-Methode vorhanden sind:
      if(all(c("Oberfl.", "0,2", "0,6", "0,8", "Sohle") %in% avail)) {
        return(5)
      }
      # Falls für die Dreipunkt-Methode alle drei (0,2, 0,6, 0,8) vorhanden sind:
      if(all(c("0,2", "0,6", "0,8") %in% avail)) {
        return(3)
      }
      # Falls die Zweipunkt-Methode möglich ist (wenn "0,2" und mindestens einer von "0,6" oder "0,8" vorhanden sind):
      if("0,2" %in% avail && (("0,6" %in% avail) || ("0,8" %in% avail))) {
        return(if(useKreps) "2_kreps" else 2)
      }
      # Falls mindestens "0,6" vorhanden ist (Einpunkt)
      if("0,6" %in% avail) {
        return(1)
      }
      # Sonst behalte den Originalwert
      return(orig)
    }
    
    # Mapping: Ordnet jeden Messpunktnamen (wie "0,2", "0,6" etc.) der entsprechenden Spalte für die Geschwindigkeit in dfL zu
    mapping <- list(
      "Oberfl." = "Oberfl. (m/s)",
      "0,2"     = "0,2 (m/s)",
      "0,4"     = "0,4 (m/s)",
      "0,5"     = "0,5 (m/s)",
      "0,6"     = "0,6 (m/s)",
      "0,62"    = "0,62 (m/s)",
      "0,7"     = "0,7 (m/s)",
      "0,8"     = "0,8 (m/s)",
      "0,9"     = "0,9 (m/s)",
      "Sohle"   = "Sohle (m/s)"
    )
    
    # Aktualisiere in new_row für jede Messpunkt-Spalte den Wert anhand der noch vorhandenen Messpunkte in subsetP
    for(punkt in names(mapping)) {
      col_name <- mapping[[punkt]]
      vals <- subsetP %>% filter(Messpunkt == punkt) %>% pull(v_ms)
      if(length(vals) > 0) {
        new_row[[col_name]] <- vals[1]  # Alternativ: mean(vals, na.rm = TRUE)
      } else {
        new_row[[col_name]] <- NA
      }
    }
    
    # Bestimme, welche Messpunkte aktuell vorhanden sind (als sortierter Vektor)
    available <- sort(unique(subsetP$Messpunkt))
    
    # Bestimme den ursprünglichen Methodencode aus der vorhandenen Zeile
    original_method <- getMethodNumber(new_row[["Methode"]])
    new_method <- determineNewMethod(available, useKreps = input$zweipunkt_kreps, orig = original_method)
    
    new_row[["Methode"]] <- as.character(new_method)
    new_row[["adjustedMethod"]] <- if_else(input$zweipunkt_kreps & new_method == 2, "2_kreps", as.character(new_method))
    
    # Berechne den neuen vm-Wert:
    # Falls nur ein gültiger Messpunkt vorhanden ist, nimm diesen direkt,
    # ansonsten berechne mit calcMeanFlowVelocity.
    valid_points <- subsetP %>% filter(!is.na(v_ms))
    if(nrow(valid_points) == 1) {
      new_vm <- valid_points$v_ms[1]
    } else {
      new_vm <- calcMeanFlowVelocity(new_row, useKreps = input$zweipunkt_kreps)
    }
    new_row[["mittlere Geschw. (m/s)"]] <- new_vm
    
    # Ersetze die Zeile in dfL direkt (vollständige Ersetzung)
    dfL[row_idx, ] <- new_row
    
    # Setze den aktualisierten aggregierten Datensatz
    active_data_line(dfL)
  }
  
  
  
  # observeEvent(event_data("plotly_click", source = "A"), {
  #   # Hole die Klickdaten vom interaktiven Plot
  #   click_data <- event_data("plotly_click", source = "A")
  #   req(click_data)
  #   
  #   # Hole den Long-Format-Datensatz (Messpunkte)
  #   dfP <- active_data_points()
  #   
  #   # Berechne die euklidische Distanz zum Klickpunkt – Annahme: x entspricht "Lage (m)" und y entspricht x_m (Tiefe)
  #   distances <- sqrt((dfP$`Lage (m)` - click_data$x)^2 + (dfP$x_m - click_data$y)^2)
  #   min_index <- which.min(distances)
  #   
  #   if (length(min_index) > 0) {
  #     clicked_lotrechte <- dfP$Lotrechte[min_index]
  #     clicked_messpunkt  <- dfP$Messpunkt[min_index]
  #     
  #     if (input$remove_individual) {
  #       # Entferne nur den angeklickten Messpunkt
  #       new_dfP <- dfP[!(dfP$Lotrechte == clicked_lotrechte & dfP$Messpunkt == clicked_messpunkt), ]
  #       active_data_points(new_dfP)
  #       
  #       # Aktualisiere nun den aggregierten vm-Wert für die betroffene Lotrechte anhand der aktuell
  #       # verbleibenden Messpunkte. Diese Funktion passt den Methodencode an und berechnet vm neu.
  #       update_vm_for_lotrechte(clicked_lotrechte)
  #       
  #     } else {
  #       # Standard: Entferne die ganze Lotrechte (wie bisher)
  #       removeLotrechteByCheckbox(clicked_lotrechte)
  #     }
  #   }
  # })
  
  observeEvent(event_data("plotly_click", source = "A"), {
    click_info <- event_data("plotly_click", source = "A")
    req(click_info)
    
    # Prüfe, ob customdata vorliegt und setze dann Lotrechte und Messpunkt (als Originalwert)
    if (!is.null(click_info$customdata)) {
      cd <- strsplit(click_info$customdata, "_")[[1]]
      clicked_lotrechte <- as.numeric(cd[1])
      clicked_messpunkt  <- cd[2]  # Hier entspricht das dem Originalwert
    } else {
      # Fallback: Berechne den Messpunkt über Distanzberechnung
      dfP <- active_data_points()
      distances <- sqrt((dfP$`Lage (m)` - click_info$x)^2 + (dfP$x_m - click_info$y)^2)
      min_index <- which.min(distances)
      clicked_lotrechte <- dfP$Lotrechte[min_index]
      clicked_messpunkt <- dfP$Messpunkt[min_index]
    }
    
    if (input$remove_individual) {
      # Entferne nur den angeklickten Messpunkt
      dfP <- active_data_points()
      new_dfP <- dfP[!(dfP$Lotrechte == clicked_lotrechte & as.character(dfP$Messpunkt) == clicked_messpunkt), ]
      active_data_points(new_dfP)
      
      # Aktualisiere den aggregierten vm-Wert für die betroffene Lotrechte
      update_vm_for_lotrechte(clicked_lotrechte)
    } else {
      removeLotrechteByCheckbox(clicked_lotrechte)
    }
  })
  
  output$removed_Lot <- renderText({
    removed <- rv_removed_Lot()
    paste("Entfernte Lotrechten:\n",
          paste(sort(removed), collapse = ", "),
          "\nAnzahl entfernt:", length(removed), "/", meta_data()[1,2])
  })
  
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Bestätigung",
      "Möchtest du wirklich alle Änderungen zurücksetzen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_reset", "Ja, zurücksetzen")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    removeModal()
    df_raw <- original_data()
    if (!is.null(df_raw)) {
      standard <- standardizeDataset(df_raw, useKreps = input$zweipunkt_kreps)
      active_data_line(standard$df_line)
      active_data_points(standard$df_points)
    }
    rv_removed_Lot(integer(0))
  })
  
  # -----------------------------------------------------------------------------
  # Durchflussberechnung (mittels reaktiver Variable)
  durchfluss_aktuell <- reactiveVal()
  observe({
    req(active_data_line())
    dfL <- active_data_line()
    n <- nrow(dfL)
    if (n < 2) {
      durchfluss_aktuell(0)
      return()
    }
    df_calc <- dfL %>% 
      select("Lotrechte", b = "Lage (m)", h = "Tiefe (m)", v = "mittlere Geschw. (m/s)") %>%
      mutate(q = 0)
    if (input$flussVerfahren == "mitte") {
      if (n >= 3) {
        for (i in 2:(n - 1)) {
          df_calc$q[i] <- df_calc$v[i] * df_calc$h[i] * ((df_calc$b[i + 1] - df_calc$b[i - 1]) / 2)
        }
      }
    } else {
      for (i in 1:(n - 1)) {
        df_calc$q[i] <- (df_calc$b[i + 1] - df_calc$b[i]) *
          ((df_calc$v[i] + df_calc$v[i + 1]) / 2) *
          ((df_calc$h[i] + df_calc$h[i + 1]) / 2)
      }
    }
    Q <- round(sum(df_calc$q, na.rm = TRUE), 3)
    durchfluss_aktuell(Q)
  })
  
  output$durchfluss <- renderTable({
    req(durchfluss_aktuell(), meta_data())
    Qakt <- durchfluss_aktuell()
    dfm <- meta_data()
    Qursprung <- as.numeric(dfm[3, 2])
    abw <- if (is.na(Qursprung) || Qursprung == 0) NA else round(((Qakt - Qursprung) / Qursprung * 100), 2)
    data.frame(
      c("aktuell:", "Ursprung:", "Abweichung:"),
      c(paste(Qakt, "m³/s"), paste(Qursprung, "m³/s"), paste(abw, "%"))
    )
  }, colnames = FALSE)
  
  querschnittsflaeche_aktuell <- reactiveVal()
  observe({
    req(active_data_line())
    dfL <- active_data_line()
    n <- nrow(dfL)
    if (n < 2) {
      querschnittsflaeche_aktuell(0)
      return()
    }
    df_calc <- dfL %>% select("Lotrechte", b = "Lage (m)", h = "Tiefe (m)")
    A <- sum((df_calc$b[-1] - df_calc$b[-n]) * (df_calc$h[-1] + df_calc$h[-n]) / 2)
    querschnittsflaeche_aktuell(round(A, 3))
  })
  
  output$querschnittsflaeche <- renderTable({
    req(querschnittsflaeche_aktuell(), meta_data())
    Aakt <- querschnittsflaeche_aktuell()
    dfm <- meta_data()
    Aursprung <- as.numeric(dfm[4, 2])
    abw <- if (is.na(Aursprung) || Aursprung == 0) NA else round(((Aakt - Aursprung) / Aursprung * 100), 2)
    data.frame(
      c("aktuell:", "Ursprung:", "Abweichung:"),
      c(paste(Aakt, "m²"), paste(Aursprung, "m²"), paste(abw, "%"))
    )
  }, colnames = FALSE)
  
  observeEvent(input$add_values, {
    req(meta_data())
    dfm <- meta_data()
    
    # Aktuelle Anzahl Lotrechten (Annahme: dfm[1,2] ist die ursprüngliche Anzahl, 
    # minus der bereits entfernten in rv_removed_Lot)
    current_nLot <- as.numeric(dfm[1,2]) - length(rv_removed_Lot())
    
    Qakt <- durchfluss_aktuell()
    Aakt <- querschnittsflaeche_aktuell()
    
    current <- active_data_vergleich()
    
    # Prüfe, ob der Datensatz leer oder nicht initialisiert ist
    if(is.null(current) || nrow(current) == 0) {
      # Neue Tabelle anlegen
      current <- data.frame(
        Anzahl_Lotrechten = current_nLot,
        Q = Qakt,
        A = Aakt,
        Messung = 1,           # Startet mit 1
        timeStamp = Sys.time(),# Chronologischer Zeitstempel
        stringsAsFactors = FALSE
      )
      active_data_vergleich(current)
      
    } else {
      # Sicherstellen, dass Anzahl_Lotrechten numerisch vorliegt
      if(is.factor(current$Anzahl_Lotrechten)) {
        current$Anzahl_Lotrechten <- as.numeric(as.character(current$Anzahl_Lotrechten))
      }
      # Falls die Spalte Messung fehlt, legen wir sie an
      if(!"Messung" %in% names(current)) {
        current$Messung <- 1
      }
      # Falls die Spalte timeStamp fehlt, legen wir sie an
      if(!"timeStamp" %in% names(current)) {
        current$timeStamp <- Sys.time()
      }
      
      # Ermittele alle Einträge dieser Gruppe (dieser Lotrechtenzahl)
      group_current <- current %>% filter(Anzahl_Lotrechten == current_nLot)
      
      # Bestimme die neue Messungs‑ID
      if(nrow(group_current) == 0) {
        # Gruppe existiert noch nicht -> Messung beginnt bei 1
        new_id <- 1
      } else {
        # Knüpfe an die bisher höchste ID an
        valid_ids <- group_current$Messung[!is.na(group_current$Messung)]
        new_id <- if(length(valid_ids) == 0) 1 else max(as.numeric(valid_ids)) + 1
      }
      
      # Erzeuge den neuen Eintrag, inkl. Zeitstempel
      newEntry <- data.frame(
        Anzahl_Lotrechten = current_nLot,
        Q = Qakt,
        A = Aakt,
        Messung = new_id,
        timeStamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      # Stelle sicher, dass newEntry die gleichen Spalten wie current hat
      # (falls z.B. Spaltenreihenfolge abweicht)
      missingCols <- setdiff(colnames(current), colnames(newEntry))
      for(mc in missingCols) {
        newEntry[[mc]] <- NA
      }
      newEntry <- newEntry[, colnames(current)]
      
      # Anfügen an den vorhandenen Datensatz
      active_data_vergleich(rbind(current, newEntry))
    }
  })
  
  observeEvent(input$reset_values, {
    active_data_vergleich(original_data_vergleich())
  })
  
  output$download_data_querschnitt <- downloadHandler(
    filename = function() {
      paste0(pegel_info()[1,2], "_querschnitt_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      write.csv(active_data_line(), file)
    }
  )
  
  output$Metadaten <- renderTable({
    req(meta_data())
    meta_data()
  })
  
  output$download_querschnittsplot <- downloadHandler(
    filename = function() {
      paste0(pegel_info()[1,2], "_querschnitt_plot_edited_",
             format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".png")
    },
    content = function(file) {
      ggsave(file, plot_querschnitt_static(), width = 9, height = 6, dpi = 300)
    }
  )
  
  output$download_vergleichsplot <- downloadHandler(
    filename = function() {
      paste0(pegel_info()[1,2], "_vergleich_plot_",
             format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".png")
    },
    content = function(file) {
      ggsave(file, plot_vergleich_gg(), width = 9, height = 6, dpi = 300)
    }
  )
  
  # -----------------------------------------------------------------------------
  # Ausgabe des Querschnittsplots (interaktiv)
  output$plotOutput_querschnitt <- renderPlotly({
    req(active_data_line(), active_data_points(), pegel_info())
    p <- ggplot() +
      geom_line(data = active_data_line(), aes(x = `Lage (m)`, y = `Tiefe (m)`)) +
      geom_point(
        data = active_data_points() %>% 
          mutate(Messpunkt_orig = as.character(Messpunkt),
                 Messpunkt = factor(Messpunkt,
                                    levels = c("Oberfl.", "0,2", "0,4", "0,5", "0,6", "0,62", "0,7", "0,8", "0,9", "Sohle"),
                                    labels = c("Oberfl.", "20%", "40%", "50%", "60%", "62%", "70%", "80%", "90%", "Sohle")
                 )),
        aes(
          x = `Lage (m)`, 
          y = x_m, 
          color = Messpunkt,
          text = paste("Lage:", sprintf("%.2f", `Lage (m)`), "m",
                       "\nTiefe:", sprintf("%.2f", x_m), "m (", Messpunkt, ")",
                       "\nGeschwindigkeit:", sprintf("%.3f", v_ms), "m/s"),
          # Verwende Messpunkt_orig für customdata, damit der Originalwert übergeben wird
          customdata = paste(`Lotrechte`, Messpunkt_orig, sep = "_")
        )
      ) +
      labs(x = "Lage (m)", y = "Tiefe (m)",
           title = paste("Messung:", pegel_info()[1,2]),
           caption = paste("Messdatum:", pegel_info()[3,2]),
           color = "Messpunkt") +
      scale_x_continuous(breaks = seq(round(min(active_data_line()[["Lage (m)"]]), 2),
                                      round(max(active_data_line()[["Lage (m)"]]), 2), 0.5)) +
      scale_y_reverse() +
      scale_color_manual(values = c("Oberfl." = "black", "20%" = "red", "40%" = "orange",
                                    "50%" = "purple", "60%" = "green", "62%" = "darkgreen",
                                    "70%" = "pink", "80%" = "blue", "90%" = "brown", "Sohle" = "grey")) +
      theme_bw()
    p <- ggplotly(p, tooltip = "text")
    event_register(p, "plotly_click")
    p
  })
  
  
  # -----------------------------------------------------------------------------
  # Funktion für den statischen Querschnittsplot (für den Download)
  plot_querschnitt_static <- function() {
    req(active_data_line(), active_data_points(), pegel_info())
    ggplot() +
      geom_line(data = active_data_line(), aes(x = `Lage (m)`, y = `Tiefe (m)`)) +
      geom_point(data = active_data_points() %>% 
                   mutate(Messpunkt = factor(Messpunkt,
                                             levels = c("Oberfl.", "0,2", "0,4", "0,5", "0,6", "0,62", "0,7", "0,8", "0,9", "Sohle"),
                                             labels = c("Oberfl.", "20%", "40%", "50%", "60%", "62%", "70%", "80%", "90%", "Sohle")
                   )),
                 aes(x = `Lage (m)`, y = x_m, color = Messpunkt)) +
      labs(x = "Lage (m)", y = "Tiefe (m)",
           title = paste("Profil:", pegel_info()[1,2]),
           caption = paste("Messdatum:", pegel_info()[3,2]),
           color = "Messpunkt") +
      scale_x_continuous(breaks = seq(round(min(active_data_line()[["Lage (m)"]]), 2),
                                      round(max(active_data_line()[["Lage (m)"]]), 2), 0.5)) +
      scale_y_reverse() +
      scale_color_manual(values = c("Oberfl." = "black", "20%" = "red", "40%" = "orange",
                                    "50%" = "purple", "60%" = "green", "62%" = "darkgreen",
                                    "70%" = "pink", "80%" = "blue", "90%" = "brown", "Sohle" = "grey")) +
      theme_bw()
  }
  
  
  
  # Reaktive Funktion zum Erzeugen des Vergleichsplots (als ggplot-Objekt)
  plot_vergleich_gg <- reactive({
    req(active_data_vergleich(), durchfluss_aktuell(), pegel_info())
    dfv <- active_data_vergleich()
    
    # Falls die Spalte timeStamp fehlt oder die Spalte Messung fehlt, erstellen wir sie minimal
    if(!"timeStamp" %in% names(dfv)) {
      dfv$timeStamp <- seq_len(nrow(dfv)) # Ein Minimal-Fallback
    }
    if(!"Messung" %in% names(dfv)) {
      dfv$Messung <- seq_len(nrow(dfv))
    }
    
    # Sortiere nach timeStamp -> chronologische Reihenfolge
    dfv <- dfv %>% arrange(timeStamp)
    
    # Erstelle eine neue Spalte "Gruppe", z. B.: "25 / 1", "25 / 2" ...
    dfv <- dfv %>%
      mutate(Gruppe = paste(Anzahl_Lotrechten, Messung, sep = " / "))
    
    # Die Factor-Levels werden in der Reihenfolge gesetzt, in der sie (zeitlich) angefallen sind
    dfv$Gruppe <- factor(dfv$Gruppe, levels = unique(dfv$Gruppe))
    
    # Bestimme (optional) die Abweichungslinien basierend auf dem ersten Eintrag
    if(nrow(dfv) > 0) {
      abw_up <- dfv$Q[1] + 0.05 * dfv$Q[1]
      abw_low <- dfv$Q[1] - 0.05 * dfv$Q[1]
    } else {
      abw_up <- NA
      abw_low <- NA
    }
    
    ggplot(dfv, aes(x = Gruppe, y = Q, fill = Gruppe)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
      annotate("segment", x = -Inf, xend = Inf, y = abw_low, yend = abw_low, linetype = "dashed") +
      annotate("segment", x = -Inf, xend = Inf, y = abw_up, yend = abw_up, linetype = "dashed") +
      labs(
        x = "Messung (Anzahl Lotrechten / Messung)",
        y = "Durchfluss (m³/s)",
        title = paste("Profil:", pegel_info()[1,2]),
        caption = paste("Messdatum:", pegel_info()[3,1])
      ) +
      theme_bw() +
      theme(legend.position = "none")
  })
  
  output$plotOutput_vergleich <- renderPlotly({
    ggplotly(plot_vergleich_gg(), tooltip = "text")
  })
  
  output$DataTableVergleich <- renderTable({
    active_data_vergleich()
  })
  
  output$download_data_vergleich <- downloadHandler(
    filename = function() {
      paste0(pegel_info()[1,2], "_vergleich_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      write.csv(active_data_vergleich(), file)
    }
  )
}

# Start der Shiny-App
shinyApp(ui = ui, server = server)