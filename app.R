# -----------------------------------------------------------------------------
#  HydroEdit – Bearbeitung von Durchflussmessungen
#  Copyright (C) 2025 Felix Simon, Hochschule Bochum
#
#  Diese Datei ist Teil von HydroEdit.
#
#  HydroEdit ist freie Software: Du kannst sie unter den Bedingungen
#  der GNU General Public License, Version 3, wie von der Free Software
#  Foundation veröffentlicht, weiterverbreiten und/oder modifizieren.
#
#  HydroEdit wird in der Hoffnung verteilt, dass es nützlich sein wird, aber
#  OHNE JEDE GEWÄHRLEISTUNG, sogar ohne die implizite Garantie der
#  MARKTFÄHIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK.
#  Siehe die GNU General Public License v3 für weitere Details.
#
#  Eine Kopie der GPL v3 sollte zusammen mit diesem Programm verteilt sein.
#  Falls nicht, siehe <https://www.gnu.org/licenses/>.
# -----------------------------------------------------------------------------

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
    v0_8 <- as.numeric(row[["0,8 (m/s)"]])
    if(is.na(v0_8)) {
      v0_8 <- as.numeric(row[["0,6 (m/s)"]])
    }
    return(0.5 * (v0_2 + v0_8))
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

# ---- Datei‐Typ Erkennung und spezifische Lese‐Routinen für verschiedene Firmware ----

detectFileType <- function(path) {
  # Lese die ersten 10 Zeilen ein, in denen die Metadaten typischerweise stehen
  hdr <- readLines(path, n = 10, warn = FALSE)

  # Suche nach dem Text "Anwend.:" gefolgt von einem Leerzeichen und vVersion im deutschen Format
  if (any(grepl("Anwend\\.: v2,0[5-9]", hdr))) {
    return("OTT_MF_pro_v205_plus")
  }
  if (any(grepl("Anwend\\.: v2,00", hdr))) {
    return("OTT_MF_pro_v200")
  }
  if (any(grepl("Anwend\\.: v1,06", hdr))) {
    return("OTT_MF_pro_v106")
  }

  # Alles andere → generic
  return("generic")
}

read_OTT_MF_pro_v205_plus <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr_idx <- which(grepl("^Zeit\\s+Lotrechte", lines))[1]
  if (is.na(hdr_idx)) stop("Messdaten-Kopfzeile nicht gefunden in v2.05+")
  
  read.table(path,
             skip   = hdr_idx - 1,
             header = TRUE,
             sep    = "\t",
             dec    = ",",
             fill   = TRUE,
             encoding = "latin1",
             stringsAsFactors = FALSE)
}

read_OTT_MF_pro_v200 <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr_idx <- which(grepl("^Zeit\\s+Lotrechte", lines))[1]
  if (is.na(hdr_idx)) stop("Messdaten-Kopfzeile nicht gefunden in v2.00")
  
  read.table(path,
             skip   = hdr_idx - 1,
             header = TRUE,
             sep    = "\t",
             dec    = ",",
             fill   = TRUE,
             encoding = "latin1",
             stringsAsFactors = FALSE)
}

read_OTT_MF_pro_v106 <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr_idx <- which(grepl("^Zeit\\s+Lotrechte", lines))[1]
  if (is.na(hdr_idx)) stop("Messdaten-Kopfzeile nicht gefunden in v1.06")
  
  df <- read.table(path,
                   skip = hdr_idx - 1,
                   header = TRUE,
                   sep = "\t",
                   dec = ",",
                   fill = TRUE,
                   encoding = "latin1",
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
  
  # Bereinige und repariere Spaltennamen
  names(df) <- iconv(names(df), from = "latin1", to = "UTF-8")
  names(df) <- gsub("^\\s+|\\s+$", "", names(df))
  names(df) <- gsub("\\.x$|\\.y$", "", names(df))
  names(df)[names(df) == ""] <- paste0("V", which(names(df) == ""))
  
  names(df) <- gsub("Ã-Geschw\\. \\(m/s\\)|Ã˜-Geschw\\. \\(m/s\\)|Ø-Geschw\\. \\(m/s\\)", 
                    "mittlere Geschw. (m/s)", names(df))
  names(df) <- gsub("FlÃ¤che \\(m\\^2\\)", "Fläche (m^2)", names(df))
  names(df) <- gsub("Durchfl\\.", "Durchfluss", names(df))
  
  name_mapping <- c(
    "Zeit" = "Zeit", "Lotrechte" = "Lotrechte", "Lage (m)" = "Lage..m.",
    "Methode" = "Methode", "Tiefe (m)" = "Tiefe..m.", "Randfakt." = "Randfakt.",
    "mittlere Geschw. (m/s)" = "mittlere.Geschw...m.s.", "Fläche (m^2)" = "Flaeche..m.2.",
    "Durchfluss (m^3/s)" = "Durchfl...m.3.s.",
    "Oberfl. (m/s)" = "X0.0..m.s.", "0,2 (m/s)" = "X0.2..m.s.", "0,4 (m/s)" = "X0.4..m.s.",
    "0,5 (m/s)" = "X0.5..m.s.", "0,6 (m/s)" = "X0.6..m.s.", "0,62 (m/s)" = "X0.62..m.s.",
    "0,7 (m/s)" = "X0.7..m.s.", "0,8 (m/s)" = "X0.8..m.s.", "0,9 (m/s)" = "X0.9..m.s.", "Sohle (m/s)" = "Sohle..m.s.",
    "Oberfl. (m):" = "X0.0..m..", "0,2 (m):" = "X0.2..m..", "0,4 (m):" = "X0.4..m..",
    "0,5 (m):" = "X0.5..m..", "0,6 (m):" = "X0.6..m..", "0,62 (m):" = "X0.62..m..",
    "0,7 (m):" = "X0.7..m..", "0,8 (m):" = "X0.8..m..", "0,9 (m):" = "X0.9..m..", "Sohle (m):" = "Sohle..m.."
  )
  
  df <- df %>% rename_with(~ name_mapping[.x], .cols = intersect(names(df), names(name_mapping)))
  
  # Methode.1 ergänzen (numerischer Code als character)
  df$Methode.1 <- as.character(sapply(df$Methode, getMethodNumber))
  
  # Tiefe berechnen (nur für tatsächlich gemessene Punkte)
  tiefe_numeric <- suppressWarnings(as.numeric(df[["Tiefe..m."]]))
  mp_prozent <- c("X0.0" = 0, "X0.2" = 0.2, "X0.4" = 0.4, "X0.5" = 0.5, "X0.6" = 0.6,
                  "X0.62" = 0.62, "X0.7" = 0.7, "X0.8" = 0.8, "X0.9" = 0.9, "Sohle" = 1)
  
  for (mp in names(mp_prozent)) {
    x_col <- paste0(mp, "..m..")
    if (!(x_col %in% names(df))) df[[x_col]] <- NA
    method_allowed <- list(
      "0" = character(0),
      "1" = c("X0.6"),
      "2" = c("X0.2", "X0.8"),
      "2_kreps" = c("X0.0", "X0.62"),
      "3" = c("X0.2", "X0.6", "X0.8"),
      "5" = c("X0.0", "X0.2", "X0.6", "X0.8", "Sohle"),
      "6" = c("X0.0", "X0.2", "X0.4", "X0.6", "X0.8", "Sohle")
    )
    for (i in seq_len(nrow(df))) {
      m <- df$Methode.1[i]
      if (!is.na(m)) {
        allowed <- method_allowed[[m]]
        if (!mp %in% allowed) df[[x_col]][i] <- NA
        else df[[x_col]][i] <- round(tiefe_numeric[i] * mp_prozent[[mp]], 3)
      } else {
        df[[x_col]][i] <- NA
      }
    }
  }
  
  # Fehlende Spalten ergänzen
  all_cols <- c("Zeit", "Lotrechte", "Lage..m.", "Methode", "Methode.1", "Tiefe..m.",
                "Eisdicke..m.", "Referenzwasserstand..m.", "Randfakt.", "Korrekturfaktor",
                "Oberfl...m.s.", "Oberfl...m..", "X0.2..m.s.", "X0.2..m..", "X0.4..m.s.", "X0.4..m..",
                "X0.5..m.s.", "X0.5..m..", "X0.6..m.s.", "X0.6..m..", "X0.62..m.s.", "X0.62..m..",
                "X0.7..m.s.", "X0.7..m..", "X0.8..m.s.", "X0.8..m..", "X0.9..m.s.", "X0.9..m..",
                "Sohle..m.s.", "Sohle..m..", "mittlere.Geschw...m.s.", "Flaeche..m.2.", "Durchfl...m.3.s.", "X")
  
  for (col in setdiff(all_cols, names(df))) {
    df[[col]] <- NA
  }
  
  df <- df[, all_cols]
  return(df)
}

read_generic <- function(path) {
  read.table(path,
             header = TRUE,
             sep    = "\t",
             dec    = ",",
             fill   = TRUE,
             encoding = "latin1",
             stringsAsFactors = FALSE)
}

# Funktion für Meta-Daten einlesen je nach filetype
read_meta_data_by_filetype <- function(path, filetype) {
  if (filetype == "OTT_MF_pro_v106") {
    # v1.06 → andere Struktur, 5 Zeilen ab Zeile 24
    meta <- read.table(path,
                       skip = 23, nrows = 5,
                       sep = ":", dec = ",",
                       fileEncoding = "latin1",
                       stringsAsFactors = FALSE, fill = TRUE)
    colnames(meta) <- c("Information", "Wert")
    
    # Wert bereinigen
    meta$Wert <- iconv(meta$Wert, from = "latin1", to = "UTF-8")
    meta$Wert <- gsub(",", ".", meta$Wert)
    meta$Wert <- gsub("[^0-9.]", "", meta$Wert)
    meta$Wert <- as.numeric(meta$Wert)
    
    # Mittlere Geschwindigkeit berechnen: Q / A
    Q <- meta$Wert[3]
    A <- meta$Wert[4]
    mittlGeschw <- if (!is.na(Q) && !is.na(A) && A > 0) Q / A else NA_real_
    
    # Zusatzzeile anhängen
    meta <- rbind(
      meta,
      data.frame(
        Information = "mittlere Geschwindigkeit",
        Wert        = round(mittlGeschw, 3),
        stringsAsFactors = FALSE
      )
    )
    
    meta$Einheit <- c("-", "m", "m³/s", "m²", "m", "m/s")
    return(meta)
  }
  
  if (filetype == "OTT_MF_pro_v200") {
    meta <- read.table(path,
                       skip = 21, nrows = 6,
                       sep = ":", dec = ",",
                       fileEncoding = "latin1",
                       stringsAsFactors = FALSE, fill = TRUE)
    colnames(meta) <- c("Information", "Wert")
    
    meta$Wert <- iconv(meta$Wert, from = "latin1", to = "UTF-8")
    meta$Wert <- gsub(",", ".", meta$Wert)
    meta$Wert <- gsub("[^0-9.]", "", meta$Wert)
    meta$Wert <- as.numeric(meta$Wert)
    
    meta$Einheit <- c("-", "m", "m³/s", "m²", "m", "m/s")
    return(meta)
  }
  
  if (filetype == "OTT_MF_pro_v205_plus") {
    meta <- read.table(path,
                       skip = 21, nrows = 6,
                       sep = ":", dec = ",",
                       fileEncoding = "latin1",
                       stringsAsFactors = FALSE, fill = TRUE)
    colnames(meta) <- c("Information", "Wert")
    
    meta$Wert <- iconv(meta$Wert, from = "latin1", to = "UTF-8")
    meta$Wert <- gsub(",", ".", meta$Wert)
    meta$Wert <- gsub("[^0-9.]", "", meta$Wert)
    meta$Wert <- as.numeric(meta$Wert)
    
    meta$Einheit <- c("-", "m", "m³/s", "m²", "m", "m/s")
    return(meta)
  }
  
  # generic fallback
  return(data.frame(
    Information = NA,
    Wert        = NA,
    Einheit     = NA,
    stringsAsFactors = FALSE
  ))
}


# Funktion für Pegelinfo einlesen je nach filetype
read_pegel_info_by_filetype <- function(path, filetype) {
  skip_val <- switch(
    filetype,
    OTT_MF_pro_v106      = 0,  # v1.06
    OTT_MF_pro_v200      = 0,   # v2.00
    OTT_MF_pro_v205_plus = 0,   # v2.05+
    0                    # fallback
  )
  
  pegelinfo <- tryCatch({
    read.table(
      path,
      skip = skip_val,
      nrows = 3,
      sep = "", 
      fileEncoding = "latin1",
      stringsAsFactors = FALSE, 
      fill = TRUE
    )
  }, error = function(e) {
    warning("Fehler beim Einlesen der Pegelinfo: ", e$message)
    return(data.frame())
  })
  
  return(pegelinfo)
}


# Beispielhafter Startdatensatz mit 20 Lotrechten
createDefaultData <- function() {
  set.seed(123)  
  
  # -----------------------
  # 1) Beispiel‐Querschnitt erzeugen
  # -----------------------
  n_lotrechte <- 12
  lage_m <- seq(0, 11, length.out = n_lotrechte)
  dist_norm <- ((lage_m - mean(lage_m))^2) / max((lage_m - mean(lage_m))^2)
  basis_tiefe <- 0.5 + (1 - dist_norm) * 1.5
  tiefe_jitter <- runif(n_lotrechte, -0.05, 0.05)
  tiefe_m <- round(basis_tiefe + tiefe_jitter, 3)
  
  methode <- c(
    "0", "1 Punkt", "2 Punkt", "3 Punkt", "5 Punkt", "6 Punkt",
    "3 Punkt", "2 Punkt", "1 Punkt", "2 Punkt", "3 Punkt", "0"
  )
  
  # Tiefen‐Koordinaten für Messpunkte
  x_Oberfl  <- rep(0, n_lotrechte)
  x_02      <- round(tiefe_m * 0.2, 3)
  x_04      <- round(tiefe_m * 0.4, 3)
  x_05      <- round(tiefe_m * 0.5, 3)
  x_06      <- round(tiefe_m * 0.6, 3)
  x_062     <- round(tiefe_m * 0.62, 3)
  x_07      <- round(tiefe_m * 0.7, 3)
  x_08      <- round(tiefe_m * 0.8, 3)
  x_09      <- round(tiefe_m * 0.9, 3)
  x_Sohle   <- round(tiefe_m * 1.0, 3)
  
  # Basis‐Geschwindigkeitsprofil (Parabel + zufälliges Rauschen)
  v_max_basis <- seq(0.8, 0.6, length.out = n_lotrechte) + runif(n_lotrechte, -0.05, 0.05)
  v_max_basis <- round(pmax(v_max_basis, 0.3), 3)
  
  v_Oberfl <- round(v_max_basis * (1 - (0 / tiefe_m)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_02     <- round(v_max_basis * (1 - (0.2)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_04     <- round(v_max_basis * (1 - (0.4)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_05     <- round(v_max_basis * (1 - (0.5)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_06     <- round(v_max_basis * (1 - (0.6)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_062    <- round(v_max_basis * (1 - (0.62)^2)+ runif(n_lotrechte, -0.01, 0.01), 3)
  v_07     <- round(v_max_basis * (1 - (0.7)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_08     <- round(v_max_basis * (1 - (0.8)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_09     <- round(v_max_basis * (1 - (0.9)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  v_Sohle  <- round(v_max_basis * (1 - (1.0)^2) + runif(n_lotrechte, -0.01, 0.01), 3)
  
  # Spaltenweise NA setzen, je nach Methode:
  for (i in seq_len(n_lotrechte)) {
    m <- methode[i]
    if (m == "0") {
      v_Oberfl[i] <- v_02[i] <- v_04[i] <- v_05[i] <- v_06[i] <- 
        v_062[i]    <- v_07[i] <- v_08[i] <- v_09[i] <- v_Sohle[i] <- NA
    }
    if (m == "1 Punkt") {
      v_Oberfl[i] <- v_02[i] <- v_04[i] <- v_05[i] <- v_062[i] <-
        v_07[i]     <- v_08[i] <- v_09[i] <- v_Sohle[i] <- NA
    }
    if (m == "2 Punkt") {
      v_Oberfl[i] <- v_04[i] <- v_05[i] <- v_062[i] <- 
        v_07[i]     <- v_08[i] <- v_09[i] <- v_Sohle[i] <- NA
    }
    if (m == "3 Punkt") {
      v_Oberfl[i] <- v_04[i] <- v_05[i] <- v_062[i] <- 
        v_07[i]     <- v_09[i] <- v_Sohle[i] <- NA
    }
    if (m == "5 Punkt") {
      v_04[i] <- v_05[i] <- v_062[i] <- v_07[i] <- v_09[i] <- NA
    }
    # "6 Punkt": alle Messpunkte bleiben gültig → nichts tun
  }
  
  # -----------------------
  # 2) DataFrame erzeugen
  # -----------------------
  df <- data.frame(
    Lotrechte                = seq_len(n_lotrechte),
    `Lage (m)`               = round(lage_m, 2),
    Methode                  = methode,
    `Tiefe (m)`              = tiefe_m,
    `Oberfl. (m/s)`          = v_Oberfl,
    `Oberfl. (m):`           = x_Oberfl,
    `0,2 (m/s)`              = v_02,
    `0,2 (m):`               = x_02,
    `0,4 (m/s)`              = v_04,
    `0,4 (m):`               = x_04,
    `0,5 (m/s)`              = v_05,
    `0,5 (m):`               = x_05,
    `0,6 (m/s)`              = v_06,
    `0,6 (m):`               = x_06,
    `0,62 (m/s)`             = v_062,
    `0,62 (m):`              = x_062,
    `0,7 (m/s)`              = v_07,
    `0,7 (m):`               = x_07,
    `0,8 (m/s)`              = v_08,
    `0,8 (m):`               = x_08,
    `0,9 (m/s)`              = v_09,
    `0,9 (m):`               = x_09,
    `Sohle (m/s)`            = v_Sohle,
    `Sohle (m):`             = x_Sohle,
    `mittlere Geschw. (m/s)` = NA_real_,
    `Flaeche (m^2)`          = NA_real_,
    `Durchfl. (m^3/s)`       = NA_real_,
    check.names    = FALSE,
    stringsAsFactors = FALSE
  )
  
  # -----------------------------------------------------------
  # 3) GANZ AM ANFANG und ENDE: alle Tiefen‐ und Speed‐Spalten auf NA setzen
  # -----------------------------------------------------------
  depth_cols <- c(
    "Oberfl. (m):", "0,2 (m):", "0,4 (m):", "0,5 (m):",
    "0,6 (m):",   "0,62 (m):", "0,7 (m):", "0,8 (m):",
    "0,9 (m):",   "Sohle (m):"
  )
  speed_cols <- c(
    "Oberfl. (m/s)", "0,2 (m/s)", "0,4 (m/s)", "0,5 (m/s)",
    "0,6 (m/s)",   "0,62 (m/s)", "0,7 (m/s)", "0,8 (m/s)",
    "0,9 (m/s)",   "Sohle (m/s)"
  )
  
  # tatsächlich vorhandene Spalten ermitteln
  actual_depth_cols <- intersect(depth_cols, names(df))
  actual_speed_cols <- intersect(speed_cols, names(df))
  
  # Index 1 und Index n_lotrechte auf NA setzen
  df[1, actual_depth_cols]       <- NA
  df[n_lotrechte, actual_depth_cols] <- NA
  df[1, actual_speed_cols]       <- NA
  df[n_lotrechte, actual_speed_cols] <- NA
  
  # -----------------------------------------------------------
  # 4) Mittlere Fließgeschwindigkeit berechnen
  # -----------------------------------------------------------
  df$`mittlere Geschw. (m/s)` <- vapply(
    seq_len(nrow(df)),
    function(i) calcMeanFlowVelocity(df[i, ], useKreps = FALSE),
    numeric(1)
  )
  
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
  
  cat("<<< Spaltennamen vor getMethod:\n")
  # # print(names(df_line))
  # print(str(df_line$Methode))
  # Erzeuge die Spalte "Method" als numerischen Code basierend auf "Methode"
  cat("== DEBUG Methode RawCheck ==\n")
  for (i in seq_len(min(10, nrow(df_line)))) {
    val <- df_line$Methode[i]
    cat(i, ": ", dQuote(val), " → ", getMethodNumber(val), "\n", sep = "")
  }
  
  df_line$Method <- sapply(df_line$Methode, getMethodNumber)
  print(unique(df_line$Method))
  
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
  
  join_keys <- c("Lotrechte", "Messpunkt", "Lage (m)", "Tiefe (m)")
  # nur die Keys verwenden, die in beiden Dataframes existieren
  use_keys  <- join_keys[join_keys %in% names(df_depth) & join_keys %in% names(df_speed)]
  
  df_points <- left_join(
    df_depth,
    df_speed,
    by           = use_keys,
    relationship = "many-to-many"  # unterdrückt die Warning, falls Messpunkt-Kombinationen mehrfach vorkommen
  )
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
                          selected = "mitte"),
              actionButton("show_help", "Hilfe", icon = icon("question-circle"), class = "btn btn-info btn-sm")
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
  div(
    style = "display: flex; justify-content: space-between; align-items: center; padding: 10px;",
    # Linke Seite: Logo + Titel
    div(style = "display: flex; align-items: center; gap: 20px;",
        tags$img(src = "hydroedit.png", height = "95px"),
        tags$h3("HydroEdit – Bearbeitung von Durchflussmessungen", style = "margin: 0; color: #2c3e50")
    ),
    # Rechte Seite: About‐Button
    actionButton("about", "About", class = "btn btn-outline-primary")
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
  
  # About-Button
  observeEvent(input$about, {
    showModal(modalDialog(
      size = "l",      # größeres Modal-Fenster
      easyClose = TRUE,
      footer = modalButton("Schließen"),
      
      # -----------------------
      # Überschrift
      tags$h3("About", style = "margin-top: 0;"),
      
      # -----------------------
      # Versions‐ und Stand‐Information
      tags$p(
        tags$b("Version:"), "1.0.0", 
        HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"), 
        tags$b("Stand: 07.2025"),
        style = "margin-top: -10px; font-size: 0.9em; color: #666"
      ),
      
      # -----------------------
      # Einleitungstext
      tags$p(
        "Diese App wurde im Rahmen eines Vortrags beim 5. Bochumer Hydrometrie- Kolloqiums am ",
        tags$a(href = "https://www.hochschule-bochum.de/fbb/einrichtungen-im-fachbereich/labore/labor-fuer-wasserbau/", "Lehrgebiet Wasserbau und Hydromechanik (LWH)"),
        " der Hochschule Bochum entwickelt."
      ),
      
      # -----------------------
      # Lizenz-Information
      tags$p(
        "Dieses Tool steht unter der ",
        tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.de.html", "GPL v3"),
        "-Lizenz. Du kannst den Quellcode unter den Bestimmungen von GPL v3 einsehen, verändern und weiterverbreiten."
      ),
      
      # -----------------------
      # Liste der verwendeten R-Pakete
      tags$h4("Verwendete R-Pakete"),
      tags$ul(
        tags$li(tags$b("shiny,"), "Chang et al. (2018)"),
        tags$li(tags$b("bslib,"), "Chang & Borges Ribeiro (2018)"),
        tags$li(tags$b("ggplot2,"), "Wickham (2016)"),
        tags$li(tags$b("dplyr,"), "Wickham et al. (2018)"),
        tags$li(tags$b("tidyr,"), "Wickham et al. (2018)"),
        tags$li(tags$b("plotly,"), "Sievert (2020)")
      ),
      
      # -----------------------
      # Kontakt
      tags$h4("Kontakt"),
      tags$p(
        "Bei Fragen und Feedback wende dich bitte an: ",
        tags$a(href = "mailto:felix.simon@hs-bochum.de", "Felix Simon")
      ),
      
      # -----------------------
      # Literaturverzeichnis
      tags$h4("Literatur & Referenzen"),
      tags$ul(
        tags$li(
          "Simon, F., Gaj, M., Schwarting, J., Mudersbach, C. (2025): Bewertung von Durchflussmessungen in Bezug auf die Messlotrechtenanzahl 2.0, WasserWirtschaft (7-8/2025), DOI: ",
          tags$em("10.1007/s35147-025-2532-z"), " ",
          tags$a(
            href = "https://doi.org/10.1007/s35147-025-2532-z",
            "https://doi.org/10.1007/s35147-025-2532-z",
            target = "_blank"
          )
        ),
        tags$li(
          "Chang, W., Cheng, J., Allaire, J., Das, S., Xie, Y., & McPherson, J. (2018). ",
          tags$em("shiny: Web Application Framework for R."), " ",
          tags$a(
            href = "https://CRAN.R-project.org/package=shiny",
            "https://CRAN.R-project.org/package=shiny",
            target = "_blank"
          )
        ),
        tags$li(
          "Chang, W., & Borges Ribeiro, B. (2018). ",
          tags$em("bslib: 'Bootstrap' Themes for 'Shiny'."), " ",
          tags$a(
            href = "https://CRAN.R-project.org/package=bslib",
            "https://CRAN.R-project.org/package=bslib",
            target = "_blank"
          )
        ),
        tags$li(
          "Wickham, H. (2016). ",
          tags$em("ggplot2: Elegant Graphics for Data Analysis."), " Springer-Verlag New York. ",
          tags$a(
            href = "http://ggplot2.org",
            "http://ggplot2.org",
            target = "_blank"
          )
        ),
        tags$li(
          "Wickham, H., François, R., Henry, L., & Müller, K. (2018). ",
          tags$em("dplyr: A Grammar of Data Manipulation."), " ",
          tags$a(
            href = "https://CRAN.R-project.org/package=dplyr",
            "https://CRAN.R-project.org/package=dplyr",
            target = "_blank"
          )
        ),
        tags$li(
          "Wickham, H., & Henry, L. (2018). ",
          tags$em("tidyr: Tidy Messy Data."), " ",
          tags$a(
            href = "https://CRAN.R-project.org/package=tidyr",
            "https://CRAN.R-project.org/package=tidyr",
            target = "_blank"
          )
        ),
        tags$li(
          "Sievert, C. (2020). ",
          tags$em("Interactive Web-Based Data Visualization with 'plotly' in R."), " ",
          tags$a(
            href = "https://plotly.com/r/",
            "https://plotly.com/r/",
            target = "_blank"
          )
        )
      )
    ))
  })
  
  # Hilfe-Modal anzeigen
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Kurzanleitung zur Nutzung von HydroEdit",
      tags$div(
        tags$ul(
          tags$li(strong("Datei hochladen:"), " Hier können Sie Ihre .tsv-Messdatei hochladen."),
          tags$li(strong("Zweipunkt-Kreps-Berechnung:"), " Aktivieren, falls bei 2-Punkt-Messung die Kreps-Formel verwendet werden soll."),
          tags$li(strong("Durchflussverfahren:"), " Wählen Sie zwischen 'Querschnittsmitte', (Standard) und 'Mittlerer Querschnitt'."),
          tags$li(strong("Lotrechte entfernen:"), " Mit den Buttons entfernen Sie ganze Lotrechten; mit Aktivieren von 'Einzelne Messpunkte entfernen' klicken Sie auf einen Punkt im Plot, um diesen einzelnen Messwert aus der Lotrechte zu löschen."),
          tags$li(strong("Werte anzeigen / Vergleich:"), " Nach jeder Änderung können Sie den aktuellen Durchfluss & die Fläche sehen und mit 'Werte hinzufügen' in den Vergleichs-Tab übernehmen."),
          tags$li(strong("Reset:"), " Setzt alle Änderungen an den Lotrechten zurück.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Schließen")
      
    ))
  })
  
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
  
  # Datei einlesen und standardisieren (mit Fallback bei Kopfzeilen-Fehler)
  observeEvent(input$file, {
    req(input$file)
    
    # 1) Typ ermitteln
    filetype <- detectFileType(input$file$datapath)
    
    # 1a) Feedback geben
    system_name <- switch(
      filetype,
      OTT_MF_pro_v205_plus = "OTT MF pro (Firmware ≥ v2.05)",
      OTT_MF_pro_v200      = "OTT MF pro (Firmware v2.00)",
      OTT_MF_pro_v106      = "OTT MF pro (Firmware v1.06)",
      generic              = "unbekanntes oder generisches Format"
    )
    
    showNotification(
      paste0("Datei erkannt als: ", system_name),
      type = "message", duration = 5
    )
    
    # 2) Reader aufrufen, mit Fallback auf generic, wenn Kopfzeile fehlt
    df_raw <- tryCatch(
      {
        switch(filetype,
               OTT_MF_pro_v205_plus = read_OTT_MF_pro_v205_plus(input$file$datapath),
               OTT_MF_pro_v200      = read_OTT_MF_pro_v200(input$file$datapath),
               OTT_MF_pro_v106      = read_OTT_MF_pro_v106(input$file$datapath),
               generic              = read_generic(input$file$datapath)
        )
      },
      error = function(e) {
        if (grepl("Messdaten-Kopfzeile nicht gefunden", e$message)) {
          showNotification(
            "Automatischer Import fehlgeschlagen – verwende generischen Import.",
            type = "warning",
            duration = 5
          )
          return(read_generic(input$file$datapath))
        } else {
          # alle anderen Fehler weiterwerfen
          stop(e)
        }
      }
    )
    
    # 2a) Fallback, falls der spezialisierte Reader kein gültiges Daten-Frame geliefert hat:
    # Fallback nur, wenn wirklich weder "Zeit" noch "Lotrechte" gefunden wurden
    if (!any(c("Zeit", "Lotrechte") %in% colnames(df_raw))) {
      showNotification(
        "Import liefert weder 'Zeit' noch 'Lotrechte' – verwende Fallback skip=29.",
        type = "error", duration = 5
      )
      df_raw <- read.table(
        input$file$datapath,
        skip   = 29,
        header = TRUE,
        sep    = "\t",
        dec    = ",",
        stringsAsFactors = FALSE
      )
    }
    
    # 3) Debug-Ausgabe
    cat(">>> Spalten nach Import:\n")
    print(colnames(df_raw))
    cat("<<< Methoden:\n")
    print(unique(df_raw$Methode))
    print(df_raw$Methode.1)
    
    # 4) Entferne mögliche X-Spalte
    df_raw <- df_raw %>% select(-tidyr::any_of("X"))
    
    # 5) Umbenennen der Messspalten auf Standardschema
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
    for (newName in names(new_names)) {
      oldName <- new_names[[newName]]
      if (oldName %in% colnames(df_raw)) {
        df_raw <- df_raw %>% rename(!!newName := all_of(oldName))
      }
    }
    
    # 6) 0 in den Tiefen-Spalten → NA
    depthCols <- c("X0.2..m..", "X0.4..m..", "X0.5..m..", "X0.6..m..", 
                   "X0.62..m..", "X0.7..m..", "X0.8..m..", "X0.9..m..", 
                   "Oberfl. (m):", "Sohle (m):")
    for (cc in depthCols) {
      if (cc %in% colnames(df_raw)) {
        df_raw[[cc]] <- ifelse(df_raw[[cc]] == 0, NA, df_raw[[cc]])
      }
    }
    
    # 7) Methode-Spalte füllen (Fallback auf Methode.1)
    if (!"Methode" %in% names(df_raw) ||
        all(is.na(df_raw$Methode)) ||
        all(df_raw$Methode == "")) {
      if ("Methode.1" %in% names(df_raw)) {
        df_raw$Methode <- df_raw$`Methode.1`
      } else {
        df_raw$Methode <- NA_character_
      }
    }
    
    # 8) in reactives schreiben
    original_data(df_raw)
    standard <- standardizeDataset(df_raw, useKreps = input$zweipunkt_kreps)
    cat("<<<< Standard-Datensatz:\n")
    print(standard$df_line$Method)
    active_data_line(standard$df_line)
    cat(">>> Kontrolle nach Setzen von active_data_line:\n")
    print(unique(active_data_line()$Method))
    active_data_points(standard$df_points)
    
    # 9) Metadaten + Vergleichsdaten wie gehabt
    meta_pegel <- read_meta_data_by_filetype(input$file$datapath, filetype)
    meta_data(meta_pegel)
    
    pegelinfo <- read_pegel_info_by_filetype(input$file$datapath, filetype)
    pegel_info(pegelinfo)
    
    vergleich_data_start <- data.frame(
      Anzahl_Lotrechten = meta_data()[1,2],
      Q                = meta_data()[3,2],
      A                = meta_data()[4,2],
      stringsAsFactors = FALSE
    )
    vergleich_data_start$Anzahl_Lotrechten <- factor(vergleich_data_start$Anzahl_Lotrechten)
    original_data_vergleich(vergleich_data_start)
    active_data_vergleich(vergleich_data_start)
  })
  
  observe({
    if(is.null(input$file)) {
      defaultDF <- createDefaultData()
      # defaultDF <- generate_default_dataset()
      standard <- standardizeDataset(defaultDF, useKreps = input$zweipunkt_kreps)
      original_data(defaultDF)
      active_data_line(standard$df_line)
      active_data_points(standard$df_points)
      
      meta_data(data.frame(
        Info = c("nLot", "Pegel", "Qursprung", "Aursprung"),
        Wert = c(nrow(defaultDF), "Pegel XY", 1.234, 5.678),
        stringsAsFactors = FALSE
      ))
      
      pegel_info(
        data.frame(
          V1 = "Profilname",
          V2 = "BEISPIELDATENSATZ",
          V3 = "2099-01-01",
          stringsAsFactors = FALSE
        )
      )
      
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
                                  if_else(!is.na(suppressWarnings(as.numeric(Method))),
                                          methodLabels[as.numeric(Method) + 1],
                                          as.character(Method))))
    
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
      mutate(
        b = as.numeric(b),
        h = as.numeric(h),
        v = as.numeric(v),
        q = 0
      )
    
    # Optional: Debug-Ausgabe bei fehlerhaften Werten
    if (any(is.na(df_calc$b)) || any(is.na(df_calc$h)) || any(is.na(df_calc$v))) {
      warning("Nicht-numerische Werte in 'Lage', 'Tiefe' oder 'Geschwindigkeit' – Durchflussberechnung könnte fehlschlagen.")
      print(str(df_calc))
    }
    
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
    
    df_calc <- dfL %>% 
      select("Lotrechte", b = "Lage (m)", h = "Tiefe (m)") %>%
      mutate(
        b = as.numeric(b),
        h = as.numeric(h)
      )
    
    # Prüfe auf NA-Werte (z. B. durch nicht-numerische Eingaben)
    if (any(is.na(df_calc$b)) || any(is.na(df_calc$h))) {
      warning("Nicht-numerische Werte in 'Lage' oder 'Tiefe' – Flächenberechnung könnte fehlschlagen.")
      print(str(df_calc))
      querschnittsflaeche_aktuell(NA_real_)
      return()
    }
    
    A <- sum((df_calc$b[-1] - df_calc$b[-n]) * (df_calc$h[-1] + df_calc$h[-n]) / 2, na.rm = TRUE)
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
    
    print("=== DEBUG active_data_points() ===")
    print(str(active_data_points()))
    print(head(active_data_points()))
    print("Spaltennamen:")
    print(colnames(active_data_points()))
    
    req(active_data_line(), active_data_points(), pegel_info())
    
    df_line <- active_data_line()
    df_points <- active_data_points()
    
    # Zwangskonvertierung der Spalten
    df_line$`Lage (m)` <- as.numeric(df_line$`Lage (m)`)
    df_line$`Tiefe (m)` <- as.numeric(df_line$`Tiefe (m)`)
    
    df_points$`Lage (m)` <- as.numeric(df_points$`Lage (m)`)
    df_points$x_m <- as.numeric(df_points$x_m)
    df_points$v_ms <- as.numeric(df_points$v_ms)
    
    # Prüfe, ob die relevanten Spalten gültige Werte haben
    if (all(is.na(df_line$`Lage (m)`)) || all(is.na(df_line$`Tiefe (m)`))) {
      warning("Plot kann nicht erzeugt werden – 'Lage (m)' oder 'Tiefe (m)' enthält nur NAs.")
      return(NULL)
    }
   
    
    p <- ggplot() +
      geom_line(data = df_line, aes(x = `Lage (m)`, y = `Tiefe (m)`)) +
      geom_point(
        data = df_points %>%
          mutate(
            Messpunkt_orig = as.character(Messpunkt),
            Messpunkt = factor(Messpunkt,
                               levels = c("Oberfl.", "0,2", "0,4", "0,5", "0,6", "0,62", "0,7", "0,8", "0,9", "Sohle"),
                               labels = c("Oberfl.", "20%", "40%", "50%", "60%", "62%", "70%", "80%", "90%", "Sohle")
            )
          ),
        aes(
          x = `Lage (m)`,
          y = x_m,
          color = Messpunkt,
          text = paste("Lage:", sprintf("%.2f", `Lage (m)`), "m",
                       "\nTiefe:", sprintf("%.2f", x_m), "m (", Messpunkt, ")",
                       "\nGeschwindigkeit:", sprintf("%.3f", v_ms), "m/s"),
          customdata = paste(`Lotrechte`, Messpunkt_orig, sep = "_")
        )
      ) +
      labs(
        x = "Lage (m)", y = "Tiefe (m)",
        title = paste("Messung:", pegel_info()[1, 2]),
        caption = paste("Messdatum:", pegel_info()[3, 2]),
        color = "Messpunkt"
      ) +
      scale_x_continuous(
        breaks = seq(
          round(min(df_line$`Lage (m)`, na.rm = TRUE), 2),
          round(max(df_line$`Lage (m)`, na.rm = TRUE), 2),
          0.5
        )
      ) +
      scale_y_reverse() +
      scale_color_manual(values = c(
        "Oberfl." = "black", "20%" = "red", "40%" = "orange", "50%" = "purple",
        "60%" = "green", "62%" = "darkgreen", "70%" = "pink",
        "80%" = "blue", "90%" = "brown", "Sohle" = "grey"
      )) +
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