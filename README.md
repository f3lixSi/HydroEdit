
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# HydroEdit – Shiny-App zur Auswertung von OTT MF pro Messdaten

HydroEdit ist eine interaktive R-Shiny-Anwendung zur Visualisierung, Standardisierung und Auswertung von Durchflussmessungen mit OTT MF pro-Geräten (Firmware v1.06, v2.00, v2.05+). Ziel ist es, hydrometrische Querschnittsdaten einfach einlesen, aufbereiten und grafisch analysieren zu können.

Lizenz: GNU General Public License v3.0 (GPL v3). Siehe LICENSE-Datei.

## :wrench: Funktionen

- Automatische Erkennung der Firmware-Version (v1.06, v2.00, v2.05+)
- Standardisierte Aufbereitung der Messdaten
- Berechnung der mittleren Geschwindigkeit (vm) je nach Messmethode (inkl. 2-Punkt-Kreps)
- Methodenabhängiges Setzen nicht gemessener Punkte auf NA
- Interaktive Visualisierung des Querschnitts mit Tiefen und Geschwindigkeiten
- Anzeige von Metadaten (z. B. Q, A, n_Lotrechten) und Vergleich mit Originaldaten

## :floppy_disk: Eingabedaten

Die App unterstützt folgende Formate:

| Firmware | Status             | Messsystem     |
|----------|--------------------|----------------|
| v1.06    | :white_check_mark: | OTT MF pro     |
| v2.00    | :white_check_mark: | OTT MF pro     |
| v2.05+   | :white_check_mark: | OTT MF pro     |

## :arrow_forward: Starten der App

```r
# In RStudio oder R-Konsole:
shiny::runApp("Pfad/zur/App")
```

Oder direkt per Klick auf “Run App” in RStudio.

Oder direkt online nutzen unter:  
👉 [HydroEdit auf Posit Cloud](https://felixsimon-hydroedit.share.connect.posit.cloud/)

## :package: Abhängigkeiten

- R (>= 4.1)
- Benötigte Pakete:
  - shiny
  - plotly
  - dplyr
  - tidyr
  - readr
  - stringr
  - lubridate
  - data.table
  - magrittr
  - shinyWidgets

Installation in R:

```r
install.packages(c("shiny", "plotly", "dplyr", "tidyr", "readr", "stringr", "lubridate", "data.table", "magrittr", "shinyWidgets"))
```

## :brain: Hintergrund

Diese Anwendung entstand im Rahmen hydrometrischer Untersuchungen zur Qualitätssicherung und Bewertung von Durchflussmessdaten. 
Sie dient u.a. zur methodischen Prüfung der eingesetzten Messverfahren und zur schnellen visuellen Kontrolle.
Zusätzlich können einzelne Messpunkte oder ganze Lotrechten entfernt werden. Der Druchfluss und die Querschnittsfläche wird entsprechend neu berechnet
und somit die Abweichungen bestimmt werden.

## :page_facing_up: Lizenz

MIT License – siehe LICENSE

## :bust_in_silhouette: Autor

**Felix Simon**
Hochschule Bochum
Lehrgebiet Wasserbau und Hydromechanik  
[https://github.com/f3lixSi](https://github.com/f3lixSi)
