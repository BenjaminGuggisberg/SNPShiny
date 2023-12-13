library(jsonlite)
library(sf)
library(RJSONIO)
library(dplyr)
library(tidyr)
library(ggplot2)

prepareRelativData <- function(data) {
  # Gruppiere nach id und finde die frÃ¼hste Startzeit
  result <- data %>%
    group_by(Place) %>%
    summarise(earliest_start_time = min(Time))
  
  # Zeige das Ergebnis
  # print(result[, c("Place", "earliest_start_time")])
  
  # Start der einzelnen Time-Sequenzen der File-IDs definieren
  dtstart <- result$earliest_start_time
  # print(dtstart)
  
  # Pivotiere die Daten
  dlong <- data %>%
    pivot_longer(cols = c("stille_count_mean", "vogel_count_mean", "natur_count_mean", "laerm_count_mean", "average_RMS_dB_mean"), names_to = "key", values_to = "values")
  
  
  # Return das Ergebnis
  return(list(result = result[, c("Place", "earliest_start_time")],
              dtstart = dtstart,
              dlong = dlong))
}




# createRelativPlot <- function(data, selected_time) {
#   data$values <- as.numeric(data$values)
#   
#   # Darstellung der Daten für mehrere Geräte
#   plot <- ggplot(data) +
#     geom_tile(aes(x = Time, y = key, fill = values, group = key), show.legend = FALSE) +
#     theme_minimal() +
#     labs(title = "Aggregation", x = "Zeit", y = "Klasse", fill = "Klasse", caption = ~Place) +
#     scale_fill_gradient(low = "white", high = "lightgreen") +
#     facet_wrap(~Place, ncol = 1) +
#     geom_vline(xintercept = selected_time, linetype = "dashed", color = "red") +
#     theme(legend.position="none") +
#     theme(legend.position="none") +
#     theme(axis.text.x=element_text(angle=45, hjust=1)) +
#     theme(legend.position="none") +
#     theme(strip.text = element_text(size = 6)) +
#     theme(legend.position="none")
#   
#   return(plot)
# }

createRelativPlot <- function(data, selected_time) {
  # print(data)
  data$values <- as.numeric(data$values)
  
  # Definiere die Reihenfolge der Werte in key umgekehrt
  key_order <- c("average_RMS_dB_mean", "laerm_count_mean", "natur_count_mean", "stille_count_mean", "vogel_count_mean")
  
  # Wende reorder auf die key-Spalte an, um die Reihenfolge beizubehalten
  data$key <- factor(data$key, levels = key_order)
  data$key <- reorder(data$key, as.numeric(data$key))
  
  # Definiere die gewünschte Reihenfolge der Klassen
  class_order <- c("vogel_count_mean", "stille_count_mean", "natur_count_mean", "laerm_count_mean")
  
  # Legende definieren
  alpha_legend <- data.frame(key = class_order, alpha = 1)
  
  plot <- ggplot(data) +
    geom_tile(aes(x = Time, y = key, fill = key, alpha = values)) +
    theme_minimal() +
    labs(title = "Aggregation", x = "Zeit", y = "Klasse", fill = "Klasse", caption = ~Place) +
    scale_fill_manual(values = c(
      "vogel_count_mean" = "#E67C3E",   # Rot
      "stille_count_mean" = "#64C6FA",  # Blau
      "natur_count_mean" = "#71FF5C",   # Grün
      "laerm_count_mean" = "#D34045",   # Orange
      "average_RMS_dB_mean" = "#000000" # Schwarz
    ), breaks = class_order, na.value = "grey90") +
    facet_wrap(~Place, ncol = 1) +
    geom_vline(xintercept = selected_time, linetype = "dashed", color = "red") +
    scale_x_discrete(breaks = unique(data$Time)[c(1, 17, 33, 49, 65, 81, 96)]) +
    scale_y_discrete(labels = c(
      "average_RMS_dB_mean" = "Volume [dB]",
      "laerm_count_mean" = "Noise",
      "natur_count_mean" = "Nature",
      "vogel_count_mean" = "Bird",
      "stille_count_mean" = "Silence"
    )) +
    guides(fill = "none", alpha = guide_legend(title = "Frequency")) +  
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(strip.text = element_text(size = 6))
  
  return(plot)
}



merge_dataframes_with_fixed_metadata <- function(df, by_x, by_y, all_x = TRUE) {
  standorte <- c("ls01", "ls02", "ls03", "ls04", "ls05", "ost-01-u", "ls06", "west-01-u", "ls07", "ls08", "ls09", "ost-02-u", "ost-02-o", "ost-03-u", "ost-03-o", "ost-04-u", "ost-04-o", "ost-05-u", "ost-05-o", "ost-05-m", "west-02-u", "west-02-o", "west-03-u", "west-03-o", "west-04-u", "west-04-o", "west-05-u", "west-05-o", "west-05-m")
  
  # Koordinatenliste erstellen (fix) im WGS84 Format
  lon <- c(10.1675962508331, 10.1673887658203, 10.1691194669723, 10.1721970879587, 10.1731328088571, 
           10.2235122241789, 10.2241650953879, 10.2327776440826, 10.2334304972921, 10.2554025907691, 
           10.2590165054098, 10.2332061358329, 10.2338590003952, 10.2336642591609, 10.2343171337194, 
           10.2339148551515, 10.2345677388336, 10.2341456471544, 10.2344720948978, 10.2344953751885, 
           10.2239961719573, 10.2246490547872, 10.2239686257749, 10.2246215204282, 10.2243292744421, 
           10.2249821788409, 10.2245958542118, 10.2252487702508, 10.2249455137181)
  
  lat <- c(46.6734405610732, 46.6661633067188, 46.6625238637962, 46.6605855065555, 46.6584038624856, 
           46.6613791256261, 46.6613631591673, 46.6601160204256, 46.6601000008722, 46.6532132288703, 
           46.6517341790277, 46.6610656885164, 46.6610496665105, 46.6619038136096, 46.661887788981, 
           46.66266466932, 46.6626486432578, 46.6636388003977, 46.6636307871744, 46.6640802785641, 
           46.6623522307682, 46.662336261539, 46.6633295903229, 46.6633136212544, 46.6641444980066, 
           46.664128526874, 46.6651138083342, 46.6650978356769, 46.6655553157365)
  
  
  # Erstelle ein DataFrame
  metadata <- data.frame(standort = standorte, lon = lon, lat = lat)
  
  # Merge basierend auf den angegebenen Spalten
  merged_df <- merge(df, metadata, by.x = by_x, by.y = by_y, all.x = all_x)
  # Rückgabe des gemergten DataFrames
  return(merged_df)
}

# Zuvor: size = 100
# stroke='#ffffff' stroke-width='1' 
# Jetzt: size = doppelter dB Wert zur Veranschaulichung der Lautstärke auf der Karte
draw_svg <- function(dB, natur, laerm, vogel, stille, name) {
  size <- dB * 2
  
  values <- c(natur,laerm,vogel,stille)
  
  max <- max(values)
  
  values <- values/max*size
  
  
  # cat("size: ", size, "\n")
  
  natur_x <- size - values[1]
  natur_y <- size - values[1]
  laerm_x <- size
  laerm_y <- size - values[2]
  vogel_x <- size - values[3]
  vogel_y <- size
  stille_x <- size
  stille_y <- size
  
  svg <- paste(
    paste("<svg xmlns='http://www.w3.org/2000/svg' width='", as.character(size*2), "' height='", as.character(size*2), "' viewBox='0 0", as.character(size*2), as.character(size*2),"'>"),
    "<!-- Quadrat 1 -->",
    paste("<rect x='", natur_x, "' y='", natur_y, "' width='", values[1], "' height='", values[1], "' fill='#71FF5C' />", sep=''),
    "<!-- Quadrat 2 -->",
    paste("<rect x='", laerm_x, "' y='", laerm_y, "' width='", values[2], "' height='", values[2], "' fill='#D34045' />", sep=''),
    "<!-- Quadrat 3 -->",
    paste("<rect x='", vogel_x, "' y='", vogel_y, "' width='", values[3], "' height='", values[3], "' fill='#E67C3E' />", sep=''),
    "<!-- Quadrat 4 -->",
    paste("<rect x='", stille_x, "' y='", stille_y, "' width='", values[4], "' height='", values[4], "' fill='#64C6FA' />", sep=''),
    "</svg>"
  )
  
  svg_path <- paste("glyphs/", name, ".svg", sep='')
  writeLines(svg, svg_path)
  # close(svg_path)
  
  return(svg_path)
  
}

draw_multiple_svg <- function(dB, natur, laerm, vogel, stille, name) {
  size <- dB
  
  values <- c(natur,laerm,vogel,stille)
  
  max <- max(values)
  
  values <- values/max*size
  
  
  # cat("size: ", size, "\n")
  
  natur_x <- size - values[1]
  natur_y <- size - values[1]
  laerm_x <- size
  laerm_y <- size - values[2]
  vogel_x <- size - values[3]
  vogel_y <- size
  stille_x <- size
  stille_y <- size
  
  svg <- paste(
    paste("<svg xmlns='http://www.w3.org/2000/svg' width='", as.character(size*2), "' height='", as.character(size*2), "' viewBox='0 0", as.character(size*2), as.character(size*2),"'>"),
    "<!-- Quadrat 1 -->",
    paste("<rect x='", natur_x, "' y='", natur_y, "' width='", values[1], "' height='", values[1], "' fill='#71FF5C' />", sep=''),
    "<!-- Quadrat 2 -->",
    paste("<rect x='", laerm_x, "' y='", laerm_y, "' width='", values[2], "' height='", values[2], "' fill='#D34045' />", sep=''),
    "<!-- Quadrat 3 -->",
    paste("<rect x='", vogel_x, "' y='", vogel_y, "' width='", values[3], "' height='", values[3], "' fill='#E67C3E' />", sep=''),
    "<!-- Quadrat 4 -->",
    paste("<rect x='", stille_x, "' y='", stille_y, "' width='", values[4], "' height='", values[4], "' fill='#64C6FA' />", sep=''),
    "</svg>"
  )
  
  svg_path <- paste("overview_sites/", name, ".svg", sep='')
  writeLines(svg, svg_path)
  # close(svg_path)
  
  return(svg_path)
  
}



