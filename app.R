library(shiny)
library(mongolite)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(rsconnect)
source("functions/functions.R")

timeslots <- c("00:00:00", "00:15:00", "00:30:00", "00:45:00", "01:00:00", "01:15:00", "01:30:00", "01:45:00", "02:00:00", "02:15:00", "02:30:00", "02:45:00",
               "03:00:00", "03:15:00", "03:30:00", "03:45:00", "04:00:00", "04:15:00", "04:30:00", "04:45:00", "05:00:00", "05:15:00", "05:30:00", "05:45:00",
               "06:00:00", "06:15:00", "06:30:00", "06:45:00", "07:00:00", "07:15:00", "07:30:00", "07:45:00", "08:00:00", "08:15:00", "08:30:00", "08:45:00",
               "09:00:00", "09:15:00", "09:30:00", "09:45:00", "10:00:00", "10:15:00", "10:30:00", "10:45:00", "11:00:00", "11:15:00", "11:30:00", "11:45:00",
               "12:00:00", "12:15:00", "12:30:00", "12:45:00", "13:00:00", "13:15:00", "13:30:00", "13:45:00", "14:00:00", "14:15:00", "14:30:00", "14:45:00",
               "15:00:00", "15:15:00", "15:30:00", "15:45:00", "16:00:00", "16:15:00", "16:30:00", "16:45:00", "17:00:00", "17:15:00", "17:30:00", "17:45:00",
               "18:00:00", "18:15:00", "18:30:00", "18:45:00", "19:00:00", "19:15:00", "19:30:00", "19:45:00", "20:00:00", "20:15:00", "20:30:00", "20:45:00",
               "21:00:00", "21:15:00", "21:30:00", "21:45:00", "22:00:00", "22:15:00", "22:30:00", "22:45:00", "23:00:00", "23:15:00", "23:30:00", "23:45:00")

sites <- c("ls01", "ls02", "ls03", "ls04", "ls05", "ost-01-u", "ls06", "west-01-u", "ls07", 
           "ls08", "ls09", "ost-02-u", "ost-02-o", "ost-03-u", "ost-03-o", "ost-04-u", "ost-04-o", 
           "ost-05-u", "ost-05-o", "ost-05-m", "west-02-u", "west-02-o", "west-03-u", "west-03-o", 
           "west-04-u", "west-04-o", "west-05-u", "west-05-o", "west-05-m")

ui <- fluidPage(
  tags$head(tags$script(src = "https://use.fontawesome.com/releases/v5.15.3/js/all.js")),
  tags$head(
    tags$style(HTML("
      /* Fügen Sie benutzerdefiniertes CSS hier ein */
      body {
        max-height: 100vh;
        overflow-y: auto;
      }
      .spacer {
        margin-bottom: 40px; /* Ändern Sie dies nach Bedarf für den gewünschten Abstand */
      }
      .spacer2 {
      margin-bottom: 15px;
      }
      .spacer3 {
      margin-bottom: 25px;
      }
      .info-content {
        display: none; /* Initial verstecken */
      }
      .nav-tabs > li > a {
                    color: #92a8D1; /* Ändere dies zu deiner gewünschten Farbe */
      }
      h4 {
        color: black;
      }
      .custom-button {
        float: right;
        background: none;
        border: none;
      }
      .custom-button:hover {
        border: 1px solid #92a8D1;
        background: none;
      }
    "))
  ),
  tags$h1("Audio analysis Swiss National Park"),
  tags$h4("Study on the influence of anthropogenic noise on birds"),
  tags$div(class = "spacer2"),  
  
  tabsetPanel(
    tabPanel(
      "Aggregated Analysis",
      tags$div(class = "spacer3"),
      sidebarLayout(
        sidebarPanel(
          style = "height: 80vh;",
          fluidRow(
            column(12, dateRangeInput("date_range", "Date Range", 
                                      start = "2023-06-01", end = "2023-06-30",
                                      separator = " - "))
          ),
          fluidRow(
            column(12, actionButton("info_button", "ℹ️ data availability", 
                                    style = "color: #92a8D1; background-color: transparent; border: none; padding: 0;"),
                   align = "right")
          ),
          conditionalPanel(
            condition = "input.info_button % 2 == 1",
            tags$p(HTML("
            <div style='color: black; background: white; padding: 5px; border: 1px solid lightgrey; border-radius: 5px'>
            <p style='font-family: Arial, sans-serif; font-size: 13px;'>Langzeitmessstationen</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>LS01, LS02, LS03,LS04, LS05, ost-01 (LS06), west-01 (LS07), LS08, LS09 </p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Von: 2023-05-12 Bis: 2023-08-15</p>
            <p style='font-family: Arial, sans-serif; font-size: 13px;'>Querachsen</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Ost- und Westachse</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Von: 2023-09-06 Bis: 2023-09-12</p>
            </div>"))
          ),
          selectInput("inputLocation", "Logger prefix", choices = c("Along the road", "Transverse axis", "All")),
          selectInput("inputLogger", "Logger", choices = sites),
          selectInput("ClassSelector", "Select Classes", choices = c("All Classes", "Specific Classes"), selected = "All Classes"),
          selectInput("inputWeekday", "Weekday", choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
          conditionalPanel(
            condition = "input.ClassSelector == 'Specific Classes'",
            checkboxGroupInput("class_checkbox", "Select Classes",
                               choices = c("stille_count_mean", "vogel_count_mean", "laerm_count_mean", "natur_count_mean", "average_RMS_dB_mean"),
                               selected = c("stille_count_mean", "vogel_count_mean", "laerm_count_mean", "natur_count_mean"),
            )
          ),
          sliderInput("time_slider", "Select Time", 
                      min = 0, 
                      max = 95,  # 96 intervals, starting from 0
                      value = 48, 
                      step = 1,
                      ticks = FALSE),
          div(textOutput("selected_time_text"), style = "text-align: center;"),
          tags$div(HTML("<br>")),
          actionButton("back_button", label = "15 Min", icon = icon("chevron-left"), class = "custom-button", style="float: left;"),
          actionButton("next_button", HTML("15 Min <i class='fas fa-chevron-right'></i>"), class = "custom-button"),
          tags$div(HTML("<br><br><br>")),
          actionButton("submit", "Plot Data")
        ),
        
        mainPanel(
          conditionalPanel(
            condition = "!output.snp_plot",
            tags$div(
              style = "border: 2px dashed #ccc; padding: 20px; text-align: center; color: #777; height: 45vh; margin-bottom: 5vh;",
              "Waiting for plot..."
            ),
            tags$div(
              style = "margin-top: 20px;",
              leafletOutput("defaultmap", height = "30vh")
            )
          ),
          # conditionalPanel(
            # condition = "plot_frame !== null",
            plotOutput("snp_plot"),
            leafletOutput("map", height = "325"),
          # )
        )
      )
    ),
    tabPanel(
      "Overview Sites",
      tags$div(class = "spacer3"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12, dateRangeInput("date_range2", "Date Range", 
                                      start = "2023-06-01", end = "2023-06-30",
                                      separator = " - "))
          ),
          fluidRow(
            column(12, actionButton("info", "ℹ️ data availability", 
                                    style = "color: #92a8D1; background-color: transparent; border: none; padding: 0;"),
                   align = "right")
          ),
          conditionalPanel(
            condition = "input.info % 2 == 1",
            tags$p(HTML("
            <div style='color: black; background: white; padding: 5px; border: 1px solid lightgrey; border-radius: 5px'>
            <p style='font-family: Arial, sans-serif; font-size: 13px;'>Langzeitmessstationen</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>LS01, LS02, LS03,LS04, LS05, ost-01 (LS06), west-01 (LS07), LS08, LS09 </p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Von: 2023-05-12 Bis: 2023-08-15</p>
            <p style='font-family: Arial, sans-serif; font-size: 13px;'>Querachsen</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Ost- und Westachse</p>
            <p style='font-family: Arial, sans-serif; font-size: 10px;'>Von: 2023-09-06 Bis: 2023-09-12</p>
            </div>"))
          ),
          selectInput("location_choice", "Location Selection", 
                      choices = c("Specific Locations", "All Locations")),
          conditionalPanel(
            condition = "input.location_choice == 'Specific Locations'",
            selectInput("inputLocation2", "Logger prefix", choices = c("Along the road", "Transverse axis", "All")),
            selectInput("selectedStandorte", "Choose Locations (min. 2)", sites, 
                        multiple = TRUE)
          ),
          selectInput("inputweekday", "Weekday", choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
          selectInput("inputTime", "Select Time", choices = timeslots, selected = "12:00:00"),
          actionButton("back_overview", label = "15 Min", icon = icon("chevron-left"), class = "custom-button", style="float: left;"),
          actionButton("next_overview", HTML("15 Min <i class='fas fa-chevron-right'></i>"), class = "custom-button"),
          tags$div(HTML("<br><br><br>")), 
          actionButton("overviewer", "Show Map"),
          ),
        
        mainPanel(
          leafletOutput("overviewMap", height = "600", width = "100%"),
        )
      )
    ),
    tabPanel(
      "Metadata description",
      fluidRow(
        column(
          width = 9,
          tags$div(
            h3("Glossary"),
            HTML(
              "
              <div style='padding-left: 20px'>
                <table style='width:100%; border-collapse: collapse;'>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Date Range</td>
                    <td style='padding: 8px;'>By entering the date range, you can control a time period for calculating the averaging of the bird, noise, nature, and silence classes in the 15-minute intervals.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Logger</td>
                    <td style='padding: 8px;'>The selection of the location can be set with the Logger input (see map below).</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Weekday</td>
                    <td style='padding: 8px;'>The selection of Weekday defines which weekdays within the entered date range are taken into account for aggregation.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Select Classes</td>
                    <td style='padding: 8px;'>Select Classes is a purely visual setting parameter that can be used to display the individual classes in the aggregation plot.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Select Time</td>
                    <td style='padding: 8px;'>Select Time is an important setting parameter that can be applied within the averaged 24 hours to select a time window / 15-minute interval for displaying the averaged glyph. The selected time period for the glyph display on the minimap is indicated in the aggregation plot by a red dashed line.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>-15 MM / +15 MM</td>
                    <td style='padding: 8px;'>-15 MM / +15 MM is a fine-tuning parameter for Select Time for shifting in 15-minute intervals.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Location Selection</td>
                    <td style='padding: 8px;'>Location Selection defines the input for the location overview. With 'All Locations', the glyphs are displayed for all locations (reduces performance due to high computing power). With 'Specific Locations,' any locations can be visualized, at least 2 must be selected.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Logger prefix</td>
                    <td style='padding: 8px;'>A prefix can be entered in the 'Location prefix' to simplify the search in the 'Choose Location' filter.</td>
                  </tr>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Choose Location</td>
                    <td style='padding: 8px;'>Choose Locations is used to select any locations for 'Specific Locations' from the 'Location Selection.</td>
                  </tr>
                </table>
              </div>
              <br>
              <div style='padding-left: 20px'>
              <img src='soundlogger.png' alt='Soundlogger Uebersicht' style='width: 49%;'>
              <img src='soundloggerachsen.png' alt='Soundlogger-Achsen Uebersicht' style='width: 49%;'>
              </div>
              <br>
              <div style='padding-left: 20px'>
                <table style='width:100%; border-collapse: collapse;'>
                  <tr>
                    <td style='width: 15%; padding: 8px;'><img src='Glyphe.png' alt='Glyphe' style='width: 100%;'></td>
                    <td padding: 8px;'>The glyph is a graphical symbolization of the percentage distribution of the classes bird, noise, nature and silence at a specific 15-minute interval over an average period of time. The glyph has the following symbolism:
                    <ul style='list-style-type: none; padding-left: 0;'>
                      <li style='color: #D34045;'>&#9632; Noise</li>
                      <li style='color: #71FF5C;'>&#9632; Nature</li>
                      <li style='color: #64C6FA;'>&#9632; Silence</li>
                      <li style='color: #E67C3E;'>&#9632; Bird</li>
                    </ul>
                    </td>
                  </tr>
                </table>
              </div>
              <div style='padding-left: 20px'>
                <table style='width:100%; border-collapse: collapse;'>
                  <tr>
                    <td style='font-size: 13px; font-weight: bold; padding: 8px;'>Model Description</td>
                    <td style='padding: 8px;'>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</td>
                  </tr>
                </table>
              </div>
              <hr>
              "
            ),
            h3("Updates"),
            HTML("
              <div style='padding-left: 20px'>
              <br>
              <ul style='padding-left: 20;'>
                <li>Detail file analysis with spectogram & audio track</li>
                <li>Expansion of aggregated presentation to include additional locations for direct comparison</li>
                <li>Visualization of statistical analyses (ZCR, RMSE, etc.)</li>
              </ul>
              </div>
            ")
          )
        ),
        column(
          width = 2,
          style = "background-color: #f2f2f2; padding: 15px; height: 1300px; margin: 10px; ",
          # Hier können Sie zusätzlichen Inhalt für die rechte Spalte hinzufügen, wenn nötig
        )
      )
    ),
    tabPanel(
      "Contact us",
      fluidRow(
        column(
          width = 4,
          HTML('
           <div style="text-align: center;">
           <h3>Tim von Felten</h3>
           <br>
           <img src="Tiv.jpg" alt="Profilbild 1" style="border-radius: 50%; width: 70%;">
           <br><br>
           <p style="font-size: 14px"><strong>Implementation Deep-Learning</strong></p>
           <p>Contact: <a href="mailto:tim.vonfelten@students.fhnw.ch">tim.vonfelten@students.fhnw.ch</a></p>
           </div>
           ')
        ),
        column(
          width = 4,
          HTML('
           <div style="text-align: center;">
           <h3>Benjamin Guggisberg</h3>
           <br>
           <img src="BeG.jpg" alt="Profilbild 1" style="border-radius: 50%; width: 70%;">
           <br><br>
           <p style="font-size: 14px"><strong>Implementation R-Shiny Application</strong></p>
           <p>Contact: <a href="mailto:benjamin.guggisberg@students.fhnw.ch">benjamin.guggisberg@students.fhnw.ch</a></p>
           </div>
           ')
        ),
        column(
          width = 4,
          HTML('
           <div style="text-align: center;">
           <h3>Marosch Novak</h3>
           <br>
           <img src="MaN.jpg" alt="Profilbild 1" style="border-radius: 50%; width: 70%;">
           <br><br>
           <p style="font-size: 14px"><strong>Implementation Shiny for Python</strong></p>
           <p>Contact: <a href="marosch.novak@students.fhnw.ch">marosch.novak@students.fhnw.ch</a></p>
           </div>
           ')
        )
      )
    )
  )
)
  

server <- function(input, output, session) {
  
  observeEvent(input$inputLocation, {
    locations <- switch(input$inputLocation,
                        "All" = sites,
                        "Along the road" = grep("^ls", sites, value = TRUE),
                        "Transverse axis" = grep("^ost-|^west-", sites, value = TRUE))
    updateSelectInput(session, "inputLogger", choices = locations)
  })
  observeEvent(input$inputLocation2, {
    locations <- switch(input$inputLocation2,
                        "All" = sites,
                        "Along the road" = grep("^ls", sites, value = TRUE),
                        "Transverse axis" = grep("^ost-|^west-", sites, value = TRUE))
    updateSelectInput(session, "selectedStandorte", choices = locations)
  })
  
  output$defaultmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 9.8277, lat = 46.8182, zoom = 8)  
  })
  
  connection_string  <- 'mongodb://127.0.0.1:27017'
  snp_collection <- mongo(collection = "aggregated_files_per_day", db = "ClassifiedAudioSNP", url = connection_string)
  

  observeEvent(input$submit, {
    if (input$inputWeekday == "All") {
      query <- paste0('{
      "Date": {
        "$gte": "', input$date_range[1], '",
        "$lte": "', input$date_range[2], '"
      },
      "Place": "', input$inputLogger, '"
    }')
    } else {
      query <- paste0('{
      "Date": {
        "$gte": "', input$date_range[1], '",
        "$lte": "', input$date_range[2], '"
      },
      "Place": "', input$inputLogger, '",
      "Weekday": "', input$inputWeekday, '"
    }')
    }
    daten <- snp_collection$find(query)
    result_df <- as.data.frame(daten)
    # print(result_df)
    if (is.null(result_df) || nrow(result_df) < 1) {  
      showModal(
        modalDialog(
          title = "No Data :(",
          "Couldn't find Data! Choose another date for data aggregation!",
          easyClose = TRUE
        )
      )
      return()  
    }
    
    print(result_df)

    # Initialisiere leeres DataFrame
    merged_df <- data.frame()
    
    # Iteriere über jedes DataFrame
    for (i in 1:length(result_df[[4]])) {
      # Berechne den Mittelwert für jedes DataFrame
      mean_df <- aggregate(. ~ Time, data = result_df[[4]][[i]], mean)
      
      # Benenne die Spalten um, um Konflikte bei der Zusammenführung zu vermeiden
      col_names <- paste(c("stille_count", "vogel_count", "laerm_count", "natur_count", "average_RMS_dB"), "_df", i, sep = "")
      names(mean_df)[names(mean_df) != "Time"] <- col_names
      
      # Merge mit dem bisherigen Ergebnis-DataFrame
      if (i == 1) {
        merged_df <- mean_df
      } else {
        # Schrittweises Merging
        merged_df <- left_join(merged_df, mean_df, by = "Time")
      }
    }
    
    # print(merged_df)
    
    # # Berechne den Mittelwert über alle DataFrames für jede numerische Spalte
    # merged_df$stille_count_mean <- rowMeans(merged_df[, grepl("stille_count_df", names(merged_df))], na.rm = TRUE)
    # merged_df$vogel_count_mean <- rowMeans(merged_df[, grepl("vogel_count_df", names(merged_df))], na.rm = TRUE)
    # merged_df$laerm_count_mean <- rowMeans(merged_df[, grepl("laerm_count_df", names(merged_df))], na.rm = TRUE)
    # merged_df$natur_count_mean <- rowMeans(merged_df[, grepl("natur_count_df", names(merged_df))], na.rm = TRUE)
    # merged_df$average_RMS_dB_mean <- rowMeans(merged_df[, grepl("average_RMS_dB_df", names(merged_df))], na.rm = TRUE)
    # 
    # Berechne den Mittelwert über alle DataFrames für jede numerische Spalte
    if (nrow(merged_df) > 0) {
      merged_df <- merged_df %>%
        rowwise() %>%
        mutate(
          stille_count_mean = mean(c_across(contains("stille_count_df")), na.rm = TRUE),
          vogel_count_mean = mean(c_across(contains("vogel_count_df")), na.rm = TRUE),
          laerm_count_mean = mean(c_across(contains("laerm_count_df")), na.rm = TRUE),
          natur_count_mean = mean(c_across(contains("natur_count_df")), na.rm = TRUE),
          average_RMS_dB_mean = mean(c_across(contains("average_RMS_dB_df")), na.rm = TRUE)
        )
    }
    
    # Erstelle das finale DataFrame
    result_mean_df <- data.frame(Time = merged_df$Time, 
                                 stille_count_mean = merged_df$stille_count_mean,
                                 vogel_count_mean = merged_df$vogel_count_mean,
                                 laerm_count_mean = merged_df$laerm_count_mean,
                                 natur_count_mean = merged_df$natur_count_mean,
                                 average_RMS_dB_mean = merged_df$average_RMS_dB_mean)
    
    
    
    # Assuming that result_df[[4]][[1]] has the column "Place"
    result_mean_df$Place <- rep(result_df$Place[1], length.out = nrow(result_mean_df))
    result <- prepareRelativData(result_mean_df)
    
    selected_classes <- input$class_checkbox
    plot_frame <- result$dlong %>% filter(key %in% selected_classes)
    # print(result_mean_df)
    # Initializing Glyphs
    glyphs <- merge_dataframes_with_fixed_metadata(result_mean_df, "Place", "standort", TRUE)
    
    # Variable to store selected time
    selected_time <- reactiveVal(NULL)
    
    # Funktion zum Zeichnen der Glyphen
    drawGlyphs <- function(timeIndex) {
      i <- timeIndex + 1
      original_time <- as.character(glyphs$Time[i])
      nametag_time <- gsub(":", "_", original_time)
      svg_path <- draw_svg(glyphs$average_RMS_dB_mean[i], glyphs$natur_count_mean[i], glyphs$laerm_count_mean[i], glyphs$vogel_count_mean[i], glyphs$stille_count_mean[i], nametag_time)

      # Marker zur Karte Aggregation hinzufügen
      leafletProxy("map", data = glyphs) %>%
        clearMarkers() %>%
        addMarkers(
          lng = ~lon,
          lat = ~lat,
          icon = makeIcon(iconUrl = svg_path, iconWidth = 80, iconHeight = 80),
          label = ~Place
          # label = ~as.character(Place), 
          # labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, direction = "auto")
        )
      selected_time(as.POSIXct(original_time, format = "%H:%M:%S"))
    }
    
    swissimageAttribution <- "© swisstopo (source for swissimage): WMS swisstopo"
    # Karte initialisieren
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "BaseTiles") %>%
        addMarkers(
          data = glyphs,
          lng = ~lon,
          lat = ~lat,
          icon = makeIcon(iconUrl = "", iconWidth = 80, iconHeight = 80)
        ) %>%
        addWMSTiles(
          baseUrl = "https://wms.geo.admin.ch/",
          layers = "ch.swisstopo.swissimage",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          attribution = swissimageAttribution,
          group = "SwissImage"
        ) %>%
        addLayersControl(
          baseGroups = c("BaseTiles", "SwissImage"),  
          options = layersControlOptions(collapsed = FALSE)  
        )
    })
    
    observe({
      leafletProxy("map") %>%
        addControl(
          html = HTML("
        <div style='background: white; padding: 10px'>
          <p><strong>Legend</strong></p>
          <ul style='list-style-type: none; padding-left: 0;'>
            <li style='color: #D34045;'>&#9632; Noise</li>
            <li style='color: #71FF5C;'>&#9632; Nature</li>
            <li style='color: #64C6FA;'>&#9632; Silence</li>
            <li style='color: #E67C3E;'>&#9632; Bird</li>
          </ul>
        </div>
      "),
          position = "bottomleft"
        )
    })

    output$snp_plot <- renderPlot({
      createRelativPlot(plot_frame, input$time_slider)
    })
    observeEvent(input$time_slider, {
      drawGlyphs(input$time_slider)
    })
    observeEvent(input$next_button, {
      new_value <- input$time_slider + 1
      if (new_value <= 95) {
        updateSliderInput(session, "time_slider", value = new_value)
        drawGlyphs(new_value)
      }
    })
    observeEvent(input$back_button, {
      new_value <- input$time_slider - 1
      if (new_value >= 0) {
        updateSliderInput(session, "time_slider", value = new_value)
        drawGlyphs(new_value)
      }
    })
    output$selected_time_text <- renderText({
      if (!is.null(input$time_slider)) {
        selected_index <- input$time_slider
        selected_time <- result_mean_df$Time[selected_index + 1]  # Index + 1, da R bei 1 zu zählen beginnt
        paste("Your chosen time: ", selected_time)
      }
    })
    observeEvent(input$class_checkbox, {
      # Show or hide classes based on checkbox input
      for (class_name in c("stille_count_mean", "vogel_count_mean", "laerm_count_mean", "natur_count_mean", "average_RMS_dB_mean")) {
        if (class_name %in% selected_classes) {
          shinyjs::enable(class_name)
        } else {
          shinyjs::disable(class_name)
        }
      }
      shinyjs::click("submit")
    })
  })
  
  
  
  output$outputId1 <- renderText({
    paste("You entered:", input$inputId1)
  })
  
  output$outputId2 <- renderText({
    paste("You entered:", input$inputId2)
  })
  
  output$outputId3 <- renderText({
    paste("You entered:", input$inputId3)
  })
  
  selectedValues <- reactive({
    input$selectedStandorte
  })
  
  observe({
    # print(selectedValues())
  })
  
  output$overviewMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 9.8277, lat = 46.5582, zoom = 10)  # Standardansicht für Graubünden, Schweiz
  })
  
  observeEvent(input$overviewer, {
    # Überprüfung, ob Daten für die eingegebene Date Range nicht vorhanden sind
    date_range_query <- toJSON(list(
      "Date" = list(
        "$gte" = as.character(input$date_range2[1]),
        "$lte" = as.character(input$date_range2[2])
      )
    ))
    
    date_range_data <- snp_collection$find(date_range_query)
    
    if (is.null(date_range_data) || nrow(date_range_data) < 1) {
      showModal(
        modalDialog(
          title = "No Data :(",
          "Couldn't find Data! Choose another date for data aggregation!",
          easyClose = TRUE
        )
      )
      return()
    }

    if (input$inputweekday == "All") {
      if (input$location_choice == "All Locations") {
        query <- toJSON(list(
          "Date" = list(
            "$gte" = as.character(input$date_range2[1]),
            "$lte" = as.character(input$date_range2[2])
          )
        ))
      } else {
        if (length(selectedValues()) < 2) {
          showModal(
            modalDialog(
              title = "Location Error :(",
              "Choose at least two Locations, please!",
              easyClose = TRUE
            )
          )
          return(NULL)  
        }
        # Abfrage für "Specific Locations"
        query <- toJSON(list(
          "Date" = list(
            "$gte" = as.character(input$date_range2[1]),
            "$lte" = as.character(input$date_range2[2])
          ),
          "Place" = list("$in" = selectedValues())
        ))
      }
    } else {
      if (input$location_choice == "All Locations") {
        query <- toJSON(list(
          "Date" = list(
            "$gte" = as.character(input$date_range2[1]),
            "$lte" = as.character(input$date_range2[2])
          ),
          "Weekday" = input$inputweekday
        ))
      } else {
        query <- toJSON(list(
          "Date" = list(
            "$gte" = as.character(input$date_range2[1]),
            "$lte" = as.character(input$date_range2[2])
          ),
          "Place" = list("$in" = selectedValues()),
          "Weekday" = input$inputweekday
        ))
      }
    }
    daten <- snp_collection$find(query)
    result_df <- as.data.frame(daten)

    
    # Initialisiere leeres DataFrame
    mergedDF <- data.frame()
    
    # Iteriere über jedes DataFrame
    for (i in 1:length(result_df[[4]])) {
      # Berechne den Mittelwert für jedes DataFrame
      mean_df <- aggregate(. ~ Time, data = result_df[[4]][[i]], mean)
      
      # Benenne die Spalten um, um Konflikte bei der Zusammenführung zu vermeiden
      col_names <- paste(c("stille_count", "vogel_count", "laerm_count", "natur_count", "average_RMS_dB"), "__", result_df$Date[i], "__", result_df$Place[i], sep = "")
      names(mean_df)[names(mean_df) != "Time"] <- col_names
      
      # Merge mit dem bisherigen Ergebnis-DataFrame
      if (i == 1) {
        mergedDF <- mean_df
      } else {
        # Schrittweises Merging
        mergedDF <- left_join(mergedDF, mean_df, by = "Time")
      }
    }
    
    colnames(mergedDF)
    
    result <- mergedDF %>%
      gather(key = "col_name", value = "value", -Time) %>%
      separate(col_name, into = c("metric", "date", "place"), sep = "__", extra = "merge") %>%
      group_by(Time, metric, place) %>%
      summarize(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    
    glyphs <- data.frame(svg_path = character(), place = character(), time = character(), stringsAsFactors = FALSE)
    
    # Iteriere über jeden Standort und jede Zeit
    for (place in unique(result$place)) {
      for (time in unique(result$Time)) {
        # Filtere den DataFrame für den aktuellen Standort und die Zeit
        subset_data <- result[result$place == place & result$Time == time, ]
        
        # Extrahiere die Parameter für die draw_svg-Funktion
        db <- subset_data$mean_value[subset_data$metric == "average_RMS_dB"]
        natur <- subset_data$mean_value[subset_data$metric == "natur_count"]
        stille <- subset_data$mean_value[subset_data$metric == "stille_count"]
        laerm <- subset_data$mean_value[subset_data$metric == "laerm_count"]
        vogel <- subset_data$mean_value[subset_data$metric == "vogel_count"]
        
        # Erstelle den Namen für die SVG-Datei
        nametag_time <- gsub(":", "_", time)
        name <- paste(place, nametag_time, sep = "_")
        
        # Rufe die draw_svg-Funktion auf
        svg_path <- draw_multiple_svg(db, natur, laerm, vogel, stille, name)
        
        # Füge die Informationen zu glyphs hinzu
        glyphs <- rbind(glyphs, data.frame(svg_path = svg_path, place = place, time = time))
      }
    }
    
    
    generate_glyphs <- reactive({
      time <- input$inputTime  # Verwenden Sie die ausgewählte Zeit aus dem Dropdown-Menü
      # print(time)
      glyphs <- data.frame(svg_path = character(), place = character(), time = character(), stringsAsFactors = FALSE)
      
      for (place in unique(result$place)) {
        subset_data <- result[result$place == place & result$Time == time, ]
        db <- subset_data$mean_value[subset_data$metric == "average_RMS_dB"]
        natur <- subset_data$mean_value[subset_data$metric == "natur_count"]
        stille <- subset_data$mean_value[subset_data$metric == "stille_count"]
        laerm <- subset_data$mean_value[subset_data$metric == "laerm_count"]
        vogel <- subset_data$mean_value[subset_data$metric == "vogel_count"]
        
        nametag_time <- gsub(":", "_", time)
        name <- paste(place, nametag_time, sep = "_")
        
        svg_path <- draw_multiple_svg(db, natur, laerm, vogel, stille, name)
        
        glyphs <- rbind(glyphs, data.frame(svg_path = svg_path, place = place, time = time))
      }
      print(glyphs)
      return(glyphs)
    })
    
    glyphs$place <- sub("\\.x$|\\.y$", "", glyphs$place)
    # print(unique(glyphs$place))
    glyphFrame <- merge_dataframes_with_fixed_metadata(glyphs, "place", "standort", TRUE)
    
    mean_lon <- mean(as.numeric(unique(glyphFrame$lon)))
    mean_lat <- mean(as.numeric(unique(glyphFrame$lat)))
    # print(paste("Mean Lon:", mean_lon))
    # print(paste("Mean Lat:", mean_lat))

    
    # Filter data based on selected time
    filteredData <- reactive({
      filter(glyphFrame, time == input$inputTime)  # Verwenden Sie die ausgewählte Zeit aus dem Dropdown-Menü
    })
    
    swissimageAttribution <- "© swisstopo (source for swissimage): WMS swisstopo"
    
    # Render the Leaflet map
    output$overviewMap <- renderLeaflet({
      # print("Debug: filteredData()")
      # print(filteredData())  # Add this line to check the content of filteredData()
      
      leaflet_obj <- leaflet() %>%
        addTiles(group = "BaseTiles") %>%
        setView(lng = mean_lon, lat = mean_lat, zoom = 14) %>%
        addMarkers(data = filteredData(), ~as.numeric(lon), ~as.numeric(lat), 
                   icon = makeIcon(iconUrl = filteredData()$svg_path, iconWidth = 80, iconHeight = 80),
                   label = ~place, group = "Locations"
        ) %>%
      addWMSTiles(
        baseUrl = "https://wms.geo.admin.ch/",
        layers = "ch.swisstopo.swissimage",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = swissimageAttribution,
        group = "SwissImage"
      ) %>%
        addLayersControl(
          baseGroups = c("BaseTiles", "SwissImage"),  
          overlayGroups = c("Locations"),
          options = layersControlOptions(collapsed = FALSE)  
        )
      
      # shinyjs::enable("overviewer")
      # shinyjs::disable("loadingAnimation")
      
      return(leaflet_obj)
    })
    
    observe({
      leafletProxy("overviewMap") %>%
        addControl(
          html = HTML("
        <div style='background: white; padding: 10px'>
          <p><strong>Legend</strong></p>
          <ul style='list-style-type: none; padding-left: 0;'>
            <li style='color: #D34045;'>&#9632; Noise</li>
            <li style='color: #71FF5C;'>&#9632; Nature</li>
            <li style='color: #64C6FA;'>&#9632; Silence</li>
            <li style='color: #E67C3E;'>&#9632; Bird</li>
          </ul>
        </div>
      "),
          position = "bottomleft"
        )
    })
  
    
  })
  observeEvent(input$next_overview, {
    current_time <- input$inputTime
    
    # Find the index of the current time in timeslots
    current_index <- which(timeslots == current_time)
    
    # Check if the current time is in the list, and if it has a next time
    if (!is.na(current_index) && current_index < length(timeslots)) {
      # Get the next time
      next_time <- timeslots[current_index + 1]
      
      # Update the input$inputTime with the next time
      updateSelectInput(session, "inputTime", selected = next_time)
    }
  })
  observeEvent(input$back_overview, {
    current_time <- input$inputTime
    
    # Find the index of the current time in timeslots
    current_index <- which(timeslots == current_time)
    
    # Check if the current time is in the list, and if it has a next time
    if (!is.na(current_index) && current_index < length(timeslots)) {
      # Get the next time
      next_time <- timeslots[current_index - 1]
      
      # Update the input$inputTime with the next time
      updateSelectInput(session, "inputTime", selected = next_time)
    }
  })
  
}

shinyApp(ui, server)

