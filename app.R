# ---------------- Packages ----------------
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(shinyjs)

# ---------------- Data Setup ----------------
rds_file <- "person.rds"

if (file.exists(rds_file)) {
  person_data <- readRDS(rds_file)
} else {
  csv_files <- list.files(pattern = "^person\\d{4}\\.csv$")
  light_files <- list.files(pattern = "^accident\\d{4}\\.csv$")
  
  lightInfo <- light_files %>%
    lapply(read_csv) %>%
    bind_rows(.id = "source") %>%
    mutate(Year = str_extract(light_files[as.integer(source)], "\\d{4}")) %>%
    select(-source)
  
  bigData <- csv_files %>%
    lapply(read_csv) %>%
    bind_rows(.id = "source") %>%
    mutate(Year = str_extract(csv_files[as.integer(source)], "\\d{4}")) %>%
    select(-source)
  
  person_data <- bigData %>%
    rename(
      State = STATENAME,
      ID = ST_CASE,
      VehicleNumber = VEH_NO,
      PersonNumber = PER_NO,
      Month = MONTHNAME,
      Day = DAYNAME,
      Hour = HOUR,
      EventType = HARM_EVNAME,
      VehicleYear = MOD_YEAR,
      VehicleMake = VPICMAKENAME,
      VehicleModel = VPICMODELNAME,
      VehicleType = VPICBODYCLASSNAME,
      Age = AGE,
      Gender = SEXNAME,
      PersonType = PER_TYPNAME,
      Severity = INJ_SEVNAME,
      Seatbelt = REST_USENAME,
      Alcohol = DRINKINGNAME,
      BAC = ALC_RESNAME,
      Drugs = DRUGSNAME,
      DeathLocation = DOANAME,
      RuralUrban = RUR_URBNAME
    ) %>%
    select(
      State, ID, PersonNumber, Month, Day, Hour,
      EventType, VehicleYear, VehicleMake, VehicleModel, VehicleType,
      Age, Gender, PersonType, Severity, Seatbelt, Alcohol, BAC,
      Drugs, DeathLocation, RuralUrban, Year
    ) %>%
    mutate(
      ID = paste0(Year, "_", ID),
      Hour = as.numeric(Hour),
      EventType = if_else(str_detect(EventType, "Pedalcyclist"), "Cyclist", "Other"),
      Season = case_when(
        Month %in% c("December", "January", "February") ~ "Winter",
        Month %in% c("March", "April", "May") ~ "Spring",
        Month %in% c("June", "July", "August") ~ "Summer",
        Month %in% c("September", "October", "November") ~ "Fall",
        TRUE ~ NA_character_
      ),
      Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))
    )
  
  lightData <- lightInfo %>%
    rename(
      ID = ST_CASE,
      Light = LGT_CONDNAME,
      Weather = WEATHERNAME
    ) %>%
    mutate(ID = paste0(Year, "_", ID)) %>%
    select(ID, Light, Weather, Year)
  
  person_data <- person_data %>% left_join(lightData, by = c("ID", "Year"))
  saveRDS(person_data, rds_file)
}

# ---------------- Preprocess Light Column ----------------
person_data <- person_data %>%
  mutate(Light_grouped = if_else(
    str_detect(Light, regex("dark", ignore_case = TRUE)),
    "Dark",
    Light
  ))

# ---------------- Helper Function ----------------
valid_choices <- function(x) {
  x <- unique(x)
  x <- x[!x %in% c("Other", "Reported as Unknown", "Not Reported", "Unknown",
                   "Trafficway Not in State Inventory", "Blowing Sand, Soil, Dirt",
                   "Blowing Snow", "Freezing Rain or Drizzle", "Severe Crosswinds",
                   "Sleet or Hail","Snow")]
  sort(x)
}

# ---------------- UI ----------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("🚴 Fatal Cyclist Traffic Accidents Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "chartView", "Select View:",
        choices = c(
          "Time Of Accident and Factors",
          "Proportion of Bicycle Fatalities by Light Level",
          "Proportion of Bicycle Fatalities by Season",
          "Cyclist Hourly Density by Season"
        ),
        selected = "Time Of Accident and Factors"
      ),
      
      # --- Hour selection for the third chart ---
      conditionalPanel(
        condition = "input.chartView == 'Proportion of Bicycle Fatalities by Season'",
        fluidRow(
          column(6, selectInput(
            "hour_start", "Start Hour:", choices = 0:23, selected = 0
          )),
          column(6, selectInput(
            "hour_end", "End Hour:", choices = 0:23, selected = 23
          ))
        ),
        fluidRow(
          column(12, actionButton(
            "reset_hours", "🔄 Reset Hours",
            class = "btn btn-warning",
            style = "width:100%; font-weight:bold;"
          ))
        )
      ),
      
      # --- Filters for first chart ---
      conditionalPanel(
        condition = "input.chartView == 'Time Of Accident and Factors'",
        selectInput("chartType", "Chart Type:", choices = c("Density", "Boxplot", "Bar Chart"), selected = "Density"),
        
        fluidRow(
          column(6, selectInput("state", "State:", choices = c("All States", valid_choices(person_data$State)),
                                selected = "All States", multiple = TRUE)),
          column(6, selectInput("season", "Season:", choices = c("All Seasons", "Spring","Summer","Fall","Winter"),
                                selected = "All Seasons", multiple = TRUE))
        ),
        fluidRow(
          column(4, selectInput("gender", "Gender:", choices = c("All", valid_choices(person_data$Gender)),
                                selected = "All", multiple = TRUE)),
          column(4, numericInput("age_min", "Min Age:", value = 0, min = 0, max = 120)),
          column(4, numericInput("age_max", "Max Age:", value = 120, min = 0, max = 120))
        ),
        fluidRow(
          column(6, selectInput("alcohol", "Alcohol:", choices = c("All", valid_choices(person_data$Alcohol)),
                                selected = "All", multiple = TRUE)),
          column(6, selectInput("drugs", "Drugs:", choices = c("All", valid_choices(person_data$Drugs)),
                                selected = "All", multiple = TRUE))
        ),
        fluidRow(
          column(6, selectInput("ruralurban", "Rural vs Urban:", choices = c("All", valid_choices(person_data$RuralUrban)),
                                selected = "All", multiple = TRUE)),
          column(6, selectInput("light", "Light Conditions:", choices = c("All", valid_choices(person_data$Light_grouped)),
                                selected = "All", multiple = TRUE))
        ),
        fluidRow(
          column(12, selectInput("weather", "Weather:", choices = c("All", valid_choices(person_data$Weather)),
                                 selected = "All", multiple = TRUE))
        ),
        fluidRow(
          column(12, actionButton("reset_filters", "🔄 Reset Filters", 
                                  class = "btn btn-warning", 
                                  style = "width:100%; font-weight:bold;"))
        )
      ),
      
      # --- Filters for fourth chart ---
      conditionalPanel(
        condition = "input.chartView == 'Cyclist Hourly Density by Season'",
        fluidRow(
          column(6, selectInput("state_density", "State:", choices = c("All States", valid_choices(person_data$State)), selected = "All States", multiple = TRUE)),
          column(6, selectInput("ruralurban_density", "Rural vs Urban:", choices = c("All", valid_choices(person_data$RuralUrban)), selected = "All", multiple = TRUE))
        ),
        fluidRow(
          column(6, selectInput("light_density", "Light Conditions:", choices = c("All", valid_choices(person_data$Light_grouped)), selected = "All", multiple = TRUE)),
          column(6, selectInput("weather_density", "Weather:", choices = c("All", valid_choices(person_data$Weather)), selected = "All", multiple = TRUE))
        ),
        fluidRow(
          column(12, actionButton("reset_density_filters", "🔄 Reset Density Filters",
                                  class = "btn btn-warning",
                                  style = "width:100%; font-weight:bold;"))
        )
      )
    ),
    
    mainPanel(
      div(style = "text-align:center; font-size:16px; margin-bottom:10px;", htmlOutput("incidentSummary")),
      plotlyOutput("mainPlot", height = "600px")
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  default_values <- list(
    chartType = "Density",
    state = "All States",
    season = "All Seasons",
    gender = "All",
    age_min = 0,
    age_max = 120,
    alcohol = "All",
    drugs = "All",
    ruralurban = "All",
    light = "All",
    weather = "All",
    hour_start = 0,
    hour_end = 23,
    state_density = "All States",
    ruralurban_density = "All",
    light_density = "All",
    weather_density = "All"
  )
  
  # --- Observers for reset buttons ---
  observe({
    changed <- any(c(
      input$chartType != default_values$chartType,
      !setequal(input$state, default_values$state),
      !setequal(input$season, default_values$season),
      !setequal(input$gender, default_values$gender),
      input$age_min != default_values$age_min,
      input$age_max != default_values$age_max,
      !setequal(input$alcohol, default_values$alcohol),
      !setequal(input$drugs, default_values$drugs),
      !setequal(input$ruralurban, default_values$ruralurban),
      !setequal(input$light, default_values$light),
      !setequal(input$weather, default_values$weather)
    ))
    shinyjs::toggleState("reset_filters", condition = changed)
  })
  
  observe({
    hour_changed <- input$hour_start != default_values$hour_start ||
      input$hour_end   != default_values$hour_end
    shinyjs::toggleState("reset_hours", condition = hour_changed)
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "chartType", selected = default_values$chartType)
    updateSelectInput(session, "state", selected = default_values$state)
    updateSelectInput(session, "season", selected = default_values$season)
    updateSelectInput(session, "gender", selected = default_values$gender)
    updateNumericInput(session, "age_min", value = default_values$age_min)
    updateNumericInput(session, "age_max", value = default_values$age_max)
    updateSelectInput(session, "alcohol", selected = default_values$alcohol)
    updateSelectInput(session, "drugs", selected = default_values$drugs)
    updateSelectInput(session, "ruralurban", selected = default_values$ruralurban)
    updateSelectInput(session, "light", selected = default_values$light)
    updateSelectInput(session, "weather", selected = default_values$weather)
  })
  
  observeEvent(input$reset_hours, {
    updateSelectInput(session, "hour_start", selected = default_values$hour_start)
    updateSelectInput(session, "hour_end", selected = default_values$hour_end)
  })
  
  observeEvent(input$reset_density_filters, {
    updateSelectInput(session, "state_density", selected = "All States")
    updateSelectInput(session, "ruralurban_density", selected = "All")
    updateSelectInput(session, "light_density", selected = "All")
    updateSelectInput(session, "weather_density", selected = "All")
  })
  
  # --- Data reactives ---
  cyclist_data <- reactive({
    d <- person_data %>% filter(PersonType == "Bicyclist")
    
    if (!("All States" %in% input$state)) d <- d %>% filter(State %in% input$state)
    if (!("All Seasons" %in% input$season)) d <- d %>% filter(Season %in% input$season)
    if (!("All" %in% input$gender)) d <- d %>% filter(Gender %in% input$gender)
    d <- d %>% filter(Age >= input$age_min & Age <= input$age_max)
    if (!("All" %in% input$alcohol)) d <- d %>% filter(Alcohol %in% input$alcohol)
    if (!("All" %in% input$drugs)) d <- d %>% filter(Drugs %in% input$drugs)
    if (!("All" %in% input$ruralurban)) d <- d %>% filter(RuralUrban %in% input$ruralurban)
    if (!("All" %in% input$light)) d <- d %>% filter(Light_grouped %in% input$light)
    if (!("All" %in% input$weather)) d <- d %>% filter(Weather %in% input$weather)
    d
  })
  
  cyclist_density_data <- reactive({
    d <- person_data %>% filter(PersonType == "Bicyclist")
    if (!("All States" %in% input$state_density)) d <- d %>% filter(State %in% input$state_density)
    if (!("All" %in% input$ruralurban_density)) d <- d %>% filter(RuralUrban %in% input$ruralurban_density)
    if (!("All" %in% input$light_density)) d <- d %>% filter(Light_grouped %in% input$light_density)
    if (!("All" %in% input$weather_density)) d <- d %>% filter(Weather %in% input$weather_density)
    d
  })
  
  # --- Summary ---
  output$incidentSummary <- renderUI({
    d <- cyclist_data()
    count <- n_distinct(d$ID)
    msg <- paste0("<b>Cyclist Incidents:</b> ", count, "<b> Out Of 3159</b>")
    if (count < 10) msg <- paste0(msg, "<br><span style='color:#d9534f;'>⚠️ Fewer than 10 incidents — may not be meaningful.</span>")
    HTML(msg)
  })
  
  # --- Plot ---
  output$mainPlot <- renderPlotly({
    if (input$chartView == "Cyclist Hourly Density by Season") {
      d <- cyclist_density_data()
      hour_labels <- c("Midnight","6 AM","Noon","6 PM","11 PM")
      hour_breaks <- c(0,6,12,18,23)
      season_colors <- c("Spring"="#00BFC4","Summer"="#FCE100","Fall"="#FF7F0E","Winter"="#1F77B4")
      
      p <- ggplot(d, aes(x=Hour, fill=Season, color=Season)) +
        geom_density(alpha=0.5) +
        scale_fill_manual(values=season_colors) +
        scale_color_manual(values=season_colors) +
        scale_x_continuous(limits=c(0,23), breaks=hour_breaks, labels=hour_labels) +
        labs(title="Cyclist Accidents by Hour - Seasonal Density", x="Hour of Day", y="Density") +
        theme_minimal() +
        facet_wrap(~Season, ncol=2)
      
      ggplotly(p)
    }
    else if (input$chartView == "Proportion of Bicycle Fatalities by Light Level") {
      df <- person_data %>%
        filter(PersonType == "Bicyclist", !is.na(Season), !is.na(Light_grouped)) %>%
        count(Season, Light_grouped) %>%
        group_by(Season) %>% mutate(prop = n / sum(n))
      
      light_colors <- c(
        "Dark" = "#001f4d",
        "Daylight" = "#87CEFA",
        "Dawn" = "#FFA07A",
        "Dusk" = "#9370DB",
        "Other" = "#A9A9A9"
      )
      
      p <- ggplot(df, aes(x = Season, y = prop, fill = Light_grouped,
                          text = paste(Light_grouped, ":", n))) +
        geom_bar(stat = "identity", position = "fill") +
        scale_fill_manual(values = light_colors) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = "Seasonal Variation in Light Conditions",
             x = "Season", y = "Proportion of Crashes", fill = "Light Condition") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
    } else if (input$chartView == "Proportion of Bicycle Fatalities by Season") {
      start_hr <- as.numeric(input$hour_start)
      end_hr   <- as.numeric(input$hour_end)
      
      df_all <- person_data %>%
        filter(PersonType == "Bicyclist", !is.na(Season), Hour >= 0, Hour <= 23) %>%
        count(Season, Hour)
      
      df_range <- df_all %>%
        filter(Hour >= start_hr, Hour <= end_hr) %>%
        count(Season, wt = n, name = "n_range")
      
      df_all_sum <- df_all %>% count(Season, wt = n, name = "n_total")
      df_merge <- left_join(df_all_sum, df_range, by = "Season") %>%
        mutate(n_range = replace_na(n_range, 0))
      
      season_colors <- c(
        "Spring" = "#00BFC4",
        "Summer" = "#FCE100",
        "Fall" = "#FF7F0E",
        "Winter" = "#1F77B4"
      )
      
      p <- ggplot() +
        geom_col(data = df_merge, aes(x = Season, y = n_total, fill = Season), alpha = 0.2, show.legend = FALSE) +
        geom_col(data = df_merge, aes(x = Season, y = n_range, fill = Season), alpha = 0.8, show.legend = FALSE) +
        scale_fill_manual(values = season_colors) +
        labs(title = "Bicycle Fatalities by Season", x = "Season", y = "Number of Fatalities") +
        theme_minimal()
      
      ggplotly(p)
      
    } else if (input$chartView == "Cyclist Hourly Density by Season") {
      d <- cyclist_density_data()
      hour_labels <- c("Midnight","6 AM","Noon","6 PM","11 PM")
      hour_breaks <- c(0,6,12,18,23)
      season_colors <- c(
        "Spring" = "#00BFC4",
        "Summer" = "#FCE100",
        "Fall" = "#FF7F0E",
        "Winter" = "#1F77B4"
      )
      
      p <- ggplot(d, aes(x = Hour, fill = Season, color = Season)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = season_colors) +
        scale_color_manual(values = season_colors) +
        scale_x_continuous(limits = c(0,23), breaks = hour_breaks, labels = hour_labels) +
        labs(title = "Cyclist Accidents by Hour - Seasonal Density",
             x = "Hour of Day", y = "Density") +
        theme_minimal()
      
      ggplotly(p)
      
    } else {
      filtered <- cyclist_data() %>% filter(Hour >= 0 & Hour <= 23)
      all_cyclists <- person_data %>% filter(PersonType == "Bicyclist", Hour >= 0 & Hour <= 23)
      hour_labels <- c("Midnight","6 AM","Noon","6 PM","11 PM")
      hour_breaks <- c(0,6,12,18,23)
      
      p <- switch(input$chartType,
                  "Density" = {
                    ggplot() +
                      geom_density(data = all_cyclists, aes(x = Hour), fill = "gray", alpha = 0.2) +
                      geom_density(data = filtered, aes(x = Hour), fill = "deepskyblue", alpha = 0.6) +
                      scale_x_continuous(limits = c(0,23), breaks = hour_breaks, labels = hour_labels) +
                      labs(title = "Cyclist Accidents by Hour (Density)", x = "Hour of Day", y = "Density") +
                      theme_minimal()
                  },
                  "Boxplot" = {
                    ggplot(filtered, aes(x = Season, y = Hour, fill = Season)) +
                      geom_boxplot(alpha = 0.7, outlier.color = "red", show.legend = FALSE) +
                      scale_fill_brewer(palette = "Pastel1") +
                      scale_y_continuous(limits = c(0,23), breaks = hour_breaks, labels = hour_labels) +
                      labs(title = "Distribution of Cyclist Accident Times by Season", x = "Season", y = "Hour of Day") +
                      theme_minimal()
                  },
                  "Bar Chart" = {
                    df_bar <- filtered %>% count(Hour)
                    ggplot(df_bar, aes(x = Hour, y = n)) +
                      geom_col(fill = "deepskyblue") +
                      scale_x_continuous(limits = c(0,23), breaks = hour_breaks, labels = hour_labels) +
                      labs(title = "Cyclist Accidents by Hour (Bar Chart)", x = "Hour of Day", y = "Number of Accidents") +
                      theme_minimal()
                  }
      )
      
      ggplotly(p)
    }
  })
}

# ---------------- Run App ----------------
shinyApp(ui = ui, server = server)
