# ============================================================
#  CRASHLENS - ROAD ACCIDENT ANALYTICS BY USING R. (R Shiny dashboard)
#  Run:  install.packages("shiny")  
#        shiny::runApp("app.R")   
# ============================================================
library(shiny)

# ETL- EXTRACT
set.seed(123)
n <- 1000

locations       <- c("City", "Highway", "Rural", "Intersection", "Residential Area", "School Zone")
causes          <- c("Overspeeding", "Drunk Driving", "Distracted Driving",
                     "Poor Road Condition", "Weather", "Signal Violation")
weather_types   <- c("Clear", "Rainy", "Foggy", "Cloudy")
vehicle_types   <- c("Car", "Motorcycle", "Truck", "Bus", "Auto")
severity_types  <- c("Minor", "Major", "Fatal")
road_conditions <- c("Good", "Moderate", "Poor")
day_parts       <- c("Morning", "Afternoon", "Evening", "Night")

accident_date <- sample(seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "day"),
                        n, replace = TRUE)

accidents <- data.frame(
  Accident_ID    = 1:n,
  Date           = accident_date,
  Year           = as.integer(format(accident_date, "%Y")),
  Month          = as.integer(format(accident_date, "%m")),
  Location       = sample(locations, n, replace = TRUE, prob = c(.25, .22, .15, .16, .13, .09)),
  Cause          = sample(causes, n, replace = TRUE, prob = c(.30, .12, .20, .15, .10, .13)),
  Weather        = sample(weather_types, n, replace = TRUE, prob = c(.50, .20, .10, .20)),
  Vehicle        = sample(vehicle_types, n, replace = TRUE),
  Road_Condition = sample(road_conditions, n, replace = TRUE, prob = c(.50, .30, .20)),
  Time_of_Day    = sample(day_parts, n, replace = TRUE),
  Speed_kmh      = sample(20:120, n, replace = TRUE),
  Visibility_km  = round(runif(n, .2, 10), 1),
  Severity       = sample(severity_types, n, replace = TRUE, prob = c(.55, .32, .13)),
  stringsAsFactors = FALSE
)

# ETL - TRANSFORM
accidents$Deaths <- ifelse(accidents$Severity == "Fatal", sample(1:4, n, replace = TRUE),
                           ifelse(accidents$Severity == "Major", sample(0:1, n, replace = TRUE), 0))
accidents$Injuries <- ifelse(accidents$Severity == "Fatal", sample(1:6, n, replace = TRUE),
                             ifelse(accidents$Severity == "Major", sample(1:5, n, replace = TRUE),
                                    sample(0:2, n, replace = TRUE)))
accidents <- accidents[order(accidents$Date), ]
rownames(accidents) <- NULL

accidents$Risk_Flag <- ifelse(accidents$Speed_kmh >= 80 | accidents$Visibility_km < 2 |
                                accidents$Road_Condition == "Poor", "Review Required", "Other")

pal <- c("#3b82f6", "#e5484d", "#f5a524", "#22c55e", "#a855f7", "#14b8a6")
cnt <- function(x, lv, sorted = TRUE) {
  t <- table(factor(x, levels = lv))
  if (sorted) sort(t, decreasing = TRUE) else t
}
bar <- function(t, main, las = 1, cex = .85, cols = pal) {
  barplot(t, main = main, ylab = "Accident Count", las = las, cex.names = cex,
          col = rep(cols, length.out = length(t)), border = NA)
}

css <- "
body{background:#f5f6fa;font-family:'Segoe UI',system-ui,sans-serif;color:#1b1f2a}
.hero{background:#12172b;color:#fff;padding:30px 28px 60px;margin:-15px -15px 0}
.hero h1{margin:0;font-weight:700;letter-spacing:-.5px}.hero h1 span{color:#e5484d}
.hero p{color:#aab2c5;margin:6px 0 0}
.hero .badge2{display:inline-block;background:#ffffff1a;border-radius:99px;padding:3px 12px;font-size:12px;margin-bottom:10px}
.filters{background:#fff;border:1px solid #e3e6ee;border-radius:14px;padding:14px 18px 0;margin:-36px 0 18px;
         box-shadow:0 4px 18px #0000001a}
.kpi{background:#fff;border:1px solid #e3e6ee;border-left:4px solid #3b82f6;border-radius:14px;padding:14px;margin-bottom:14px}
.kpi.red{border-left-color:#e5484d}.kpi small{color:#667085}.kpi b{display:block;font-size:26px}
.card2{background:#fff;border:1px solid #e3e6ee;border-radius:14px;padding:14px;margin-bottom:14px}
.card2 h4{margin-top:0;font-size:15px;font-weight:600}
.insight{background:#fff;border:1px solid #e3e6ee;border-left:4px solid #e5484d;border-radius:14px;padding:14px;margin-bottom:14px}
.nav-tabs>li>a{color:#667085}.nav-tabs>li.active>a{color:#e5484d!important;font-weight:600}
.foot{color:#667085;font-size:12px;text-align:center;padding:20px}
"

ui <- fluidPage(
  tags$head(tags$title("CrashLens"), tags$style(HTML(css))),
  
  div(class = "hero",
      span(class = "badge2", "ETL: Extract \u2192 Transform \u2192 Load"),
      h1("Crash", span("Lens")),
      p("Interactive road accident analytics built with R Shiny. Change a filter and everything updates.")),
  
  div(class = "filters",
      fluidRow(
        column(3, selectInput("year", "Year", c("All", 2021:2025))),
        column(3, selectInput("loc", "Location", c("All", locations))),
        column(2, selectInput("sev", "Severity", c("All", severity_types))),
        column(2, selectInput("wea", "Weather", c("All", weather_types))),
        column(2, br(), actionButton("reset", "Reset filters", width = "100%"))
      )),
  
  uiOutput("kpis"),
  uiOutput("insights"),
  
  tabsetPanel(
    tabPanel("Overview", br(),
             fluidRow(
               column(4, div(class = "card2", h4("Accidents by location"), plotOutput("p_loc", height = 280))),
               column(4, div(class = "card2", h4("Accidents by cause"),    plotOutput("p_cause", height = 280))),
               column(4, div(class = "card2", h4("Accident severity"),     plotOutput("p_sev", height = 280)))
             ),
             fluidRow(
               column(4, div(class = "card2", h4("Yearly trend"),          plotOutput("p_year", height = 280))),
               column(4, div(class = "card2", h4("Accidents by vehicle"),  plotOutput("p_veh", height = 280))),
               column(4, div(class = "card2", h4("Monthly pattern"),       plotOutput("p_month", height = 280)))
             )),
    tabPanel("Patterns", br(),
             fluidRow(
               column(6, div(class = "card2", h4("Distribution of accident speeds"), plotOutput("p_speed", height = 300))),
               column(6, div(class = "card2", h4("Injuries by severity"),            plotOutput("p_box", height = 300)))
             ),
             fluidRow(
               column(6, div(class = "card2", h4("Correlation heatmap"), plotOutput("p_heat", height = 320))),
               column(6, div(class = "card2", h4("Weather / time of day / road condition"),
                             plotOutput("p_misc", height = 320)))
             )),
    tabPanel("Comparisons", br(),
             fluidRow(
               column(4, div(class = "card2", h4("Severity by location"), tableOutput("t_loc"))),
               column(4, div(class = "card2", h4("Vehicle by severity"),  tableOutput("t_veh"))),
               column(4, div(class = "card2", h4("Deaths and injuries by cause"), tableOutput("t_cause")))
             ),
             fluidRow(
               column(12, div(class = "card2", h4("Cause by weather"), tableOutput("t_cw")))
             )),
    tabPanel("Records", br(),
             div(class = "card2",
                 fluidRow(
                   column(5, textInput("q", NULL, placeholder = "Search records (e.g. Highway, Fatal, Rainy)...")),
                   column(3, sliderInput("rows", NULL, min = 10, max = 200, value = 25, step = 5)),
                   column(2, checkboxInput("flagonly", "Review Required only", FALSE)),
                   column(2, downloadButton("dl", "Download CSV"))
                 ),
                 textOutput("rec_count"),
                 div(style = "overflow-x:auto", tableOutput("t_rec"))
             ))
  ),
  
  div(class = "foot",
      "These are practice results from generated sample data, not official findings.",
      br(), "For real-world decisions, replace the sample with verified accident records.")
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "year", selected = "All")
    updateSelectInput(session, "loc",  selected = "All")
    updateSelectInput(session, "sev",  selected = "All")
    updateSelectInput(session, "wea",  selected = "All")
    updateTextInput(session, "q", value = "")
  })
  
  d <- reactive({
    x <- accidents
    if (input$year != "All") x <- x[x$Year == as.integer(input$year), ]
    if (input$loc  != "All") x <- x[x$Location == input$loc, ]
    if (input$sev  != "All") x <- x[x$Severity == input$sev, ]
    if (input$wea  != "All") x <- x[x$Weather == input$wea, ]
    x
  })
  need_rows <- function() validate(need(nrow(d()) > 0, "No records match these filters."))
  
  output$kpis <- renderUI({
    x <- d()
    fluidRow(
      column(2, div(class = "kpi", tags$small("Total accidents"), tags$b(nrow(x)))),
      column(2, div(class = "kpi red", tags$small("Total deaths"), tags$b(sum(x$Deaths)))),
      column(2, div(class = "kpi", tags$small("Total injuries"), tags$b(sum(x$Injuries)))),
      column(2, div(class = "kpi", tags$small("Average speed"),
                    tags$b(if (nrow(x)) paste(round(mean(x$Speed_kmh), 1), "km/h") else "-"))),
      column(2, div(class = "kpi", tags$small("Fatal accidents"), tags$b(sum(x$Severity == "Fatal")))),
      column(2, div(class = "kpi red", tags$small("Flagged for review"), tags$b(sum(x$Risk_Flag == "Review Required"))))
    )
  })
  
  output$insights <- renderUI({
    x <- d()
    if (nrow(x) == 0) return(div(class = "insight", "No records match these filters."))
    top <- function(v) names(sort(table(v), decreasing = TRUE))[1]
    fl <- sum(x$Risk_Flag == "Review Required")
    div(class = "insight",
        tags$b("What the results suggest"),
        tags$ul(
          tags$li("Location with most accidents: ", tags$b(top(x$Location))),
          tags$li("Most common cause: ", tags$b(top(x$Cause))),
          tags$li("Most common vehicle: ", tags$b(top(x$Vehicle)),
                  "; weather: ", tags$b(top(x$Weather)),
                  "; time: ", tags$b(top(x$Time_of_Day))),
          tags$li(fl, " records (", round(100 * fl / nrow(x), 1),
                  "%) are marked Review Required (speed 80+ km/h, visibility under 2 km, or poor road).")
        ))
  })
  
  output$p_loc <- renderPlot({ need_rows(); par(mar = c(7, 4, 2, 1))
    bar(cnt(d()$Location, locations), "", las = 2) })
  output$p_cause <- renderPlot({ need_rows(); par(mar = c(9, 4, 2, 1))
    bar(cnt(d()$Cause, causes), "", las = 2, cex = .75, cols = rev(pal)) })
  output$p_sev <- renderPlot({ need_rows(); par(mar = c(1, 1, 1, 1))
    t <- cnt(d()$Severity, severity_types, sorted = FALSE)
    pie(t, col = c("#3b82f6", "#f5a524", "#e5484d"),
        labels = paste0(names(t), " (", round(100 * t / sum(t), 1), "%)")) })
  output$p_year <- renderPlot({ need_rows(); par(mar = c(4, 4, 2, 1))
    t <- cnt(d()$Year, 2021:2025, sorted = FALSE)
    plot(as.integer(names(t)), as.integer(t), type = "b", pch = 19, lwd = 2, col = "#a855f7",
         xlab = "Year", ylab = "Accident Count", ylim = c(0, max(t) * 1.1)) })
  output$p_veh <- renderPlot({ need_rows(); par(mar = c(5, 4, 2, 1))
    bar(cnt(d()$Vehicle, vehicle_types), "", las = 1) })
  output$p_month <- renderPlot({ need_rows(); par(mar = c(4, 4, 2, 1))
    t <- cnt(d()$Month, 1:12, sorted = FALSE); names(t) <- month.abb
    bar(t, "", cex = .8, cols = "#14b8a6") })
  
  output$p_speed <- renderPlot({ need_rows()
    hist(d()$Speed_kmh, breaks = 10, main = "", xlab = "Speed (km/h)",
         col = "#93c5fd", border = "white") })
  output$p_box <- renderPlot({ need_rows()
    x <- d(); x$Severity <- factor(x$Severity, levels = severity_types)
    boxplot(Injuries ~ Severity, data = x, xlab = "Severity", ylab = "Number of Injuries",
            col = c("#3b82f6", "#f5a524", "#e5484d")) })
  output$p_heat <- renderPlot({ need_rows()
    nd <- d()[, c("Speed_kmh", "Visibility_km", "Deaths", "Injuries")]
    cm <- suppressWarnings(cor(nd)); cm[is.na(cm)] <- 0
    cols <- colorRampPalette(c("darkblue", "deepskyblue", "white", "lightgreen",
                               "yellow", "orange", "red"))(100)
    par(mar = c(6, 8, 2, 2))
    image(1:4, 1:4, t(cm[4:1, ]), col = cols, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
    axis(1, 1:4, colnames(cm), las = 2, cex.axis = .85)
    axis(2, 1:4, rev(rownames(cm)), las = 1, cex.axis = .85)
    for (i in 1:4) for (j in 1:4) text(j, 5 - i, round(cm[i, j], 2), cex = .9)
    box() })
  output$p_misc <- renderPlot({ need_rows()
    par(mfrow = c(1, 3), mar = c(7, 3, 2, 1))
    barplot(cnt(d()$Weather, weather_types), main = "Weather", las = 2, col = pal[1:4], border = NA)
    barplot(cnt(d()$Time_of_Day, day_parts), main = "Time of day", las = 2, col = pal[3:6], border = NA)
    barplot(cnt(d()$Road_Condition, road_conditions, FALSE), main = "Road", las = 2,
            col = c("#22c55e", "#f5a524", "#e5484d"), border = NA) })
  
  mat <- function(a, b, la, lb) as.data.frame.matrix(table(factor(a, levels = la), factor(b, levels = lb)))
  output$t_loc <- renderTable(mat(d()$Location, d()$Severity, locations, severity_types), rownames = TRUE)
  output$t_veh <- renderTable(mat(d()$Vehicle, d()$Severity, vehicle_types, severity_types), rownames = TRUE)
  output$t_cw  <- renderTable(mat(d()$Cause, d()$Weather, causes, weather_types), rownames = TRUE)
  output$t_cause <- renderTable({
    x <- d()
    data.frame(Cause = causes,
               Accidents = as.integer(table(factor(x$Cause, levels = causes))),
               Deaths    = sapply(causes, function(cz) sum(x$Deaths[x$Cause == cz])),
               Injuries  = sapply(causes, function(cz) sum(x$Injuries[x$Cause == cz])),
               row.names = NULL)
  })
  
  rec <- reactive({
    x <- d()
    if (input$flagonly) x <- x[x$Risk_Flag == "Review Required", ]
    if (nzchar(input$q)) {
      hit <- apply(x, 1, function(r) any(grepl(input$q, r, ignore.case = TRUE)))
      x <- x[hit, ]
    }
    x
  })
  output$rec_count <- renderText(paste("Showing", min(input$rows, nrow(rec())), "of", nrow(rec()), "records"))
  output$t_rec <- renderTable({
    x <- head(rec(), input$rows); x$Date <- format(x$Date); x
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  # ETL STAGE 3 - LOAD
  output$dl <- downloadHandler(
    filename = function() "CrashLens_Accident_Data.csv",
    content  = function(file) write.csv(rec(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)