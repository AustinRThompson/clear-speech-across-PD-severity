# app.R

# Put this at the VERY TOP of the file:
options(shiny.maxRequestSize = 200*1024^2)  # 200 MB upload limit

library(shiny)
library(tuneR)
library(seewave)

ui <- fluidPage(
  titlePanel("Audio + TT Movement Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("audio_file", "Upload audio file (.wav)",
                accept = c(".wav")),
      fileInput("mov_file", "Upload movement file (.csv)",
                accept = c(".csv")),
      
      numericInput("start_time", "Start time (s)", value = 0, min = 0, step = 0.1),
      numericInput("end_time", "End time (s, 0 = full duration)", value = 0, min = 0, step = 0.1),
      
      checkboxInput("log_spectro", "Log spectrogram amplitude", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("spec_mov_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive: read audio
  audio_data <- reactive({
    req(input$audio_file)
    readWave(input$audio_file$datapath)
  })
  
  # Reactive: read movement
  mov_data <- reactive({
    req(input$mov_file)
    df <- read.csv(input$mov_file$datapath)
    
    validate(
      need("time" %in% names(df), "Movement file must have a 'time' column."),
      need("tf_y" %in% names(df), "Movement file must have a 'tf_y' column.")
    )
    
    df
  })
  
  output$spec_mov_plot <- renderPlot({
    wav <- audio_data()
    mov <- mov_data()
    
    # Audio duration in seconds
    audio_duration <- length(wav@left) / wav@samp.rate
    
    # Movement duration (if weird, guard it)
    mov_duration_raw <- suppressWarnings(max(mov$time, na.rm = TRUE))
    if (!is.finite(mov_duration_raw)) {
      mov_duration_raw <- audio_duration
    }
    
    # Overall max duration we can safely use
    full_duration <- min(audio_duration, mov_duration_raw)
    
    # Get requested start/end
    start_in <- ifelse(is.null(input$start_time), 0, input$start_time)
    end_in   <- ifelse(is.null(input$end_time), 0, input$end_time)
    
    # Clamp and interpret end = 0 as "use full"
    t_start <- max(0, start_in)
    if (end_in <= 0) {
      t_end <- full_duration
    } else {
      t_end <- min(end_in, full_duration)
    }
    
    # Ensure t_end > t_start
    if (t_end <= t_start) {
      t_end <- min(t_start + 0.05, full_duration)  # at least 50 ms window
    }
    
    # Subset movement data within time window
    mov_sub <- mov[mov$time >= t_start & mov$time <= t_end, ]
    
    # --- Compute tf_y velocity (simple numerical derivative) ---
    # Make sure we have enough points
    if (nrow(mov_sub) >= 2) {
      dt   <- c(NA, diff(mov_sub$time))
      dpos <- c(NA, diff(mov_sub$tf_y))
      vel  <- dpos / dt   # units: tf_y units per second (if time is in seconds)
    } else {
      vel <- rep(NA, nrow(mov_sub))
    }
    
    # Set layout: 3 rows (spectrogram, tf_y position, tf_y velocity)
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    
    par(mfrow = c(3, 1),
        mar = c(0, 4, 3, 1))  # first plot: no x-axis labels
    
    # 1) Spectrogram (0–5 kHz as you requested)
    spectro(wav,
            scale = input$log_spectro,
            wn = "hanning",
            ovlp = 75,
            osc = FALSE,
            tlim = c(t_start, t_end),
            flim = c(0, 5),  # 0–5 kHz
            main = sprintf("Spectrogram (%.2f–%.2f s)", t_start, t_end))
    
    # 2) TT IS position
    par(mar = c(0, 4, 2, 1))  # no x-axis labels, a bit of top margin for title
    plot(mov_sub$time, mov_sub$tf_y, type = "l",
         xlab = "",
         ylab = "TT IS (units)",
         main = "Tongue Tip IS Position",
         xlim = c(t_start, t_end))
    
    # 3) TT IS velocity
    par(mar = c(4, 4, 2, 1))  # normal bottom margin for x axis
    plot(mov_sub$time, vel, type = "l",
         xlab = "Time (s)",
         ylab = "TT IS vel\n(units/s)",
         main = "Tongue Tip IS Velocity",
         xlim = c(t_start, t_end),
         )
    abline(h = 0, col = "gray40", lwd = 1, lty = 2)
  })
  
}

shinyApp(ui = ui, server = server)
