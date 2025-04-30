library(shiny)
library(ggplot2)
library(dplyr)

# Simplified letter-to-frequency mapping (can expand this)
letter_frequencies <- list(
  A = 440,  # A4
  B = 494,  # B4
  C = 523,  # C5
  D = 587,  # D5
  E = 659,  # E5
  F = 698,  # F5
  G = 784,  # G5
  H = 880, # A5
  I = 988,
  J = 1046,
  K = 1174,
  L = 1318,
  M = 1396,
  N = 1568,
  O = 1760,
  P = 1976,
  Q = 2093,
  R = 2349,
  S = 2637,
  T = 2793,
  U = 3136,
  V = 3520,
  W = 3951,
  X = 4186,
  Y = 4698,
  Z = 5274
)

ui <- fluidPage(
  titlePanel("Sound Wave Visualizer"),
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Enter a word or phrase:", value = "Hello"),
      helpText("This app visualizes the sound wave based on your input.  Each letter corresponds to a different frequency.")
    ),
    mainPanel(
      plotOutput("waveformPlot")
    )
  )
)

server <- function(input, output) {
  
  output$waveformPlot <- renderPlot({
    phrase <- toupper(input$phrase)  # Convert to uppercase
    frequencies <- unlist(lapply(strsplit(phrase, "")[[1]], function(letter) {
      letter_frequencies[[letter]] %||% 0  # Return 0 if letter not found
    }))
    
    # Remove 0 frequencies
    frequencies <- frequencies[frequencies != 0]
    
    if (length(frequencies) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid letters entered.") + theme_void())
    }
    
    # Create a time vector
    time <- seq(0, 1, length.out = 1000)
    
    # Generate the combined waveform
    waveform <- Reduce("+", lapply(frequencies, function(freq) {
      sin(2 * pi * freq * time)
    }))
    
    # Create a data frame for plotting
    plot_data <- data.frame(time = time, amplitude = waveform)
    
    # Create the plot using ggplot2
    ggplot(plot_data, aes(x = time, y = amplitude)) +
      geom_line() +
      labs(x = "Time (s)", y = "Amplitude", title = "Simulated Waveform") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)