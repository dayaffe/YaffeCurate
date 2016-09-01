library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Yaffe Music Curation"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    textInput("trackID", "Enter track ID:"),
    textInput("playlistQuery", "Query playlists with what title:"),
    sliderInput("playlistNum", "Enter number of playlists to query:", 
                min=1, max=5, value=2),
    sliderInput("specificty", "How close do you want the song to be in general (20-30 is recommended for best results)?:", 
                min = 2, max = 100, value = 20),
    helpText("Choose what you would like to emphasize:"),
    sliderInput("danceEmph", "How much do you want the danceability to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("energyEmph", "How much do you want the energy to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("keyEmph", "How much do you want the key to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("loudnessEmph", "How much do you want the loudness to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("modeEmph", "How much do you want the mode to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("speechinessEmph", "How much do you want the speechiness to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("acousticnessEmph", "How much do you want the acousticness to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("instrumentalnessEmph", "How much do you want the instrumentalness to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("livenessEmph", "How much do you want the liveness to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("valenceEmph", "How much do you want the valence to be emphasized?:", 
                min = 1, max = 10, value = 1),
    sliderInput("tempoEmph", "How much do you want the tempo to be emphasized?:", 
                min = 1, max = 10, value = 5),
    sliderInput("time_signatureEmph", "How much do you want the time signature to be emphasized?:", 
                min = 1, max = 10, value = 1),
    actionButton("curateBtn", "Curate")
    
  ),

  mainPanel(uiOutput('matrix'))
))
