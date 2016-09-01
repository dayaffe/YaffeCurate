
library(spotifyr)
# First Step: make client_id and client_secret available to other function
set_credentials(client_id="f29c0e5b664640ec81413e38ad07cf4e",client_secret="81745be4ae564b8ea0d2c2114b5f38a9", "")
client_tokens <- get_tokens()
access_token = client_tokens$access_token  

#' Set credentials to be accessed by all functions
#'
#' @param client_id the client ID given from Spotify
#' @param client_secret the client secret ID given from Spotify
set_credentials <- function(client_id,client_secret,client_redirect_uri){
  assign('client_id', client_id,envir=.GlobalEnv)
  assign('client_secret',client_secret,envir=.GlobalEnv)
  assign('client_redirect_uri',client_redirect_uri,envir=.GlobalEnv)
}


#' Get tokens for Client Credential
#' This function looks for client_id and client_secret in the global environment
get_tokens <- function(){
  response <- POST('https://accounts.spotify.com/api/token',
                   accept_json(),
                   authenticate(client_id,client_secret),
                   body=list(grant_type='client_credentials'),
                   encode='form')
  
  get_response_content(response)
}


#' Get user code for Authorization Code user code
#' Lauches Selenium Webbrowser to handle process
#' This function looks for client_id and client_secret in the global environment
#'
get_user_code <- function(){
  
  response <- GET(url=authorize_url,
                  query=list(client_id=client_id,
                             response_type='code',
                             scope=all_scopes,
                             redirect_uri='http://www.bertplot.com/visualization/'))
  
  unlink(system.file("bin", package = "RSelenium"), recursive = T)
  checkForServer()
  startServer(log = FALSE, invisible = FALSE)
  
  webd <- remoteDriver()
  Sys.sleep(3)
  # webd <- remoteDriver(remoteServerAddr = "localhost",browserName='chrome',port=4455)
  x <- webd$open()
  x <- webd$navigate(response$url)
  
  ## Need to wait for next page
  ## Probably a better way to do this.
  while(str_detect(webd$getCurrentUrl()[[1]],'accounts.spotify.com')) Sys.sleep(2)
  
  user_code <- str_split(webd$getCurrentUrl()[[1]],pattern='code=')[[1]][2]
  
  # Close everything down
  webd$closeWindow()
  webd$closeServer()
  
  user_code
}

#' Using the user_code, generates tokens for user code
#'
#' @param user_code user_code from get_user_code() function
get_user_token <- function(user_code){
  
  response <- POST('https://accounts.spotify.com/api/token',
                   accept_json(),
                   authenticate(client_id,client_secret),
                   body=list(grant_type='authorization_code',
                             code=user_code,
                             redirect_uri=client_redirect_uri),
                   encode='form')
  
  content <- get_response_content(response)
  
  # Make accessible globally
  assign('access_token',content$access_token,envir = .GlobalEnv)
  assign('refresh_token',content$refresh_token,envir = .GlobalEnv)
  
  content
}

#' Refresh your tokens
#'
#' @param token default = NULL.  If no token is given, it will look for refresh_token in global environment
refresh_user_token <- function(token=NULL){
  
  if(is.null(token) && !exists('refresh_token')) stop("Need to provide refresh token")
  if(is.null(token) && exists('refresh_token')) token <- refresh_token
  
  response <- POST('https://accounts.spotify.com/api/token',
                   accept_json(),
                   authenticate(client_id,client_secret),
                   body=list(grant_type='refresh_token',
                             refresh_token=token,
                             redirect_uri='http://www.bertplot.com/visualization/'),
                   encode='form')
  
  content <- get_response_content(response)
  
  # Make accessible globally
  assign('access_token',content$access_token,envir = .GlobalEnv)
  
  content
}

get_track_feature_data_url <- 'https://api.spotify.com/v1/audio-features/'
get_track_url <- 'https://api.spotify.com/v1/tracks/'

#' Get a Track's feature data
get_track_data <- function(track_id,...){
  response <- GET(url = paste0(get_track_feature_data_url, track_id,sep=''),
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',client_tokens$access_token)))
  get_response_content(response)
}

#' Get a Track
get_track <- function(track_id,...){
  response <- GET(url = paste0(get_track_url, track_id,sep=''),
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',client_tokens$access_token)))
  get_response_content(response)
}

yaffeCurateAI <- function(trackID, genre, playlist_num, machineLearningSpecificity, tempEmphasis, danceEmph, energyEmph, keyEmph, loudnessEmph, modeEmph, speechinessEmph, acousticnessEmph, instrumentalnessEmph, livenessEmph, valenceEmph, time_signatureEmph)
{ 
  #trackID ="6LlQBOQweWj8N5TK4S2HtH"
  #genre="Alternative"
  #playlist_num=3
  GENRE = genre
  PLAYLIST_LIMIT = playlist_num
  print("Gathering data on your track")
  TARGET_DATA <- get_track_data(trackID)
  TARGET_TEMPO <- TARGET_DATA$tempo
  TARGET_ENERGY <- TARGET_DATA$energy
  resultIds <- list()
  playlistSearchResults <- search(GENRE, type="playlist", limit=PLAYLIST_LIMIT)
  counts <- list()
  totalNumCounter <- 0
  for(a in playlistSearchResults$playlists$items)
  {
    playlistResult <- strsplit(a$uri, ":")
    possibleTracks <- get_playlist_tracks(playlistResult[[1]][3], playlistResult[[1]][5])
    totalNumCounter <- totalNumCounter + length(possibleTracks$items)
  }
  totalNumCounter <- totalNumCounter + 1
  rowcounter <- 2
  #print(totalNumCounter)
  myTracksMatrix <- matrix(nrow = totalNumCounter, ncol = 1)
  myTracksMatrixData <- matrix(nrow = totalNumCounter, ncol = 12)
  myTracksMatrix[1, ] = c(trackID)
  myTracksMatrixData[1, ] = c(TARGET_DATA$danceability, TARGET_DATA$energy, TARGET_DATA$key, TARGET_DATA$loudness, TARGET_DATA$mode, TARGET_DATA$speechiness, TARGET_DATA$acousticness, TARGET_DATA$instrumentalness, TARGET_DATA$liveness, TARGET_DATA$valence, TARGET_DATA$tempo, TARGET_DATA$time_signature)
  print("Gathering data on tracks in queried playlists")
  for(a in playlistSearchResults$playlists$items)
  {
    playlistResult <- strsplit(a$uri, ":")
    possibleTracks <- get_playlist_tracks(playlistResult[[1]][3], playlistResult[[1]][5])
    for(i in possibleTracks$items)
    {
      dataFeatures <- get_track_data(i$track$id)
      myTracksMatrix[rowcounter, ] <- c(i$track$id)
      myTracksMatrixData[rowcounter, ] <- c(dataFeatures$danceability, dataFeatures$energy, dataFeatures$key, dataFeatures$loudness, dataFeatures$mode, dataFeatures$speechiness, dataFeatures$acousticness, dataFeatures$instrumentalness, dataFeatures$liveness, dataFeatures$valence, dataFeatures$tempo, dataFeatures$time_signature)
      rowcounter <- rowcounter + 1
    }
  }
  features <- names(get_track_data("2Z2vOukzwOgqR4dkymRSWd"))
  features <- features[-(12:17)]
  myTracksMatrixData[is.na(myTracksMatrixData)] <- 0
  myTracksMatrixData <- scale(myTracksMatrixData)
  myTracksMatrixData[is.na(myTracksMatrixData)] <- 0
  myTracksMatrix <- cbind(myTracksMatrix, myTracksMatrixData)
  colnames(myTracksMatrix) = c("ID", features)
  colnames(myTracksMatrixData) = c(features)
  #View(myTracksMatrix)
  print("Creating intelligence")
  print("Using artificial intelligence")
  #KMEANS
  myTracksMatrixData[,1] = myTracksMatrixData[,1] * danceEmph
  myTracksMatrixData[,2] = myTracksMatrixData[,2] * energyEmph
  myTracksMatrixData[,3] = myTracksMatrixData[,3] * keyEmph
  myTracksMatrixData[,4] = myTracksMatrixData[,4] * loudnessEmph
  myTracksMatrixData[,5] = myTracksMatrixData[,5] * modeEmph
  myTracksMatrixData[,6] = myTracksMatrixData[,6] * speechinessEmph
  myTracksMatrixData[,7] = myTracksMatrixData[,7] * acousticnessEmph
  myTracksMatrixData[,8] = myTracksMatrixData[,8] * instrumentalnessEmph
  myTracksMatrixData[,9] = myTracksMatrixData[,9] * livenessEmph
  myTracksMatrixData[,10] = myTracksMatrixData[,10] * valenceEmph
  myTracksMatrixData[,11] = myTracksMatrixData[,11] * tempEmphasis
  myTracksMatrixData[,12] = myTracksMatrixData[,12] * time_signatureEmph
  results <- kmeans(myTracksMatrixData, machineLearningSpecificity)
  #df <- data.frame(myTracksMatrixData)
  #df$cluster <- factor(results$cluster)
  #centers <- as.data.frame(results$centers)
  print("Sorting")
  countClusters <- 2
  TARGET_CLUSTER = results$cluster[1]
  while(countClusters <= length(results$cluster))
  {
    if(results$cluster[countClusters] == TARGET_CLUSTER)
    {
      resultIds <- c(resultIds, myTracksMatrix[countClusters, 1])
    }
    countClusters <- countClusters + 1
  }
  print("done")
  resultIds <- unlist(resultIds)
  
  #yaffeCurateResults <- matrix(nrow=length(resultIds), ncol = 3)
  #counter <- 1
  #for(i in resultIds)
  #{
  #  trackInfo <- get_track(i)
  #  yaffeCurateResults[counter, ] <- c(trackInfo$name, trackInfo$artist[[1]]$name, trackInfo$id)
  #  counter <<- counter + 1
  #}
  #colnames(yaffeCurateResults) = c("Name", "Artist", "ID")
  #return(yaffeCurateResults)
  return(resultIds)
}

yaffeCurate <- function(trackID, genre, playlist_num, machineLearningSpecificity, tempoEmphasis, danceEmph, energyEmph, keyEmph, loudnessEmph, modeEmph, speechinessEmph, acousticnessEmph, instrumentalnessEmph, livenessEmph, valenceEmph, time_signatureEmph)
{
  
  
  resultIds <- list()
  results <- yaffeCurateAI(trackID, genre, playlist_num, machineLearningSpecificity, tempoEmphasis, danceEmph, energyEmph, keyEmph, loudnessEmph, modeEmph, speechinessEmph, acousticnessEmph, instrumentalnessEmph, livenessEmph, valenceEmph, time_signatureEmph)
  print("compiling")
  names <- list()
  for(i in results)
  {
    names <- c(names, get_track(i)$name)
  }
  artists <- list()
  for(i in results)
  {
    artists <- c(artists, get_track(i)$artist[[1]]$name)
  }
  names <- unlist(names)
  artists <- unlist(artists)
  result <- cbind(names, artists)
  print("done")
  return(result)
}




shinyServer(function(input, output) {
  df <- eventReactive(input$curateBtn, {
    yaffeCurate(input$trackID, input$playlistQuery, input$playlistNum, input$specificty, input$tempoEmph, input$danceEmph, input$energyEmph, input$keyEmph, input$loudnessEmph, input$modeEmph, input$speechinessEmph, input$acousticnessEmph, input$instrumentalnessEmph, input$livenessEmph, input$valenceEmph, input$time_signatureEmph)
  })
  output$matrix <- renderTable({
    df()
  })
  #observeEvent(input$curateBtn, { 
    
  #  output$matrix = renderTable({
  #    results <- yaffeCuratePlus(input$trackID, input$playlistQuery, input$playlistNum, input$specificty, scaleData=TRUE)
  #    results
  #    })
  #})
})
