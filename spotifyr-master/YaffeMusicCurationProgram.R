library(spotifyr)

# First Step: make client_id and client_secret available to other function
set_credentials(client_id="f29c0e5b664640ec81413e38ad07cf4e",client_secret="81745be4ae564b8ea0d2c2114b5f38a9")
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

#Example
#genre = 'Good Singing'
#id = "6LlQBOQweWj8N5TK4S2HtH"
#playlistnum = 3

yaffeCurate <- function(trackID, genre, playlist_num)
{
  #trackID ="6LlQBOQweWj8N5TK4S2HtH"
  #genre="Alternative"
  #playlist_num=3
  GENRE = genre
  PLAYLIST_LIMIT = playlist_num
  TARGET_DATA <- get_track_data(trackID)
  TARGET_TEMPO <- TARGET_DATA$tempo
  TARGET_ENERGY <- TARGET_DATA$energy
  resultIds <- list()
  playlistSearchResults <- search(GENRE, type="playlist", limit=PLAYLIST_LIMIT)
  for(a in playlistSearchResults$playlists$items)
  {
    playlistResult <- strsplit(a$uri, ":")
    possibleTracks <- get_playlist_tracks(playlistResult[[1]][3], playlistResult[[1]][5])
    for(i in possibleTracks$items)
    {
      data <- get_track_data(i$track$id)
      possTempo <- data$tempo
      possEnergy <- data$liveness
      if(TARGET_TEMPO - possTempo > -10 && TARGET_TEMPO - possTempo < 10 &&  TARGET_ENERGY - possEnergy > -0.8 && TARGET_ENERGY - possEnergy < 0.8)
      {
        resultIds <- c(resultIds, i$track$id)
      }
    }
  }
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

yaffeCurateAI <- function(trackID, genre, playlist_num, machineLearningSpecificity, scaleData)
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
  if(scaleData == TRUE)
  {
    myTracksMatrixData <- scale(myTracksMatrixData)
  }
  myTracksMatrixData[is.na(myTracksMatrixData)] <- 0
  myTracksMatrix <- cbind(myTracksMatrix, myTracksMatrixData)
  colnames(myTracksMatrix) = c("ID", features)
  colnames(myTracksMatrixData) = c(features)
  #View(myTracksMatrix)
  print("Creating intelligence")
  print("Using artificial intelligence")
  #KMEANS
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

yaffeCuratePlus2 <- function(trackID, genre, playlist_num, machineLearningSpecificity, scaleData)
{
  TARGET_DATA <- get_track_data(trackID)
  results <- yaffeCurate(trackID, genre, playlist_num)
  myTracksMatrix <- matrix(nrow = length(results)+1, ncol = 1)
  myTracksMatrixData <- matrix(nrow = length(results)+1, ncol = 12)
  myTracksMatrix[1, ] = c(trackID)
  myTracksMatrixData[1, ] = c(TARGET_DATA$danceability, TARGET_DATA$energy, TARGET_DATA$key, TARGET_DATA$loudness, TARGET_DATA$mode, TARGET_DATA$speechiness, TARGET_DATA$acousticness, TARGET_DATA$instrumentalness, TARGET_DATA$liveness, TARGET_DATA$valence, TARGET_DATA$tempo, TARGET_DATA$time_signature)
  rowcounter <- 2
  resultIds <- list()
  for(i in results)
  {
    dataFeatures <- get_track_data(i)
    myTracksMatrix[rowcounter, ] <- c(i)
    myTracksMatrixData[rowcounter, ] <- c(dataFeatures$danceability, dataFeatures$energy, dataFeatures$key, dataFeatures$loudness, dataFeatures$mode, dataFeatures$speechiness, dataFeatures$acousticness, dataFeatures$instrumentalness, dataFeatures$liveness, dataFeatures$valence, dataFeatures$tempo, dataFeatures$time_signature)
    rowcounter <- rowcounter + 1
  }
  features <- names(get_track_data("2Z2vOukzwOgqR4dkymRSWd"))
  features <- features[-(12:17)]
  if(scaleData == TRUE)
  {
    myTracksMatrixData <- scale(myTracksMatrixData)
  }
  myTracksMatrix <- cbind(myTracksMatrix, myTracksMatrixData)
  colnames(myTracksMatrix) = c("ID", features)
  colnames(myTracksMatrixData) = c(features)
  #View(myTracksMatrix)
  print("Creating intelligence")
  print("Using artificial intelligence")
  #KMEANS
  myTracksMatrixData[is.na(myTracksMatrixData)] <- 0
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
  resultIds <- cbind(resultIds, names, artists)
  print("done")
  return(resultIds)
}

yaffeCuratePlus <- function(trackID, genre, playlist_num, machineLearningSpecificity, scaleData)
{
  resultIds <- list()
  TARGET_DATA <- get_track_data(trackID)
  TARGET_TEMPO <- TARGET_DATA$tempo
  TARGET_ENERGY <- TARGET_DATA$energy
  results <- yaffeCurateAI(trackID, genre, playlist_num, machineLearningSpecificity, scaleData)
  print("Doing further analysis")
  for(i in results)
  {
    data <- get_track_data(i)
    possTempo <- data$tempo
    possEnergy <- data$liveness
    if(TARGET_TEMPO - possTempo > -5 && TARGET_TEMPO - possTempo < 5 &&  TARGET_ENERGY - possEnergy > -0.4 && TARGET_ENERGY - possEnergy < 0.4)
    {
      resultIds <- c(resultIds, data$id)
    }
  }
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
  resultIds <- cbind(resultIds, names, artists)
  print("done")
  return(resultIds)
}




features <- names(get_track_data("2Z2vOukzwOgqR4dkymRSWd"))
features <- features[-(12:17)]
myTracks <- get_playlist_tracks("1249508096", "2ecPMHWNpjixgpFUCxFr9h")
myTracksMatrix <- matrix(nrow = length(myTracks$items), ncol = 1)
myTracksMatrixData <- matrix(nrow = length(myTracks$items), ncol = 12)
rowcounter <- 1
counter <- 1
while(counter <= length(myTracks$items))
{
  i = myTracks$items[[counter]]
  dataFeatures <- get_track_data(i$track$id)
  myTracksMatrix[rowcounter, ] <- c(i$track$id)
  myTracksMatrixData[rowcounter, ] <- c(dataFeatures$danceability, dataFeatures$energy, dataFeatures$key, dataFeatures$loudness, dataFeatures$mode, dataFeatures$speechiness, dataFeatures$acousticness, dataFeatures$instrumentalness, dataFeatures$liveness, dataFeatures$valence, dataFeatures$tempo, dataFeatures$time_signature)
  rowcounter <<- rowcounter + 1
  counter <<- counter + 1
}

myTracksMatrixData[is.na(myTracksMatrixData)] <- 0
myTracksMatrixData <- scale(myTracksMatrixData)
myTracksMatrix <- cbind(myTracksMatrix, myTracksMatrixData)
colnames(myTracksMatrix) = c("ID", features)
colnames(myTracksMatrixData) = c(features)


#CLUSTERING
#Hierarchical Clustering
d <- dist(myTracksMatrixData, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
#groups <- cutree(fit, k=3) # cut tree into clusters
# draw dendogram with red borders around the clusters
#rect.hclust(fit, k=3, border="red")

#KMEANS
library(ggplot2)
results <- kmeans(myTracksMatrixData, 3)
df <- data.frame(myTracksMatrixData)
df$cluster <- factor(results$cluster)
centers <- as.data.frame(results$centers)
ggplot(data=df, aes(x=tempo,y=loudness,color=cluster)) + geom_point()






# Opens Browser window asking for permission to access data
# Requires Spotify account and for user to login
user_code <- get_user_code()

# Assigns tokens for later use.
# This creates a variable access_token in the Global Environment
# that all future functions will access.
user_tokens <- get_user_token(user_code)

# If your access_token expires and you need to refresh your tokens
# With no parameters specified, looks for refresh_token in the Global Environment
refresh_tokens <- refresh_user_token()

# If you want, you can explicitly specify the refresh token
refresh_tokens <- refresh_user_token(user_tokens$refresh_token)


