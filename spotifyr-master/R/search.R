

#' Get Spotify catalog information about artists, albums,
#' tracks or playlists that match a keyword string.
#'
#' @param q Required. The search query's keywords
#' (and optional field filters and operators),
#' for example q=roadhouse%20blues.
#' @param Required. A comma-separated list of item types
#' to search across. Valid types are: album, artist,
#' playlist, and track
search <- function(q,type,...){

  response <- GET(url = search_url,
                query=list(q=q,type=type,...))
  get_response_content(response)
}

