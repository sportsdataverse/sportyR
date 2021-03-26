usethis::use_package("glue")

#' Check to see if a league can be plotted, and alert the user as to which
#' functions that league will work for
#'
#' @param league_code The case-insensitive league code to be plotted
#' @return Nothing, but a message is sent to the console for the user
#'
#' @export
caniplot_league = function(league_code){
  # Force the league code to be capitalized, as that is how it appears in the
  # JSON file
  league_code = toupper(league_code)

  # Get the number of sports the league should be able to plot. NOTE: this
  # should only be greater than 1 for NCAA/college
  n_sports = length(league_lookup[[league_code]])

  # If the league code is not in the JSON file, notify user to create an issue
  # on GitHub to add the league to the package
  if(n_sports < 1){
    message(glue::glue("Sorry, {league_code} is not a viable league to plot at this time. Please create an issue on GitHub with the league's playing surface specifications for the league to be added to the package"))
  }

  # If the league code is only associated with 1 sport, notify user that the
  # league can be plotted with geom_{sport}()
  else if(n_sports == 1){
    message(glue::glue("A plot for {league_code} can be created via the geom_{tolower(league_lookup[[league_code]])}() function"))
  }

  # If the league code is associated with more than 1 sport (i.e. NCAA or
  # COLLEGE), notify user of sports available for plotting
  else {
    # Initialize empty string for message
    functions_string = ""

    # Get the sports available for plotting (in alphabetical order)
    sports = sort(league_lookup[[league_code]])

    # Initialize indexer for while loop
    i = 1
    while(i < n_sports){
      # Append the geom_{sport}s to the string
      functions_string = glue::glue("{functions_string}geom_{tolower(sports[i])}(), ")
      i = i + 1
    }

    # Notify user of viable functions
    message(glue::glue("{league_code} can be used in the following functions: {functions_string}or geom_{tolower(sports[i])}()"))
  }
}

#' Check to see if a sport can be plotted, and alert the user as to which
#' leagues are plottable for the sport
#'
#' @param sport The case-insensitive sport name
#' @return Nothing, but a message is sent to the console for the user
#'
#' @export
caniplot_sport = function(sport){
  # Force the sport to be capitalized, as that is how it appears in the JSON
  # file
  sport = toupper(sport)

  # Get the number of leagues the sport should be able to plot
  n_leagues = length(sport_lookup[[sport]])

  # If the sport is not in the JSON file, notify user to create an issue on
  # GitHub to add the sport to the package
  if(n_leagues < 1){
    message(glue::glue("Sorry, {sport} is not a viable sport to plot at this time. Please create an issue on GitHub with the sport's playing surface specifications for the league to be added to the package"))
  }

  # If the sport is only associated with 1 league, notify user that the
  # league can be plotted with geom_{sport}()
  else if(n_leagues == 1){
    message(glue::glue("A plot for {sport} can be created via the geom_{tolower(sport)}() function for the following league: {sport_lookup[[sport]]}"))
  }

  # If the sport is associated with more than 1 league notify user of leagues available for plotting
  else {
    # Get the leagues available for plotting (in alphabetical order)
    leagues = sort(sport_lookup[[sport]])

    # Notify user of viable functions
    message_str = glue::glue("geom_{tolower(sport)}() can be used to plot for the following leagues: ")

    # Initialize indexer for while loop
    i = 1
    while(i <= n_leagues){
      # Append the leagues to the string
      leagues_str = paste(leagues, collapse = ', ')
      i = i + 1
    }

    message(glue::glue("{message_str}{leagues_str}"))
  }
}
