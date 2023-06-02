#' Check to see if a league can be plotted, and alert as to which function(s)
#' that league will work for
#'
#' @param league_code The case-insensitive league code to be plotted
#'
#' @return Nothing, but a message is sent to the console
#'
#' @export
#'
#' @examples
#' cani_plot_league("MLB")
cani_plot_league <- function(league_code) {
  # Force the league code to be lower cased, as that is how it appears in the
  # JSON file
  league_code <- tolower(league_code)

  # Get the sports that sportyR currently supports
  supported_sports <- names(surface_dimensions)

  # Iterate over the sports and get the supported leagues
  for (i in seq_along(supported_sports)) {
    sport <- supported_sports[i]
    if (!exists("sport_league_pairs")) {
      sport_league_pairs <- paste(
        sport,
        names(surface_dimensions[[i]]),
        sep = " - "
      )
    } else {
      sport_league_pairs <- c(
        sport_league_pairs,
        paste(
          sport,
          names(surface_dimensions[[i]]),
          sep = " - "
        )
      )
    }
  }

  # Split the sport-league pairs into a data frame
  sport_league_pairs <- data.frame(
    t(
      sapply(
        strsplit(sport_league_pairs, " - "),
        `[`
      )
    )
  )

  # Rename to make easier to work with
  names(sport_league_pairs) <- c("sport", "league")

  # Drop custom
  sport_league_pairs <- sport_league_pairs[
    sport_league_pairs$league != "custom",
  ]

  # Get the number of sports the league should be able to plot. NOTE: this
  # should only be greater than 1 for NCAA/college
  n_sports <- nrow(
    sport_league_pairs[sport_league_pairs$league == league_code, ]
  )

  # If the league code is not in the JSON file, notify user to create an issue
  # on GitHub to add the league to the package
  if (n_sports < 1) {
    message(
      glue::glue(
        "Sorry, {toupper(league_code)} is not a viable league to plot ",
        "at this time. Please create an issue on GitHub with the league's ",
        "playing surface specifications for the league to be added to the ",
        "package"
      )
    )
  } else if (n_sports == 1) {
    # If the league code is only associated with 1 sport, notify user that the
    # league can be plotted with geom_{sport}()
    league_sport <- sport_league_pairs[
      sport_league_pairs$league == league_code,
      "sport"
    ]
    message(
      glue::glue(
        "A plot for {toupper(league_code)} can be created via the ",
        "geom_{tolower(league_sport)}",
        "() function"
      )
    )
  } else {
    # If the league code is associated with more than 1 sport (i.e. NCAA),
    # notify user of sports available for plotting

    # Initialize empty string for message
    functions_string <- ""

    # Get the sports available for plotting (in alphabetical order)
    league_sports <- sport_league_pairs[
      sport_league_pairs$league == league_code,
      "sport"
    ]

    # Initialize indexer for while loop
    i <- 1
    while (i < n_sports) {
      # Append the geom_{sport}s to the string
      functions_string <- glue::glue(
        "{functions_string}geom_{tolower(league_sports[i])}(), "
      )
      i <- i + 1
    }

    # Notify user of viable functions
    message(
      glue::glue(
        "{toupper(league_code)} can be used in the following functions: ",
        "{functions_string}or geom_{tolower(league_sports[i])}()"
      )
    )
  }
}

#' Check to see if a sport can be plotted, and alert as to which league(s) are
#' plottable for the sport
#'
#' @param sport_code The case-insensitive sport name
#'
#' @return Nothing, but a message is sent to the console
#'
#' @export
#'
#' @examples
#' cani_plot_sport("basketball")
cani_plot_sport <- function(sport_code) {
  # Force the sport to be lower case, as that is how it appears in the JSON file
  sport_code <- tolower(sport_code)

  # Get the sports that sportyR currently supports
  supported_sports <- names(surface_dimensions)

  # If the sport is not in the JSON file, notify user to create an issue on
  # GitHub to add the sport to the package
  if (!(sport_code %in% supported_sports)) {
    message(
      glue::glue(
        "Sorry, {tolower(sport_code)} is not a viable sport to plot at this ",
        "time. Please create an issue on GitHub with the sport's playing ",
        "surface specifications for the league to be added to the package"
      )
    )
  } else {
    # Iterate over the sports and get the supported leagues
    for (i in seq_along(supported_sports)) {
      sport <- supported_sports[i]
      if (!exists("sport_league_pairs")) {
        sport_league_pairs <- paste(
          sport,
          names(surface_dimensions[[i]]),
          sep = " - "
        )
      } else {
        sport_league_pairs <- c(
          sport_league_pairs,
          paste(
            sport,
            names(surface_dimensions[[i]]),
            sep = " - "
          )
        )
      }
    }

    # Split the sport-league pairs into a data frame
    sport_league_pairs <- data.frame(
      t(
        sapply(
          strsplit(sport_league_pairs, " - "),
          `[`
        )
      )
    )

    # Rename to make easier to work with
    names(sport_league_pairs) <- c("sport", "league")

    # Drop custom
    sport_league_pairs <- sport_league_pairs[
      sport_league_pairs$league != "custom",
    ]

    # Get the number of leagues the sport should be able to plot
    n_leagues <- nrow(
      sport_league_pairs[sport_league_pairs$sport == sport_code, ]
    )

    if (n_leagues == 1) {
      # If the sport is only associated with 1 league, notify user that the
      # league can be plotted with geom_{sport}()
      league <- sport_league_pairs[
        sport_league_pairs$sport == sport_code,
        "league"
      ]

      message(
        glue::glue(
          "A plot for {sport_code} can be created via the ",
          "geom_{tolower(sport_code)}() function for the following league: ",
          "{toupper(league)}"
        )
      )
    } else {
      # If the sport is associated with more than 1 league, notify the user of
      # all leagues available for plotting

      # Get the leagues available for plotting (in alphabetical order)
      leagues <- sort(
        sport_league_pairs[
          sport_league_pairs$sport == sport_code,
          "league"
        ]
      )

      # Notify user of viable functions
      message_str <- glue::glue(
        "geom_{tolower(sport_code)}() can be used to plot for the following ",
        "leagues: "
      )

      # Initialize indexer for while loop
      i <- 1
      while (i <= n_leagues) {
        # Append the leagues to the string
        leagues_str <- paste(toupper(leagues), collapse = ", ")
        i <- i + 1
      }

      message(glue::glue("{message_str}{leagues_str}"))
    }
  }
}

#' Check to see what features of a surface can be colored
#'
#' @param league_code The case-insensitive league code to be plotted
#' @param sport_name The name of a sport to use in the event that the
#'   \code{league_code} supplied has more than one sport associated with it.
#'   Default: \code{NULL}
#'
#' @return Nothing, but a message is sent to the console
#'
#' @export
#'
#' @examples
#' cani_color_league_features("NCAA", "basketball")
cani_color_league_features <- function(league_code, sport_name = NULL) {
  # Force the league code to be lower cased, as that is how it appears in the
  # JSON file
  league_code <- tolower(league_code)

  # Get the sports that sportyR currently supports
  supported_sports <- names(surface_dimensions)

  # Iterate over the sports and get the supported leagues
  for (i in seq_along(supported_sports)) {
    sport <- supported_sports[i]
    if (!exists("sport_league_pairs")) {
      sport_league_pairs <- paste(
        sport,
        names(surface_dimensions[[i]]),
        sep = " - "
      )
    } else {
      sport_league_pairs <- c(
        sport_league_pairs,
        paste(
          sport,
          names(surface_dimensions[[i]]),
          sep = " - "
        )
      )
    }
  }

  # Split the sport-league pairs into a data frame
  sport_league_pairs <- data.frame(
    t(
      sapply(
        strsplit(sport_league_pairs, " - "),
        `[`
      )
    )
  )

  # Rename to make easier to work with
  names(sport_league_pairs) <- c("sport", "league")

  # Drop custom
  sport_league_pairs <- sport_league_pairs[
    sport_league_pairs$league != "custom",
  ]

  # Get the number of sports the league should be able to plot. NOTE: this
  # should only be greater than 1 for NCAA/college
  n_sports <- nrow(
    sport_league_pairs[sport_league_pairs$league == league_code, ]
  )

  # If the league code is not in the JSON file, notify user to create an issue
  # on GitHub to add the league to the package
  if (n_sports < 1) {
    message(
      glue::glue(
        "Sorry, {toupper(league_code)} is not a viable league to plot ",
        "at this time. Please create an issue on GitHub with the league's ",
        "playing surface specifications for the league to be added to the ",
        "package"
      )
    )
  } else if (n_sports == 1) {
    # If the league code is only associated with 1 sport, notify user that the
    # league can be plotted with geom_{sport}()
    sport_name <- sport_league_pairs[
      sport_league_pairs$league == league_code,
      "sport"
    ]

    # Get the features
    feature_names <- switch(
      sport_name,
      "baseball" = baseball_features_set_colors(),
      "basketball" = basketball_features_set_colors(),
      "football" = football_features_set_colors(),
      "hockey" = hockey_features_set_colors(),
      "lacrosse" = lacrosse_features_set_colors(),
      "soccer" = soccer_features_set_colors(),
      "tennis" = tennis_features_set_colors(),
      stop(
        glue::glue(
          "Sorry, {toupper(league_code)} is not a viable league to plot ",
          "at this time. Please create an issue on GitHub with the league's ",
          "playing surface specifications for the league to be added to the ",
          "package"
        )
      )
    )

    # Get the names of the features
    feature_names <- names(feature_names)
  } else {
    if (is.null(sport_name)) {
      stop(
        glue::glue(
          "Sorry, to get the feature names you can color for ",
          "{toupper(league_code)}, you must specify the sport_name parameter ",
          "as there are multiple sports that correspond to this league"
        )
      )
    } else {
      league_sports <- sport_league_pairs[
        sport_league_pairs$league == league_code,
        "sport"
      ]
      if (tolower(sport_name) %in% league_sports) {
        # Get the features
        feature_names <- switch(
          sport_name,
          "baseball" = baseball_features_set_colors(),
          "basketball" = basketball_features_set_colors(),
          "football" = football_features_set_colors(),
          "hockey" = hockey_features_set_colors(),
          "lacrosse" = lacrosse_features_set_colors(),
          "soccer" = soccer_features_set_colors(),
          "tennis" = tennis_features_set_colors(),
          stop(
            glue::glue(
              "Sorry, {tolower(sport_name)} is not a viable sport to plot ",
              "for {toupper(league_code)} at this time. Please create an ",
              "issue on GitHub with the league's playing surface ",
              "specifications for the sport-league combination to be added to ",
              "the package"
            )
          )
        )

        # Get the names of the features
        feature_names <- names(feature_names)
      }
    }
  }

  # Finally, display the feature names
  feature_names_string <- ""
  for (feature_name in feature_names) {
    feature_names_string <- glue::glue("{feature_names_string}\n{feature_name}")
  }

  message(
    glue::glue(
      "Here are the viable plotting features to color for ",
      "{toupper(league_code)} {tolower(sport_name)}:\n{feature_names_string}"
    )
  )
}
