# Animating Tracking Data

For the following vignette, you’ll need the `sportyR`, `ggplot2`, and
`gganimate` packages loaded into your workspace.

``` r

library(sportyR)
library(ggplot2)
library(gganimate)
#> No renderer backend detected. gganimate will default to writing frames to separate files
#> Consider installing:
#> - the `gifski` package for gif output
#> - the `av` package for video output
#> and restarting the R session
```

If this is your first experience with plotting tracking data, please
check out the
[plotting-tracking-data](https://sportyR.sportsdataverse.org/articles/plotting-tracking-data.md)
vignette. Otherwise, let’s see how to make GIFs with `sportyR` and
`gganimate`.

## The Data

For this example, we’ll use a play from Week 15 of the 2018 NFL season
between the Chicago Bears and Green Bay Packers. Data made available for
the [Big Data Bowl
2021](https://www.kaggle.com/c/nfl-big-data-bowl-2021) Kaggle
competition.

``` r

# Load the play data
example_nfl_play <- data.table::fread(
  glue::glue(
    "https://raw.githubusercontent.com/sportsdataverse/sportyR/",
    "main/data-raw/example-pbp-data.csv"
  )
)

# Convert to data frame
example_nfl_play <- as.data.frame(example_nfl_play)
```

To keep things easy, let’s specify the colors for each team’s dots on
the resulting GIF. We’ll make the Bears orange and the Packers yellow.
The football will also need a dot to be seen; let’s make it brown.

``` r

# Prep data for plotting
example_nfl_play[example_nfl_play["team"] == "home", "color"] <- "#c83803"
example_nfl_play[example_nfl_play["team"] == "away", "color"] <- "#ffb612"
example_nfl_play[example_nfl_play["team"] == "football", "color"] <- "#624a2e"
```

First, let’s draw an NFL field via `geom_football("nfl")`. We’ll adjust
the origin to be in the lower left corner of the field, as per the notes
on the coordinate system on the Kaggle page describing the data.

``` r

# Create the field
nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

# Display the field
nfl_field
```

![Image of NFL field rendered from
sportyR](https://raw.githubusercontent.com/sportsdataverse/sportyR/main/vignettes/img/animating-tracking-data-bdb-example-draw-field.png)

Image of NFL field rendered from sportyR

Looks good! Now, let’s animate using `gganimate`.

``` r

# Add the points on the field
play_anim <- nfl_field +
  geom_point(
    data = example_nfl_play,
    aes(x, y),
    color = example_nfl_play$color
  ) +
  transition_time(example_nfl_play$frameId)

# Show the animation
play_anim
```

![Gif of tracking
data](https://raw.githubusercontent.com/sportsdataverse/sportyR/main/vignettes/img/animating-tracking-data-bdb-example-animate-play.gif)

Gif of tracking data

Easy peasy. As noted on the
[plotting-tracking-data](https://sportyR.sportsdataverse.org/articles/plotting-tracking-data.md)
vignette, this too works so long as the geospatial data is provided and
contains a way to identify and order the frames of the resulting GIF.
