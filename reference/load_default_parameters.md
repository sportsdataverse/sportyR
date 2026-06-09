# Load Default Surface Parameters

Load default parameters for a specified league. This should only be used
when debugging the package

## Usage

``` r
load_default_parameters(
  league = "",
  display_range = "full",
  court_updates = list(),
  field_updates = list(),
  pitch_updates = list(),
  rink_updates = list(),
  sheet_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  court_units = NULL,
  field_units = NULL,
  pitch_units = NULL,
  rink_units = NULL,
  sheet_units = NULL,
  xlims = NULL,
  ylims = NULL
)
```

## Arguments

- league:

  The league to load into the global environment

- display_range:

  The display range to load into the global environment

- court_updates:

  The default `court_updates` to load into the global environment. This
  will default to an empty list

- field_updates:

  The default `field_updates` to load into the global environment. This
  will default to an empty list

- pitch_updates:

  The default `pitch_updates` to load into the global environment. This
  will default to an empty list

- rink_updates:

  The default `rink_updates` to load into the global environment. This
  will default to an empty list

- sheet_updates:

  The default `sheet_updates` to load into the global environment. This
  will default to an empty list

- color_updates:

  The default `color_updates` to load into the global environment. This
  will default to an empty list

- rotation:

  The default rotation to load into the global environment. This will
  default to `0`

- x_trans:

  The default translation in the `x` direction to load into the global
  environment. This will default to `0`

- y_trans:

  The default translation in the `y` direction to load into the global
  environment. This will default to `0`

- court_units:

  The default units for a court-like surface. The default will be `NULL`

- field_units:

  The default units for a field-like surface. The default will be `NULL`

- pitch_units:

  The default units for a pitch-like surface. The default will be `NULL`

- rink_units:

  The default units for a rink-like surface. The default will be `NULL`

- sheet_units:

  The default units for a sheet-like surface. The default will be `NULL`

- xlims:

  The default limits on the plot to use in the `x` direction. The
  default will be `NULL`

- ylims:

  The default limits on the plot to use in the `y` direction. The
  default will be `NULL`

## Value

Nothing, but environment variables should be set
