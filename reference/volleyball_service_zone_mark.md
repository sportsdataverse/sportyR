# Volleyball Service Zone Mark

The service zone marks are the lines beyond the end lines that denote
where a legal serve must take place. These appear as four hash marks
that are out of bounds of the court, but contained within the free zone
(see
[`volleyball_free_zone()`](https://sportyR.sportsdataverse.org/reference/volleyball_free_zone.md)
for reference)

## Usage

``` r
volleyball_service_zone_mark(service_zone_mark_length = 0, line_thickness = 0)
```

## Arguments

- service_zone_mark_length:

  The distance the service zone mark extends away from the outer edge of
  the end line

- line_thickness:

  The thickness of the service zone marks

## Value

A data frame containing the bounding box of the service zone mark
