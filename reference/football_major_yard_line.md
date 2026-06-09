# Football Major Yard Line

The major yard lines are the yard lines that span the entire width of
the field. Typically, these lines are placed every 5 yards, but the
customization is left to the user. These lines may feature a cross-hash,
which runs in the x-direction

## Usage

``` r
football_major_yard_line(
  field_width = 0,
  feature_thickness = 0,
  dist_to_sideline = 0,
  cross_hash_length = 0,
  cross_hash_separation = 0
)
```

## Arguments

- field_width:

  The width of the field

- feature_thickness:

  The thickness of each of the major yard lines

- dist_to_sideline:

  The distance from the end of the yard line to the interior edge of the
  sideline

- cross_hash_length:

  The length of each cross-hash mark

- cross_hash_separation:

  The interior separation between the cross-hashes

## Value

A data frame containing the bounding box of the major yard lines
