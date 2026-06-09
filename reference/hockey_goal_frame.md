# Hockey Goal Frame

The goal frame is where the puck enters after crossing the goal line to
score a legal goal. The front face of the goal is flush with the goal
line, while the back edge features rounded corners and expands outside
of the front posts. The goal frame is composed of two pieces: the frame
(this method) and the fill (see
[`hockey_goal_frame_fill()`](https://sportyR.sportsdataverse.org/reference/hockey_goal_frame_fill.md))

## Usage

``` r
hockey_goal_frame(
  feature_radius = 0,
  goal_mouth_width = 0,
  goal_back_width = 0,
  goal_depth = 0,
  goal_post_diameter = 0
)
```

## Arguments

- feature_radius:

  The radius of the circular part of the goal frame

- goal_mouth_width:

  The width of the goal mouth

- goal_back_width:

  The width of the back of the frame of the goal

- goal_depth:

  The depth of the goal from the front of the goal line to the back of
  the goal frame

- goal_post_diameter:

  The diameter of the post of the goal

## Value

A data frame containing the bounding coordinates of the frame of the
goal

## Details

The goal frame has two thicknesses to be careful of: the outer diameter
of the posts, and the outer diameter of the pipe in the back of the
goal. The frame of the goal is usually red in color
