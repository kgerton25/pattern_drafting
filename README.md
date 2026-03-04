# pattern_drafting
Draft sewing patterns to your own measurements using R
Approach based on Dorothy Moore's Pattern Drafting and Design

# Basic Pants
```
install.packages(c(
  'tidyverse',
  'here',
  'cowplot'
))

library(tidyverse)
source(here::here("pattern_functions/pants_front.R"))
source(here::here("pattern_functions/pants_back.R"))
source(here::here("pattern_functions/utils.R"))

pattern_front <- pants_front(
  crotch_length = 11,
  waist = 35,
  hip = 49,
  inseam = 33,
  outseam = 44,
  leg_opening = 13
)

pattern_back <- pants_back(
  crotch_length = 11,
  waist = 35,
  hip = 49,
  inseam = 33,
  outseam = 44,
  leg_opening = 13,
  large_seat_adj = 1
)

pattern_front$pattern
pattern_back$pattern

ggplot2::ggsave("sloper_pants_front.pdf", 
                pattern_front$pattern,
                width = max(pattern_front$points$x) - min(pattern_front$points$x),
                height = max(pattern_front$points$y) - min(pattern_front$points$y),
                units = "in",
                limitsize = FALSE)

ggplot2::ggsave("sloper_pants_back.pdf", 
                pattern_back$pattern,
                width = max(pattern_back$points$x) - min(pattern_back$points$x),
                height = max(pattern_back$points$y) - min(pattern_back$points$y),
                units = "in",
                limitsize = FALSE
)
```
