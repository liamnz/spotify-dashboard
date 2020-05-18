library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(glue)


# Prepare -----------------------------------------------------------------

access_token <- get_spotify_access_token()

terms <- c('short_term', 'medium_term', 'long_term')

my_top_tracks <- map_dfr(terms, ~ get_my_top_artists_or_tracks('tracks', limit = 50, time_range = .x), .id = 'term') %>% as_tibble()
my_top_tracks <- my_top_tracks %>% mutate(term = factor(term, labels = terms))

track_features <- map_dfr(terms, ~ get_track_audio_features(my_top_tracks$id[my_top_tracks$term == .x]), .id = 'term')
track_features <- track_features %>% mutate(term = factor(term, labels = terms))

# Spotify docs: https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/

features <- read_csv('features.csv', col_types = cols(.default = col_character()))

feature_charts <- list()

for (i in seq_along(features$feature)){
  chart <- track_features %>% 
    ggplot(aes(!!sym(features$feature[i]), colour = str_to_title(str_replace(term, "_", " ")))) +
    geom_density(bw = 'nrd') +
    guides(colour = guide_legend(title = 'Listening Range', title.position = 'top')) +
    labs(x = str_to_title(features$feature[i])) +
    theme_minimal() +
    theme(legend.position = 'top',
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.x.bottom = element_line(colour = 'azure4'),
          axis.text.x = element_text(colour = 'azure4'))
  
  if (features$scale[i] == 'unit'){
    chart <- chart + scale_x_continuous(limits = c(0, 1))
  }
  
  feature_charts[[i]] <- chart
  
}

names(feature_charts) <- features$feature

# Render dashboard ------------------------------------------------

me <- get_my_profile()

rmd_yml <- glue("
---
title: {me$display_name}
output:
  flexdashboard::flex_dashboard:
    vertical_layout: scroll
---
")

rmd_feature <- function(index){
  glue(
"
  
{str_to_title(features$feature[index])} {{data-orientation=rows}}
================================================================================

```{{r}}
feature <- '{features$feature[index]}'
```



Row {{data-height=500}}
--------------------------------------------------------------

### dist {{.no-title}}

```{{r dev='svg'}}
feature_charts[[feature]]
```



Row {{data-height=200}}
--------------------------------------------------------------------------------


### {features$alt_name_2[index]} track

```{{r}}
track <- track_features$id[which.min(track_features[[feature]])]
track <- my_top_tracks %>% filter(id == track) %>% slice(1)
artist <- get_artist(track$artists[[1]]$id[1])

```

{str_to_title(features$feature[index])}: `r head(track_features[[feature]][track_features$id == track$id], 1)`

Track: [`r track$name`](`r track$external_urls.spotify`)

Artist: [`r track$artists[[1]]$name`](`r track$artists[[1]]$external_urls.spotify`)

Album: [`r track$album.name`](`r track$album.external_urls.spotify`)

### Artist {{.no-title}}

![](`r artist$images[2,2]`)


### Artist {{.no-title}}

```{{r}}
track <- track_features$id[which.max(track_features[[feature]])]
track <- my_top_tracks %>% filter(id == track) %>% slice(1)
artist <- get_artist(track$artists[[1]]$id[1])
```


![](`r artist$images[2,2]`)

### {features$alt_name_1[index]} track

{str_to_title(features$feature[index])}: `r head(track_features[[feature]][track_features$id == track$id], 1)`

Track: [`r track$name`](`r track$external_urls.spotify`)

Artist: [`r track$artists[[1]]$name`](`r track$artists[[1]]$external_urls.spotify`)

Album: [`r track$album.name`](`r track$album.external_urls.spotify`)

Row {{data-height=100}}
--------------------------------------------------------------------------------

### Description

`r features$description[features$feature == feature]`


")
} 

rmd_features <- map_chr(seq_along(features$feature), rmd_feature)

rmd_doc <- glue_collapse(c(rmd_yml, rmd_features))

write_lines(rmd_doc, 'dashboard.Rmd')

rmarkdown::render('dashboard.Rmd', output_file = 'Spotify Dashboard.html')

