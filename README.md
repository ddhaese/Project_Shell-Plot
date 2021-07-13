# Shell plot

(for visualizing positively skewed longitudinal data)

![](.media/cover_02.png)

## Introduction

This repository introduces a new plot type for positively skewed longitudinal data. It is written for the R statistical language. Contact me on [LinkedIn](https://www.linkedin.com/in/david-d-haese-bab30652/) for help or suggestions.

See the [tutorial]() on how to prepare this or similar _interactive_ plot from _scratch_ plus some examples.

## Quick start

1. Clone this repository
2. Copy the shell subfolder and make it a subfolder of your R project
3. Start from longitudinal data and make sure individual events have `Start` and `End` times (or distances, &hellip;) grouped by `Subject` (one composite annulus in the plot)

```r
library(data.table)
library(magrittr)

waves <- fread("dat/waves.tsv", sep = "\t")

waves[, Start :=  Wave_Distance %>% add(.1) %>% log10]
waves[, End := Start + .1]
waves[, Subject :=  Quake %>% as.character %>% as.factor]
```

4. Decide on which variable to use for coloring and create the `Group` variable accordingly

```r
breaks <- c(-Inf, 1, 2, 5, 10, 25, Inf)
labels <- c("< 1m", "1-2m", "2-5m", "5-10m", "10-25m", "> 25m")

waves[, Group := Wave_Height %>%
    cut(breaks = breaks, labels = labels)]
```

5. Provide legend details

```r
layout <- copy(layout_default) # Intrigued about the `copy`? Let me know and I'll explain.

layout$legend <- data.table(
  Id = labels %>% c(NA),
  Label = labels %>% c("(missing)"),
  Color = colorRampPalette(c("steelblue", "orange"))(6) %>%
    c("lightgrey"))
layout$legend_title <- "Wave height"
layout$fx <- -.4
layout$fy <- -.9
layout$s <- .3
layout$unit <- function (x) {
  return(paste0(10 ^ x, "km"))
}
```

6. Decide on the hover-over information to show and adjust an infobox template accordingly

```html
<div class="infobox-template" style="display: none;">
  <aside>
    <p class="shell-message">Click episode to copy the data to the clipboard.</p>
    <table class="cycle">
      <caption>Wave {{Wave}}</caption>
      <colgroup>
        <col class="prop-label">
        <col class="prop-value">
      </colgroup>
      <tr>
        <th scope="row">Location</th>
        <td>{{Wave_Province}} ({{Wave_Country}})<br>lat: {{Wave_Latitude}}, lon: {{Wave_Longitude}}</td>
      </tr>
      <tr>
        <th scope="row">Time</th>
        <td>{{Wave_Date}}</td>
      </tr>
      <tr>
        <th scope="row">Height</th>
        <td>{{Wave_Height}} m</td>
...
```

7. Knit in Rmarkdown (or write to HTML using `%>% write("out/out.html")` instead)

```r
waves %>%
  shell_plot (
    width = "500px",
    height = "800px",
    layout = layout,
    infobox_template = "html/waves.html") %>% cat
```

## More

Often, when we observe natural phenomena, the distribution of the total observation period is long-tailed. This limits the capacity to visualize events that take place during these observations. Several years ago, I realised there is a simple solution for this problem and that is to bend the observation axis into a circle.

![](.media/cover_03.png)

Now, I revisited this plot and decided to make it using vector-based graphics (SVG) and add some interactivity. I haven't wrapped the code into an R package yet, let me know if you would find it useful.

## Cite

To site this work:

D'Haese D. (2021) Shellplot for visualizing positively skewed longitudinal data. Accessed on yyy-mm-dd. url: 
