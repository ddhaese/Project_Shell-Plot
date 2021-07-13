library(magrittr)
library(data.table)
library(whisker)
library(systemfonts)
library(OpenRepGrid)
library(config)
library(uuid)

config <- config::get(file = "shell/config.yaml")

layout_default <- config$layout
style_default <- config$style

rem_overlap <- function(x) {
  epi <- copy(x)
  epi <- epi[order(Subject, Start)]
  epi[, To_Delete := FALSE]
  epi[, Index := 1:.N, Subject]
  
  subjects <- epi$Subject %>% unique
  pb <- txtProgressBar()
  
  for (cur_sub in subjects) {
    setTxtProgressBar(pb, match(cur_sub, subjects) / length(subjects))
    mem_ind <- 1
    n <- epi[Subject == cur_sub, .N]
    
    if (n < 2) {
      next
    }
    
    for (cur_ind in 2:n) {
      cur_sta <- epi[Subject == cur_sub & Index == cur_ind, Start]
      
      if (cur_sta < epi[mem_ind, End]) {
        cur_end <- epi[Subject == cur_sub & Index == cur_ind, End]
        epi[Subject == cur_sub & Index == mem_ind, End := cur_end]
        epi[Subject == cur_sub & Index == cur_ind, To_Delete := TRUE]
        
      } else {
        mem_ind <- cur_ind
      }
    }
  }
  
  epi <- epi[To_Delete == FALSE]
  epi[, To_Delete := NULL]
  epi[, Index := NULL]
  
  close(pb)
  
  return(epi)
}

obs_to_rad <- function(x, layout) {
  rad <- (5 * pi / 2) - (x / layout$max_obs * layout$alpha_max)
  rad <- rad %% (2 * pi)
  
  return(rad)
}

get_subjects <- function(episodes, layout, style) {
  subjects <- episodes[, .(
    Episode_Count = .N,
    Obs_Start = min(Start) %>% round(style$digits),
    Obs_End = max(End) %>% round(style$digits),
    Lab_X = -layout$b), Subject]

  subjects <- subjects[order(Obs_End)]
  
  subjects[, Subject_Index := .I ]
  subjects[, Lab_Y := (layout$v * layout$m + Subject_Index * layout$m - 1) %>%
      round(style$digits)]
  
  return(subjects)
}

get_label_widths <- function(labels, font_height, style) {
  labels <- labels %>% as.character
  glyphs <- labels %>% strsplit("") %>% unlist %>% unique
  glyph_details <- glyph_info(glyphs, style$font$family) %>% as.data.table
  glyph_ar <- glyph_details[, .(Glyph = glyph, Width = x_advance, Height = height)]
  
  setkey(glyph_ar, "Glyph")
  
  label_width <- function(label) {
    label_ar <- glyph_ar[label %>% strsplit("") %>% unlist, sum(Width) / max(Height)]
    
    return(font_height * label_ar)
  }

  label_widths <- labels %>%
    lapply(label_width) %>%
    unlist
  
  return(label_widths)
}

calculate_alpha_max <- function(label_widths, layout) {
  l_i <- cummax(label_widths %>% rev) %>% rev
  i <- 1:(layout$n)

  x <- -(l_i + layout$b)
  y <- (i - layout$n) * (1  - layout$a) / layout$n - layout$a

  atan3 <- Vectorize(atan2)

  alpha_max <- 5 * pi / 2 - min(atan3(-y, x)) - layout$s

  return(alpha_max)
}

add_observation_sectors <- function(subjects, layout, style) {
  subjects[, Obs_Start_Rad := Obs_Start %>% obs_to_rad(layout) %>%
      round(style$digits)]
  subjects[, Obs_End_Rad := Obs_End %>% obs_to_rad(layout) %>%
      round(style$digits)]
  subjects[, Delta_Angle := ((Obs_End - Obs_Start) / layout$max_obs *
      layout$alpha_max) %>% round(style$digits)]
  
  subjects <- subjects[order(Subject_Index)]
  
  subjects[, Large_Arc := 1 * (Delta_Angle > pi)]
  subjects[, R_Outer := (layout$a + (1 - layout$a) * (.N - .I + 1) / .N)
     %>% round(style$digits)]
  subjects[, R_Inner := (layout$a + (1 - layout$a) * (.N - .I) / .N)
    %>% round(style$digits)]
  subjects[, X_Start_Outer :=
    (R_Outer * cos(Obs_Start_Rad)) %>% round(style$digits)]
  subjects[, Y_Start_Outer :=
    (-1 * R_Outer * sin(Obs_Start_Rad)) %>% round(style$digits)]
  subjects[, X_End_Outer :=
    (R_Outer * cos(Obs_End_Rad)) %>% round(style$digits)]
  subjects[, Y_End_Outer :=
    (-1 * R_Outer * sin(Obs_End_Rad)) %>% round(style$digits)]
  subjects[, X_Start_Inner :=
    (R_Inner * cos(Obs_Start_Rad)) %>% round(style$digits)]
  subjects[, Y_Start_Inner :=
    (-1 * R_Inner * sin(Obs_Start_Rad)) %>% round(style$digits)]
  subjects[, X_End_Inner :=
    (R_Inner * cos(Obs_End_Rad)) %>% round(style$digits)]
  subjects[, Y_End_Inner :=
    (-1 * R_Inner * sin(Obs_End_Rad)) %>% round(style$digits)]
  
  return(subjects)
}

add_episode_sectors <- function(episodes, layout, style) {
  episodes[, Start_Rad := Start %>% obs_to_rad(layout) %>% round(style$digits)]
  episodes[, End_Rad := End %>% obs_to_rad(layout) %>% round(style$digits)]
  episodes[, Delta_Angle := ((End - Start) / layout$max_obs *
      layout$alpha_max) %>% round(style$digits)]
  
  spacer <- layout$m * (1 - layout$e) / 2
  
  episodes[, Large_Arc := 1 * (Delta_Angle > pi)]
  episodes[, R_Outer := layout$a + (1 - layout$a) *
      (layout$n - Subject_Index + 1) / layout$n - spacer]
  episodes[, R_Inner := layout$a + (1 - layout$a) *
      (layout$n - Subject_Index) / layout$n + spacer]
  episodes[, X_Start_Outer :=
    (R_Outer * cos(Start_Rad)) %>% round(style$digits)]
  episodes[, Y_Start_Outer :=
    (-1 * R_Outer * sin(Start_Rad)) %>% round(style$digits)]
  episodes[, X_End_Outer :=
    (R_Outer * cos(End_Rad)) %>% round(style$digits)]
  episodes[, Y_End_Outer :=
    (-1 * R_Outer * sin(End_Rad)) %>% round(style$digits)]
  episodes[, X_Start_Inner :=
    (R_Inner * cos(Start_Rad)) %>% round(style$digits)]
  episodes[, Y_Start_Inner :=
    (-1 * R_Inner * sin(Start_Rad)) %>% round(style$digits)]
  episodes[, X_End_Inner :=
    (R_Inner * cos(End_Rad)) %>% round(style$digits)]
  episodes[, Y_End_Inner :=
    (-1 * R_Inner * sin(End_Rad)) %>% round(style$digits)]
  episodes[, Fill := layout$legend[match(Group, Id), Color]]
  
  return(episodes)
}

add_gridlines <- function(subjects, layout, style, gridlines = NULL) {
  if (is.null(gridlines)) {
    gridlines <- data.table(
      Obs = c(0, layout$max_obs) %>%
        pretty %>% 
        extract(. < (layout$max_obs)))
    
  } else {
    gridlines <- data.table(Obs = gridlines)
  }
  
  spacer <- layout$z * layout$max_obs / layout$alpha_max
  
  get_line_length <- function(Obs) {
    line_length <- subjects[, Obs_End > (Obs - spacer)] %>%
      sum %>% 
      multiply_by(layout$m) %>% 
      add(layout$a)
    
    return(line_length)
  }
  
  if (is.character(layout$unit)) {
    gridlines[, Label := Obs %>% paste(layout$unit)]
    
  } else {
    gridlines[, Label := Obs %>% (layout$unit)]
  }
  
  gridlines[, Length := Obs %>% lapply(get_line_length) %>% unlist]
  gridlines[, Angle := Obs %>% obs_to_rad(layout) %>% round(style$digits)]
  gridlines[, Start_X := (layout$a * cos(Angle)) %>% round(style$digits)]
  gridlines[, Start_Y := (-layout$a * sin(Angle)) %>% round(style$digits)]
  gridlines[, End_X := ((Length + layout$d) * cos(Angle)) %>% round(style$digits)]
  gridlines[, End_Y := (-(Length + layout$d) * sin(Angle)) %>% round(style$digits)]
  gridlines[, Lab_X := ((Length + layout$d + layout$c) * cos(Angle)) %>%
    round(style$digits)]
  gridlines[, Lab_Y := (-(Length + layout$d + layout$c) * sin(Angle) + layout$w) %>%
    round(style$digits)]
  gridlines[, Anchor := ifelse(abs(Lab_X) < layout$a, "middle",
    ifelse(Lab_X > 0, "start", "end"))]

  return(gridlines)
}

define_viewbox <- function(subjects, layout, style) {
  top <- -1 - layout$d - layout$c - layout$h - layout$k
  right <- 1 - layout$m *
    (min(subjects[Delta_Angle >= (pi / 2), Subject_Index]) - 1)
  right <- max(c(right, subjects[Delta_Angle < (pi / 2),
    X_End_Outer])) +
    layout$k
  right <- max(c(right, layout$legend$Lab_X +
      get_label_widths(layout$legend$Label, layout$o, style))) +
    layout$k
  right <- max(c(right, layout$gridlines$Lab_X +
      get_label_widths(layout$gridlines$Label, layout$h, style))) +
    layout$k
  bottom <- 1 - layout$m *
    (min(subjects[Delta_Angle >= pi, Subject_Index]) - 1) + layout$k
  bottom <- max(c(bottom, layout$legend$Lab_Y + layout$k))
  bottom <- max(c(bottom, layout$gridlines$Lab_Y + layout$k + layout$w))
  left <- suppressWarnings(-(1 - layout$m *
    (min(subjects[Delta_Angle >= (3 * pi / 2), Subject_Index]) - 1)))
  left <- min(c(left, layout$fx - layout$k))
  left <- min(c(left, subjects$Lab_X -
      get_label_widths(subjects$Subject, layout$m, style) - layout$k))
  left <- min(c(left, layout$gridlines$Lab_X - 
      get_label_widths(layout$gridlines$Label, layout$m, style) -
      layout$k))
  
  width <- abs(right - left)
  height <- abs(top - bottom)
  
  viewbox <- c(left, top, width, height) %>%
    round(style$digits)
  
  return(viewbox)
}

add_legend <- function(layout) {
  legend <- layout$legend[, .(
    Label,
    Color,
    Symbol_Y = layout$fy + layout$p * (.I - 1) + .5 * (layout$p - layout$q),
    Lab_X = layout$fx + layout$q + layout$g,
    Lab_Y = layout$fy + layout$p * (.I + layout$u))]
  
  return(legend)
}

shell_plot <- function(episodes, width = "600px", height = "800px",
  layout = layout_default, style = style_default, id = UUIDgenerate(),
  gridlines = NULL, shell_template = "shell/svg/shell_template.svg",
  infobox_template = "shell/html/infobox-template.html",
  javascript = "shell/js/shell.js", css = "shell/css/shell.css") {
  
  shell_template <- shell_template %>%
    readChar(., file.info(.)$size)
  
  infobox_template <- infobox_template %>%
    readChar(., file.info(.)$size)
  
  javascript <- javascript %>%
    readChar(., file.info(.)$size)
  
  css <- css %>%
    readChar(., file.info(.)$size)
  
  layout$n <- episodes$Subject %>% uniqueN
  layout$m <- (1 - layout$a) / layout$n
  layout$max_obs <- episodes %$% End %>% max
  
  # Subjects
  subjects <- get_subjects(episodes, layout, style)
  label_widths <- get_label_widths(subjects$Subject, layout$m, style)
  layout$alpha_max <- calculate_alpha_max(label_widths, layout)

  subjects <- subjects %>% add_observation_sectors(layout, style)
  subjects[, Subject_ID := paste0(id, "-", Subject_Index)]
  
  # Episodes
  episodes <- episodes %>%
    merge.data.table(subjects[,
      .(Subject, Subject_Index, Episode_Count, Obs_End, Subject_ID)])
  col_keep <- names(episodes) %>% copy
  data_strings <- episodes[, do.call(paste, c(.SD, sep = "|")),
    .SDcols = col_keep] %>% 
    copy
  
  episodes[, Data := data_strings]
  episodes <- episodes %>% add_episode_sectors(layout, style)
  
  # Legend
  layout$legend <- add_legend(layout)

  # Grid lines
  layout$gridlines <- add_gridlines(subjects, layout, style, gridlines)
  
  # Generate View-box
  layout$viewbox <- define_viewbox(subjects, layout, style)
  
  layout$legend <- layout$legend %>% rowSplit %>% unname
  layout$gridlines <- layout$gridlines %>% rowSplit %>% unname
  layout$viewbox <- layout$viewbox %>% paste0(collapse = " ")
  
  out_data <- list(
    subjects = subjects %>% rowSplit %>% unname,
    episodes = episodes %>% rowSplit %>% unname,
    width = width,
    height = height,
    layout = layout,
    style = style,
    headers = col_keep %>% paste0(collapse = "|"),
    javascript = javascript,
    css = css,
    infobox_template = infobox_template)
  
  out <- whisker.render(
    template = shell_template,
    data = out_data)
  
  return(out)
}

generate_data <- function (n, a = .2, b = 4, c1 = .1, c2 = .75,
  d1 = 1, d2 = 1, e = .2) {
  
  observations <- b * exp(a * 1:n)
  
  get_events <- function (o) {
    starts <- c(0, runif(round(o * runif(1, c1, c2)), 0, o - e))
    lengths <- rnorm(length(starts), d1, d2)
    stops <- pmin(pmax(starts + e, starts + lengths), o)
    out <- data.table(Start = starts, End = stops, Observation = o)
    out <- out[order(Start)
      ]
    return(out)
  }
  
  observations <- observations %>% lapply(get_events) %>% rbindlist
  observations[, Max_Subject := max(End), Observation]
  observations <- observations[order(Max_Subject)]
  observations[, Subject := .GRP, Observation]
  observations <- observations %>% rem_overlap
  observations[, Group := (Start / max(End) * pi * 4) %>% jitter(,1) %>%
      sin %>% multiply_by(1.5) %>% add(1.5) %>%  ceiling]
  observations[, Group := LETTERS[Group]]
  
  labels <- LETTERS[1:3]
  
  layout <- layout_default
  layout$legend <- data.table(
    Id = labels,
    Label = labels,
    Color = palette())
  
  out <- observations %>%
    shell_plot(layout = layout)
  
  write(out, "out/out.html")
  
  return(observations)
}

flip <- function(x) {
  return(c(x, rev(x)))
}

piep <- generate_data(25)

par(bg = "#323232")
piep[, plot(26 - Subject ~ End, type = "n", xlab = "", ylab = "",
  bty = "n", axes = FALSE)] -> dummy
axis(1, col = "white", col.ticks = "white", family = "Roboto",
  col.axis="white")

piep[, polygon(
  x = range(c(Start, End)) %>% flip,
  y = rep((26 - .GRP) + .5 * c(-1, 1), each = 2),
  border = NA, col = "#646464"), Subject] -> dummy

draw_episode <- function(start, end, subject, group) {
  polygon(
    x = c(start, end) %>% flip,
    y = rep((26 - subject) + .5 * c(-1, 1), each = 2),
    border = NA, col = palette()[match(group, LETTERS[1:3])])
}

palette(colorRampPalette(c("steelblue", "orange"))(3))
piep[, draw_episode(Start, End, Subject, Group),
  by = seq_len(nrow(piep))] -> dummy
abline(v = 0:5 * 100, col = "white", lwd =  2, lty = 1)
