# ============================================================
# Script: SV_Durchschnitte_Leaflet_new.R
# Purpose: Generate interactive deviation map for 2024
# Returns: A Leaflet object
# ============================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(stringr)

# ------------------------------------------------------------
# 1. Helper Function: Custom Legend (Must be defined first!)
# ------------------------------------------------------------
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 0) na.color <- NULL
    if (type != "numeric" && !missing(bins)) warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) pretty(values, bins) else bins   
      if (length(bins) > 2) if (!all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))) stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

# ------------------------------------------------------------
# 2. Main Function: Generate Map
# ------------------------------------------------------------
generate_emp_deviation_map <- function(ar, munich_map_sf) {
  
  # 1. Process Employment Data
  ar_data_long <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich",
      Raumbezug != "Stadt München"
    ) %>%
    mutate(
      Value = 100 * `Basiswert 1` / `Basiswert 2`, 
      bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
    ) %>%
    select(Jahr, bezirksnummer, Value) %>% 
    filter(!is.na(bezirksnummer))
  
  # 2. Join with Map Geometry AND Transform to WGS84
  # ★★★ CRITICAL: st_transform(4326) ★★★
  final_sf_data_long <- munich_map_sf %>%
    st_transform(4326) %>%
    mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0")) %>%
    left_join(ar_data_long, by = c("sb_nummer" = "bezirksnummer")) %>%
    filter(!is.na(Jahr))
  
  # 3. Filter for 2024
  data_2024 <- final_sf_data_long %>%
    filter(Jahr == 2024)
  
  # 4. Calculate Munich Average
  munich_2024 <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich",
      Raumbezug == "Stadt München",
      Jahr == 2024
    ) %>%
    mutate(Value = 100 * `Basiswert 1` / `Basiswert 2`) %>%
    pull(Value)
  mean_2024 <- munich_2024
  
  # 5. Deviation
  data_2024 <- data_2024 %>% mutate(Value_dev = Value - mean_2024)
  
  # 6. Palette
  max_abs <- max(abs(data_2024$Value_dev), na.rm = TRUE)
  pal_div <- colorNumeric(palette = "PuOr", domain = c(-max_abs, max_abs), reverse = TRUE)
  
  # 7. Popup
  popup_content_2024 <- paste0(
    "<b>Stadtteil:</b> ", data_2024[["name"]], "<br/>",
    "<b>Frauenbeschäftigung 2024:</b> ", round(data_2024$Value, 1), " %", "<br/>",
    "<b>München-Durchschnitt 2024:</b> ", round(mean_2024, 1), " %", "<br/>",
    "<b>Abweichung:</b> ", sprintf("%+.1f%%", data_2024$Value_dev)
  )
  
  # 8. Leaflet Map
  leaflet_map <- leaflet(data_2024, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
    addTiles(urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png", attribution = '&copy; CartoDB') %>%    
    setView(lng = 11.5761, lat = 48.1372, zoom = 10) %>%
    addPolygons(
      fillColor = ~pal_div(Value_dev),
      weight = 1.5, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
      highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
      label = lapply(popup_content_2024, HTML)
    ) %>%
    addLegend_decreasing(  # <--- 调用这里时，上面的 Helper 函数必须已定义
      pal = pal_div, values = c(-max_abs, max_abs), opacity = 0.7,
      title = HTML("Abweichung der<br>Frauenbeschäftigung<br>vom Durchschnitt (2024)"),
      labFormat = labelFormat(suffix = " % ", digits = 1),
      position = "bottomright", decreasing = TRUE
    )
  
  return(leaflet_map)
}