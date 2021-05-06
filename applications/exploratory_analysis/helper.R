openSQLconnection <- function(){con_air <- dbConnect(
    RMySQL::MySQL(),
    dbname = "airlines",
    host = "mdsr.cdc7tgkkqd0n.us-east-1.rds.amazonaws.com",
    user = "mdsr_public",
    password = "ImhsmflMDSwR"
  )
}

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())   
  print(all_cons)
  
  for(con in all_cons)
    +dbDisconnect(con)
 
  print(paste(length(all_cons), " connections killed."))
 
}

flights_import <- function(con_air,local) {
  # Creating a database connection object to be used in dbplyr queries
  
  cols <- c(
    "AA" = "#0078d2",
    "AS" = "#9abd63",
    "B6" = "#7eb4f7",
    "DL" = "#ab1740",
    "F9" = "#006341",
    "HA" = "#cb0686",
    "NK" = "#f7e501",
    "OO" = "#6d8ba6",
    "UA" = "#00319b",
    "VX" = "#dc2424",
    "WN" = "#f29802",
    "YV" = "#251f21"
  )
  
  airline_names <- c(
    "AA" = "American (incl. Subsidiaries)",
    "AS" = "Alaska",
    "B6" = "JetBlue",
    "DL" = "Delta (incl. Subsidiaries)",
    "F9" = "Frontier",
    "HA" = "Hawaii",
    "NK" = "Spirit",
    "OO" = "SkyWest",
    "UA" = "United (incl. Subsidiaries)",
    "VX" = "Virgin",
    "WN" = "Southwest",
    "YV" = "Mesa"
  ) %>%
    as.data.frame() %>%
    rownames_to_column("parent_carrier")
  
  colnames(airline_names)[2] <- "name"
  
  colors <- as.data.frame(cols) %>%
    rownames_to_column()
  
  colnames(colors)[1] <- "parent_carrier"
  colnames(colors)[2] <- "color"
  
  parent_carrier <-
    c("DL","AA","AS","B6","UA","DL","UA","F9","WN","HA","AA","NK","AA","OO","UA","AA","VX","WN","UA","YV")
  
  # Filtering for only flights from defined local airport
  flights_fromlocal <- tbl(con_air, "flights") %>%
    filter(origin == local) %>%
    mutate(date = substr(time_hour, 1, 10)) %>%
    select(date, year, month, day, hour, carrier, dest, dep_delay, distance) %>%
    ungroup() %>%
    collect()
  
  # SELECT-ing for carriers and merging with parent-carrier and custom colour scale data (see Setting up the R Environment above for more details)
  carriers <- tbl(con_air, "flights") %>%
    group_by(carrier) %>%
    summarise() %>%
    collect() %>%
    cbind(parent_carrier) %>%
    left_join(colors)
  
  # Separate data frame of destination airport latitude and longitude for flights that depart from local airport for spatial visualisation later on.
  routes_fromlocal <- tbl(con_air, "flights") %>%
    filter(origin == local) %>%
    group_by(dest) %>%
    summarise(n = n()) %>%
    collect()
  routes_fromlocal <- tbl(con_air, "airports") %>%
    select(dest = faa, lat, lon, city) %>%
    collect() %>%
    right_join(routes_fromlocal) %>%
    na.omit()
  
  # Data point of geographical location for local airport.
  local_geo <- tbl(con_air, "airports") %>%
    filter(faa == local) %>%
    select(lat, lon) %>%
    collect()
  
  # Separate data frame for airports that do not receive any flights from local airport.
  airports_no_flights <- tbl(con_air, "airports") %>%
    select(dest = faa, lat, lon, city) %>%
    collect() %>%
    anti_join(routes_fromlocal) %>%
    na.omit()
  
  return(
    list(
      "cols"=cols,
      "airline_names"        = airline_names,
      "flights_fromlocal"    = flights_fromlocal,
      "carriers"             = carriers,
      "routes_fromlocal"     = routes_fromlocal,
      "local_geo"            = local_geo,
      "airports_no_flights"  = airports_no_flights
    )
  )}

makeplot_carriers_1 <- function(importedSQL,local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    mutate(is_late = ifelse(dep_delay > 0, T, F)) %>%
    group_by(parent_carrier, is_late) %>%
    summarise(n = n()) %>%
    mutate(prop_on_time = round(n / sum(n), digits = 2)) %>%
    left_join(importedSQL$airline_names) %>%
    ggplot(
      aes(
        x = fct_reorder(name, n),
        y = n,
        alpha = ifelse(is_late != 1, 1, .85),
        fill = parent_carrier,
        color = parent_carrier
      )
    ) +
    geom_col() +
    geom_text(aes(
      x = fct_reorder(name, n),
      y = -4000,
      label = format(round(prop_on_time, 2), nsmall = 2),
      text = paste("")
    ), hjust = 1) +
    coord_flip() +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    expand_limits(y=-12000) +
    #scale_y_sqrt()+
    theme_minimal() +
    labs(
      x = "",
      y = paste(
        "\nFilled portions indicate on-time flights, departing from",
        local
      )
    ) +
    theme(legend.position = "none")
  
  return(p)
}

makeplot_carriers_2 <- function(importedSQL,local) {
  p <- importedSQL$flights_fromlocal %>%
    filter(dep_delay > 0) %>%
    left_join(importedSQL$carriers) %>%
    select(parent_carrier, dep_delay) %>%
    left_join(importedSQL$airline_names) %>%
    ggplot(
      aes(
        x = dep_delay,
        y = name,
        fill = parent_carrier,
        colour = parent_carrier,
        group = name,
        height = ..density..
      )
    ) +
    ggridges::geom_density_ridges(
      stat = "binline",
      trim = TRUE,
      binwidth = 30,
      panel_scaling = TRUE,
      draw_baseline = F,
      scale = .9
    ) +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    theme_minimal() +
    labs(
      x = paste(
        "Departure Delay Distribution by Minutes, departing from",
        local
      ),
      y = ""
    ) +
    theme(legend.position = "none")
  
  return(p)
}

makeplot_carriers_3 <- function(importedSQL,local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    group_by(parent_carrier) %>%
    summarise(delay = mean(dep_delay),
              n = n()) %>%
    left_join(importedSQL$airline_names) %>%
    filter(n >= 100) %>%
    ggplot(
      aes(
        x = fct_reorder(name, delay),
        y = delay,
        fill = parent_carrier,
        color = parent_carrier,
        text = paste(
          "Carrier: ",name,"\n",
          "Number of flights: ",n,"\n",
          "Average Delay: ~",round(delay, digits = 2)," minutes","\n",
          sep = ""
        )
      )
    ) +
    geom_col(size = .01) +
    coord_flip() +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    theme_minimal() +
    labs(x = "",
         y = paste("\nAverage Delay by Minutes, departing from", local)) +
    theme(legend.position = "none")
  
  return(p %>% ggplotly(tooltip = c("text")))
}

makeplot_years_1 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
  left_join(importedSQL$carriers) %>%
  filter(dep_delay > 0) %>%
  group_by(year) %>%
  summarise(delay = median(dep_delay),
            n = n()) %>% 
  mutate(valtype = ifelse(T, "Delayed Only"),
         shapetype = ifelse(T,"Median")) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      group_by(year) %>%
      summarise(delay = median(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "All Flights"),
             shapetype = ifelse(T,"Median"))
  ) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      group_by(year) %>%
      summarise(delay = mean(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "All Flights"),
             shapetype = ifelse(T,"Average"))
  ) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      filter(dep_delay > 0) %>%
      group_by(year) %>%
      summarise(delay = mean(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "Delayed Only"),
             shapetype = ifelse(T,"Average"))
  ) %>% 
    ggplot(
      aes(
        x = year,
        y = delay,
        #fill = parent_carrier,
        color = valtype,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        
        text = paste(
          "Year: ",year,"\n",
          "Number of flights: ",n,"\n",
          valtype, " Delay: ~",round(delay,digits = 1)," minutes","\n",
          "Group: ",valtype,
          sep = ""
        )
      )) +
    geom_line(size = 0.7) +
    geom_point() +
    #scale_fill_manual(values = cols)+
    scale_color_viridis_d()+
    #facet_wrap(~name)+
    theme_minimal() +
    labs(
      x = paste("\nYear-over-Year Average Delay by Minutes, departing from", local),
      y = ""
    ) +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
}

makeplot_years_2 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    group_by(year, parent_carrier) %>%
    summarise(delay = mean(dep_delay),
              n = n()) %>%
    mutate(shapetype = ifelse(1, "Average")) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        group_by(year, parent_carrier) %>%
        summarise(delay = median(dep_delay),
                  n = n()) %>%
        mutate(shapetype = ifelse(T, "Median"))
    ) %>%
    filter(n >= 100) %>%
    left_join(importedSQL$airline_names) %>%
    ggplot(
      aes(
        x = year,
        y = delay,
        fill = parent_carrier,
        color = parent_carrier,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        
      text = paste("Carrier: ", name,"\n",
                   "Year: ", year, "\n",
                   "Number of flights: ", n, "\n",
                   shapetype, " Delay: ~",round(delay)," minutes","\n", sep = "")
      )
    ) +
    geom_line(size = 0.7) +
    geom_point() +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    facet_wrap( ~ name) +
    theme_minimal() +
    labs(
      x = paste(
        "\nYear-over-Year Mean/Median Delay across Carriers by Minutes, departing from",
        local
      ),
      y = ""
    ) +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
  
}

makeplot_months_1 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
  left_join(importedSQL$carriers) %>%
  filter(dep_delay > 0) %>%
  group_by(month) %>%
  summarise(delay = median(dep_delay),
            n = n()) %>% 
  mutate(valtype = ifelse(T, "Delayed Only"),
         shapetype = ifelse(T,"Median")) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      group_by(month) %>%
      summarise(delay = median(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "All Flights"),
             shapetype = ifelse(T,"Median"))
  ) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      group_by(month) %>%
      summarise(delay = mean(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "All Flights"),
             shapetype = ifelse(T,"Average"))
  ) %>%
  rbind(
    importedSQL$flights_fromlocal %>%
      left_join(importedSQL$carriers) %>%
      filter(dep_delay > 0) %>%
      group_by(month) %>%
      summarise(delay = mean(dep_delay),
                n = n()) %>%
      mutate(valtype = ifelse(T, "Delayed Only"),
             shapetype = ifelse(T,"Average"))
  ) %>% 
    ggplot(
      aes(
        x = month,
        y = delay,
        #fill = parent_carrier,
        color = valtype,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        
        text = paste(
          "Month: ",month,"\n",
          "Number of flights: ",n,"\n",
          valtype, " Delay: ~",round(delay,digits = 1)," minutes","\n",
          "Group: ",valtype,
          sep = ""
        )
      )) +
    geom_line(size = 0.7) +
    geom_point() +
    #scale_fill_manual(values = cols)+
    scale_color_viridis_d()+
    #facet_wrap(~name)+
    theme_minimal() +
    labs(
      x = paste("\nMonthly Average Delay by Minutes, departing from", local),
      y = ""
    ) +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
}

makeplot_months_2 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    group_by(month, parent_carrier) %>%
    summarise(delay = mean(dep_delay),
              n = n()) %>%
    mutate(shapetype = ifelse(1, "Average")) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        group_by(month, parent_carrier) %>%
        summarise(delay = median(dep_delay),
                  n = n()) %>%
        mutate(shapetype = ifelse(T, "Median"))
    ) %>%
    filter(n >= 100) %>%
    left_join(importedSQL$airline_names) %>%
    ggplot(
      aes(
        x = month,
        y = delay,
        fill = parent_carrier,
        color = parent_carrier,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        
      text = paste("Carrier: ", name,"\n",
                   "Month: ", month, "\n",
                   "Number of flights: ", n, "\n",
                   shapetype, " Delay: ~",round(delay)," minutes","\n", sep = "")
      )
    ) +
    geom_line(size = 0.7) +
    geom_point() +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    facet_wrap( ~ name) +
    theme_minimal() +
    labs(
      x = paste(
        "\nMonthly Mean/Median Delay across Carriers by Minutes, departing from",
        local
      ),
      y = ""
    ) +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
  
}

makeplot_wdays_1 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
    mutate(time_day = date(date),
           wday = wday(time_day)) %>%
    left_join(importedSQL$carriers) %>%
    filter(dep_delay > 0) %>%
    group_by(wday) %>%
    summarise(delay = median(dep_delay),
              n = n()) %>%
    mutate(valtype = ifelse(T, "Delayed Only"),
           shapetype = ifelse(T, "Median")) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        mutate(time_day = date(date),
               wday = wday(time_day)) %>%
        left_join(importedSQL$carriers) %>%
        group_by(wday) %>%
        summarise(delay = median(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "All Flights"),
          shapetype = ifelse(T, "Median")
        )
    ) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        mutate(time_day = date(date),
               wday = wday(time_day)) %>%
        left_join(importedSQL$carriers) %>%
        group_by(wday) %>%
        summarise(delay = mean(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "All Flights"),
          shapetype = ifelse(T, "Average")
        )
    ) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        mutate(time_day = date(date),
               wday = wday(time_day)) %>%
        left_join(importedSQL$carriers) %>%
        filter(dep_delay > 0) %>%
        group_by(wday) %>%
        summarise(delay = mean(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "Delayed Only"),
          shapetype = ifelse(T, "Average")
        )
    )  %>%
    ggplot(
      aes(
        x = wday,
        y = delay,
        #fill = parent_carrier,
        color = valtype,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        text = paste(
          "Weekday: ",wday,"\n",
          "Number of flights: ",n,"\n",
          shapetype," Delay: ~",round(delay,digits = 1)," minutes","\n",
          "Group: ",valtype,
          sep = ""
        )
  )) +
    geom_line(size = 0.7) +
    geom_point() +
    #scale_fill_manual(values = cols)+
    scale_color_viridis_d()+
    #facet_wrap(~name)+
    scale_x_discrete(limits = c(1:7)) +
    theme_minimal() +
    labs(
      x = paste(
        "\nMean/Median Delay across days of the week by Minutes, departing from",
        local
      ),
      y = ""
    ) +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
}

makeplot_wdays_2 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>% 
  mutate(time_day = date(date),
         wday = wday(time_day)) %>% 
  left_join(importedSQL$carriers) %>%
  group_by(wday,parent_carrier) %>% 
  summarise(delay = mean(dep_delay),
            n=n()) %>% 
  mutate(shapetype = ifelse(1,"Average")) %>% 
  rbind(
    importedSQL$flights_fromlocal %>%
      mutate(time_day = date(date),
             wday = wday(time_day)) %>% 
      left_join(importedSQL$carriers) %>%
      group_by(wday,parent_carrier) %>%
      summarise(delay = median(dep_delay),
                n = n()) %>%
      mutate(shapetype = ifelse(T, "Median"))
  ) %>% 
  left_join(importedSQL$airline_names) %>% 
  filter(n>=100) %>% 
  ggplot(
    aes(
      x = wday,
      y = delay,
      fill = parent_carrier,
      color = parent_carrier,
      shape = shapetype,
      linetype = shapetype,
      group = 1,
      text = paste("Carrier: ", name,"\n",
                   "Weekday: ", wday, "\n",
                   "Number of flights: ", n, "\n",
                   shapetype," Delay: ~",round(delay)," minutes","\n", sep = "")
    )
  )+
  geom_line(size = 0.7)+
  geom_point()+
  scale_fill_manual(values = importedSQL$cols)+
  scale_color_manual(values = importedSQL$cols)+
  scale_x_discrete(limits=c(1:7))+
  facet_wrap(~name)+
  theme_minimal()+
  labs(
    x = paste("\nMean/Median Delay across days of the week by Minutes, departing from",local),
    y = ""
  )+
  theme(legend.position = "none")

p %>% ggplotly(tooltip = c("text"))
}

makeplot_hours_1 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    filter(dep_delay > 0) %>%
    group_by(hour) %>%
    summarise(delay = median(dep_delay),
              n = n()) %>%
    mutate(valtype = ifelse(T, "Delayed Only"),
           shapetype = ifelse(T, "Median")) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        group_by(hour) %>%
        summarise(delay = median(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "All Flights"),
          shapetype = ifelse(T, "Median")
        )
    ) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        group_by(hour) %>%
        summarise(delay = mean(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "All Flights"),
          shapetype = ifelse(T, "Average")
        )
    ) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        filter(dep_delay > 0) %>%
        group_by(hour) %>%
        summarise(delay = mean(dep_delay),
                  n = n()) %>%
        mutate(
          valtype = ifelse(T, "Delayed Only"),
          shapetype = ifelse(T, "Average")
        )
    )  %>%
    filter(n > 100) %>%
    ggplot(
      aes(
        x = hour,
        y = delay,
        #fill = parent_carrier,
        color = valtype,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        text = paste(
          "Hour: ",hour,"\n",
          "Number of flights: ",n,"\n",
          shapetype," Delay: ~",round(delay,digits = 1)," minutes","\n",
          "Group: ",valtype,
          sep = ""
        )
  )) +
    geom_line(size = 0.7) +
    geom_point() +
    #scale_fill_manual(values = cols)+
    scale_color_viridis_d()+
    #facet_wrap(~name)+
    scale_x_discrete(limits = seq(0, 24, 2)) +
    theme_minimal() +
    labs(x = paste("Mean/Median Delay across hours, departing from", local),
         y = "") +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
}

makeplot_hours_2 <- function(importedSQL, local) {
  p <- importedSQL$flights_fromlocal %>%
    left_join(importedSQL$carriers) %>%
    group_by(hour, parent_carrier) %>%
    summarise(delay = mean(dep_delay),
              n = n()) %>%
    mutate(shapetype = ifelse(1, "Average")) %>%
    rbind(
      importedSQL$flights_fromlocal %>%
        left_join(importedSQL$carriers) %>%
        group_by(hour, parent_carrier) %>%
        summarise(delay = median(dep_delay),
                  n = n()) %>%
        mutate(shapetype = ifelse(T, "Median"))
    ) %>%
    left_join(importedSQL$airline_names) %>%
    filter(n >= 100) %>%
    ggplot(
      aes(
        x = hour,
        y = delay,
        fill = parent_carrier,
        color = parent_carrier,
        shape = shapetype,
        linetype = shapetype,
        group = 1,
        text = paste("Carrier: ", name,"\n",
                     "Hour: ", hour, "\n",
                     "Number of flights: ", n, "\n",
                     shapetype, " Delay: ~",round(delay)," minutes","\n", sep = "")
      )
    ) +
    geom_line(size = 0.7) +
    geom_point() +
    scale_fill_manual(values = importedSQL$cols) +
    scale_color_manual(values = importedSQL$cols) +
    scale_x_discrete(limits = seq(0, 24, 2)) +
    facet_wrap( ~ name) +
    theme_minimal() +
    labs(x = paste("Mean/Median Delay across hours, departing from", local),
         y = "Average Delay (Minutes)") +
    theme(legend.position = "none")
  
  p %>% ggplotly(tooltip = c("text"))
}

makeplot_dist <- function(importedSQL, local){
  importedSQL$flights_fromlocal %>%
    group_by(dest) %>%
    summarise(delay = mean(dep_delay)) %>%
    left_join(importedSQL$routes_fromlocal) %>%
    mutate(dist_km = geosphere::distHaversine(cbind(lon, lat),
                                              cbind(importedSQL$local_geo$lon,importedSQL$local_geo$lat))/1000) %>% 
    ggplot(aes(x    = dist_km,
               y    = delay,
               text = paste("Airport: ",dest,
                            "\nCity: ",city,
                            "\nDistance :",round(dist_km,digits=2),
                            "\nAvg. Delay: ",round(delay,digits=2),
                            sep = ""))) +
  geom_jitter()+
  labs(
    x = "Distance (km)",
    y = "Average Delay"
  )+
  theme_minimal() -> p

p %>% ggplotly(tooltip = c("text"))
}

makeplot_geo <- function(importedSQL, local) {
  importedSQL$flights_fromlocal %>%
    group_by(dest) %>%
    summarise(delay = mean(dep_delay)) %>%
    left_join(importedSQL$routes_fromlocal) -> d2
  
  geo <- list(
    #scope = "usa",
    projection = list(
      type = 'orthographic',
      rotation = list(
        lon = importedSQL$local_geo$lon,
        lat = importedSQL$local_geo$lat,
        roll = 0
      )
    ),
    showland = T,
    landcolor = 'transparent',
    countrycolor = 'transparent'
  )
  
  p <- plot_geo() %>%
    add_markers(
      data = importedSQL$airports_no_flights,
      x = ~ lon,
      y = ~ lat,
      size = .1,
      opacity = .16,
      hoverinfo = "none",
      symbol = I("x-thin"),
    ) %>%
    add_segments(
      data = d2,
      x = importedSQL$local_geo$lon,
      xend = ~ lon,
      y = importedSQL$local_geo$lat,
      yend = ~ lat,
      alpha = .175,
      size = ~ sqrt(n),
      opacity = .5
    ) %>%
    add_markers(
      data = d2,
      x = ~ lon,
      y = ~ lat,
      text = ~ paste(
        "Destination Airport:   ",
        dest,
        "\n",
        "City:                          ",
        city,
        "\n",
        "Average Delay:         ",
        round(delay),
        " minutes",
        "\n",
        "Annual # Flights:      ",
        n / 8,
        sep = ""
      ),
      hoverinfo = "text",
      size = ~ delay ^ 2
    ) %>%
    add_markers(
      data = d2,
      x = importedSQL$local_geo$lon,
      y = importedSQL$local_geo$lat,
      text = paste(local),
      hoverinfo = "text",
      size = I(10),
      symbol = I("star"),
      color = I("red")
    )
  
  ggplotly(p) %>%
    layout(
      title = 'Delay by Destination Airport',
      geo = geo,
      showlegend = F,
      plot_bgcolor = 'transparent',
      paper_bgcolor = 'transparent',
      height = 800, 
      width = 1200
    )
}

