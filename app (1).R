# ============================================================
# Finviz Market Explorer
# Author: Parth Gupta
# Run with: shiny::runApp()
# Packages: shiny, bslib, dplyr, ggplot2, scales, DT, rvest, httr
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(rvest)
library(httr)

AUTHOR     <- "Parth Gupta"
CACHE_FILE <- "finviz_data.csv"
BLUE       <- "steelblue"

# ---------- Parsing helpers (same cleaning logic as the original script) ----------

parse_cap <- function(x) {
  x <- gsub("B", "e9", x); x <- gsub("M", "e6", x); x <- gsub("K", "e3", x)
  x[x == "-"] <- NA
  suppressWarnings(as.numeric(x))
}
parse_num <- function(x) {
  x <- gsub("[%,]", "", x); x[x == "-"] <- NA
  suppressWarnings(as.numeric(x))
}

scrape_page <- function(r) {
  url  <- paste0("https://finviz.com/screener?v=111&r=", r)
  html <- read_html(httr::content(httr::GET(url), "text"))
  col  <- function(n, link = TRUE) {
    sel <- paste0("tr.styled-row td:nth-child(", n, ")", if (link) " a" else "")
    html %>% html_elements(sel) %>% html_text(trim = TRUE)
  }
  ticker <- html %>% html_elements("tr.styled-row td:nth-child(2)") %>%
    html_text() %>% sub("^.", "", .)
  df <- data.frame(
    Ticker   = ticker,
    Company  = col(3), Sector = col(4), Industry = col(5), Country = col(6),
    MarketCap = parse_cap(col(7)), PE = parse_num(col(8)),
    Price = parse_num(col(9)), Change = parse_num(col(10)),
    Volume = parse_num(col(11)),
    stringsAsFactors = FALSE
  )
  df
}

scrape_finviz <- function(max_r = 11641, on_progress = function(i, n) {}) {
  starts <- seq(1, max_r, 20)
  out <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    Sys.sleep(0.5)  # avoid being flagged by finviz
    out[[i]] <- tryCatch(scrape_page(starts[i]), error = function(e) NULL)
    on_progress(i, length(starts))
  }
  bind_rows(out)
}

# ---------- Synthetic demo data (only used when no scraped data exists) ----------

make_demo <- function(n = 800) {
  set.seed(42)
  sectors <- c("Technology", "Healthcare", "Financial", "Energy", "Real Estate",
               "Industrials", "Consumer Cyclical", "Utilities", "Basic Materials")
  industries <- paste0("Industry ", LETTERS[1:15])
  countries <- c("USA", "China", "Taiwan", "South Korea", "Japan", "Canada", "UK", "India")
  mcap <- exp(rnorm(n, 21.5, 2))
  data.frame(
    Ticker = paste0("DEMO", seq_len(n)), Company = paste("Demo Co", seq_len(n)),
    Sector = sample(sectors, n, TRUE), Industry = sample(industries, n, TRUE),
    Country = sample(countries, n, TRUE, prob = c(.55, .1, .05, .05, .08, .07, .05, .05)),
    MarketCap = mcap,
    PE = pmax(rnorm(n, 20, 12), 1),
    Price = exp(rnorm(n, 3, 1)),
    Change = rt(n, 3) * 1.5,
    Volume = exp(log(mcap) * 0.35 + rnorm(n, 0, 1.2)),
    stringsAsFactors = FALSE
  )
}

num_axis <- label_number(scale_cut = cut_short_scale())

# ---------- UI ----------

ui <- page_navbar(
  title = "Finviz Market Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  header = uiOutput("data_banner"),
  footer = div(class = "text-center text-muted p-3",
               paste0("Built by ", AUTHOR, " \u00b7 Data: finviz.com screener")),

  nav_panel("Overview",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Companies", textOutput("n_companies")),
      value_box("Sectors", textOutput("n_sectors")),
      value_box("Countries", textOutput("n_countries")),
      value_box("Total market cap", textOutput("total_cap"))
    ),
    card(
      card_header("About this app"),
      p("An interactive version of an R analysis of ~11,500 stock listings scraped from Finviz.",
        "Explore how market cap relates to trading volume, which industries trade the most,",
        "how countries and sectors rank by market cap, and how volatile each sector is."),
      p(strong("Author: "), AUTHOR),
      hr(),
      p("Use the button below to scrape fresh data (takes roughly 5\u201310 minutes and is",
        "rate-limited to 0.5s per request). Results are cached to ", code(CACHE_FILE), "."),
      actionButton("scrape", "Scrape live Finviz data", class = "btn-primary")
    )
  ),

  nav_panel("Market Cap vs Volume",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("mv_zoom", "Zoom in (cap \u2264 $50B, volume \u2264 5M)", FALSE),
        checkboxInput("mv_log", "Log-log scale", FALSE),
        sliderInput("mv_alpha", "Point opacity", 0.1, 1, 0.5, step = 0.1)
      ),
      card(plotOutput("mv_plot", height = 480)),
      card(textOutput("mv_cor"))
    )
  ),

  nav_panel("Industries",
    layout_sidebar(
      sidebar = sidebar(sliderInput("ind_n", "Top N industries", 5, 25, 10)),
      card(plotOutput("ind_plot", height = 500))
    )
  ),

  nav_panel("Countries",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("ctry_metric", "Rank by",
                     c("Total market cap" = "total", "Average market cap per company" = "avg")),
        sliderInput("ctry_n", "Top N countries", 5, 20, 10),
        sliderInput("ctry_min", "Min. companies per country", 1, 50, 1),
        helpText("Averages are dominated by countries with few, very large companies.",
                 "Raising the minimum filters out those small samples.")
      ),
      card(plotOutput("ctry_plot", height = 500))
    )
  ),

  nav_panel("Sectors",
    layout_sidebar(
      sidebar = sidebar(sliderInput("sec_n", "Top N sectors", 3, 15, 10)),
      card(plotOutput("sec_plot", height = 500))
    )
  ),

  nav_panel("Volatility",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("vol_range", "Y-axis range (% change)", 1, 50, 7.5, step = 0.5),
        helpText("Wider boxes = more volatile sector on the day scraped.",
                 "This is a one-day snapshot, so it shows daily movement, not long-term growth.")
      ),
      card(plotOutput("vol_plot", height = 500)),
      card(card_header("Median and spread by sector"), DTOutput("vol_table"))
    )
  ),

  nav_panel("Data", card(DTOutput("data_table")))
)

# ---------- Server ----------

server <- function(input, output, session) {

  rv <- reactiveValues(
    data   = if (file.exists(CACHE_FILE)) read.csv(CACHE_FILE, stringsAsFactors = FALSE) else make_demo(),
    source = if (file.exists(CACHE_FILE)) "cached" else "demo"
  )

  observeEvent(input$scrape, {
    withProgress(message = "Scraping Finviz...", value = 0, {
      res <- scrape_finviz(on_progress = function(i, n) setProgress(i / n, detail = paste(i, "/", n, "pages")))
    })
    if (nrow(res) > 0) {
      write.csv(res, CACHE_FILE, row.names = FALSE)
      rv$data <- res; rv$source <- "live"
      showNotification(paste("Scraped", nrow(res), "companies."), type = "message")
    } else {
      showNotification("Scrape returned no rows (Finviz may have blocked the request or changed its layout).",
                       type = "error", duration = 10)
    }
  })

  output$data_banner <- renderUI({
    if (rv$source == "demo")
      div(class = "alert alert-warning m-2",
          strong("Demo mode: "), "this is synthetic placeholder data, not real market data.",
          " Click \u201cScrape live Finviz data\u201d on the Overview tab to load the real thing.")
  })

  d <- reactive(rv$data)

  # Overview
  output$n_companies <- renderText(comma(nrow(d())))
  output$n_sectors   <- renderText(n_distinct(d()$Sector))
  output$n_countries <- renderText(n_distinct(d()$Country))
  output$total_cap   <- renderText(paste0("$", num_axis(sum(d()$MarketCap, na.rm = TRUE))))

  # Market cap vs volume
  mv_data <- reactive(d() %>% filter(!is.na(MarketCap), !is.na(Volume), MarketCap > 0, Volume > 0))

  output$mv_plot <- renderPlot({
    p <- ggplot(mv_data(), aes(MarketCap, Volume)) +
      geom_point(alpha = input$mv_alpha, color = BLUE) +
      theme_minimal(base_size = 14) +
      labs(title = if (input$mv_zoom) "Market Cap vs Volume (zoomed)" else "Market Cap vs Volume",
           x = "Market Cap ($)", y = "Volume (shares)")
    if (input$mv_log) {
      p <- p + scale_x_log10(labels = num_axis) + scale_y_log10(labels = num_axis)
    } else {
      p <- p + scale_x_continuous(labels = num_axis) + scale_y_continuous(labels = num_axis)
      if (input$mv_zoom) p <- p + coord_cartesian(xlim = c(0, 5e10), ylim = c(0, 5e6))
    }
    p
  })

  output$mv_cor <- renderText({
    x <- mv_data()
    r_raw <- cor(x$MarketCap, x$Volume)
    r_log <- cor(log10(x$MarketCap), log10(x$Volume))
    sprintf("Pearson r (raw) = %.2f; Pearson r (log-log) = %.2f. The raw value is pulled around by a few mega-caps; the log version reflects the typical relationship better.",
            r_raw, r_log)
  })

  # Industries
  output$ind_plot <- renderPlot({
    d() %>% filter(!is.na(Volume)) %>%
      group_by(Industry) %>% summarize(Total_Volume = sum(Volume), .groups = "drop") %>%
      slice_max(Total_Volume, n = input$ind_n) %>%
      ggplot(aes(reorder(Industry, Total_Volume), Total_Volume)) +
      geom_col(fill = BLUE) + coord_flip() +
      scale_y_continuous(labels = num_axis) +
      theme_minimal(base_size = 14) +
      labs(title = "Industry vs Volume Traded", x = "Industry", y = "Total volume")
  })

  # Countries
  output$ctry_plot <- renderPlot({
    g <- d() %>% filter(!is.na(MarketCap)) %>%
      group_by(Country) %>%
      summarize(Total = sum(MarketCap), Average = mean(MarketCap), n = n(), .groups = "drop") %>%
      filter(n >= input$ctry_min)
    metric <- if (input$ctry_metric == "total") "Total" else "Average"
    g <- g %>% mutate(Value = .data[[metric]]) %>% slice_max(Value, n = input$ctry_n)
    ggplot(g, aes(reorder(Country, Value), Value)) +
      geom_col(fill = BLUE) + coord_flip() +
      scale_y_continuous(labels = num_axis) +
      theme_minimal(base_size = 14) +
      labs(title = if (metric == "Total") "Country vs Total Market Cap" else "Country vs Average Market Cap per Company",
           x = "Country", y = if (metric == "Total") "Market cap ($)" else "Average market cap ($)")
  })

  # Sectors
  output$sec_plot <- renderPlot({
    d() %>% filter(!is.na(MarketCap)) %>%
      group_by(Sector) %>% summarize(Cap = sum(MarketCap), .groups = "drop") %>%
      slice_max(Cap, n = input$sec_n) %>%
      ggplot(aes(reorder(Sector, Cap), Cap)) +
      geom_col(fill = "orange") + coord_flip() +
      scale_y_continuous(labels = num_axis) +
      theme_minimal(base_size = 14) +
      labs(title = "Sector vs Market Cap", x = "Sector", y = "Market cap ($)")
  })

  # Volatility
  output$vol_plot <- renderPlot({
    r <- input$vol_range
    d() %>% filter(!is.na(Change)) %>%
      ggplot(aes(Sector, Change)) +
      geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.alpha = 0.5) +
      coord_flip(ylim = c(-r, r)) +
      theme_minimal(base_size = 14) +
      labs(title = "Sector Volatility: Distribution of % Change", x = "Sector", y = "Change (%)")
  })

  output$vol_table <- renderDT({
    d() %>% filter(!is.na(Change)) %>%
      group_by(Sector) %>%
      summarize(Companies = n(), Median = round(median(Change), 2),
                IQR = round(IQR(Change), 2), SD = round(sd(Change), 2), .groups = "drop") %>%
      arrange(desc(IQR)) %>%
      datatable(rownames = FALSE, options = list(pageLength = 12, dom = "tp"))
  })

  # Data
  output$data_table <- renderDT({
    datatable(d(), filter = "top", rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
