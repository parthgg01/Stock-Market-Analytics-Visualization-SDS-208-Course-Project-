# Finviz Market Explorer

An interactive Shiny app for exploring ~11,500 stock listings scraped from [Finviz](https://finviz.com/screener.ashx). It turns an R analysis of market cap, trading volume and daily price movement into something you can filter, zoom and click through.

**Author:** Parth Gupta

## What it does

| Tab | What you can explore |
|---|---|
| **Overview** | Headline stats (companies, sectors, countries, total market cap) and a button to scrape fresh data |
| **Market Cap vs Volume** | Scatter plot with zoom and log-log toggles; correlation reported on both raw and log scales |
| **Industries** | Top N industries by total volume traded |
| **Countries** | Top N countries by total or average market cap, with a minimum-companies filter |
| **Sectors** | Top N sectors by total market cap |
| **Volatility** | Boxplots of daily % change by sector, plus a median / IQR / SD table |
| **Data** | Searchable, filterable table of the full dataset |

## Getting started

### 1. Install dependencies

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2",
                   "scales", "DT", "rvest", "httr"))
```

### 2. Run the app

Put `app.R` in a folder, then from that folder:

```r
shiny::runApp()
```

### 3. Load real data

The first time you run it, the app has no data and shows **synthetic demo data** (a warning banner makes this obvious). To load the real thing, either:

- Click **Scrape live Finviz data** on the Overview tab, or
- Drop a previously saved `finviz_data.csv` into the same folder as `app.R`.

A scrape requests about 580 pages at one request per 0.5 s, so expect roughly 5-10 minutes. The result is saved to `finviz_data.csv` and loaded automatically on future launches.

## Data columns

`Ticker`, `Company`, `Sector`, `Industry`, `Country`, `MarketCap` (USD), `PE`, `Price`, `Change` (% change on the day scraped), `Volume` (shares).

Market cap strings like `1.2B` / `350M` / `20K` are converted to numbers, and `-` becomes `NA`.

## Notes and limitations

- **Volatility is a one-day snapshot.** `Change` is the % move on the day the data was scraped, so the Volatility tab shows daily dispersion, not long-term growth or decline. A sector with a positive median that day is not necessarily a "growing" sector.
- **Missing values are handled per plot.** Each chart only drops rows missing the columns it uses, rather than dropping every row without a P/E (which would exclude all unprofitable companies).
- **Average market cap by country is skewed by small samples.** A country with a handful of very large companies ranks high, so use the minimum-companies slider to filter these out.
- **The scraper depends on Finviz's HTML layout.** If Finviz changes its page structure, or rate-limits or blocks requests, the scrape may return no rows. The app will show an error and keep the data it already has. The CSS selectors in `scrape_page()` are the place to fix.
- **Be polite when scraping.** Keep the delay in `scrape_finviz()` and check Finviz's terms of use before scraping at scale or redistributing the data.

## Project structure

```
.
├── app.R              # the entire Shiny app (UI, server, scraper)
├── finviz_data.csv    # created after a scrape (not required to launch)
└── README.md
```

## Built with

R, Shiny, bslib, dplyr, ggplot2, scales, DT, rvest, httr.
