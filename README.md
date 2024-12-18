European countries’ GDP, euro or not
================
Mitsuo Shiota
2019-04-11

- [Libraries](#libraries)
- [Get GDP data](#get-gdp-data)
- [Add country names by looking up
  codes](#add-country-names-by-looking-up-codes)
- [Which country introduced euro, and
  when](#which-country-introduced-euro-and-when)
- [Plot](#plot)

Updated: 2024-12-18

I used to predict whether the country adopts euro or not by fitting
logistic regression using its GDP recovery from the Great Recession up
to the first quarter of 2020 in [old README](README_old.md). I dropped
that part, as the coronavirus disrupted the economies.

## Libraries

As usual I attach tidyverse package. As I will struggle with dates, I
attach lubridate packeage. I also attach rvest package for web scraping
to get the euro entry date of each country. Although I don’t attach, I
use eurostat package to get GDP data, and countrycode package to convert
country codes to names.

``` r
library(tidyverse)
library(rvest)

theme_set(theme_light())
```

## Get GDP data

[Eurostat Database](https://ec.europa.eu/eurostat/data/database)
provides a wide variety of data. Honestly, it is hard to find the right
table and parameters. Anyway, I dig Data navigation tree down to
“Quarterly national accounts”, reach “GDP and main components (output,
expenditure and income) (namq_10_gdp)”, and know the table name is
“namq_10_gdp”. Click Data Explorer icon, and a new window with a large
table appears. Look at the upper part, and click + icon to know the
parameters, like:

- unit: CLV15_MNAC: Chain linked volumes (2015), million units of
  national currency
- s_adj: SCA: Seasonally and calendar adjusted data
- na_item: B1GQ; Gross domestic product at market prices

[Cheat sheet: eurostat R
package](https://cran.r-project.org/web/packages/eurostat/vignettes/cheatsheet.html)
helps me.

``` r
eu_gdp <- eurostat::get_eurostat(id = "namq_10_gdp",
                                 time_format = "raw",
                                 filters = list(
                                   unit = "CLV15_MNAC",
                                   s_adj = "SCA",
                                   na_item = "B1GQ")
                                 )
```

    ## Dataset query already saved in cache_list.json...

    ## Reading cache file /tmp/RtmpsFPuYa/eurostat/0e6b5f4b12c2c954023c717c539aa99a.rds

    ## Table  namq_10_gdp  read from cache file:  /tmp/RtmpsFPuYa/eurostat/0e6b5f4b12c2c954023c717c539aa99a.rds

``` r
eu_gdp <- eu_gdp %>% 
  select(time, geo, values)

# eu_gdp$geo <- as.character(eu_gdp$geo)

eu_gdp <- eu_gdp |> 
  mutate(time = yq(time))
```

## Add country names by looking up codes

I follow the order of [“Tutorial: Country codes and protocol
order”](https://ec.europa.eu/eurostat/statistics-explained/index.php/Tutorial:Country_codes_and_protocol_order).

``` r
country_codes_eu28 <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE",
                   "EL", "ES", "FR", "HR", "IT", "CY", "LV",
                   "LT", "LU", "HU", "MT", "NL", "AT", "PL", 
                   "PT", "RO", "SI", "SK", "FI", "SE", "UK") 
```

GDP data contain 3 countries outside EU, so I add them to codes.

``` r
country_codes <- c(country_codes_eu28, "NO", "CH", "RS")
```

I create the lookup table of country code and name by utilizing
countrycode package.

``` r
lookup_tbl <- tibble(
  geo = country_codes,
  name = countrycode::countrycode(country_codes, 'eurostat', 'country.name')
)
```

Then I add “name” column to GDP data by matching code and name, and drop
organizations, like EU-28, which don’t match country codes.

``` r
eu_gdp <- eu_gdp %>% 
  left_join(lookup_tbl, by = "geo") %>% 
  drop_na(name)
```

## Which country introduced euro, and when

Now I would like to know when countries switched from national
currencies to euro. I use rvest package for web scraping. I have found
[wiki page “Euro”](https://en.wikipedia.org/wiki/Euro), and managed to
get when euro area countries fixed their currenicies to the euro.

``` r
wiki <- read_html("https://en.wikipedia.org/wiki/Euro")

euro_entry <- wiki %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_nodes("td") %>% 
  html_text() %>% 
  str_sub(1, -1) %>% 
  str_remove("\n")

euro_entry_tbl <- 
  tibble(
    geo = euro_entry[seq(1, 95, 5) + 1],
    date_fixed = euro_entry[seq(1, 95, 5) + 3]
  )

euro_entry_tbl$date_fixed <- as_date(euro_entry_tbl$date_fixed, format = "%d %B %Y")

euro_entry_tbl <- euro_entry_tbl %>% 
  mutate(
    geo = str_sub(geo, 1L, 2L),
    geo = if_else(geo == "GR", "EL", geo) # Greece
  )
```

I add “date_fixed” column to GDP data, compare it with “time” column,
and get “euro” column to show whether each time (row) is in euro (“Y”)
or not (“N”).

``` r
eu_gdp <- eu_gdp %>% 
  left_join(euro_entry_tbl, by = "geo") %>% 
  mutate(euro = if_else(time >= date_fixed, "Y", "N")) %>% 
  replace_na(list(euro = "N"))

eu_gdp$name <- factor(eu_gdp$name, levels = lookup_tbl$name)

eu_gdp$euro <- factor(eu_gdp$euro)
```

## Plot

I plot real GDP from 1Q 1995 by setting 1Q 2007 (just before the Great
Recession) = 100.

I set the common y-axis range so that I can see the movements of most
countries easily. As a result, some countries went out of the plot. If
you don’t like it, set YLIM as you like.

``` r
# set the parameters to plot
START <- "1995-01-01"
STD <- "2007-01-01"
YLIM <- c(65, 135)
```

Line is colored differently depending on whether it is in euro or not.

``` r
# index STD = 100
eu_gdp <- eu_gdp %>% 
  group_by(name) %>% 
  mutate(index = values / values[which(time == STD)] * 100) %>% 
  ungroup()

# plot
eu_gdp %>% 
  filter(time >= START) %>% 
  ggplot(aes(x = time, y = index)) + 
  geom_hline(yintercept = 100, color = "gray70", linewidth = 0.5) +
  geom_line(aes(color = euro), linewidth = 1) +
  facet_wrap(vars(name)) +
  coord_cartesian(ylim = YLIM) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black")
    ) +
  labs(
    title = str_c("Europe; Real GDP, ", quarter(STD, with_year = TRUE),
                  "Q=100")
    )
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

``` r
ggsave(filename = "output/GDP-euro-or-not.pdf",
       width = 10, height = 8, units = "in", dpi = 300)
```

EOL
