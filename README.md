European countries’ GDP, euro or not
================
Mitsuo Shiota
2019-4-11

  - [Summary](#summary)
  - [Libraries](#libraries)
  - [Get GDP data](#get-gdp-data)
  - [Add country names by looking up
    codes](#add-country-names-by-looking-up-codes)
  - [Which country introduced euro, and
    when](#which-country-introduced-euro-and-when)
  - [Plot](#plot)
  - [Euro or not](#euro-or-not)

## Summary

  - [European countries’ GDP line chart in
    pdf](output/GDP-euro-or-not.pdf)
  - [How much recovered from the Great Recession boxplot, in euro or
    not, in pdf](output/GDP-euro-or-not2.pdf)

This is my second attempt to utilize R language for economic analysis
after [Yellen’s US Labor Market Dashboard](yellen-dashboard).

## Libraries

As usual I attach tidyverse package. I also attach rvest package for web
scraping to get the euro entry date of each country. Although I don’t
attach, I use eurostat package to get GDP data, and countrycode package
to convert country codes to names.

``` r
library(tidyverse)
library(rvest)
```

## Get GDP data

[Eurostat Database](https://ec.europa.eu/eurostat/data/database)
provides a wide variety of data. I dig Data navigation tree down to
“Quarterly national accounts”, reach “GDP and main components (output,
expenditure and income) (namq\_10\_gdp)”, and know the table name is
“namq\_10\_gdp”. Click Data Explorer icon, and a new window with a
large table appears. Look at the upper part, and click + icon to know
the parameters, like:

  - unit: CLV10\_MNAC: Chain linked volumes (2010), million units of
    national currency
  - s\_adj: SCA: Seasonally and calendar adjusted data
  - na\_item: B1GQ; Gross domestic product at market prices

[Cheat sheet: eurostat R
package](https://cran.r-project.org/web/packages/eurostat/vignettes/cheatsheet.html)
helps me.

``` r
eu_gdp <- eurostat::get_eurostat(id = "namq_10_gdp",
                                 filters = list(
                                   unit = "CLV10_MNAC",
                                   s_adj = "SCA",
                                   na_item = "B1GQ")
                                 )

eu_gdp <- eu_gdp %>% 
  select(time, geo, values)

eu_gdp$geo <- as.character(eu_gdp$geo)
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
  .[[5]] %>% 
  html_nodes("td") %>% 
  html_text() %>% 
  str_sub(1, -2)

euro_entry_tbl <- 
  tibble(
    name = euro_entry[seq(1, 132, 6)],
    date_fixed = euro_entry[seq(1, 132, 6) + 4]
  )

euro_entry_tbl$date_fixed <- as.Date(euro_entry_tbl$date_fixed)
```

I add “date\_fixed” column to GDP data, compare it with “time” column,
and get “euro” column to show whether each time (row) is in euro (“Y”)
or not (“N”).

``` r
eu_gdp <- eu_gdp %>% 
  left_join(euro_entry_tbl, by = "name") %>% 
  mutate(euro = if_else(time >= date_fixed, "Y", "N")) %>% 
  replace_na(list(euro = "N"))

eu_gdp$name <- factor(eu_gdp$name, levels = lookup_tbl$name)

eu_gdp$euro <- factor(eu_gdp$euro)
```

## Plot

I plot quarterly real GDP from 2000 by setting 1Q 2007 (just before the
Great Recession) = 100.

I set the common y-axis range so that I can see the movements of most
countries easily. But, as a result, some countries went out of the plot.
If you don’t like it, set YLIM as you like.

Line is colored differently depending on whether it is in euro or not.

``` r
# set the parameters to plot
START = "2000-01-01"
YLIM = c(75, 125)

# index "2007-01-01" = 100
eu_gdp <- eu_gdp %>% 
  group_by(name) %>% 
  mutate(index = values / values[which(time == "2007-01-01")] * 100) %>% 
  ungroup()

# plot
eu_gdp %>% 
  filter(time >= START) %>% 
  ggplot(aes(x = time, y = index)) + 
  geom_hline(yintercept = 100, color = "white", size = 2) +
  geom_line(aes(color = euro), size = 1) +
  facet_wrap(~ name) +
  coord_cartesian(ylim = YLIM) +
  labs(
    title = "Europe; Real GDP, 1Q 2007=100",
    x = "",
    y = "")
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

``` r
ggsave(filename = "output/GDP-euro-or-not.pdf",
       width = 10, height = 8, units = "in", dpi = 300)
```

## Euro or not

Can I say something by looking at the plot?

GDP does not reflect employment, if foreigners just borrow the land to
produce goods they bring home. GNI would be better for [Leprechaun
Economics](https://en.wikipedia.org/wiki/Leprechaun_economics).

Euro area countries are so advanced that they have already reaped low
hanging fruits of productivity, and that their demography is not
favorable for growth, so their growth path is not steep. On the
contrary, some European non-euro counties are still in the developing
stage, so their growth path is steep. If I compare how much GDP
recovered from the Great Recession between euro and non-euro countries
like below, I am not fair.

``` r
latest_gdp <- eu_gdp %>% 
  group_by(name) %>% 
  filter(time == max(time)) %>% 
  ungroup() %>% 
  mutate(euro = if_else(date_fixed <= "2007-01-01", "Y", "N")) %>% 
  replace_na(list(euro = "N"))

latest_gdp %>% 
  ggplot(aes(x = euro, y = index)) +
  geom_hline(yintercept = 100, color = "white", size = 2) +
  geom_boxplot() +
  labs(
    title = "How much recovered?",
    x = "in euro as of 1Q 2007",
    y = "Latest Real GDP, 1Q 2007=100")
```

![](README_files/figure-gfm/latest%20GDP_boxplot-1.png)<!-- -->

``` r
ggsave(filename = "output/GDP-euro-or-not2.pdf",
       width = 3, height = 4, units = "in", dpi = 300)
```

Who are the winners, and who are the losers?

``` r
latest_gdp$euro <- factor(latest_gdp$euro)
latest_gdp %>% 
  select(time, name, euro, index) %>% 
  arrange(desc(index))
```

    ## # A tibble: 30 x 4
    ##    time       name       euro  index
    ##    <date>     <fct>      <fct> <dbl>
    ##  1 2018-10-01 Malta      N      167.
    ##  2 2018-10-01 Ireland    Y      159.
    ##  3 2018-10-01 Poland     N      151.
    ##  4 2018-10-01 Romania    N      141.
    ##  5 2018-10-01 Bulgaria   N      129.
    ##  6 2018-10-01 Lithuania  N      124.
    ##  7 2018-10-01 Luxembourg Y      124.
    ##  8 2018-10-01 Serbia     N      123.
    ##  9 2018-10-01 Czechia    N      123.
    ## 10 2018-10-01 Sweden     N      122.
    ## # ... with 20 more rows

``` r
latest_gdp %>% 
  select(time, name, euro, index) %>% 
  arrange(index)
```

    ## # A tibble: 30 x 4
    ##    time       name     euro  index
    ##    <date>     <fct>    <fct> <dbl>
    ##  1 2018-10-01 Greece   Y      77.7
    ##  2 2018-10-01 Italy    Y      95.4
    ##  3 2018-10-01 Croatia  N     102. 
    ##  4 2018-10-01 Portugal Y     103. 
    ##  5 2018-10-01 Finland  Y     106. 
    ##  6 2018-10-01 Spain    Y     108. 
    ##  7 2018-10-01 Latvia   N     110. 
    ##  8 2018-10-01 Denmark  N     111. 
    ##  9 2018-10-01 France   Y     111. 
    ## 10 2018-10-01 Cyprus   N     112. 
    ## # ... with 20 more rows

I feel sympathy with Greece and Italy.

EOL
