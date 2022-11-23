# m1financeR - analyze your M1 Finance portfolio in R
[M1 Finance](https://www.m1finance.com/) does not have its own API through which users can access and download things like realized/unrealized gain. According to M1 Finance:

> While M1 does not have realized gain information on the app or website, you can view this information on our clearing firm, Apex Corporation's website. 

`m1financeR` provides a set of functions that make it easy to import your M1 Finance data into R for analysis via an opinionated wrapper to APEX Clearing's individual account API. Using this package, you can access your account information through a set of intuitive functions instead of manual CSV downloads.

# Installation
Install the development version of `m1financeR` with `devtools`:

```R
devtools::install_github("mistermichaelll/m1financeR")
```

# Setup
You will need to create an account with [APEX Clearing](https://www.apexclearing.com/)--this is M1 Finance's clearing firm and custodial bank. You can access all of your account information from here. 

Once you have your username and password, and `m1financeR` installed, you're ready to use the package.

## Authentication 
At the beginning of your script, call the `get_APEX_auth_token()` function. 

If you are using RStudio, the function will prompt you to enter your username and password. Otherwise, this function will search `.Renviron` for `APEX_USER` and `APEX_PASS`. You can set these for non-interactive sessions if you choose.

Running this function creates an environment variable called `APEX_token` which is used for authentication for the rest of the session.

# Example of Use
```R
## libraries ----
library(m1financeR)
library(dplyr)

## authenticate ----
get_APEX_auth_token()

## get the realized gains/losses for your portfolio -----
realized_gain_loss <- get_realized_gains_losses("AB1234")

realized_gain_loss |> 
    group_by(symbol) |> 
    summarize(total_gain_loss = sum(short_term_gain_loss + long_term_gain_loss, na.rm = T)) |> 
    head(10)
    
#> # A tibble: 10 × 2
#> symbol     total_gain_loss
#> <chr>               <dbl>
#> 1 VTI               174. 
#> 2 AAPL              50.1
#> 3 GOOG              44.3
#> 4 MELI              35.9
#> 5 MSFT              32.2
#> 6 DIS               30.6
#> 7 AMZN              30.0
#> 8 SQ                29.9
#> 9 SBUX              16.7
#> 10 TGT              15.9       
```

# Extendability to Other Brokers
Though the intended use of this package is for users of M1 Finance, this package is a wrapper to the APEX Clearing API. That means if APEX is your broker's clearing firm and custodial bank, it's likely that this package will operate in the same way for that broker.

# APEX Clearing's API Support
APEX's Individual API is not publicly documented. This package was constructed through careful analysis of network activity when accessing APEX's site. 

Thus, it is entirely possible that APEX may introduce a breaking change to the API in the future. Please keep this in mind as you write scripts based on this package. 
