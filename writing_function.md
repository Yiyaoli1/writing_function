function
================
Yiyao LI
2020/11/11

## start with something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -1.50952924 -1.02945496 -0.08077871 -0.13022333 -2.75084772 -0.29739049
    ##  [7] -0.62190037 -0.48353837  0.87626404  0.15517741  2.24658311 -1.22900254
    ## [13]  0.39872560  0.88882475  0.07143060  0.84774885  0.44430680  0.19133017
    ## [19] -0.83840495  0.03197929  1.63440416  0.68717068 -0.20543972 -1.26745378
    ## [25]  0.38987071  0.77744713  1.11169359 -0.64123691 -0.10852611  0.44077030

a function to calculate z\_score

``` r
z_score = function(x){
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if(length(x)<3){
    stop("Input must have at least 3 numbers")
  }
  z = (x - mean(x))/sd(x)
  
  return(z)
  
}

z_score(x_vec)
```

    ##  [1] -1.50952924 -1.02945496 -0.08077871 -0.13022333 -2.75084772 -0.29739049
    ##  [7] -0.62190037 -0.48353837  0.87626404  0.15517741  2.24658311 -1.22900254
    ## [13]  0.39872560  0.88882475  0.07143060  0.84774885  0.44430680  0.19133017
    ## [19] -0.83840495  0.03197929  1.63440416  0.68717068 -0.20543972 -1.26745378
    ## [25]  0.38987071  0.77744713  1.11169359 -0.64123691 -0.10852611  0.44077030

try my function on other things, which should have errors.

``` r
z_score(3)
```

    ## Error in z_score(3): Input must have at least 3 numbers

``` r
z_score("my name is yiyao")
```

    ## Error in z_score("my name is yiyao"): Input must be numeric

``` r
z_score(c(TRUE, TRUE, FALSE, FALSE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, FALSE)): Input must be numeric

``` r
z_score(c(1,2))
```

    ## Error in z_score(c(1, 2)): Input must have at least 3 numbers

## Multiple outputs

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if(length(x)<3){
    stop("Input must have at least 3 numbers")
  }
 
   mean_x = mean(x)
   sd_x = sd(x)
   
   tibble(
     mean_x, sd_x
   )
  
}
```

check that function

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   4.37  2.77

``` r
mean_and_sd(c(1,2,4,7))
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1    3.5  2.65

``` r
x_vec = rnorm(100, mean=3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   2.98  4.12

## multiple input

I’d like to make this as a function

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 7, sd = 3)
  )
sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.46  3.03

``` r
sim_mean_sd = function(n, mu = 2, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(100,2,4)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   1.41      4.27

review function

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

check the function

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                  stars text                                            
    ##    <chr>                  <dbl> <chr>                                           
    ##  1 Vote for Pedro!            5 Just watch the movie. Gosh!                     
    ##  2 Just watch the freaki~     5 Its a great movie, gosh!!                       
    ##  3 Great Value                5 Great Value                                     
    ##  4 I LOVE THIS MOVIE          5 THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES      
    ##  5 Don't you wish you co~     5 Watch it 100 times. Never. Gets. Old.           
    ##  6 Stupid, but very funn~     5 If you like stupidly funny '90s teenage movies ~
    ##  7 The beat                   5 The best                                        
    ##  8 Hilarious                  5 Super funny! Loved the online rental.           
    ##  9 Love this movie            5 We love this product.  It came in a timely mann~
    ## 10 Entertaining, limited~     4 Entertainment level gets a 5 star but having pr~
    ## # ... with 40 more rows

## mean scoping example

``` r
f = function(x){
  z = x+y
  z
}
x = 1
y = 2
f(x = y)
```

    ## [1] 4

## functions as arguments

``` r
my_summary = function(x,summ_func){
  
  summ_func(x)
  
}

x_vec = rnorm(100,3,7)


my_summary(x_vec, IQR)
```

    ## [1] 10.31296
