edit-5.0
================

## GitHub Documents

РАЗВЕДОЧНЫЙ АНАЛИЗ ДАННЫХ

``` r
diplom_1802_imp = read.csv('/Users/olesyamba/Downloads/data/diplom_1802_imp.csv')
diplom_1802_w_imp = read.csv('/Users/olesyamba/Downloads/data/diplom_1802_w_imp.csv')
```

``` r
diplom_1802_imp = filter(diplom_1802_imp, year != "2016")
desc = stat.desc(diplom_1802_imp[, c(6, 7,10:20)])
desc = t(round(desc, digits = 2))
desc = desc[, c('nbr.val','min', 'max', 'mean', 'median', 'std.dev')]
desc
```

    ##                         nbr.val      min    max   mean median std.dev
    ## price_to_book               612     0.04  11.17   3.02   2.67    1.43
    ## polarity_news               612     0.03   0.14   0.09   0.09    0.01
    ## polarity_twitter            612     0.01   0.30   0.12   0.12    0.02
    ## yoy_revenue_growth          612   -11.55 261.18  19.02  16.64   27.85
    ## google_trends               612 -1021.73 683.00 167.66 164.36  102.03
    ## number_of_news              612     0.00  32.15   2.15   1.28    3.26
    ## number_of_likes_twitter     612     0.02 220.08  15.42  14.91   12.68
    ## number_of_tweets            612     0.28 487.69  76.10  72.89   38.56
    ## sales                       612  -189.12 103.60  25.30  26.19   12.00
    ## ROA                         612   -22.57  38.05  14.15  13.96    6.65
    ## fin_leverage                612  -105.15  43.95   0.56   0.45    6.60
    ## buyback_yield               612   -24.97  37.22   2.38   2.29    3.92
    ## log_sales                   607     1.82   4.65   3.25   3.30    0.30

На графике представлены описательные статистики до предварительной
обработки, включающей обработку пропущенных значений, обработку
выбросов, а также уменьшение дисперсии некоторых переменных с целью
борьбы с шумом.

``` r
diplom_1802_w_imp = filter(diplom_1802_w_imp, year != "2016")
desc = stat.desc(diplom_1802_w_imp[, c(6, 7,10:20)])
desc = t(round(desc, digits = 2))
desc = desc[, c('nbr.val','min', 'max', 'mean', 'median', 'std.dev')]
desc
```

    ##                         nbr.val    min    max   mean median std.dev
    ## price_to_book               612   0.04  11.17   3.02   2.67    1.43
    ## polarity_news               612   0.03   0.14   0.09   0.09    0.01
    ## polarity_twitter            612   0.01   0.30   0.12   0.12    0.02
    ## yoy_revenue_growth          612 -11.55  47.14  16.73  16.64   12.82
    ## google_trends               612  10.00 683.00 169.48 164.36   89.90
    ## number_of_news              612   0.00  32.15   2.20   1.38    3.28
    ## number_of_likes_twitter     612   0.02 220.08  15.42  14.91   12.68
    ## number_of_tweets            612   0.28 487.69  76.10  72.89   38.56
    ## sales                       612   7.80 103.60  25.76  26.22    8.02
    ## ROA                         612  -1.22  38.05  14.24  13.96    6.33
    ## fin_leverage                612  -5.96  18.82   1.03   0.45    3.15
    ## buyback_yield               612  -4.15  17.90   2.51   2.35    3.50
    ## log_sales                   612   2.17   4.65   3.24   3.30    0.31

КОРРЕЛЯЦИОННЫЙ АНАЛИЗ

``` r
library("Hmisc")
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'lattice'

    ## The following object is masked from 'package:regclass':
    ## 
    ##     qq

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following object is masked from 'package:parsnip':
    ## 
    ##     translate

    ## The following object is masked from 'package:simputation':
    ## 
    ##     impute

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
res2 <- rcorr(as.matrix(diplom_1802_w_imp[, c(6:20)]))
```

``` r
matrix_r = as.data.frame(res2$r)
matrix_p = as.data.frame(res2$P)

empty_as_na <- function(x){
  ifelse(is.na(x), as.integer('1'), x)
}
matrix_r = round(matrix_r, 3)
matrix_p = matrix_p %>% mutate_each(funs(empty_as_na)) 
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## Warning: `mutate_each_()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## ℹ The deprecated feature was likely used in the dplyr package.
    ##   Please report the issue at <]8;;https://github.com/tidyverse/dplyr/issueshttps://github.com/tidyverse/dplyr/issues]8;;>.

``` r
for (i in seq.int(1,15,1)){
  for (j in seq.int(1,15,1)){
    if (matrix_p[i,j] <= 0.01){
      matrix_r[i,j] = str_c(as.character(matrix_r[i,j]), as.character('*'))
    }
    if (matrix_p[i,j] <= 0.05){
      matrix_r[i,j] = str_c(as.character(matrix_r[i,j]), as.character('*'))
    }
    if (matrix_p[i,j] <= 0.1){
      matrix_r[i,j] = str_c(as.character(matrix_r[i,j]), as.character('*'))
    }
  }
}
```

``` r
matrix_r
```

    ##                         price_to_book polarity_news subjectivity_news
    ## price_to_book                       1      0.446***          0.485***
    ## polarity_news                0.446***             1          0.784***
    ## subjectivity_news            0.485***      0.784***                 1
    ## pol_sub_news                 0.361***      0.728***          0.726***
    ## polarity_twitter             0.193***      0.353***          0.432***
    ## yoy_revenue_growth             0.076*     -0.315***          -0.38***
    ## google_trends                   -0.04        -0.054         -0.122***
    ## number_of_news               -0.16***         0.066            0.069*
    ## number_of_likes_twitter     -0.158***         -0.02             0.003
    ## number_of_tweets            -0.222***     -0.127***         -0.122***
    ## sales                          -0.028      0.132***          0.141***
    ## ROA                          0.685***       0.25***          0.382***
    ## fin_leverage                -0.109***        -0.033             0.009
    ## buyback_yield               -0.196***     -0.158***            -0.057
    ## log_sales                      -0.015      0.143***           0.19***
    ##                         pol_sub_news polarity_twitter yoy_revenue_growth
    ## price_to_book               0.361***         0.193***             0.076*
    ## polarity_news               0.728***         0.353***          -0.315***
    ## subjectivity_news           0.726***         0.432***           -0.38***
    ## pol_sub_news                       1         0.351***          -0.368***
    ## polarity_twitter            0.351***                1          -0.336***
    ## yoy_revenue_growth         -0.368***        -0.336***                  1
    ## google_trends              -0.107***         -0.19***           0.129***
    ## number_of_news                0.074*          0.091**          -0.119***
    ## number_of_likes_twitter         0.01            0.035             -0.037
    ## number_of_tweets           -0.116***            -0.01            0.094**
    ## sales                       0.116***            0.052          -0.195***
    ## ROA                         0.269***         0.261***          -0.279***
    ## fin_leverage                    0.03           -0.041              0.045
    ## buyback_yield                  -0.06           -0.024              0.006
    ## log_sales                    0.14***          0.096**           -0.23***
    ##                         google_trends number_of_news number_of_likes_twitter
    ## price_to_book                   -0.04       -0.16***               -0.158***
    ## polarity_news                  -0.054          0.066                   -0.02
    ## subjectivity_news           -0.122***         0.069*                   0.003
    ## pol_sub_news                -0.107***         0.074*                    0.01
    ## polarity_twitter             -0.19***        0.091**                   0.035
    ## yoy_revenue_growth           0.129***      -0.119***                  -0.037
    ## google_trends                       1       0.258***                 0.085**
    ## number_of_news               0.258***              1                0.478***
    ## number_of_likes_twitter       0.085**       0.478***                       1
    ## number_of_tweets             0.153***       0.549***                0.739***
    ## sales                        0.304***       0.336***                   0.041
    ## ROA                         -0.145***         -0.039                  -0.1**
    ## fin_leverage                 -0.084**         -0.029                   0.015
    ## buyback_yield                 -0.075*       0.141***                   0.028
    ## log_sales                    0.282***       0.266***                   0.038
    ##                         number_of_tweets     sales       ROA fin_leverage
    ## price_to_book                  -0.222***    -0.028  0.685***    -0.109***
    ## polarity_news                  -0.127***  0.132***   0.25***       -0.033
    ## subjectivity_news              -0.122***  0.141***  0.382***        0.009
    ## pol_sub_news                   -0.116***  0.116***  0.269***         0.03
    ## polarity_twitter                   -0.01     0.052  0.261***       -0.041
    ## yoy_revenue_growth               0.094** -0.195*** -0.279***        0.045
    ## google_trends                   0.153***  0.304*** -0.145***     -0.084**
    ## number_of_news                  0.549***  0.336***    -0.039       -0.029
    ## number_of_likes_twitter         0.739***     0.041    -0.1**        0.015
    ## number_of_tweets                       1     0.021 -0.154***       -0.013
    ## sales                              0.021         1     0.052    -0.128***
    ## ROA                            -0.154***     0.052         1       -0.07*
    ## fin_leverage                      -0.013 -0.128***    -0.07*            1
    ## buyback_yield                      0.047  0.369***    -0.032     0.388***
    ## log_sales                         -0.003  0.923***   0.087**    -0.127***
    ##                         buyback_yield log_sales
    ## price_to_book               -0.196***    -0.015
    ## polarity_news               -0.158***  0.143***
    ## subjectivity_news              -0.057   0.19***
    ## pol_sub_news                    -0.06   0.14***
    ## polarity_twitter               -0.024   0.096**
    ## yoy_revenue_growth              0.006  -0.23***
    ## google_trends                 -0.075*  0.282***
    ## number_of_news               0.141***  0.266***
    ## number_of_likes_twitter         0.028     0.038
    ## number_of_tweets                0.047    -0.003
    ## sales                        0.369***  0.923***
    ## ROA                            -0.032   0.087**
    ## fin_leverage                 0.388*** -0.127***
    ## buyback_yield                       1  0.349***
    ## log_sales                    0.349***         1

Цикл позволяет вывести корреляционную матрицу с указанием уровней
значимости \* = 10%, \*\* = 5%,\*\*\* = 1% соответственно. Борьба с
мультиколлинеарностью проводится далее. На данном этапе лишь
подтверждается ее необходимость.

МОДЕЛИРОВАНИЕ

``` r
dataPanel <- pdata.frame(diplom_1802_w_imp, index=c("ticker","year"))# трансформация в панельные данные
filter(dataPanel, year == "2017") %>%
  group_by(sector)%>%
  count(sector) # выводим представленный в выборке набор секторов
```

    ## # A tibble: 7 × 2
    ## # Groups:   sector [7]
    ##   sector                     n
    ##   <chr>                  <int>
    ## 1 Communication Services    13
    ## 2 Consumer Discretionary    15
    ## 3 Consumer Staples           7
    ## 4 Health Care               13
    ## 5 Industrials                8
    ## 6 Information Technology    42
    ## 7 Utilities                  4

МОДЕЛЬ МНОЖЕСТВЕННОЙ ЛИНЕЙНОЙ РЕГРЕССИИ

``` r
dataPanel_std <- pdata.frame(data.frame(diplom_1802_w_imp[,c(1:5)], scale(diplom_1802_w_imp[,-c(1:5)])), index=c("ticker","year")) #дополнительно создаем дф с стандартнизированными переменными
```

``` r
ols1 <-plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield , data = filter(dataPanel_std, year != "2016"), model="within")
```

``` r
summary(ols1) # оцениваем первую спецификацию как обычную множественную линейную регрессию со стандартизированными коэффициентами 
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel_std, 
    ##     year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -1.903461 -0.172692 -0.021048  0.172409  1.617357 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t-value  Pr(>|t|)    
    ## polarity_news       0.1975949  0.0280822  7.0363 6.535e-12 ***
    ## polarity_twitter    0.0070015  0.0238181  0.2940  0.768913    
    ## yoy_revenue_growth  0.1687012  0.1014829  1.6624  0.097067 .  
    ## google_trends       0.0666675  0.0237914  2.8022  0.005273 ** 
    ## number_of_news      0.0220853  0.0313411  0.7047  0.481339    
    ## number_of_tweets    0.0603640  0.0362501  1.6652  0.096497 .  
    ## sales              -0.0543929  0.0243196 -2.2366  0.025754 *  
    ## ROA                 0.6609974  0.0258889 25.5321 < 2.2e-16 ***
    ## fin_leverage       -0.0521373  0.0255184 -2.0431  0.041563 *  
    ## buyback_yield      -0.0145474  0.0289439 -0.5026  0.615461    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    208.97
    ## Residual Sum of Squares: 80.697
    ## R-Squared:      0.61384
    ## Adj. R-Squared: 0.52811
    ## F-statistic: 79.4792 on 10 and 500 DF, p-value: < 2.22e-16

``` r
ols2 <-plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_likes_twitter + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel_std, year != "2016"), model="within")
```

``` r
summary(ols2)# оцениваем вторую спецификацию как обычную множественную линейную регрессию со стандартизированными коэффициентами
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_likes_twitter + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel_std, 
    ##     year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -1.861411 -0.172475 -0.018288  0.159766  1.607813 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t-value  Pr(>|t|)    
    ## polarity_news            0.2111822  0.0275253  7.6723 8.910e-14 ***
    ## polarity_twitter         0.0027737  0.0231268  0.1199 0.9045831    
    ## yoy_revenue_growth       0.1741428  0.0989866  1.7593 0.0791455 .  
    ## google_trends            0.0839150  0.0234737  3.5749 0.0003844 ***
    ## number_of_news           0.0276843  0.0283873  0.9752 0.3299146    
    ## number_of_likes_twitter  0.1217805  0.0230780  5.2769 1.961e-07 ***
    ## sales                   -0.0617157  0.0235184 -2.6241 0.0089523 ** 
    ## ROA                      0.6754596  0.0253669 26.6276 < 2.2e-16 ***
    ## fin_leverage            -0.0546116  0.0248894 -2.1942 0.0286822 *  
    ## buyback_yield           -0.0087167  0.0282713 -0.3083 0.7579650    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    208.97
    ## Residual Sum of Squares: 76.864
    ## R-Squared:      0.63218
    ## Adj. R-Squared: 0.55052
    ## F-statistic: 85.9362 on 10 and 500 DF, p-value: < 2.22e-16

``` r
ols3 <-plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel, year != "2016"), model="within")
```

``` r
summary(ols3) # оцениваем первую спецификацию как обычную множественную линейную регрессию
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel, 
    ##     year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -2.726513 -0.247364 -0.030149  0.246958  2.316698 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news      25.11107946  3.56879052  7.0363 6.535e-12 ***
    ## polarity_twitter    0.48001275  1.63293902  0.2940  0.768913    
    ## yoy_revenue_growth  0.01885132  0.01134010  1.6624  0.097067 .  
    ## google_trends       0.00106223  0.00037908  2.8022  0.005273 ** 
    ## number_of_news      0.00965490  0.01370117  0.7047  0.481339    
    ## number_of_tweets    0.00224214  0.00134646  1.6652  0.096497 .  
    ## sales              -0.00971785  0.00434496 -2.2366  0.025754 *  
    ## ROA                 0.14950853  0.00585571 25.5321 < 2.2e-16 ***
    ## fin_leverage       -0.02371977  0.01160952 -2.0431  0.041563 *  
    ## buyback_yield      -0.00594522  0.01182875 -0.5026  0.615461    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    428.76
    ## Residual Sum of Squares: 165.57
    ## R-Squared:      0.61384
    ## Adj. R-Squared: 0.52811
    ## F-statistic: 79.4792 on 10 and 500 DF, p-value: < 2.22e-16

``` r
ols4 <-plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_likes_twitter + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel, year != "2016"), model="within")
```

``` r
summary(ols4)# оцениваем вторую спецификацию как обычную множественную линейную регрессию 
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_likes_twitter + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel, 
    ##     year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -2.666281 -0.247053 -0.026195  0.228849  2.303028 
    ## 
    ## Coefficients:
    ##                            Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news           26.83779857  3.49801536  7.6723 8.910e-14 ***
    ## polarity_twitter         0.19016168  1.58554481  0.1199 0.9045831    
    ## yoy_revenue_growth       0.01945939  0.01106114  1.7593 0.0791455 .  
    ## google_trends            0.00133704  0.00037401  3.5749 0.0003844 ***
    ## number_of_news           0.01210258  0.01240990  0.9752 0.3299146    
    ## number_of_likes_twitter  0.01376069  0.00260772  5.2769 1.961e-07 ***
    ## sales                   -0.01102614  0.00420181 -2.6241 0.0089523 ** 
    ## ROA                      0.15277968  0.00573763 26.6276 < 2.2e-16 ***
    ## fin_leverage            -0.02484544  0.01132336 -2.1942 0.0286822 *  
    ## buyback_yield           -0.00356232  0.01155387 -0.3083 0.7579650    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    428.76
    ## Residual Sum of Squares: 157.71
    ## R-Squared:      0.63218
    ## Adj. R-Squared: 0.55052
    ## F-statistic: 85.9362 on 10 and 500 DF, p-value: < 2.22e-16

МОДЕЛЬ МНОЖЕСТВЕННОЙ ЛИНЕЙНОЙ РЕГРЕССИИ \| ВЫБОР СПЕЦИФИКАЦИИ

``` r
ols <-lm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel, year != "2016"))
summary(ols)
```

    ## 
    ## Call:
    ## lm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel, 
    ##     year != "2016"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5872 -0.3956 -0.0876  0.3342  4.3366 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -4.2611883  0.3583478 -11.891  < 2e-16 ***
    ## polarity_news      47.2520171  3.2026623  14.754  < 2e-16 ***
    ## polarity_twitter    1.8313043  1.7020330   1.076  0.28238    
    ## yoy_revenue_growth  0.0428334  0.0027884  15.361  < 2e-16 ***
    ## google_trends       0.0009137  0.0004016   2.275  0.02324 *  
    ## number_of_news     -0.0361348  0.0127386  -2.837  0.00471 ** 
    ## number_of_tweets   -0.0024498  0.0010205  -2.401  0.01667 *  
    ## sales               0.0001444  0.0051885   0.028  0.97780    
    ## ROA                 0.1542498  0.0053542  28.809  < 2e-16 ***
    ## fin_leverage       -0.0133935  0.0115023  -1.164  0.24471    
    ## buyback_yield      -0.0357095  0.0116653  -3.061  0.00230 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7735 on 601 degrees of freedom
    ## Multiple R-squared:  0.7132, Adjusted R-squared:  0.7084 
    ## F-statistic: 149.5 on 10 and 601 DF,  p-value: < 2.2e-16

``` r
regclass::VIF(ols)
```

    ##      polarity_news   polarity_twitter yoy_revenue_growth      google_trends 
    ##           1.330886           1.291543           1.304850           1.331091 
    ##     number_of_news   number_of_tweets              sales                ROA 
    ##           1.779327           1.581860           1.767337           1.174227 
    ##       fin_leverage      buyback_yield 
    ##           1.339493           1.707357

Мультиколлинеарности в модели после удаления части переменных нет, так
как коэффициенты VIF \< 5. Дополнительное подтверждение отсутствию
необходимости включения регуляризации. Однако в выборке присутствуют
компании и года, значительно отличающиеся друг от друга, проверим
дополнительные спецификации.

``` r
bptest(ols4, data = dataPanel)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  ols4
    ## BP = 117.69, df = 10, p-value < 2.2e-16

``` r
plm::pbgtest(ols4, data = dataPanel)
```

    ## 
    ##  Breusch-Godfrey/Wooldridge test for serial correlation in panel models
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  ...
    ## chisq = 71.11, df = 6, p-value = 2.42e-13
    ## alternative hypothesis: serial correlation in idiosyncratic errors

Есть автокорреляция и гетероскедастичность, необходимо использовать
скорректированную ковариационную матрицу.

``` r
coeftest(ols4, vcovHC(ols4, method = "arellano", type = "HC2"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                            Estimate  Std. Error t value  Pr(>|t|)    
    ## polarity_news           26.83779857  7.37249982  3.6403 0.0003007 ***
    ## polarity_twitter         0.19016168  1.91886821  0.0991 0.9210978    
    ## yoy_revenue_growth       0.01945939  0.01052297  1.8492 0.0650148 .  
    ## google_trends            0.00133704  0.00064068  2.0869 0.0374018 *  
    ## number_of_news           0.01210258  0.02130224  0.5681 0.5701972    
    ## number_of_likes_twitter  0.01376069  0.00479490  2.8699 0.0042804 ** 
    ## sales                   -0.01102614  0.00530562 -2.0782 0.0382006 *  
    ## ROA                      0.15277968  0.01074163 14.2231 < 2.2e-16 ***
    ## fin_leverage            -0.02484544  0.00850044 -2.9228 0.0036257 ** 
    ## buyback_yield           -0.00356232  0.00979264 -0.3638 0.7161795    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plm::pcdtest(ols4, test = c("cd"))
```

    ## 
    ##  Pesaran CD test for cross-sectional dependence in panels
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +     google_trends + number_of_news + number_of_likes_twitter +     sales + ROA + fin_leverage + buyback_yield
    ## z = -0.5697, p-value = 0.5689
    ## alternative hypothesis: cross-sectional dependence

no cross-sectional dependence

``` r
plm::pcdtest(ols4, test = c("lm"))
```

    ## 
    ##  Breusch-Pagan LM test for cross-sectional dependence in panels
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +     google_trends + number_of_news + number_of_likes_twitter +     sales + ROA + fin_leverage + buyback_yield
    ## chisq = 6560.3, df = 5151, p-value < 2.2e-16
    ## alternative hypothesis: cross-sectional dependence

cross-sectional dependence Вывод: результаты тестов разнятся, однако
даже в случае наличия кросс-секциональной зависимости это небольшая
проблема, так как у нас короткий временной интервал и большая выборка
компаний.

``` r
dwtest(ols)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  ols
    ## DW = 1.0993, p-value < 2.2e-16
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
fixed <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel, year != "2016"), model="within")
summary(fixed)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel, 
    ##     year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -2.726513 -0.247364 -0.030149  0.246958  2.316698 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news      25.11107946  3.56879052  7.0363 6.535e-12 ***
    ## polarity_twitter    0.48001275  1.63293902  0.2940  0.768913    
    ## yoy_revenue_growth  0.01885132  0.01134010  1.6624  0.097067 .  
    ## google_trends       0.00106223  0.00037908  2.8022  0.005273 ** 
    ## number_of_news      0.00965490  0.01370117  0.7047  0.481339    
    ## number_of_tweets    0.00224214  0.00134646  1.6652  0.096497 .  
    ## sales              -0.00971785  0.00434496 -2.2366  0.025754 *  
    ## ROA                 0.14950853  0.00585571 25.5321 < 2.2e-16 ***
    ## fin_leverage       -0.02371977  0.01160952 -2.0431  0.041563 *  
    ## buyback_yield      -0.00594522  0.01182875 -0.5026  0.615461    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    428.76
    ## Residual Sum of Squares: 165.57
    ## R-Squared:      0.61384
    ## Adj. R-Squared: 0.52811
    ## F-statistic: 79.4792 on 10 and 500 DF, p-value: < 2.22e-16

Фиксированные эффекты = константы для каждой компании:

``` r
fixef(fixed)
```

    ##      AAPL      ABNB      ADBE       ADI       ADP      ADSK       AEP      ALGN 
    ## -5.268724 -1.091923 -0.060543 -2.979120 -1.645704 -0.895133 -1.607038 -1.266904 
    ##      AMAT       AMD      AMGN      AMZN      ANSS      ASML      ATVI      AVGO 
    ## -3.188847 -1.965730 -2.417254 -0.540221 -1.584121 -1.364192 -1.833405 -1.468335 
    ##       AZN      BIDU      BIIB      BKNG      CDNS       CEG      CHTR     CMCSA 
    ## -1.578474 -1.750082 -2.571405 -2.171583 -1.806305 -1.410195 -1.947064 -2.910469 
    ##      COST      CPRT      CRWD      CSCO       CSX      CTAS      CTSH      DDOG 
    ## -2.488231 -1.200323 -1.636930 -2.711529 -2.488288 -1.798977 -2.166980 -1.546219 
    ##      DLTR      DOCU      DXCM        EA      EBAY       EXC      FAST      FISV 
    ## -2.104780 -1.724635 -1.692139 -1.924382 -2.194261 -2.431851 -2.245624 -2.126222 
    ##      FTNT      GILD      GOOG     GOOGL       HON      IDXX      ILMN      INTC 
    ## -1.744955 -2.105113 -1.974287 -1.895555 -2.403201 -1.704043 -1.415760 -3.245168 
    ##      INTU      ISRG        JD       KDP       KHC      KLAC      LCID      LRCX 
    ##  0.301209 -1.157126 -3.115394 -2.097613 -2.180186 -2.170309 -1.021640 -2.232596 
    ##      LULU       MAR      MCHP      MDLZ      MELI      META      MNST      MRNA 
    ## -2.190834 -2.034622 -1.618909 -2.464274 -1.823332 -1.236988 -1.848202 -1.457964 
    ##      MRVL      MSFT      MTCH        MU      NFLX      NTES      NVDA      NXPI 
    ## -1.637366 -2.262962 -1.970139 -1.610258 -0.130539 -1.848994  0.938387 -2.187915 
    ##      ODFL      OKTA      ORLY      PANW      PAYX      PCAR       PDD       PEP 
    ## -1.667947 -1.788360 -2.294968 -1.904230 -1.789004 -2.418683 -1.699515 -2.224405 
    ##      PYPL      QCOM      REGN      ROST      SBUX      SGEN      SIRI      SNPS 
    ## -1.710553 -2.543108 -2.125301 -1.949423 -2.435735 -1.672976 -2.049722 -1.405538 
    ##      SPLK      SWKS      TEAM      TMUS      TSLA       TXN      VRSK      VRSN 
    ## -2.028079 -2.163760 -1.843044 -2.184615 -0.903707 -2.073659 -2.307856 -2.221563 
    ##      VRTX       WBA      WDAY       XEL        ZM        ZS 
    ## -2.158579 -2.331539 -1.634870 -1.886763 -1.307891 -1.736754

Сравним спецификации с учетом фиксированных эффектов и без:

``` r
pFtest(fixed, ols)
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  ...
    ## F = 5.7995, df1 = 101, df2 = 500, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

Нулевая гипотеза: спецификация МНК лучше спецификации с фиксированными
эффектами p-value \< 2.2e-16 \< 0.01, следовательно, на уровне
значимости 1% нулевая гипотеза отклоняется, фиксированные эффекты
значимы.

Оценим спецификацию со случайными эффектами:

``` r
random <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = filter(dataPanel, year != "2016"), model="random")
summary(random)
```

    ## Oneway (individual) effect Random Effect Model 
    ##    (Swamy-Arora's transformation)
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = filter(dataPanel, 
    ##     year != "2016"), model = "random")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Effects:
    ##                  var std.dev share
    ## idiosyncratic 0.3311  0.5755 0.632
    ## individual    0.1926  0.4388 0.368
    ## theta: 0.528
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -2.610474 -0.308718 -0.057921  0.260679  3.231782 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error z-value  Pr(>|z|)    
    ## (Intercept)        -2.82509910  0.37697534 -7.4941 6.674e-14 ***
    ## polarity_news      34.48548368  3.31286816 10.4096 < 2.2e-16 ***
    ## polarity_twitter    0.35801533  1.59610592  0.2243  0.822520    
    ## yoy_revenue_growth  0.03597278  0.00405744  8.8659 < 2.2e-16 ***
    ## google_trends       0.00106144  0.00037318  2.8443  0.004451 ** 
    ## number_of_news     -0.00967462  0.01294532 -0.7473  0.454855    
    ## number_of_tweets   -0.00072379  0.00114784 -0.6306  0.528323    
    ## sales              -0.00562326  0.00442311 -1.2713  0.203609    
    ## ROA                 0.15024663  0.00546816 27.4766 < 2.2e-16 ***
    ## fin_leverage       -0.02323962  0.01087148 -2.1377  0.032544 *  
    ## buyback_yield      -0.02162316  0.01118163 -1.9338  0.053136 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    612.51
    ## Residual Sum of Squares: 221.69
    ## R-Squared:      0.63807
    ## Adj. R-Squared: 0.63205
    ## Chisq: 1059.54 on 10 DF, p-value: < 2.22e-16

Чтобы выбрать между фиксированными или случайными эффектами, проведем
тест Хаусмана, где нулевая гипотеза состоит в том, что предпочтительная
модель — это случайные эффекты, а альтернативная — фиксированные эффекты
(см. Green, 2008, глава 9). По сути, он проверяет, коррелируют ли
уникальные ошибки с регрессорами, но нулевая гипотеза заключается в том,
что это не так. Если значение p значимо (например, \<0,05), необходимо
использовать фиксированные эффекты, если нет, - случайные эффекты.

``` r
phtest(fixed, random)
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  ...
    ## chisq = 120.89, df = 10, p-value < 2.2e-16
    ## alternative hypothesis: one model is inconsistent

В нашем случае p-value \< 2.2e-16, на 1% уровне значимости нулевая
гипотеза отвергается, следовательно, релевантнее модель с фиксированными
эффектами. Также необходимо проверить наличие значимых временных
эффектов.

``` r
fixed.time <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield + factor(year), data = filter(dataPanel, year != "2016"), model="within")
summary(fixed.time)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield + factor(year), 
    ##     data = filter(dataPanel, year != "2016"), model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 6, N = 612
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -2.75165 -0.25545 -0.02850  0.24421  2.33601 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news      24.92529548  3.67057085  6.7906  3.21e-11 ***
    ## polarity_twitter    0.42532670  1.64773139  0.2581  0.796415    
    ## yoy_revenue_growth  0.01886452  0.01147667  1.6437  0.100867    
    ## google_trends       0.00101128  0.00038639  2.6173  0.009135 ** 
    ## number_of_news      0.00980988  0.01386546  0.7075  0.479586    
    ## number_of_tweets    0.00218942  0.00136210  1.6074  0.108609    
    ## sales              -0.01048957  0.00458596 -2.2873  0.022598 *  
    ## ROA                 0.14932862  0.00600220 24.8790 < 2.2e-16 ***
    ## fin_leverage       -0.02403491  0.01168224 -2.0574  0.040172 *  
    ## buyback_yield      -0.00612786  0.01196459 -0.5122  0.608763    
    ## factor(year)2018    0.04002950  0.08368190  0.4784  0.632610    
    ## factor(year)2019   -0.02957576  0.09070176 -0.3261  0.744504    
    ## factor(year)2020    0.00763063  0.08454971  0.0903  0.928125    
    ## factor(year)2021    0.02453276  0.08370993  0.2931  0.769592    
    ## factor(year)2022   -0.01166094  0.08362002 -0.1395  0.889150    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    428.76
    ## Residual Sum of Squares: 165.29
    ## R-Squared:      0.61449
    ## Adj. R-Squared: 0.52415
    ## F-statistic: 52.6018 on 15 and 495 DF, p-value: < 2.22e-16

``` r
pFtest(fixed.time, fixed)
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  ...
    ## F = 0.16857, df1 = 5, df2 = 495, p-value = 0.9741
    ## alternative hypothesis: significant effects

Нулевая гипотеза: проверяемый эффект не значим или равен 0. p-value =
0.9741, следовательно, на уровне значимости 1% нулевая гипотеза на
исследуемой выборке не отвергается, временные эффекты не значимы.

``` r
plmtest(fixed, c("time"), type=("bp"))
```

    ## 
    ##  Lagrange Multiplier Test - time effects (Breusch-Pagan)
    ## 
    ## data:  price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  ...
    ## chisq = 1.076, df = 1, p-value = 0.2996
    ## alternative hypothesis: significant effects

Результаты теста Бреуша-Пагана аналогичны, наилучшей является
спецификация с фиксированными эффектами.

``` r
datapanel_train = filter(dataPanel[,-c(8, 9, 20)], year != "2016" & year !="2017")
```

МОДЕЛЬ БУСТИНГ СЛУЧАЙНЫХ ЛЕСОВ

``` r
#install.packages('xgboost')
library(xgboost)
```

    ## 
    ## Attaching package: 'xgboost'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
# Create the specification with placeholders
boost_spec <- boost_tree(
  trees = 1000,
  learn_rate = tune(),
  tree_depth = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# Create the tuning grid
tunegrid_boost <- grid_regular(parameters(boost_spec), 
                               levels = 5)
```

    ## Warning: `parameters.model_spec()` was deprecated in tune 0.1.6.9003.
    ## ℹ Please use `hardhat::extract_parameter_set_dials()` instead.

``` r
tunegrid_boost
```

    ## # A tibble: 25 × 2
    ##    tree_depth learn_rate
    ##         <int>      <dbl>
    ##  1          1    0.001  
    ##  2          4    0.001  
    ##  3          8    0.001  
    ##  4         11    0.001  
    ##  5         15    0.001  
    ##  6          1    0.00422
    ##  7          4    0.00422
    ##  8          8    0.00422
    ##  9         11    0.00422
    ## 10         15    0.00422
    ## # … with 15 more rows

``` r
# Create CV folds of training data
folds <- vfold_cv(datapanel_train[,-c(1:5)], v = 6)

# Tune along the grid
tune_results <- tune_grid(boost_spec,
                          price_to_book ~ .,
                          resamples = folds,
                          grid = tunegrid_boost,
                          metrics = metric_set(rmse, mae, rsq))

# Plot the results
autoplot(tune_results)
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# Select the final hyperparameters
best_params <- select_best(tune_results)
```

    ## Warning: No value of `metric` was given; metric 'rmse' will be used.

``` r
# Finalize the specification
final_spec <- finalize_model(boost_spec, best_params)

# Train the final model on the full training data
final_model <- final_spec %>% fit(formula = price_to_book ~ ., 
                                  data = datapanel_train[,-c(1:5)])

final_model
```

    ## parsnip model object
    ## 
    ## ##### xgb.Booster
    ## raw: 1.5 Mb 
    ## call:
    ##   xgboost::xgb.train(params = list(eta = 0.0177827941003892, max_depth = 4L, 
    ##     gamma = 0, colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
    ##     subsample = 1), data = x$data, nrounds = 1000, watchlist = x$watchlist, 
    ##     verbose = 0, nthread = 1, objective = "reg:squarederror")
    ## params (as set within xgb.train):
    ##   eta = "0.0177827941003892", max_depth = "4", gamma = "0", colsample_bytree = "1", colsample_bynode = "1", min_child_weight = "1", subsample = "1", nthread = "1", objective = "reg:squarederror", validate_parameters = "TRUE"
    ## xgb.attributes:
    ##   niter
    ## callbacks:
    ##   cb.evaluation.log()
    ## # of features: 11 
    ## niter: 1000
    ## nfeatures : 11 
    ## evaluation_log:
    ##     iter training_rmse
    ##        1    2.90173070
    ##        2    2.85470977
    ## ---                   
    ##      999    0.09365539
    ##     1000    0.09359102

``` r
vip::vip(final_model)
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
best_params
```

    ## # A tibble: 1 × 3
    ##   tree_depth learn_rate .config              
    ##        <int>      <dbl> <chr>                
    ## 1          4     0.0178 Preprocessor1_Model12

КАЧЕСТВО МОДЕЛИ МНОЖЕСТВЕННОЙ ЛИНЕЙНОЙ РЕГРЕССИИ С ВКЛЮЧЕНИЕМ
ФИКСИРОВАННЫХ ЭФФЕКТОВ

``` r
datapanel = filter(dataPanel[,-c(8, 9, 20)], year != "2016")
# Predict new data
predictions_fixed_1 <- predict(ols3,
                       new_data = datapanel[,-c(1:5)])
```

``` r
# Compute the mean absolute error using one single function
mae_fixed_1 = mae(cbind(datapanel[,-c(1:5)], predictions_fixed_1),
    truth = price_to_book,
    estimate = predictions_fixed_1)
# Compute the RMSE using a function
rmse_fixed_1 = rmse(cbind(datapanel[,-c(1:5)], predictions_fixed_1),
    truth = price_to_book,
    estimate = predictions_fixed_1)
rsq_fixed_1 = rsq_trad(cbind(datapanel[,-c(1:5)], predictions_fixed_1),
    truth = price_to_book,
    estimate = predictions_fixed_1)

# Print errors
mae_fixed_1["model"] = "fixed_1_1"
#mae_fixedtime
rmse_fixed_1["model"] = "fixed_1_1"
#rmse_fixedtime
rsq_fixed_1["model"] = "fixed_1_1"
#rsq_fixedtime
fixed_evaluation_1 = rbind(mae_fixed_1, rmse_fixed_1, rsq_fixed_1)
fixed_evaluation_1
```

    ## # A tibble: 3 × 4
    ##   .metric  .estimator .estimate model    
    ##   <chr>    <chr>          <dbl> <chr>    
    ## 1 mae      standard       0.359 fixed_1_1
    ## 2 rmse     standard       0.520 fixed_1_1
    ## 3 rsq_trad standard       0.868 fixed_1_1

``` r
# Predict new data
predictions_fixed_2 <- predict(ols4,
                       new_data = datapanel[,-c(1:5)])
```

``` r
# Compute the mean absolute error using one single function
mae_fixed_2 = mae(cbind(datapanel[,-c(1:5)], predictions_fixed_2),
    truth = price_to_book,
    estimate = predictions_fixed_2)
# Compute the RMSE using a function
rmse_fixed_2 = rmse(cbind(datapanel[,-c(1:5)], predictions_fixed_2),
    truth = price_to_book,
    estimate = predictions_fixed_2)
rsq_fixed_2 = rsq_trad(cbind(datapanel[,-c(1:5)], predictions_fixed_2),
    truth = price_to_book,
    estimate = predictions_fixed_2)

# Print errors
mae_fixed_2["model"] = "fixed_1_2"
#mae_fixedtime
rmse_fixed_2["model"] = "fixed_1_2"
#rmse_fixedtime
rsq_fixed_2["model"] = "fixed_1_2"
#rsq_fixedtime
fixed_evaluation_2 = rbind(mae_fixed_2, rmse_fixed_2, rsq_fixed_2)
fixed_evaluation_2
```

    ## # A tibble: 3 × 4
    ##   .metric  .estimator .estimate model    
    ##   <chr>    <chr>          <dbl> <chr>    
    ## 1 mae      standard       0.352 fixed_1_2
    ## 2 rmse     standard       0.508 fixed_1_2
    ## 3 rsq_trad standard       0.874 fixed_1_2

КАЧЕСТВО МОДЕЛИ БУСТИНГА СЛУЧАЙНОГО ЛЕСА

``` r
diplom_1802_w_imp = read.csv('/Users/olesyamba/Downloads/data/diplom_1802_w_imp.csv')
dataPanel <- pdata.frame(diplom_1802_w_imp, index=c("ticker","year"))
datapanel_test = filter(dataPanel[,-c(8, 9, 20)], year == "2016" | year =="2017")
```

``` r
# Predict new data
predictions_boostforest <- predict(final_model,
                       new_data = datapanel_test[,-c(1:5)])
# Compute the mean absolute error using one single function
mae_boostforest = mae(predictions_boostforest,
    truth = datapanel_test[,-c(1:5)]$price_to_book,
    estimate = .pred)
# Compute the RMSE using a function
rmse_boostforest = rmse(predictions_boostforest,
    truth = datapanel_test[,-c(1:5)]$price_to_book,
    estimate = .pred)
rsq_boostforest = rsq_trad(predictions_boostforest,
    truth = datapanel_test[,-c(1:5)]$price_to_book,
    estimate = .pred)

# Print errors
mae_boostforest["model"] = "boosted_random_forest"
#mae_fixedtime
rmse_boostforest["model"] = "boosted_random_forest"
#rmse_fixedtime
rsq_boostforest["model"] = "boosted_random_forest"
#rsq_fixedtime
boostforest_evaluation = rbind(mae_boostforest, rmse_boostforest, rsq_boostforest)
boostforest_evaluation
```

    ## # A tibble: 3 × 4
    ##   .metric  .estimator .estimate model                
    ##   <chr>    <chr>          <dbl> <chr>                
    ## 1 mae      standard       0.360 boosted_random_forest
    ## 2 rmse     standard       0.596 boosted_random_forest
    ## 3 rsq_trad standard       0.692 boosted_random_forest

``` r
evaluation_metrics = rbind(fixed_evaluation_1, fixed_evaluation_2, boostforest_evaluation)
colnames(evaluation_metrics) = c("Metric", "Estimator", "Estimation", "Model")
evaluation_metrics
```

    ## # A tibble: 9 × 4
    ##   Metric   Estimator Estimation Model                
    ##   <chr>    <chr>          <dbl> <chr>                
    ## 1 mae      standard       0.359 fixed_1_1            
    ## 2 rmse     standard       0.520 fixed_1_1            
    ## 3 rsq_trad standard       0.868 fixed_1_1            
    ## 4 mae      standard       0.352 fixed_1_2            
    ## 5 rmse     standard       0.508 fixed_1_2            
    ## 6 rsq_trad standard       0.874 fixed_1_2            
    ## 7 mae      standard       0.360 boosted_random_forest
    ## 8 rmse     standard       0.596 boosted_random_forest
    ## 9 rsq_trad standard       0.692 boosted_random_forest

АНАЛИЗ УСТОЙЧИВОСТИ

``` r
diplom_1802_w_imp = read.csv('/Users/olesyamba/Downloads/data/diplom_1802_w_imp.csv')
diplom_1802_w_imp1 = filter(diplom_1802_w_imp, year == "2017"|year == "2018")
diplom_1802_w_imp2 = filter(diplom_1802_w_imp, year == "2019"|year == "2020")
diplom_1802_w_imp3 = filter(diplom_1802_w_imp, year == "2021"|year == "2022")
```

``` r
dataPanel1 <- pdata.frame(diplom_1802_w_imp1, index=c("ticker","year"))
dataPanel2 <- pdata.frame(diplom_1802_w_imp2, index=c("ticker","year"))
dataPanel3 <- pdata.frame(diplom_1802_w_imp3, index=c("ticker","year"))
```

МОДЕЛЬ МНОЖЕСТВЕННОЙ ЛИНЕЙНОЙ РЕГРЕССИИ

``` r
fixed1_1 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel1, model="within")
summary(fixed1_1)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel1, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -1.0268e+00 -1.6253e-01  4.5146e-16  1.6253e-01  1.0268e+00 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news      14.37122909  6.14715589  2.3379  0.021562 *  
    ## polarity_twitter   -0.36059325  3.01081848 -0.1198  0.904930    
    ## yoy_revenue_growth  0.07705016  0.11240291  0.6855  0.494764    
    ## google_trends      -0.00027423  0.00140911 -0.1946  0.846127    
    ## number_of_news      0.18301852  0.05625504  3.2534  0.001596 ** 
    ## number_of_tweets   -0.01387097  0.00632290 -2.1938  0.030772 *  
    ## sales              -0.01402590  0.01022547 -1.3717  0.173504    
    ## ROA                 0.13511819  0.01052098 12.8427 < 2.2e-16 ***
    ## fin_leverage       -0.01282421  0.02920037 -0.4392  0.661561    
    ## buyback_yield      -0.00057185  0.02119325 -0.0270  0.978532    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    72.093
    ## Residual Sum of Squares: 19.526
    ## R-Squared:      0.72916
    ## Adj. R-Squared: 0.40238
    ## F-statistic: 24.7681 on 10 and 92 DF, p-value: < 2.22e-16

``` r
fixed1_2 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_likes_twitter + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel1, model="within")
summary(fixed1_2)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_likes_twitter + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel1, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -9.6340e-01 -1.6344e-01 -1.6523e-16  1.6344e-01  9.6340e-01 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t-value Pr(>|t|)    
    ## polarity_news           14.8841081  6.3465892  2.3452  0.02117 *  
    ## polarity_twitter         0.8663237  3.0445566  0.2845  0.77663    
    ## yoy_revenue_growth       0.0012769  0.1144324  0.0112  0.99112    
    ## google_trends           -0.0010243  0.0014198 -0.7214  0.47247    
    ## number_of_news           0.0915475  0.0514224  1.7803  0.07833 .  
    ## number_of_likes_twitter  0.0247709  0.0248682  0.9961  0.32182    
    ## sales                    0.0023871  0.0097863  0.2439  0.80783    
    ## ROA                      0.1420189  0.0115497 12.2963  < 2e-16 ***
    ## fin_leverage            -0.0062061  0.0298837 -0.2077  0.83594    
    ## buyback_yield            0.0060592  0.0215155  0.2816  0.77887    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    72.093
    ## Residual Sum of Squares: 20.328
    ## R-Squared:      0.71803
    ## Adj. R-Squared: 0.37783
    ## F-statistic: 23.4277 on 10 and 92 DF, p-value: < 2.22e-16

``` r
fixed2_1 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel2, model="within")
summary(fixed2_1)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel2, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -1.3231e+00 -1.3635e-01 -2.8428e-16  1.3635e-01  1.3231e+00 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news       1.6854e+01  7.0282e+00  2.3980   0.01850 *  
    ## polarity_twitter   -5.2471e+00  3.9080e+00 -1.3427   0.18268    
    ## yoy_revenue_growth -8.5357e-03  2.3717e-02 -0.3599   0.71975    
    ## google_trends      -6.4638e-05  7.4014e-04 -0.0873   0.93060    
    ## number_of_news      5.1190e-02  4.5631e-02  1.1218   0.26485    
    ## number_of_tweets   -3.5223e-03  3.3878e-03 -1.0397   0.30119    
    ## sales              -1.9026e-02  8.5391e-03 -2.2282   0.02831 *  
    ## ROA                 1.6573e-01  1.6697e-02  9.9260 3.267e-16 ***
    ## fin_leverage       -7.3582e-02  2.3992e-02 -3.0669   0.00284 ** 
    ## buyback_yield       3.0785e-02  2.3702e-02  1.2988   0.19725    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    38.473
    ## Residual Sum of Squares: 16.686
    ## R-Squared:      0.5663
    ## Adj. R-Squared: 0.043031
    ## F-statistic: 12.0128 on 10 and 92 DF, p-value: 5.1753e-13

``` r
fixed2_2 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_likes_twitter + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel2, model="within")
summary(fixed2_2)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_likes_twitter + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel2, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -1.3208e+00 -1.2727e-01 -5.6606e-16  1.2727e-01  1.3208e+00 
    ## 
    ## Coefficients:
    ##                            Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news           17.26980244  7.06806630  2.4434  0.016460 *  
    ## polarity_twitter        -5.07417818  4.00467520 -1.2671  0.208331    
    ## yoy_revenue_growth      -0.01266937  0.02361684 -0.5365  0.592939    
    ## google_trends           -0.00021547  0.00073264 -0.2941  0.769343    
    ## number_of_news           0.03085816  0.04530642  0.6811  0.497520    
    ## number_of_likes_twitter  0.00138078  0.00688275  0.2006  0.841442    
    ## sales                   -0.01672805  0.00835767 -2.0015  0.048282 *  
    ## ROA                      0.16997193  0.01676548 10.1382 < 2.2e-16 ***
    ## fin_leverage            -0.07194695  0.02411252 -2.9838  0.003645 ** 
    ## buyback_yield            0.02792485  0.02372115  1.1772  0.242147    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    38.473
    ## Residual Sum of Squares: 16.874
    ## R-Squared:      0.5614
    ## Adj. R-Squared: 0.03221
    ## F-statistic: 11.7756 on 10 and 92 DF, p-value: 8.3955e-13

``` r
fixed3_1 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_tweets + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel3, model="within")
summary(fixed3_1)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_tweets + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel3, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -6.3761e-01 -1.3421e-01  1.6827e-16  1.3421e-01  6.3761e-01 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news       4.9942e+01  1.1329e+01  4.4081  2.82e-05 ***
    ## polarity_twitter   -2.8625e+00  3.1879e+00 -0.8979 0.3715737    
    ## yoy_revenue_growth -6.1430e-02  1.5659e-02 -3.9230 0.0001684 ***
    ## google_trends      -2.8239e-05  5.3186e-03 -0.0053 0.9957752    
    ## number_of_news      8.9655e-03  3.0480e-02  0.2941 0.7693114    
    ## number_of_tweets   -7.5085e-06  3.3123e-03 -0.0023 0.9981962    
    ## sales              -2.9688e-02  1.2494e-02 -2.3762 0.0195655 *  
    ## ROA                 1.7542e-01  1.0245e-02 17.1230 < 2.2e-16 ***
    ## fin_leverage       -9.7198e-03  1.7936e-02 -0.5419 0.5891945    
    ## buyback_yield       5.5794e-02  2.4152e-02  2.3101 0.0231191 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    48.502
    ## Residual Sum of Squares: 9.0925
    ## R-Squared:      0.81253
    ## Adj. R-Squared: 0.58635
    ## F-statistic: 39.8753 on 10 and 92 DF, p-value: < 2.22e-16

``` r
fixed3_2 <- plm(price_to_book ~ polarity_news + polarity_twitter + yoy_revenue_growth +  google_trends + number_of_news + number_of_likes_twitter + sales +  ROA + fin_leverage + buyback_yield, data = dataPanel3, model="within")
summary(fixed3_2)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = price_to_book ~ polarity_news + polarity_twitter + 
    ##     yoy_revenue_growth + google_trends + number_of_news + number_of_likes_twitter + 
    ##     sales + ROA + fin_leverage + buyback_yield, data = dataPanel3, 
    ##     model = "within")
    ## 
    ## Balanced Panel: n = 102, T = 2, N = 204
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -6.3721e-01 -1.3447e-01  1.5873e-16  1.3447e-01  6.3721e-01 
    ## 
    ## Coefficients:
    ##                            Estimate  Std. Error t-value  Pr(>|t|)    
    ## polarity_news            4.9996e+01  1.1189e+01  4.4683 2.241e-05 ***
    ## polarity_twitter        -3.0370e+00  3.3248e+00 -0.9134 0.3634047    
    ## yoy_revenue_growth      -6.0963e-02  1.5708e-02 -3.8809 0.0001955 ***
    ## google_trends            2.2066e-05  5.1682e-03  0.0043 0.9966026    
    ## number_of_news           1.0572e-02  2.3456e-02  0.4507 0.6532559    
    ## number_of_likes_twitter -9.4426e-04  6.3769e-03 -0.1481 0.8826073    
    ## sales                   -3.0066e-02  1.2236e-02 -2.4572 0.0158772 *  
    ## ROA                      1.7514e-01  1.0238e-02 17.1072 < 2.2e-16 ***
    ## fin_leverage            -9.6844e-03  1.7763e-02 -0.5452 0.5869370    
    ## buyback_yield            5.5892e-02  2.4141e-02  2.3152 0.0228244 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    48.502
    ## Residual Sum of Squares: 9.0904
    ## R-Squared:      0.81258
    ## Adj. R-Squared: 0.58645
    ## F-statistic: 39.887 on 10 and 92 DF, p-value: < 2.22e-16

``` r
datapanel1 = dataPanel1[,-c(8, 9, 16:20)]
datapanel2 = dataPanel2[,-c(8, 9, 16:20)]
datapanel3 = dataPanel3[,-c(8, 9, 16:20)]
# Train the final model on the full training data
final_model1 <- final_spec %>% fit(formula = price_to_book ~ ., 
                                  data = datapanel1[,-c(1:5)])
final_model2 <- final_spec %>% fit(formula = price_to_book ~ ., 
                                  data = datapanel2[,-c(1:5)])
final_model3 <- final_spec %>% fit(formula = price_to_book ~ ., 
                                  data = datapanel3[,-c(1:5)])

vip::vip(final_model1)
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
vip::vip(final_model2)
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

``` r
vip::vip(final_model3)
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

``` r
h1 = vip::vi_model(final_model1)
h1 = h1 %>% 
  mutate(model = c(1,1,1,1,1,1,1))
h2 = vip::vi_model(final_model2)
h2 = h2 %>% 
  mutate(model = c(2,2,2,2,2,2,2))
h3 = vip::vi_model(final_model3)
h3 = h3 %>% 
  mutate(model = c(3,3,3,3,3,3,3))
h = rbind(h1,h2,h3)
colnames(h) = c("Variable", "Importance", "Model")
h
```

    ## # A tibble: 21 × 3
    ##    Variable                Importance Model
    ##    <chr>                        <dbl> <dbl>
    ##  1 number_of_tweets            0.457      1
    ##  2 polarity_news               0.231      1
    ##  3 google_trends               0.100      1
    ##  4 yoy_revenue_growth          0.0653     1
    ##  5 number_of_likes_twitter     0.0588     1
    ##  6 polarity_twitter            0.0483     1
    ##  7 number_of_news              0.0398     1
    ##  8 number_of_tweets            0.467      2
    ##  9 polarity_news               0.320      2
    ## 10 number_of_likes_twitter     0.0611     2
    ## # … with 11 more rows

``` r
p = ggplot(h, aes(Variable, Importance, group = Model)) 
p + geom_line(aes(colour = Model)) + geom_point(aes(colour = Model))
```

![](edit-5.0_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
