### Twitter'dan R ve Python ile Veri Çekilmesi
---
Twitter'da Developer hesabı açmak [için](https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html). 

R için gerekli olan paketler;
```r
if (!requireNamespace("rtweet")) install.packages("rtweet", quiet = T)
if (!requireNamespace("dplyr")) install.packages("dplyr", quiet = T)
if (!requireNamespace("stopwords")) install.packages("stopwords", quiet = T)
if (!requireNamespace("wordcloud")) install.packages("wordcloud", quiet = T)
if (!requireNamespace("tidytext")) install.packages("tidytext", quiet = T)
if (!requireNamespace("ggplot2")) install.packages("ggplot2", quiet = T)
if (!requireNamespace("jsonlite")) install.packages("jsonlite", quiet = T)
```

Python için gerekli olan paketler:
```python
$ pip install tweepy==3.10.0 pandas==1.3.3
```



