class07 lab
================
Nickolas Beam

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
```

> Q1. How many rows and columns are in your new data frame named x? What
> R functions could you use to answer this questions?

``` r
dim(x)
```

    [1] 17  5

``` r
#used after updating x columns
```

``` r
head(x)
```

                   X England Wales Scotland N.Ireland
    1         Cheese     105   103      103        66
    2  Carcass_meat      245   227      242       267
    3    Other_meat      685   803      750       586
    4           Fish     147   160      122        93
    5 Fats_and_oils      193   235      184       209
    6         Sugars     156   175      147       139

Fix the amount of columns

``` r
dim(x)
```

    [1] 17  5

``` r
x <- read.csv(url, row.names=1)
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

> Q2. Which approach to solving the ‘row-names problem’ mentioned above
> do you prefer and why? Is one approach more robust than another under
> certain circumstances?

The second method is more robust and easier.

``` r
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

![](class07-lab_files/figure-gfm/unnamed-chunk-6-1.png)

> Q3: Changing what optional argument in the above barplot() function
> results in the following plot?

``` r
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](class07-lab_files/figure-gfm/unnamed-chunk-7-1.png)

beside = F turns the plot into this one

> Q5: Generating all pairwise plots may help somewhat. Can you make
> sense of the following code and resulting figure? What does it mean if
> a given point lies on the diagonal for a given plot?

``` r
pairs(x, col=rainbow(10), pch=16)
```

![](class07-lab_files/figure-gfm/unnamed-chunk-8-1.png)

The pairwise plots compares results between different countries. A
diagonal for a given plot gives rise to possibility of correlation
between eating habits between countries.

> Q6. What is the main differences between N. Ireland and the other
> countries of the UK in terms of this data-set?

``` r
pca <- prcomp(t(x))
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3       PC4
    Standard deviation     324.1502 212.7478 73.87622 4.189e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

``` r
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))
```

![](class07-lab_files/figure-gfm/unnamed-chunk-10-1.png)

``` r
v <- round(pca$sdev^2/sum(pca$sdev^2)*100)
v
```

    [1] 67 29  4  0

``` r
z <- summary(pca)
z$importance
```

                                 PC1       PC2      PC3          PC4
    Standard deviation     324.15019 212.74780 73.87622 4.188568e-14
    Proportion of Variance   0.67444   0.29052  0.03503 0.000000e+00
    Cumulative Proportion    0.67444   0.96497  1.00000 1.000000e+00

``` r
barplot(v, xlab="Principal Component", ylab="Percent Variation")
```

![](class07-lab_files/figure-gfm/unnamed-chunk-13-1.png)

``` r
par(mar=c(10,3,0.35,0))
barplot(pca$rotation[,1], las=2)
```

![](class07-lab_files/figure-gfm/unnamed-chunk-14-1.png)

``` r
biplot(pca)
```

![](class07-lab_files/figure-gfm/unnamed-chunk-15-1.png)

``` r
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

           wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    gene1  439 458  408  429 420  90  88  86  90  93
    gene2  219 200  204  210 187 427 423 434 433 426
    gene3 1006 989 1030 1017 973 252 237 238 226 210
    gene4  783 792  829  856 760 849 856 835 885 894
    gene5  181 249  204  244 225 277 305 272 270 279
    gene6  460 502  491  491 493 612 594 577 618 638

``` r
dim(rna.data)
```

    [1] 100  10

``` r
pca <- prcomp(t(rna.data), scale=TRUE)
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")
```

![](class07-lab_files/figure-gfm/unnamed-chunk-18-1.png)

``` r
summary(rna.data)
```

          wt1              wt2             wt3              wt4        
     Min.   :  24.0   Min.   : 25.0   Min.   :  12.0   Min.   :  13.0  
     1st Qu.: 218.8   1st Qu.:231.0   1st Qu.: 211.0   1st Qu.: 219.8  
     Median : 459.0   Median :480.5   Median : 477.0   Median : 464.0  
     Mean   : 480.1   Mean   :481.1   Mean   : 481.8   Mean   : 478.7  
     3rd Qu.: 746.8   3rd Qu.:733.2   3rd Qu.: 733.2   3rd Qu.: 704.5  
     Max.   :1007.0   Max.   :994.0   Max.   :1030.0   Max.   :1020.0  
          wt5              ko1             ko2              ko3        
     Min.   :  21.0   Min.   : 18.0   Min.   :  14.0   Min.   :  19.0  
     1st Qu.: 222.0   1st Qu.:292.8   1st Qu.: 301.2   1st Qu.: 295.0  
     Median : 481.5   Median :505.5   Median : 530.5   Median : 512.5  
     Mean   : 479.2   Mean   :522.2   Mean   : 525.8   Mean   : 525.4  
     3rd Qu.: 729.0   3rd Qu.:757.5   3rd Qu.: 750.8   3rd Qu.: 750.0  
     Max.   :1027.0   Max.   :997.0   Max.   :1031.0   Max.   :1033.0  
          ko4              ko5        
     Min.   :  18.0   Min.   :  14.0  
     1st Qu.: 303.0   1st Qu.: 286.5  
     Median : 504.5   Median : 510.0  
     Mean   : 524.4   Mean   : 523.5  
     3rd Qu.: 772.5   3rd Qu.: 752.0  
     Max.   :1021.0   Max.   :1036.0  

``` r
plot(pca, main="Quick scree plot")
```

![](class07-lab_files/figure-gfm/unnamed-chunk-20-1.png)

``` r
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
```

     [1] 92.6  2.3  1.1  1.1  0.8  0.7  0.6  0.4  0.4  0.0

``` r
barplot(pca.var.per, main="Scree Plot",
        names.arg=paste0("PC", 1:10),
        xlab="Principal Component", ylab="Percent Variation")
```

![](class07-lab_files/figure-gfm/unnamed-chunk-22-1.png)

``` r
colvec <- colnames(rna.data)
colvec[grep("wt",colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))

text(pca$x[,1], pca$x[2], labels=colnames(rna.data), pos=c(rep(4,5), rep(2,5)))
```

![](class07-lab_files/figure-gfm/unnamed-chunk-23-1.png)

``` r
library(ggplot2)

df <- as.data.frame(pca$x)

ggplot(df) + aes(PC1,PC2) + geom_point()
```

![](class07-lab_files/figure-gfm/unnamed-chunk-24-1.png)

``` r
df$samples <- colnames(rna.data) 
df$condition <- substr(colnames(rna.data),1,2)

p <- ggplot(df) + 
        aes(PC1, PC2, label=samples, col=condition) + 
        geom_label(show.legend = FALSE)
p
```

![](class07-lab_files/figure-gfm/unnamed-chunk-25-1.png)

``` r
p + labs(title="PCA of RNASeq Data",
         subtitle="PC1 clearly separates wild-type from knock-out samples",
         x=paste0("PC1 (", pca.var.per[1], "%)"),
         y=paste0("PC2 (", pca.var.per[2], "%)"),
         caption="Class example data") + theme_bw()
```

![](class07-lab_files/figure-gfm/unnamed-chunk-26-1.png)
