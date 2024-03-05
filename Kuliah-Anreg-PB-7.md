ANREG Individu PB 7
================
Alista Sava Davina
2024-03-05

# Mengimport Data

``` r
data <- read.csv("C:/Users/Alista/Documents/Semester 4/Tugas Anreg PB 7.csv", sep=";" )
str(data)
```

    ## 'data.frame':    15 obs. of  3 variables:
    ##  $ no: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ x : int  2 5 7 10 14 19 26 31 34 38 ...
    ##  $ y : int  54 50 45 37 35 25 20 16 18 13 ...

``` r
y <- data$y
x <- data$x

plot(x,y)
```

![](Kuliah-Anreg-PB-7_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Membentuk Model Regresi

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.3.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.3.3

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
model1 = lm(y~x, data = data)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.1628 -4.7313 -0.9253  3.7386  9.0446 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 46.46041    2.76218   16.82 3.33e-10 ***
    ## x           -0.75251    0.07502  -10.03 1.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.891 on 13 degrees of freedom
    ## Multiple R-squared:  0.8856, Adjusted R-squared:  0.8768 
    ## F-statistic: 100.6 on 1 and 13 DF,  p-value: 1.736e-07

Didapatkan bahwa nilai p-value sebesar 1.736e-07 sehingga dapat
disimpulkan bahwa dalam taraf kepercayaan 95% terdapat cukup bukti untuk
menyatakan bahwa variabel x memengaruhi variabel y. Dalam uji parsial
didapatkan p-value intercept sebesar 3.33e-10 dan x 1.74e-07 dengan
indikasi bintang 3 yang mengindikasikan bahwa signifikan pada level
tersebut dalam taraf kepercayaan 95%. Sehingga, baik secara simultan
ataupun parsial, terdapat cukup bukti untuk menyatakan bahwa variabel x
memengaruhi variabel y secara signifikan. Nilai adjusted R-squared
sebesar 0.8768 mengindikasikan bahwa variabel x mampu menjelaskan
keragaman variabel y sebesar 87.68%.

# Uji Asumsi Klasik Regresi Linear Sederhana

``` r
error = model1$residuals
error
```

    ##          1          2          3          4          5          6          7 
    ##  9.0446035  7.3021275  3.8071435 -1.9353325 -0.9253005 -7.1627605 -6.8952045 
    ##          8          9         10         11         12         13         14 
    ## -7.1326645 -2.8751405 -4.8651085 -4.5975525  3.6700035  1.4225115  2.6900675 
    ##         15 
    ##  8.4526075

## Uji Normalitas Residual

H0: Nilai residual berdistribusi normal

H1: Nilai residual tidak berdistribusi normal

``` r
library(nortest)
plot(model1, 2)
```

![](Kuliah-Anreg-PB-7_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
shapiro.test(error)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  error
    ## W = 0.92457, p-value = 0.226

``` r
ad.test(error)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  error
    ## A = 0.35232, p-value = 0.4178

``` r
lillie.test(error)
```

    ## 
    ##  Lilliefors (Kolmogorov-Smirnov) normality test
    ## 
    ## data:  error
    ## D = 0.12432, p-value = 0.7701

Berdasarkan p-value normality test uji Shapiro Wilk (0.226), Anderson
Darling (0.4178), dan Kolmogorov-Smirnov (0.7701). Didapatkan nilai
p-value yang lebih besar dari taraf kesalahan 0.05, sehingga dapat
dikatakan bahwa tidak terdapat cukup bukti untuk menyatakan bahwa nilai
error tidak berdistribusi normal dalam tingkat kepercayaan 95%. Sehingga
dalam uji normalitas, asumsi kenormalan data yang diduga dengan residual
tidak terlanggar atau terpenuhi.

## Uji Non Autokorelasi

H0: Tidak terjadi autokorelasi dalam residual

H1: Terdapat autokorelasi dalam resiual

``` r
library(lmtest)
dwtest(model1)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model1
    ## DW = 0.48462, p-value = 1.333e-05
    ## alternative hypothesis: true autocorrelation is greater than 0

Didapatkan nilai p-value sebesar 1.33e-05 \< 0.05 atau tolak H0.
Sehingga dapat dikatakan terdapat cukup bukti untuk menyatakan bahwa
terdapat autokorelasi pada residual dalam taraf kepercayaan 95%. Hal ini
mengindikasikan bahwa asumsi non autokorelasi tidak terpenuhi atau
residual tidak saling bebas. Nilai durbin watson yang tidak memiliki
autokorelasi berada dalam rentang 1 sampai 3, karena nilai yang
didapatkan berada pada angka 0.48462, maka hal ini mengindikasikan bahwa
terjadi autokorelasi.

## Uji Homokedastisitas

H0: Varian residual konstan H1: Varian residual tidak konstan

``` r
library(lmtest)
bptest(model1, studentize = TRUE, data = data)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  model1
    ## BP = 0.52819, df = 1, p-value = 0.4674

Uji kehomogenan variansi residual menggunakan Breush-Pagan test.
Berdasarkan uji Breush-Pagan didapatkan nilai p-value sebesar 0.4674 \>
0.05 sehingga gagal tolak H0. Tidak terdapat cukup bukti untuk
menyatakan bahwa varian residual dalam model tidak konstan atau gagal
untuk menyatakan adanya heterokedastisitas, sehingga uji
homokedastisitas terpenuhi.

# Kesimpulan Pengujian Asumsi Klasik

Setelah dilakukan pengujian-pengujian asumsi klasik regresi linear
sederhana, diperlukan penanganan untuk menangani adanya autokorelasi
dalam model dengan melakukan transformasi. Transformasi dilakukan dengan
memperkecil nilai pada variabel x dan y menggunakan operasi akar ke dua
dari variabel yang ada pada model dikarenakan nilai dari koefisien b1
hasil pemodelan sebesar -0.75251 yang kurang dari nol.

# Penanganan Autokorelasi

## Transformasi Data

``` r
xt <- sqrt(x); xt
```

    ##  [1] 1.414214 2.236068 2.645751 3.162278 3.741657 4.358899 5.099020 5.567764
    ##  [9] 5.830952 6.164414 6.708204 7.211103 7.280110 7.745967 8.062258

``` r
yt <- sqrt(y); yt
```

    ##  [1] 7.348469 7.071068 6.708204 6.082763 5.916080 5.000000 4.472136 4.000000
    ##  [9] 4.242641 3.605551 2.828427 3.316625 2.828427 2.000000 2.449490

``` r
library(lmtest)
model2 = lm(yt~xt, data = data)
```

## Uji Non Autokorelasi Hasil Transformasi

H0: Tidak terjadi autokorelasi dalam residual

H1: Terdapat autokorelasi dalam residual

``` r
library(lmtest)
dwtest(model2)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model2
    ## DW = 2.6803, p-value = 0.8629
    ## alternative hypothesis: true autocorrelation is greater than 0

Berdasarkan uji autokorelasi hasil transformasi menggunakan
Durbin-Watson test didapatkan nilai DW sebesar 2.6803 yang menunjukkan
bahwa tidak terdapat autokorelasi pada model karena berada dalam selang
1 sampai 3. Nilai p-value sebesar 0.8629 \> batas kesalahan 0.05 atau
terima H0 mengindikasikan bahwa tidak terdapat cukup bukti untuk
menyatakan terjadinya autokorelasi dalam model.

## Regresi Linear Sederhana Hasil Transformasi

``` r
plot(xt,yt)
```

![](Kuliah-Anreg-PB-7_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = yt ~ xt, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.42765 -0.17534 -0.05753  0.21223  0.46960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.71245    0.19101   45.61 9.83e-16 ***
    ## xt          -0.81339    0.03445  -23.61 4.64e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2743 on 13 degrees of freedom
    ## Multiple R-squared:  0.9772, Adjusted R-squared:  0.9755 
    ## F-statistic: 557.3 on 1 and 13 DF,  p-value: 4.643e-12

### Interpretasi dan Kesimpulan

Hasil visualisasi analisis data menggunakan diagram pencar dapat
diketahui bahwa adanya hubungan linear yang negatif antara variabel
penjelas x dengan variabel y. Hal ini mengindikasikan bahwa ketika unsur
variabel x mengalami kenaikan maka akan menghasilkan penurunan nilai
pada variabel y.

Berdasarkan hasil analisis regresi linear sederhana hasil transformasi
yang telah didapatkan, diketahui bahwa terdapat nilai adjusted R-squared
sebesar 0.9755 atau sebanyak 97.55% keragaman yang terdapat pada
variabel y dapat dijelaskan oleh variabel penjelas x dan sisanya (2.45%)
dijelaskan oleh faktor lain yang tidak terdapat dalam model. Nilai
korelasi yang didapatkan dari operasi matematis akar kedua dari nilai
determinasi yaitu sebesar 0.988 yang sangat mendekati nilai 1
mengindikasikan adanya korelasi yang sangat kuat antara keduanya.

Terdapat 2 koefisien dalam model yaitu b0(intercept) sebesar 8.71245 dan
b1(xt) -0.81339 sehingga dapat dimodelkan regresi linearnya sebagai
berikut:

$\hat y^* = 8.71245 - 0.81339x$

Nilai koefisien b1 sebesar -0.8133 menunjukkan bahwa kenaikan tiap satu
satuan pada variabel x diduga berpengaruh terhadap rata-rata penurunan
pada variabel y sebesar 0.81339. Koefisien b0 sebesar 8.71245
menunjukkan dugaan rata-rata variabel y akan sebesar 8.71245 ketika
tidak ada nilai variabel x karena adanya pengaruh faktor lain yang tidak
dimasukkan dalam model, atau dalam kata lain. terdapat rata-rata nilai y
sebesar sekian yang tidak dapat dijelaskan oleh variabel x.

Nilai p-value koefisien intercept sebesar 9.83e-16 \< taraf kesalahan
0.05 atau ada signifikansi. Terdapat cukup bukti untuk menyatakan bahwa
terdapat nilai variabel y yang tidak dapat dijelaskan oleh variabel
penjelas x pada taraf kepercayaan 95%. Nilai p-value koefisien slope
sebesar 4.64e-12 yang lebih kecil dari taraf kesalahan 0.05
mengindikasikan bahwa adanya signifikansi atau terdapat cukup bukti
untuk menyatakan bahwa ada hubungan linear antara variabel penjelas x
dengan variabel y atau variabel penjelas x memengaruhi rata-rata
penurunan variabel y dalam taraf kepercayaan 95%.
