---
title: "DUST C++ (avec comparaison FPOP)"
author: "Simon QUERNE"
date: "2024-06-11"
output:
  html_document:
    keep_md: true
    css: styles.css
    toc: true
    toc_float: true
    highlight: tango
    number_sections: true
---



# Comparaison entre les différentes version de DUST

<ol>
  <li><u><b>DUST_partitioning:</b></u> La fonction de référence sous R</li>
  <li><u><b>DUSTvector:</b></u> Les indices sont enregistrés dans un objet Vector unique qui est mis à jour par construction</li>
  <li><u><b>DUSTflistp:</b></u> Les indices sont enregistrés dans une forward list associée à un deque de pointers. Le premier indice est élagué par PELT</li>
  <li><u><b>DUST:</b></u> Les indices sont enregistrés dans une forward list associée à un vecteur de pointers</li>
  <li><u><b>DUSTclass:</b></u> La gestion des indices et de leur pointer est faite via une classe ForwardListHandler, et un vecteur runif de taille n^1.2 ou n^1.3 est généré au début du programme pour la sélection de l'indice de contrainte</li>
</ol>





## Comparaison 1: bruit blanc

Exécuter les algorithmes sur 100 vecteurs de bruit blanc de longueur 1 000


```
## Warning in microbenchmark::microbenchmark(DUST_partitioning(rnorm(n)),
## DUSTvector(rnorm(n)), : less accurate nanosecond times to avoid potential
## integer overflows
```

```
## Unit: microseconds
##                         expr       min         lq       mean     median
##  DUST_partitioning(rnorm(n)) 60175.290 69920.8260 75440.4711 73611.5435
##         DUSTvector(rnorm(n))  1369.892  1615.2155  1779.0572  1713.6565
##         DUSTflistp(rnorm(n))   756.327   811.0825   877.2946   858.5810
##               DUST(rnorm(n))   408.073   438.5770   462.9060   453.0705
##          DUSTclass(rnorm(n))   319.513   348.6230   369.9889   365.4945
##          uq        max neval
##  79643.5660 101227.934   100
##   1836.4515   2913.460   100
##    898.4330   1875.545   100
##    474.2265    923.443   100
##    384.2725    695.237   100
```


## Comparaison 2: n/50 changepoints

Exécuter les algorithmes sur 100 vecteurs de longueur 1 000 avec 20 changepoints


```
## Unit: microseconds
##                                                                 expr       min
##  DUST_partitioning(generate_random_changepoints(n, K, 0, 1, 9)$data) 51662.460
##         DUSTvector(generate_random_changepoints(n, K, 0, 1, 9)$data)   979.449
##         DUSTflistp(generate_random_changepoints(n, K, 0, 1, 9)$data)   581.052
##               DUST(generate_random_changepoints(n, K, 0, 1, 9)$data)   382.735
##          DUSTclass(generate_random_changepoints(n, K, 0, 1, 9)$data)   305.860
##          lq       mean     median         uq       max neval
##  58371.1670 61634.8408 60762.5125 64154.3195 89375.080   100
##   1116.9835  1217.4015  1194.3915  1268.5195  2180.544   100
##    648.2715   700.0791   676.5820   728.7135  1782.967   100
##    409.3850   437.1170   423.1815   442.9640   554.607   100
##    331.0545   362.4699   347.1265   379.7830   481.668   100
```


# Comparaison entre le meilleur DUST et le meilleur FPOP

## Comparaison 1: bruit blanc

Exécuter les algorithmes sur 1 000 vecteurs de bruit blanc de longueur 1 000


```
## Unit: microseconds
##                                  expr     min       lq     mean   median
##                        DUST(rnorm(n)) 384.908 431.1560 456.6846 444.9320
##                   DUSTclass(rnorm(n)) 304.507 343.2725 359.4694 356.7205
##  fpopw::Fpop(rnorm(n), lambda = beta) 247.148 266.5820 277.7342 272.2195
##        uq      max neval
##  462.3160 3102.388  1000
##  370.3325  449.811  1000
##  278.7385 2555.366  1000
```


## Comparaison 2: n/50 changepoints

Exécuter les algorithmes sur 1 000 vecteurs de longueur 1 000 avec 20 changepoints


```
## Unit: microseconds
##                                                                               expr
##             DUST(generate_random_changepoints(n, K, 0, 1, 9)$data, penalty = beta)
##        DUSTclass(generate_random_changepoints(n, K, 0, 1, 9)$data, penalty = beta)
##  fpopw::Fpop(generate_random_changepoints(n, K, 0, 1, 9)$data,      lambda = beta)
##      min       lq     mean   median       uq      max neval
##  354.486 400.5290 423.4777 412.4190 424.1655 2423.920  1000
##  284.950 320.2305 333.7324 330.9110 342.1040 2119.987  1000
##  268.304 291.3460 301.9643 297.8445 303.4000 2077.470  1000
```
