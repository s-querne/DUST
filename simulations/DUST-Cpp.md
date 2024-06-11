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
  <li><u><b>DUSTvector:</b></u> Les indices sont enregistrés dans un objet Vector unique qui est mis à jour par construction</li>
  <li><u><b>DUSTflist:</b></u> les indices sont enregistrés dans un objet forward_list et leur adresse mémoire dans un objet deque</li>
  <li><u><b>DUSTflistp:</b></u> idem + le premier indice est élagué par PELT</li>
  <li><u><b>DUSTreverse:</b></u> les adresses sont enregistrées dans un objet Vector que l'on élague par reverse_iterator (version avec inversion de l'iterator avant suppression)</li>
  <li><u><b>DUST:</b></u> idem sans inversion de l'iterator</li>
</ol>



```
## 
## Attaching package: 'DUSTpartitioning'
```

```
## The following objects are masked _by_ '.GlobalEnv':
## 
##     DUST, DUSTflist, simpleTest, simpleTestFlist
```


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
##  DUST_partitioning(rnorm(n)) 61083.276 69456.5420 76052.1251 73845.4280
##         DUSTvector(rnorm(n))  1444.840  1607.7535  1794.2404  1714.5585
##          DUSTflist(rnorm(n))   702.166   750.6485   863.0385   786.1135
##         DUSTflistp(rnorm(n))   741.116   823.8540   899.6544   872.8900
##        DUSTreverse(rnorm(n))   364.736   390.2790   444.1255   405.2850
##               DUST(rnorm(n))   348.254   372.1570   401.8357   388.7210
##          uq        max neval
##  81144.9040 102437.557   100
##   1856.9310   3176.188   100
##    834.5960   4232.553   100
##    913.9720   2171.278   100
##    428.4910   1468.292   100
##    413.7925   1042.712   100
```


## Comparaison 2: n/50 changepoints

Exécuter les algorithmes sur 100 vecteurs de longueur 1 000 avec 20 changepoints


```
## Unit: microseconds
##                                                                 expr       min
##  DUST_partitioning(generate_random_changepoints(n, K, 0, 1, 9)$data) 52666.222
##         DUSTvector(generate_random_changepoints(n, K, 0, 1, 9)$data)   984.123
##          DUSTflist(generate_random_changepoints(n, K, 0, 1, 9)$data)   616.927
##         DUSTflistp(generate_random_changepoints(n, K, 0, 1, 9)$data)   601.921
##        DUSTreverse(generate_random_changepoints(n, K, 0, 1, 9)$data)   357.315
##               DUST(generate_random_changepoints(n, K, 0, 1, 9)$data)   344.318
##          lq       mean     median         uq       max neval
##  57783.9650 61778.3502 60891.9905 64029.7000 93356.016   100
##   1127.2130  1296.7455  1208.6800  1290.5365  4452.600   100
##    674.1220   730.4593   708.4185   741.1980  1781.737   100
##    651.8385   722.3405   679.9030   726.9095  1758.531   100
##    381.3820   416.0258   396.3470   452.3530   522.299   100
##    370.9270   410.1205   383.5345   403.4195  1392.770   100
```


# Comparaison entre le meilleur DUST et le meilleur FPOP

## Comparaison 1: bruit blanc

Exécuter les algorithmes sur 1 000 vecteurs de bruit blanc de longueur 1 000


```
## Unit: microseconds
##                                  expr     min       lq     mean   median
##                        DUST(rnorm(n)) 335.995 364.7155 384.6567 375.4165
##  fpopw::Fpop(rnorm(n), lambda = beta) 248.747 265.3315 276.2229 271.0100
##       uq      max neval
##  388.680 2044.547  1000
##  277.652 4229.601  1000
```


## Comparaison 2: n/50 changepoints

Exécuter les algorithmes sur 1 000 vecteurs de longueur 1 000 avec 20 changepoints


```
## Unit: microseconds
##                                                                               expr
##             DUST(generate_random_changepoints(n, K, 0, 1, 9)$data, penalty = beta)
##  fpopw::Fpop(generate_random_changepoints(n, K, 0, 1, 9)$data,      lambda = beta)
##      min      lq     mean  median       uq      max neval
##  337.430 360.513 377.9904 368.836 378.6965 1953.937  1000
##  266.541 287.820 297.7712 293.396 300.0790 1876.488  1000
```
