seed = sample(1e3, 1)
seed = 531
set.seed(seed)

y = rnorm(1e2, .5)

tt = 70
i = 60
j = 30

Q = DUST_partitioning(y, pruning=0)$cost_record

S = cumsum(y)
n = length(y)
beta = 2 * log(n)

mi = slice_cumsum(S, i, tt) / (tt - i)
mj = slice_cumsum(S, j, tt) / (tt - j)
mji = slice_cumsum(S, j, i) / (i - j)

Qi = Q[i + 1]
Qj = Q[j + 1]



D = function(mu) {
  numerateur = (tt - i) * mi - mu * (i - j) * mji
  denominateur = (tt - i) - mu * (i - j)
  constante = Qi + mu * (Qi - Qj)
  return(- numerateur^2 / denominateur + constante)
}

A = - (i - j)^2 * ((i - j) * mji^2 + Qi - Qj)
B = 2 * (i - j) * (tt - i) * ((i - j) * mji^2 + Qi - Qj)
C = (tt - i)^2 * ((i - j) * mi * (mi - 2 * mji) - (Qi - Qj))



d = function(mu) A * mu^2 + B * mu + C

DISCRIMINANT_1 = B^2 - 4 * A * C
RACINES_1 = - (B + c(-1, 1) * sqrt(DISCRIMINANT_1)) / 2 / A


B2 = 4 * (i - j)^2 * (tt - i)^2 * ((i - j)^2 * mji^4 + 2 * (i-j) * mji^2 * (Qi - Qj) + (Qi - Qj)^2)

AC1 = (i - j)^2 * mji^2 * mi * (mi - 2 * mji)
AC2 = - (i-j) * mji^2 * (Qi - Qj)
AC3 = (i - j) * mi * (mi - 2 * mji) * (Qi - Qj)
AC4 = - (Qi - Qj)^2
AC = -(i - j)^2 * (tt - i)^2 *(AC1 + AC2 + AC3 + AC4)
B2 - 4 * AC

DISCRIMINANT_2 = B2 - 4 * AC

D1 = (i - j)^2 * mji^4
D2 = (i-j) * mji^2 * (Qi - Qj)
D3 = AC1
D4 = AC3
DISCRIMINANT_3 = 4 * (i - j)^2 * (tt - i)^2 * (D1 + D2 + D3 + D4)

D21 = (i - j)^2 * mji^2 * mji^2 + (i - j) * mji^2 * (Qi - Qj)
D22 = (i - j)^2 * mji^2 * mi^2 + (i-j) *  mi^2 * (Qi - Qj)
D23 = - 2 * (i - j)^2 * mi * mji * mji^2 - 2 * (i - j) * mi * mji * (Qi - Qj)
DIST = 4 * (i - j)^2 * (tt - i)^2 * (D21 + D22 + D23)

DISCRIMINANT_4 = 4 * (i - j)^3 * (tt - i)^2 * (mji - mi)^2 * (Qi - Qj + (i - j) * mji^2)

RACP1 = (tt - i) / (i - j)
RACP2 = - (tt - i) * abs(mji - mi) / sqrt((i - j) * (Qi - Qj + (i - j) * mji^2))

RACINES_2 = RACP1 * (1 + c(-1, 1) *  sqrt((i - j) * (mi - mji)^2 / (Qi - Qj + (i - j) * mji^2)))

x = seq(-1, 1, length.out = 200)
plot(x, d(x), type='l', col=2)
abline(v = - B / 2 / A, lty=2, col=4)
abline(v = 0, lty = 2)
abline(h = 0, lty=2)
abline(v = RACINES_2, lty=2, col=3)


L = function(theta, mu) {
  quadratic = tt - i + mu * (j -i)
  linear = - 2 * (tt - i) * mi + 2 * mu * (j - i) * mji
  constant = Qi + mu * (Qi - Qj)

  return(quadratic * theta^2 + linear * theta + constant)
}

theta_min = function(S, tt, i, j) function(mu) {

}

D_max = D(RACINES_2)

curve(D, -5, (tt - i) / (i - j), col=2)
abline(v = 0, lty=2)
abline(v = RACP1 + RACP2, lty=2, col=3)
abline(h = D_max, lty=2)
