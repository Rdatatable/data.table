
## rolling functions

- [ ] rollmean
- [ ] rollsum
- [ ] rollmin
- [ ] rollmax
- [ ] rollmedian
- [ ] rollprod
- [ ] rollfdi (roughness statistic)
- [ ] rollapply (user provided FUN)
- [ ] rollsd
- [ ] rollvar

## rolling features

- [ ] align: left/center/right
- [ ] fill constant
- [ ] adaptive window
- [ ] fill na.locf

## rollmean implementation notes

```r
x = list(v1=1:5, v2=1:5)
k = c(2, 3)
```

i - single column
j - single window
m - single row
w - current row's sum of rolling window

```
for (i in x)
  for (j in k)
    for (m in 1:length(i)) {
      w += i[m]
      w -= i[m-j]
      ans[m] = w / j
    }
```

## potential optimizations

- [ ] cache biggest window - subset of i as vector

wv - subset of i for biggest window size, not summed

```
for (i in x) {
  rangek = range(k)
  wv = i[1:rangek[2]]
  for (m in 1:length(i)) {
    if (m < rangek[1]) next
    wv = c(vw[-1], i[m])
    for (j in k) {
      w = sum(tail(wv,j))
      ans[m] = w / j
    }
  }
}   
```

## validation

- [ ] validate against zoo (already in suggested dependency)

## benchmark

- [ ] TTR::runMean
- [ ] RcppRoll::roll_mean
- [ ] caTools::runmean
- [ ] RollingWindow::RollingMean (github.com/andrewuhl/RollingWindow)
