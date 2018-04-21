
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

## validation

- [ ] validate against zoo (already in suggested dependency)

## benchmark

- [ ] TTR::runMean
- [ ] RcppRoll::roll_mean
- [ ] caTools::runmean
- [ ] RollingWindow::RollingMean (github.com/andrewuhl/RollingWindow)
