## git bisect old=equal perf, new=divergent.
cp tests.R ~/tests.R
git bisect start
git bisect old 90f1c1e7b4811ceaef4848a2805a2295d9e5c5f2
git bisect new master
git bisect run bash ~/R/data.table/.ci/atime/run-7687.sh
