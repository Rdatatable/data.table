ATIME=~/R/data.table/.ci/atime
mkdir -p $ATIME
cp ~/tests.R $ATIME/tests.R
Rscript $ATIME/bisect-7687.R

