#!/usr/bin/env bash

set -e
set -u

date=`date +%Y%m%d`

(
wget --no-check-certificate 'https://www.lendingclub.com/fileDownload.action?file=LoanStats.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > LoanStats.csv)
wget --no-check-certificate 'http://www.lendingclub.com/fileDownload.action?file=InFundingStats3.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > InFundingStats3.csv)
R --vanilla -f combined.r 

) \
2>&1 | tee go-$date.log

grep http go-$date.log| grep Detail| cut -f2 -d' '| while read url
do
        printf "$url\t"
        wget --quiet --no-check-certificate -O - $url | grep "% funded)"
done | tee go-20130425.status


