Filters to be used with Arcan, i.e.
```
.\arcan analyse -i "C:\...\ProjectFolder" -p ProjectName -o "C:\...\OutputFolder" --all -l JAVA -e --startDate 2017-06-18 --endDate 2021-01-18 --intervalDays 100 --branch master --filtersFile "C:\...\filters.yaml"
```
Filters coverage can be checked with the following cmd:
```
.\arcan dry-run -i "C:\...\ProjectFolder" --filtersFile "C:\...\filters.yaml" > coverage.txt
```