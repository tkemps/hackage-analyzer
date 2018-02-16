# hackage-analyzer

Scrape http://hackage.haskell.org/ and write the content to a PostgreSQL data base. We scrape most of the package details incl. the dependencies and the build status. There is an R script in the directory '/R' which does a first basic analysis.

Start selenium stand-alone server with e.g.:

```java -jar  bin/selenium-server-standalone-3.8.1.jar```

make sure your PostGreSQL server is running, e.g.: `pg_ctl start`

