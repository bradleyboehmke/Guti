Shiny App template project
===

This project will create a basic R Shiny Application to be deployed in the RDK.
New analytic should be created in their own branch within git.



Quickstart
---

#### Checkout and create a new application

```
git clone git@gitlab.devforce.io:arcyber-gf/shiny-apps.git
cd shiny-apps

# Create the baseline R code for a new app
./createApp.sh myApp
cd analytics/myApp
```


This will create a branch in git and add a new folder analytics/myApp with a basic app.R and associated files in it.

Test data can be places in the `data` and available to load within the app.


#### Dependency

A R dependency called 'c3Services' is required to interact with c3 AFD's.  This R package is located in the Devforce Nexus repository and can be found using the following search parameters:

```
Group: c3.r
Artifact: c3-services
Type: tgz
```

Once you have downloaded the c3-services artifact you may install it into your R environment with the following command:

```
> install.packages("/path/to/c3-services-{version}.tgz", repos=NULL, type="source")
```

Functions made available by this library include:
```
getAfdID(session)
Extracts the AFD ID from the URL query parameter.  Useful for loading an AFD into the analytic on startup.  The required session object is the user's current http session.

listAFDs(config,  session)
Returns a list of AFDs which match the 'dataTypes' and 'tags' files provided on supplied config.  The required session object is the user's current http session.

loadAFD(dataSetId, session)
Returns the AFD for the provided dataSetId.  The required session object is the user's current http session.
```

#### Commit changed code

The first time you commit your code you will want to to create a remote branch that tracks with your local branch.

```
git add analytics/myApp
git commit -m "some meaningful commit message."
git push -u origin myApp
```


Every subsequent commit should be done via

```
git add analytics/myApp
git commit -m "some meaningful commit message."
git pull
# Fix any merge issues if someone else has made commits on your branch.
git push -u origin myApp
```
