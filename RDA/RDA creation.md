rdaPackaging
------------

This script provides a simple way to create Deplicon RDA packages.

Running the script
------------------

The script can be run by either loaded it into an R or R studio session or by lunching it directly on the command line.

If running form the root directory of the ```shiny-apps``` project you should run the following commands from an R session

```
source('RDA/rdaPackaging.R')
createRDA('path/to/your/analytic')
```

You will see output loading all of the dependencies that your analytic uses followed by something like this:

```
  adding: <your analytic>_rda/ (stored 0%)
  adding: <your analytic>_rda/ShinyApp.rda (deflated 52%)
  adding: <your analytic>_rda/test.tar.gz (deflated 0%)
```

This will create a file ```<your analytic>.zip``` located in the RDA directory.
 
This file can be deployed to a cluster using the Deplicon user interface.
 
Detailed Deplicon documentation can be found in the [BDP Developers Guide](https://jenkins.devforce.io/job/Incubator/job/rdk-doc/lastSuccessfulBuild/artifact/_build/developers_guide/components/deplicon.html)
 
RDK Deployment Archive (RDA) File
---------------------------------
This section describes the Shiny specific portions of the RDA manifest.

The key to Deplicon deployments is the .rda file contained in the root directory of the RDA zip archive that is submitted to Deplicon. The .rda file informs Deplicon of the files that are contained within the archive and the purpose they serve in the deployment. Below is an example of a .rda file for the deployment of an R Shiny application to the Shiny server. All file paths are relative to the root of the RDA zip containing this file.

The .rda file for Shiny apps will automatically be generated based on the config variable in your app.
The mapping of values is as follows

| Config variable | RDA property |
| -- | -- |
| config$title | name |
| config$analyticName | appKey |
| config$analyticVersion | version |

 
```
{
    "name": "sample application",
    "appKey": "sampleApplication",
    "version": "1.0.0",
    "deployments": [
      {
        "name": "sample application",
        "target": "shiny",
        "configuration": {
          "appKey": "sampleApplication",
          "shinyApps": [
            "sampleApplication.tar.gz"
          ]
        }
      }
    ]
}
```


Details for the properties in this example are as follows

##### Metadata Section
| Property | Details |
| -- | -- |
| name | A descriptive name of the deployment |
| appKey | The unique identifier for the application within deplicon. This will also be URL for the shiny application. <nginx proxy ip addr>/shiny/<appKey>/ |
| version | The version of the deployment. Formatted as a [semver](http://semver.org/) number|


##### Deployment Section

This is passed to the function which performs the deployment. 

| Property | Details |
| -- | -- |
| deployments.name | The same "name" from the Metadata Section |
| deployments.target | The intended deployer for this artifact. For R shiny deployments this should be "shiny"|
| deployments.configuration.appKey | The same "appKey" from the Metadata Section.  |
| deployments.configuration.shinyApps | The .tar.gz file to deploy.  |
