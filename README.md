# BAStags

Basic data processing for British Antarctic Survey archival tags.
Provides facilities for importing and plotting data recorded by the
BAS tags, and in particular detecting times of twilight from the
recorded light data.

## Installing

The package is easily built with RStudio

1. Install R

2. Install [RStudio](http://www.rstudio.com)

3. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) or equivalent for your platform

4. Install [devtools](http://cran.r-project.org/web/packages/devtools/index.html) and [roxygen2](http://cran.r-project.org/web/packages/roxygen2/index.html) packages and dependencies in R

5. Install [geosphere](http://cran.r-project.org/web/packages/geosphere/index.html), [GeoLight](http://cran.r-project.org/web/packages/GeoLight/index.html) packages and dependencies.

6. Clone the repository from GitHub (https://github.com/SWotherspoon/SGAT).

7. Create an Rstudio project in the folder containing this README file.

8. In the build tab, choose `More/Configure Build Tools...` and click
`Generate documentation with Roxygen`, select `Configure` and choose to generate `Rd files` and the `NAMESPACE file`, leaving the other options as they are.

9. Choose `More/Roxygenize` from the `Build` tab

10. Choose `Build & Reload` to make the package immediately available to R, or choose `More/Build source package` `More/Build binary package` from the `Build` tab to make source or binary packages.



## TODO

- **Alternate Coordinates**.  The Metropolis sampler may be more efficient if locations are represented in geocentric ecliptic coordinates. But it is unclear whether the gains in efficiency due to higher acceptance rates would outweigh the costs of the coordinate transformation and the added code complexity. This is a low priority.

- **Parallelisation**.  At this point, the Metropolis samplers are only capable of utilizing a single core on a multicore machine.  It would be relatively simple to introduce coarse grain parallelism by having the samplers draw multiple chains in parallel, using something like the multicore facility in the parallel package.  Unfortunately, at the time of writing there does not seem to be a good parallization solution that works equally well on all platforms.  This is a low priority.



