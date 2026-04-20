.onLoad = function(libname, pkgname) {
      ## This drat repo is used to host the data for the realbooks vignette
      repos = getOption('repos')
      repos['tmfast.realbooks'] = 'https://dhicks.github.io/drat/'
      options(repos = repos)
      invisible(repos)
}
