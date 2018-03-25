
.skipable_deps <- function(image_name) {
  # we may ad more no-apt or analogue no-package exceptions here and handle them with the json config -
  # as far as we know that certain images have sertain dependencies pre-installed,
  # but at the moment it won't be necessary
  if (image_name == "rocker/geospatial") {
    return(c("libproj-dev", "libgeos-dev", "gdal-bin"))
  }
  return(c())
}

.install_sf_with_outdated_system_deps <- function(image_name, pkg_names, versioned_libs, add_apt, add_inst) {
  if ("sf" %in% pkg_names) {
    # sf-dependencies proj and gdal cannot be installed directly from apt get, because the available packages are outdated.
    sf_installed <- requireNamespace("sf")

    if (sf_installed && versioned_libs) {
      # Exceptions are handled by json config here:
      ext_soft <- sf::sf_extSoftVersion()
      mapply(function(lib, version) {
        if (!.isVersionSupported(lib, version, .package_config)) {
          futile.logger::flog.warn("No explicit for support for the version %s of the linked external software %s", version, lib)
          return()
        }

        add_apt <<- append(add_apt,
                           .get_lib_apt_requirements(
                             lib,
                             version,
                             .package_config))
        no_apt <<- append(add_apt,
                          .get_lib_pkgs_names(
                            lib = lib,
                            platform = .debian_platform,
                            config = .package_config))
        add_inst <<- append(add_inst,
                            .get_lib_install_instructions(
                              lib = lib,
                              version = version,
                              config = .package_config))

        return(invisible())
      },
      lib = names(ext_soft),
      version = as.character(ext_soft))


      #NOTE: The following is the "old" way to do it. Getting sf to work only requires a more current version gdal, while all other dependencies can be installed from APT
    } else if (!image_name == "rocker/geospatial") {
      futile.logger::flog.info(
        "The dependent package simple features for R requires current versions from gdal, geos and proj that may not be available by standard apt-get.\nWe recommend using the base image rocker/geospatial."
      )
      futile.logger::flog.info("Docker will try to install GDAL 2.1.3 from source")

      add_apt <- append(add_apt, c("wget", "make"))
      add_inst <- append(add_inst, Workdir("/tmp/gdal"))
      add_inst <-
        append(add_inst, Run_shell(
          c(
            "wget http://download.osgeo.org/gdal/2.1.3/gdal-2.1.3.tar.gz",
            "tar zxf gdal-2.1.3.tar.gz",
            "cd gdal-2.1.3",
            "./configure",
            "make",
            "make install",
            "ldconfig",
            "rm -r /tmp/gdal"
          )
        ))
    }
  }
}
