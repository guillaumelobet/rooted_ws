install_julia_packages <- function() {

  require(JuliaCall)

  julia_install_package_if_needed("Distributed")
  julia_install_package_if_needed("StaticArrays")
  julia_install_package_if_needed("Random")
  julia_install_package_if_needed("Printf")
  julia_install_package_if_needed("TypedTables")
  julia_install_package_if_needed("Parsers")
  julia_install_package_if_needed("Parameters")
  julia_install_package_if_needed("LazilyInitializedFields")
  julia_install_package_if_needed("LightXML")
  julia_install_package_if_needed("Distributions")
  julia_install_package_if_needed("Dates")
}

