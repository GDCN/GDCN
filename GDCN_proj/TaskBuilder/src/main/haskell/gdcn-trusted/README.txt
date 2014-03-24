To install gdcn-trusted, you first need to create a fresh package database with:

ghc-pkg init <db-dir>

The directory have to end with ".conf.d" and most not exist prior.
Then to build and install the package, run:

runhaskell Setup <install-dir> <db-dir>

Where install-dir is the directory the library files will reside and db-dir the
database created earlier.


When you want to include this package, you need to export the environment
variable GHC_PACKAGE_PATH with value "<db-dir>:" (; for Windows).
NOTE: This variable MUST be unset when building and installing the package!
