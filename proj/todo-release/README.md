# How to release R package rddapp 

1. edit files
2. update Version and Date in DESCRIPTION
3. update volume, number and version in CITATION
4. update NEWS.md
5. run checks
	- local Windows 10 install: check_package.R
	- local OS X install: check_package.R
	- ubuntu 14.04: Travis CI 
	- win-builder: check_win.R
	- reverse dependencies: check_revdep.R & check.R
6. update cran-comments.md
7. submit package: submit_package.R
