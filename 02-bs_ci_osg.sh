#!/bin/bash

set -e

Rscript --vanilla --default-packages=methods,utils,stats,graphics 02-bs_ci_osg.R $1 $2 $3
