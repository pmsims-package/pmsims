# Usage: make help

R = R
RSCRIPT = Rscript

# Helper: run one-liners quietly
RQ = $(RSCRIPT) -e

help:
	@printf "Targets:\n"
	@printf "  help        - Show this help\n"
	@printf "  deps        - Install package dependencies\n"
	@printf "  document    - Generate Rd/NAMESPACE via roxygen2\n"
	@printf "  build       - R CMD build\n"
	@printf "  install     - R CMD INSTALL from source dir\n"
	@printf "  test        - Run testthat tests\n"
	@printf "  check       - R CMD check --as-cran (builds first)\n"
	@printf "  vignettes   - Build vignettes\n"
	@printf "  site        - Build pkgdown site\n"
	@printf "  format      - Style code with styler\n"
	@printf "  lint        - Lint with lintr\n"
	@printf "  clean       - Remove build/check artefacts\n"
	@printf "  distclean   - clean + remove pkgdown output\n"
	@printf "  reinstall   - document then install from source\n"
	@printf "  dev         - document then test\n"

deps:
	@$(RQ) "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes'); remotes::install_deps(dependencies=TRUE)"

document:
	@$(RQ) "if (!requireNamespace('roxygen2', quietly=TRUE)) install.packages('roxygen2'); roxygen2::roxygenise()"

build:
	@$(R) CMD build .

install:
	@$(R) CMD INSTALL --preclean .

test:
	@$(RQ) "if (!requireNamespace('testthat', quietly=TRUE)) install.packages('testthat'); testthat::test_dir('tests/testthat', reporter='summary')"

# Build first, then compute tarball name in the shell (portable; no $(shell))
check: build
	@PKG=`$(RSCRIPT) -e "cat(read.dcf('DESCRIPTION')[1,'Package'])"`; \
	VER=`$(RSCRIPT) -e "cat(read.dcf('DESCRIPTION')[1,'Version'])"`; \
	TARBALL="$$PKG"_$$VER".tar.gz"; \
	$(R) CMD check --as-cran "$$TARBALL"

vignettes:
	@$(RQ) "tools::buildVignettes(dir='.')"

site:
	@$(RQ) "if (!requireNamespace('pkgdown', quietly=TRUE)) install.packages('pkgdown'); pkgdown::build_site()"

format:
	@$(RQ) "if (!requireNamespace('styler', quietly=TRUE)) install.packages('styler'); styler::style_pkg()"

lint:
	@$(RQ) "if (!requireNamespace('lintr', quietly=TRUE)) install.packages('lintr'); print(lintr::lint_package())"

clean:
	@rm -rf *.tar.gz
	@rm -rf *.Rcheck
	@rm -rf .Rhistory .RData

distclean: clean
	@rm -rf docs pkgdown

reinstall: document
	@$(R) CMD INSTALL --preclean .

dev: document
	@$(MAKE) test
