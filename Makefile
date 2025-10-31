# Makefile for an R package
# Usage: `make help` for a list of targets

# ----- Config -----
R ?= R
RSCRIPT ?= Rscript
PKG    := $(shell $(RSCRIPT) -e "cat(read.dcf('DESCRIPTION')[1,'Package'])")
VER    := $(shell $(RSCRIPT) -e "cat(read.dcf('DESCRIPTION')[1,'Version'])")
TARBALL := $(PKG)_$(VER).tar.gz

# Run R code quietly and fail fast
R_Q := $(RSCRIPT) -e

# ----- Meta -----
.PHONY: help document build install check test vignettes site clean distclean deps format lint

help: ## Show this help
	@awk 'BEGIN{FS=":.*##"; printf "\nTargets:\n"} /^[a-zA-Z0-9_-]+:.*##/ {printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
	@echo ""

# ----- Core workflow -----
document: ## Generate Rd, NAMESPACE (roxygen2/devtools)
	$(R_Q) "devtools::document(quiet = TRUE)"

build: document ## Build source tarball (R CMD build)
	$(R) CMD build .

install: document ## Install package (from source directory)
	$(R) CMD INSTALL .

check: build ## R CMD check (CRAN-like)
	$(R) CMD check --as-cran $(TARBALL)

test: ## Run tests (testthat)
	$(R_Q) "devtools::test(stop_on_failure = TRUE)"

vignettes: ## Build vignettes
	$(R_Q) "devtools::build_vignettes()"

site: document ## Build pkgdown site (docs/)
	$(R_Q) "pkgdown::build_site(lazy = TRUE)"

# ----- Extras -----
deps: ## Install package dependencies (incl. Suggests)
	$(R_Q) "devtools::install_deps(dependencies = TRUE, upgrade = 'never')"

format: ## Style code with styler
	$(R_Q) "if (!requireNamespace('styler', quietly=TRUE)) install.packages('styler'); styler::style_pkg()"

lint: ## Lint package with lintr
	$(R_Q) "if (!requireNamespace('lintr', quietly=TRUE)) install.packages('lintr'); print(lintr::lint_package())"

clean: ## Remove build artefacts
	@rm -rf *.Rcheck
	@rm -f  *.tar.gz
	@rm -rf .Rproj.user
	@rm -rf inst/doc
	@rm -rf man/figures
	@rm -rf .devcontainer
	@rm -rf _pkgdown.yml~ _pkgdown .pkgdown

distclean: clean ## Also remove pkgdown output and cache
	@rm -rf docs
	@rm -rf pkgdown

# Convenience: build-and-install in one go
reinstall: ## Document, then reinstall from source dir
	$(MAKE) document
	$(R) CMD INSTALL --preclean .

# Quick dev loop: document + test
dev: ## Document and run tests
	$(MAKE) document
	$(MAKE) test
