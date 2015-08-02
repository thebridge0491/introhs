# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

FMTS ?= tar.gz
distdir = $(proj)-$(version)

$(outputdir)/libHS$(pkg_id)-ghc$(ghc_ver).$(shlibext) :
	depns_pkgids="`for pkgX in $(depns) ; do echo -n -package-id= ; $(GHC_PKG) --global --user --simple-output field $$pkgX id | head -n1 ; done`" ; \
	$(LINK.hs) $(lang_options) -fPIC -shared -dynamic '-dynload sysdep' \
		$(redirect_opts) -this-package-key $(pkg_id) -no-auto-link-packages \
		-no-user-package-db $$depns_pkgids $^ -o $@ $(ldlibs_hs)

.PHONY: help test clean uninstall uninstall-user install install-user dist sdist doc haddock lint hlint report
help: ## help
	@echo "##### subproject: $(proj) #####"
	@echo "Usage: $(MAKE) [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
test: testCompile ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(outputdir) \
		$(outputdir)/TsMain-$(proj)/TsMain-$(proj) $(TOPTS)
clean: ## clean build artifacts
	-rm -fr core* *~ .*~ *.log .coverage *.gcno *.gcda *.tix $(outputdir)/*
uninstall install: ## [un]install artifacts to dist inplace db
	-@if [ "uninstall" = "$@" ] ; then \
		$(GHC_PKG) unregister --package-db=$(pkgdb_inplace) $(proj)-$(version) || true ; \
	else cabal install --package-db=$(pkgdb_inplace) ; fi
	-$(GHC_PKG) list --user --package-db=$(pkgdb_inplace)
uninstall-user install-user: ## [un]install artifacts to user HOME ghc pkg db
	-@if [ "uninstall-user" = "$@" ] ; then \
		$(GHC_PKG) unregister --user $(proj)-$(version) || true ; \
		rm -ir `find $(PREFIX) -name '*$(proj)-$(version)*'` ; \
	else cabal install --user ; fi
	-$(GHC_PKG) list --user --package-db=$(pkgdb_inplace)
dist: ## [FMTS="tar.gz"] archive source code
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)
	
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -L -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
sdist: ## create source code distribution [ARGS=""]
	-cabal sdist $(ARGS)
doc haddock: ## generate documentation [ARGS=""]
	-rm -fr dist/doc/html/$(proj)/*
	-haddock --html --optghc="-XFlexibleContexts" -o dist/doc/html/$(proj) \
		$(ARGS) $(src_hs)
lint hlint: ## lint check [ARGS=""]
	-hlint src --report=dist/lint_rpt.html $(ARGS)
report: ## report code coverage [ARGS=""]
	-mkdir -p dist/cov
	-hpc report --verbosity=2 --per-module --decl-list --hpcdir=$(outputdir) \
		--hpcdir=$(outputdir)/$(pkg_id) --hpcdir=$(outputdir_test) $(ARGS) \
		`find . -name '*.tix'` | tee dist/cov/cov_rpt.txt 
