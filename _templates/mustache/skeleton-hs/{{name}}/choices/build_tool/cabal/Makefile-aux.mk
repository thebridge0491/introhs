# Auxiliary targets Makefile script.
.POSIX:
help:

#MAKE = make # (GNU make variants: make (Linux) gmake (FreeBSD)

proj = $(shell sed -ne 's|^name:[ ]*\(.*\)|\1|p' *.cabal)
version = $(shell sed -ne 's|^version:[ ]*\(.*\)|\1|p' *.cabal)

.PHONY: help clobber uninstall-user install-user
help: ## help
	@echo "Usage: $(MAKE) -f Makefile-aux.mk [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile-aux.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile-aux.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clobber: ## clobber build artifacts
	-cabal clean ; cabal sandbox delete
	-rm -fr core* *~ .*~ *.log .coverage *.gcno *.gcda *.tix
uninstall-user install-user: ## [un]install to user HOME ghc package db
	-@if [ "uninstall-user" = "$@" ] ; then \
		ghc-pkg unregister --user $(proj)-$(version) || true ; \
	else cabal sandbox hc-pkg -- describe $(proj)-$(version) | \
			ghc-pkg register --user - ; fi
	-ghc-pkg list --user

#----------------------------------------
FMTS ?= tar.gz
distdir = $(proj)-$(version)

.PHONY: dist
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
