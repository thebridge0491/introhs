# Auxiliary targets Makefile script.
.POSIX:
help:

#MAKE = make # (GNU make variants: make (Linux) gmake (FreeBSD)

STACK ?= stack --resolver lts-18.10

parent = introhs
version = 0.1.0
SUBDIRS = common foreignc api app

.PHONY: help clobber uninstall-snapshot uninstall-user install-snapshot install-user
help: ## help
	@echo "Usage: $(MAKE) -f Makefile-aux.mk [SUBDIRS="$(SUBDIRS)"] [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile-aux.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile-aux.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clobber: $(SUBDIRS) ## clobber build artifacts
	-for dirX in $^ ; do (cd $$dirX ; $(MAKE) -f Makefile-aux.mk $@) ; done
	-rm -fr core* *~ .*~ *.log .coverage *.gcno *.gcda *.tix
uninstall-snapshot install-snapshot: $(SUBDIRS) ## [un]install to snapshot-pkg-db
	-@for dirX in $^ ; do \
		proj=`sed -ne 's|^name:[ ]*\(.*\)|\1|p' $$dirX/*.cabal` ; \
		version=`sed -ne 's|^version:[ ]*\(.*\)|\1|p' $$dirX/*.cabal` ; \
		if [ "uninstall-snapshot" = "$@" ] ; then \
			ghc-pkg unregister --package-db=`$(STACK) path --snapshot-pkg-db` $$proj-$$version || true ; \
		else $(STACK) exec ghc-pkg -- describe $$proj-$$version | \
				ghc-pkg register --package-db=`$(STACK) path --snapshot-pkg-db` - ; fi \
	done
	-ghc-pkg list --user --package-db=`$(STACK) path --snapshot-pkg-db`
uninstall-user install-user: $(SUBDIRS) ## [un]install to user HOME ghc package db
	-@for dirX in $^ ; do \
		proj=`sed -ne 's|^name:[ ]*\(.*\)|\1|p' $$dirX/*.cabal` ; \
		version=`sed -ne 's|^version:[ ]*\(.*\)|\1|p' $$dirX/*.cabal` ; \
		if [ "uninstall-user" = "$@" ] ; then \
			ghc-pkg unregister --user $$proj-$$version || true ; \
		else $(STACK) exec ghc-pkg -- describe $$proj-$$version | \
				ghc-pkg register --user - ; fi \
	done
	-ghc-pkg list --user

#----------------------------------------
FMTS ?= tar.gz,zip
distdir = $(parent)-$(version)

build/$(distdir) : 
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)

.PHONY: dist
dist | build/$(distdir): $(SUBDIRS)
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			7z) echo "### build/$(distdir).7z ###" ; \
				rm -f build/$(distdir).7z ; \
				(cd build ; 7za a -t7z -mx=9 $(distdir).7z $(distdir)) ;; \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.zst$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -h -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
