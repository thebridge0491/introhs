# FFI auxiliary makefile script
ffi_libdir = $(shell $(PKG_CONFIG) --variable=libdir intro_c-practice || echo .)
ffi_incdir = $(shell $(PKG_CONFIG) --variable=includedir intro_c-practice || echo .)
LD_LIBRARY_PATH := $(LD_LIBRARY_PATH):$(ffi_libdir)
export LD_LIBRARY_PATH

HSLDFLAGS := $(HSLDFLAGS) -optl-Wl,-rpath,$(ffi_libdir) -L$(ffi_libdir)
ldlibs_hs := $(ldlibs_hs) -lintro_c-practice
ldlibs_test_hs := $(ldlibs_test_hs) -l`$(GHC_PKG) --global --user --simple-output field $(proj) hs-libraries | head -n1` -lintro_c-practice
