# makefile for local CHAPTER
SHELL=/bin/sh

SRC=
OBJ=
FUN=
HELPSGML= arcdist.sgml combine.rt.sgml fill.outside.border.sgml gbplot.sgml geoarea.sgml geocontour.fill.sgml geocontour.sgml geocurve.sgml geodefine.sgml geodezoom.sgml geoexpand.sgml geogrid.sgml geoidentify.sgml geoinside.sgml geolegend.sgml geolines.sgml geolocator.sgml geopar.sgml geoplot.sgml geopoints.sgml geopolygon.sgml geosubplot.sgml geosymbols.sgml geotext.sgml geoworld.sgml geozoom.sgml grid.sgml invProj.sgml orthproj.sgml plvar.sgml pointkriging.sgml Proj.sgml reitaplott.sgml s2pre.sgml selpos.sgml variofit.sgml variogram.sgml

# Use LOCAL_CFLAGS to add arguments for the C compiler
LOCAL_CFLAGS=
# Use LOCAL_CXXFLAGS to add arguments for the C++ compiler
LOCAL_CXXFLAGS=
# Use LOCAL_FFLAGS to add arguments for the FORTRAN compiler
LOCAL_FFLAGS=

# Use LOCAL_LIBS to add arguments or additional libraries to the linker
# LOCAL_LIBS="-lf2c"
LOCAL_LIBS=

include $(SHOME)/library/S_FLAGS

all: install.funs S.so install.help

install.funs: $(FUN)
	@if [ "X$(FUN)" != "X" ] ; then \
	cat $(FUN) | $(SHOME)/cmd/Splus ; \
	fi

install.help: $(HELPSGML)
	@if [ "X$(HELPSGML)" != "X" ] ; then \
	$(SHOME)/cmd/Splus HINSTALL ./.Data $(HELPSGML) ; \
	$(SHOME)/cmd/Splus BUILD_JHELP ; \
	fi

S.so: $(OBJ) 
	@if [ "X$(OBJ)" != "X" ]; then \
	$(SHOME)/cmd/Splus LIBRARY S.so $(OBJ) $(LOCAL_LIBS) ; \
	fi

dump:
	@if test -d ./.Data; then Splus dumpChapter $(SRC); fi

boot:
	@if test -s all.Sdata; \
	  then (BOOTING_S="TRUE" export BOOTING_S; echo "terminate(should have been booting S)"| $(SHOME)/cmd/Splus); \
	fi

clean:
	-rm $(OBJ)

