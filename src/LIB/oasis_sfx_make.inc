#
# Include file for OASIS3-MCT Makefile for use in SURFEX
# fortran and mpi implementation should be passed outside
# netcdf too with $(INC_NETCDF) and $(LIB_NETCDF) variables
#
###############################################################################
#
# CHAN	: communication technique used in OASIS3 (MPI1/MPI2)
CHAN            = MPI1
#
# Paths for libraries, object files and binaries
#
# COUPLE	: path for oasis3-mct main directory
COUPLE          ?= $(DIR_OASIS)
#
# ARCHDIR       : directory created when compiling
ARCHDIR         ?= $(COUPLE)/SFX
#
# MPI library
MPIDIR           ?=
MPIBIN           ?=
MPI_INCLUDE      ?=
MPILIB           ?=
#
# Compiling and other commands
MAKE        ?= $(MAKE)
F           ?= $(F90)
f90         ?= $(F)
f           ?= $(F)
LD          ?= $(F)
AR          ?= ar
ARFLAGS     ?= -ruv
#
# CPP keys and compiler options
# 
CPPDEF    ?= -Duse_netCDF -Duse_comm_$(CHAN) -D__VERBOSE -DTREAT_OVERLAY
# 
F90FLAGS_1  ?= $(OPT)
f90FLAGS_1  ?= $(F90FLAGS_1)
FFLAGS_1    ?= $(F90FLAGS_1)
fFLAGS_1    ?= $(F90FLAGS_1)
CCFLAGS_1   ?=
#
###################
#
# Additional definitions that should not be changed
#
FLIBS           = $(LIB_NETCDF)
# BINDIR        : directory for executables
BINDIR          = $(ARCHDIR)/bin
# LIBBUILD      : contains a directory for each library
LIBBUILD        = $(ARCHDIR)/build/lib
# INCPSMILE     : includes all *o and *mod for each library
INCPSMILE       = -I$(LIBBUILD)/psmile.$(CHAN) -I$(LIBBUILD)/scrip  -I$(LIBBUILD)/mct 

F90FLAGS  = $(F90FLAGS_1) $(INCPSMILE) $(CPPDEF) $(INC_NETCDF) $(LIB_NETCDF) $(LDFLAGS)
f90FLAGS  = $(F90_FLAGS)
FFLAGS    = $(FFLAGS_1) $(INCPSMILE) $(CPPDEF) $(INC_NETCDF) $(LIB_NETCDF) $(LDFLAGS)
fFLAGS    = $(FFLAGS)
CCFLAGS   = $(CCFLAGS_1) $(INCPSMILE) $(CPPDEF) $(INC_NETCDF) $(LIB_NETCDF) $(LDFLAGS)
#
#############################################################################
