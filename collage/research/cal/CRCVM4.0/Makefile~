#==== MAKEFILE FOR GEM_THREE_BODY =============================================
#   make        ... COMPILE
#   make all    ... MAKE STAND-ALONE FORTRAN FILE
#   make run    ... EXECUTE
#   make clean  ... REMOVE OBJECT FILES
#   make tar.gz ... MAKE COMPRESSED FILE
#------------------------------------------------------------------------------
TARGET= CRCVM4.0
TAR=    CRCVM4_0.tar
TGZ=    CRCVM4_0.tar.gz

#------------------------------------------------------------------------------
# COMPILER
#CMP=  g95
CMP=  ifort
#CMPFLG=
CMPFLG= -i-dynamic -mcmodel=medium

#------------------------------------------------------------------------------
# SOURCE FILES
SRCM= MAIN.f
SRCL= READ.f TRAJECTORY.f SOLVE.f SET_NORM.f SET_KINE.f \
      SET_CENT.f SET_CENP.f SET_SPOR.f SET_SPOP.f SET_TENS.f \
      SET_COUL.f SET_THRE.f SET_PSEU.f \
			EIGENH.f POTENTIAL.f BASEMAKE.f SYSTEM.f TRANSFORM.f ANGLE.f \
      FUNCTION.f CLEB.f SIXJ.f NINJ.f DIAG_DSYM.f DIAG_CSYM.f FACTORIAL.f

SRC=  $(SRCM) $(SRCL)

# OBJECT FILES
OBJM= MAIN.o
OBJL= READ.o TRAJECTORY.o SOLVE.o SET_NORM.o SET_KINE.o \
      SET_CENT.o SET_CENP.o SET_SPOR.o SET_SPOP.o SET_TENS.o \
      SET_COUL.o SET_THRE.o SET_PSEU.o \
			EIGENH.o POTENTIAL.o BASEMAKE.o SYSTEM.o TRANSFORM.o ANGLE.o \
      FUNCTION.o CLEB.o SIXJ.o NINJ.o DIAG_DSYM.o DIAG_CSYM.o FACTORIAL.o

OBJ=  $(OBJM) $(OBJL)

#------------------------------------------------------------------------------
$(TARGET): $(OBJ)
	$(CMP) $(CMPFLG) $(OBJ) -o $(TARGET)
%.o   : %.f
	$(CMP) $(CMPFLG) -c $<
obj   :
	$(CMP) $(SRC) -c
	$(CMP) $(CMPFLG) $(OBJ) -o $(TARGET)
run   :
	./$(TARGET)
clean :
	rm -f *.o *.mod $(TARGET)
