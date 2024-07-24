#######################################################################
# Makefile for 12C=3*Alpha ('03) Ver.5.3
#   make       ... compile 
#   make all   ... make stand-alone fortran file
#   make run   ... execute 
#   make clean ... cleaning (rm *.obj)
#	make tar.gz... make compressed file (tar+gzip)
#######################################################################
TARGET= 5Li
INC= LI_INC.f
INP= INPUT
ALL=	$(HEAD)$(VER).f
TAR= 5Li.tar
TGZ= 5Li.tar.gz
##########################################
# Compiler & optimization
G77=  g77
#FFLAGS= -g -C			# for debug
# For Optimization
FFLAGS	=
#FFLAGS	= -O3			# for hyper
#FFLAGS	= -O4 			# for nucl
#FFLAGS	= -O5			# for miho

# For debug & Optimization
#FFLAGS	= -O3 -g 
#FFLAGS	= -O4 -g -cg92	# for nucl
#FFLAGS	= -O5 -g -C		# for miho

#DEBUG	= dbx			# hadron, hyper
DEBUG	= gdb			# nucl, n107b

#########################################
# Sources
SRCM= MAIN.f
SRCL= MATELE.f FUNC.f EIGENH.f DIAGO.f BESSEL.f POTENTIAL.f

SRC=	$(SRCM) $(SRCL) 

# objects
OBJM= MAIN.o
OBJL= MATELE.o FUNC.o EIGENH.o DIAGO.o BESSEL.o POTENTIAL.o

OBJ=	$(OBJM) $(OBJL) 

OUT=	O_*

# files to be saved
SAVE= $(SET) $(SRC) $(INPD) $(INC)

# remote host
DIR0=	Study/Nuclei/6He/
DIR1=	Src05.3/
DIR2=	Src05.3/
HOST1=	nucl
HOST2=	nova
HOST3=	halo
HOST4=	hadron
HOST5=	hyper

########################################################################
$(TARGET): $(OBJ)
	$(G77) $(FFLAGS) $(OBJ) -o $(TARGET)
#.f.o: .f
#	$(G77) $(FFLAGS) -c $<
.f.o: 
	$(G77) $(FFLAGS) -c $<

obj	: 
	$(G77) $(FLAGS) $(SRC) -c 
	$(G77) $(FLAGS) $(OBJ) -o $(TARGET)
link:
	$(G77) $(FLAGS) $(OBJ) -o $(TARGET)
run	: ./$(TARGET)
	./$(TARGET) < $(INP)
bug	: ./$(TARGET)
	 $(DEBUG) ./$(TARGET)

all	: $(SRC)
	cat $(SRC) > $(ALL)
ftn	: 
	cat $(SRC) > $(ALL)
	ftnchek $(ALL)

# cleaning
clean	:
	rm -f *.o *.u *.c *.core  $(OUT) $(all) $(TARGET) *~ fort.*	
clsO	:
	rm $(OUT)

#  Save
tar	: $(SAVE)
	tar -cvf  $(TAR) $(SAVE)
tar.gz	: $(SAVE)
	tar -cvf - $(SAVE) | gzip > $(TGZ)

# Copy to remote host
cp	: 
	rcp $(TGZ) $(HOST1):$(DIR0)$(DIR1)
	rcp $(TGZ) $(HOST2):$(DIR0)$(DIR1)
	rcp $(TGZ) $(HOST3):$(DIR0)$(DIR2)
	rcp $(TGZ) $(HOST4):$(DIR0)$(DIR2)
	rcp $(TGZ) $(HOST5):$(DIR0)$(DIR2)

#lha: $(TARGET).lzh
#$(TARGET).lzh: $(SAVE)
#$	lha u $(TARGET).lzh $(SAVE)
#$	lha e $(TARGET).lzh $(SAVE)
