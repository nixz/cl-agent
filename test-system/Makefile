##
## Makefile
##  
## Made by Shetty
## Login   <nikhilnewton.lite3d.com>
##
## Started on  Fri Mar 12 03:11:56 2010 Shetty
## Last update Fri Mar 12 03:12:28 2010 Shetty
##############################
# Complete this to make it ! #
##############################
NAME 	= env		# Name of executable file
SRC	= env.c	# List of *.c
INCL  	=		# List of *.h
################
# Optional add #
################
IPATH   = -I.           # path of include file
OBJOPT  = -O2 -Wall     # option for obj
EXEOPT  = -O2 -Wall     # option for exe (-lefence ...)
LPATH   = -L.           # path for librairies ... 

#####################
# Macro Definitions #
#####################
CC 	= g++
MAKE 	= make
SHELL	= /bin/sh
OBJS 	= $(SRC:.cpp=.o) 
RM 	= /bin/rm -f 	
COMP	= gzip -9v
UNCOMP	= gzip -df
STRIP	= strip

CFLAGS  = $(OBJOPT) $(IPATH)
LDFLAGS = $(EXEOPT) $(LPATH)

.SUFFIXES: .h.Z .c.Z .h.gz .c.gz .c.z .h.z 

##############################
# Basic Compile Instructions #
##############################

all:	$(NAME)
$(NAME): $(OBJS) $(SRC) $(INCL)  
	$(CC) $(OBJS) $(LDFLAGS) -o $(NAME) 
#	$(STRIP) ./$(NAME) # if you debug ,don't strip ...

depend:
	g++ $(IPATH) -MM $(SRC) 
clean:
	-$(RM) $(NAME) $(OBJS) *~
fclean:
	-$(RM) $(NAME)
comp: clean
	$(COMP) $(INCL) $(SRC)
ucomp: 
	$(UNCOMP) $(SRC) $(INCL)

.c.Z.c .h.Z.h .c.gz.c .h.gz.h .c.z.c .h.z.h :
	 -$(UNCOMP) $<

.c.o:
	$(CC) $(CFLAGS) -c $< 
################
# Dependencies #
################
