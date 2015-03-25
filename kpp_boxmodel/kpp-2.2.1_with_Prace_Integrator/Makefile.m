# -*- makefile -*-
##############################################################################
#
#  KPP - The Kinetic PreProcessor
#        Builds simulation code for chemical kinetic systems
#
#  Copyright (C) 1995-1996 Valeriu Damian and Adrian Sandu
#  Copyright (C) 1997-2004 Adrian Sandu
#
#  KPP is free software; you can redistribute it and/or modify it under the
#  terms of the GNU General Public License as published by the Free Software
#  Foundation (http://www.gnu.org/copyleft/gpl.html); either version 2 of the
#  License, or (at your option) any later version.
#
#  KPP is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
#  details.
#
#  You should have received a copy of the GNU General Public License along
##  with this program; if not, consult http://www.gnu.org/copyleft/gpl.html or
#  write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#  Boston, MA  02111-1307,  USA.
#
#  Adrian Sandu
#  Computer Science Department
#  Virginia Polytechnic Institute and State University
#  Blacksburg, VA 24060
#  E-mail: sandu@cs.vt.edu
#
##############################################################################
export

SYSTEM := $(shell uname)

include Makefile.defs.$(SYSTEM)

all: setup kpp

setup:
	@./cflags.guess $(CC)

kpp:
	@cd src;$(MAKE);cd ..

clean:
	@cd src;$(MAKE) clean;cd ..
	@rm -f *~ */*~

distclean: clean
	@rm -f bin/*
	@rm -f ../../../bin/kpp.exe

install: all
	@cp -f bin/kpp ../../../bin/kpp.exe
