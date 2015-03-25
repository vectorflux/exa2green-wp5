/******************************************************************************

  KPP - The Kinetic PreProcessor
        Builds simulation code for chemical kinetic systems

  Copyright (C) 1995-1996 Valeriu Damian and Adrian Sandu
  Copyright (C) 1997-2005 Adrian Sandu

  KPP is free software; you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation (http://www.gnu.org/copyleft/gpl.html); either version 2 of the
  License, or (at your option) any later version.

  KPP is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, consult http://www.gnu.org/copyleft/gpl.html or
  write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307,  USA.

  Adrian Sandu
  Computer Science Department
  Virginia Polytechnic Institute and State University
  Blacksburg, VA 24060
  E-mail: sandu@cs.vt.edu

******************************************************************************/

/******************************************************************************
  
  Enable KPP to create source code for CUDA C.
  
  (C) Christos Kannas, Nov. 2013
  The Cyprus Institute
  E-mail: c.kannas@cyi.ac.cy
  
******************************************************************************/

#include "gdata.h"
#include "code.h"
#include <string.h>

#define MAX_LINE  120
#define LINE_LENGTH 70

extern void C_WriteElm();
extern void C_WriteSymbol();
extern void C_WriteAssign();
extern void C_WriteComment();
extern void C_DeclareConstant();
extern void C_Declare();
extern void C_ExternDeclare();
extern void C_GlobalDeclare();
extern void C_InitDeclare();
extern void C_FunctionStart();
extern void C_FunctionPrototipe();
extern void C_FunctionBegin();
extern void C_FunctionEnd();

//int fncPrototipe = 0;

/* CUDA specific variable types */
enum CUDA_base_types { SIZE, DEVICE };

char *CUDA_types[] = {"size_t", /* Size of variables */
                      "cudaDeviceProp", /* CUDA device properties */
};

/* CUDA C Function and Variable Qualifiers */
char *CUDA_C_function_qualifiers[] = {"__device__",  /* Executed on the device, 
                                                        Callable from the device only. */
                                      "__global__",  /* Declares a function as being a kernel. 
                                                        Executed on the device, 
                                                        Callable from the host, 
                                                        Callable from the device for devices of compute capability 3.x. */
                                      "__host__"    /* Executed on the host, 
                                                       Callable from the host only. */
};
                                          
char *CUDA_C_variable_qualifiers[] = {"__device__",  /* A variable that resides on the device.
                                                        Resides in global memory space,
                                                        Has the lifetime of an application,
                                                        Is accessible from all the threads within the grid 
                                                        and from the host through the runtime library. */
                                      "__constant__",  /* Optionally used together with __device__.
                                                          Resides in constant memory space,
                                                          Has the lifetime of an application,
                                                          Is accessible from all the threads within the grid 
                                                          and from the host through the runtime library. */
                                      "__shared__",  /* Optionally used together with __device__.
                                                        Resides in the shared memory space of a thread block,
                                                        Has the lifetime of the block,
                                                        Is only accessible from all the threads within the block. */
};

/*********************************************************************/

void CUDA_C_Declare(int v, int vq){
    bprintf("%s ", CUDA_C_variable_qualifiers[vq] );
    C_Declare(v);
}

void CUDA_C_FunctionPrototipe()
{
    
}

void CUDA_C_FunctionStart()
{
    
}

/* Begin a function in CUDA C */
void CUDA_C_FunctionBegin(int q, int f, ...)
{
    Va_list args;
    int i;
    int vars[20];
    char * name;
    int narg;

    name = varTable[ f ]->name;
    narg = varTable[ f ]->maxi;

    Va_start( args, f );  
    for( i = 0; i < narg; i++ ) 
        vars[i] = va_arg( args, int );
    va_end( args );

    CommentFncBegin( f, vars );
    bprintf("%s ", CUDA_C_function_qualifiers[q]);
    C_FunctionStart( f, vars );
    bprintf("\n");
    bprintf("{\n");

    FlushBuf();

    MapFunctionComment( f, vars );
}

void CUDA_C_FunctionEnd()
{
    
}

/*********************************************************************/

void Use_CUDA_C()
{
    WriteElm          = C_WriteElm;
    WriteSymbol       = C_WriteSymbol;
    WriteAssign       = C_WriteAssign;
    WriteComment      = C_WriteComment;
  
    DeclareConstant   = C_DeclareConstant;
    Declare           = C_Declare;
    ExternDeclare     = C_ExternDeclare;
    GlobalDeclare     = C_GlobalDeclare;
    InitDeclare       = C_InitDeclare;

    FunctionStart     = C_FunctionStart;
    FunctionPrototipe = C_FunctionPrototipe;
    FunctionBegin     = C_FunctionBegin;
    FunctionEnd       = C_FunctionEnd;
  
    OpenFile( &param_headerFile, rootFileName, "_Parameters.h", "Parameter Header File" );
    OpenFile( &initFile, rootFileName, "_Initialize.c", "Initialization File" );
    OpenFile( &driverFile, rootFileName, "_Main.c", "Main Program File" );
    OpenFile( &integratorFile, rootFileName, "_Integrator.c", 
                    "Numerical Integrator (Time-Stepping) File" );
    OpenFile( &integrateFortranFile, rootFileName, "_Integrator.f90", 
                    "Numerical Integrator (Time-Stepping) File" );
    OpenFile( &linalgFile, rootFileName, "_LinearAlgebra.c", 
                    "Linear Algebra Data and Routines File" );
    OpenFile( &functionFile, rootFileName, "_Function.cu", 
                    "The ODE Function of Chemical Model File" );
    OpenFile( &jacobianFile, rootFileName, "_Jacobian.c", 
                    "The ODE Jacobian of Chemical Model File" );
    OpenFile( &rateFile, rootFileName, "_Rates.c", 
                    "The Reaction Rates File" );
    if ( useStochastic )
    OpenFile( &stochasticFile, rootFileName, "_Stochastic.c", 
                    "The Stochastic Chemical Model File" );
    if ( useStoicmat ) {
    OpenFile( &stoichiomFile, rootFileName, "_Stoichiom.c", 
                    "The Stoichiometric Chemical Model File" );
    OpenFile( &sparse_stoicmFile, rootFileName, "_StoichiomSP.c", 
                    "Sparse Stoichiometric Data Structures File" );
    }           
    OpenFile( &utilFile, rootFileName, "_Util.c", 
                    "Auxiliary Routines File" );
    OpenFile( &sparse_dataFile, rootFileName, "_Sparse.h", "Sparse Data Header File" );
    OpenFile( &global_dataFile, rootFileName, "_Global.h", "Global Data Header File" );
    if ( useJacSparse ) {
        OpenFile( &sparse_jacFile, rootFileName, "_JacobianSP.c",
        "Sparse Jacobian Data Structures File" );  
    }
    if ( useHessian ) {
        OpenFile( &hessianFile, rootFileName, "_Hessian.c", "Hessian File" );
        OpenFile( &sparse_hessFile, rootFileName, "_HessianSP.c",
            "Sparse Hessian Data Structures File" );
    }   
    OpenFile( &mapFile, rootFileName, ".map", 
                    "Map File with Human-Readable Information" );
    OpenFile( &monitorFile, rootFileName, "_Monitor.c", 
                    "Utility Data Initialization" );
}
