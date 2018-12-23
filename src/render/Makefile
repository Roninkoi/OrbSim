BITS      := 64
X11LIBDIR := -L/usr/lib/x86_64-linux-gnu
F90GLUTLIB := -lglut -lGL -lGLU
X11LIB     := -lXext -lX11 -lm

ifndef COMPILER
  COMPILER := gfortran
endif
FC=${COMPILER}
ifeq (${COMPILER},gfortran)
#  gfortran
  FFLAGS    := $(DEBUG) -DOPENGL -fno-range-check
  LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR}
  LIBS      := ${F90GLUTLIB} ${X11LIB} -lpthread -ldl -lgfortran
endif

# GLUT      := openglut
GLUT      := freeglut
# GLUT      := glut

LIBRARIES := ${OGLLIBDIR} ${X11LIBDIR} -L/usr/local/lib
MODULES    = OpenGL_${GLUT}.o OpenGL_glu.o OpenGL_gl.o gnufor2.o

all: render

%.o: %.f90
	${FC} ${FFLAGS} -c $<

render: %: %.f90 ${MODULES} force
	${FC} ${FFLAGS} -c $<
	${FC} $@.o ${MODULES} ${LIBRARIES} ${LIBS} -o $@

OpenGL%.mod: OpenGL%.f90
	${FC} ${FFLAGS} -c $<

force:

OpenGL_${GLUT}.o OpenGL_glu.o: OpenGL_gl.o



