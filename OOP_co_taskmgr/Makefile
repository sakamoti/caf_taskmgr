.PHONY. : clean
FC=nagfor

ifeq ($(FC),nagfor)
  FCFLAGS=-coarray=cosmp -g -C=all -colour -nihongo #-thread_safe #-unsharedrts
else ifeq ($(FC),gfortran)
  FCFLAGS=-g -fcoarray=lib -fcheck=all
else ifeq ($(FC),ifort)
  FCFLAGS=-g -check -coarray=shared -coarray-num-images=6
endif

%.o: %.f90
	$(FC) -c -o $@ $(INCLUDE) $(FCFLAGS) $< 

all : main
main: test_m_coarray_taskmgr.o m_coarray_task_manager.o
	$(FC) -o $@ $(INCLUDE) $(FCFLAGS) $^ $(LIBS) 
test_m_coarray_taskmgr.o:m_coarray_task_manager.o

clean:
	$(RM) *.o *.mod main
