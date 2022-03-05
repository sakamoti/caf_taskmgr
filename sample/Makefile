.PHONY : clean run
FC=gfortran
FCFLAGS=-g -fcoarray=lib -fcheck=all
FCAFLIB=`pkg-config --libs --cflags caf-openmpi`

main: co_task_test

co_task_test : M_coarray_task_manager.f90 co_task_testmain.f90 
	$(FC) $(FCFLAGS) -o $@_gfort $^ $(FCAFLIB)
	ifort -coarray=shared -coarray-num-images=12 -o $@_ifort $^ 

run:
	cafrun -n 12 --use-hwthread-cpus ./co_task_test_gfort

clean:
	$(RM) *.o *.mod co_task_test_gfort co_task_test_ifort
