# caf_taskmgr
coarray task manager sample (simplified edition)

- This is a task manager module using coarray, created for practice.
- The source code was created in a hurry, so it is not suitable to be used as a library. 
- If you wish to use it, please rewrite the module as appropriate for your own problem.

# Usage
|compiler | version |
|---------|---------|
|gfortran | GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) |
|ifort    | 2021.5.0 |

`cd sample && make && make run && ./co_task_test_ifort`

# Warning
"OOP_co_taskmgr" folder contain sources using *class*.  These sources
can be comiled with nagfor(7.1) and ifort(2021.6.0). Though compilation and linking are done,
*ifort* can't run correctry. Only nagfor works as I expect. 
