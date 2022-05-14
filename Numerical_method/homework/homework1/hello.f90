PROGRAM add_numbers
implicit none
REAL :: a, b, av1, av2, c
PRINT *, 'Input 2 numbers:'
READ (*,*) a, b
    av1 = (a + b)/2; av2 = (a*b)**0.5
PRINT *, 'Arithmetic mean: ', av1
PRINT *, 'Geometric mean: ', av2
READ (*,*) c 
END