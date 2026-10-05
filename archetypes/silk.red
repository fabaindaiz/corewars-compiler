;redcode-94b
;name silk (hand-written reference)
;assert CORESIZE==8000
; A Silk-style paper, from the idea of Silk (Juha Pohjalainen, 1994), with constants of its own:
; eight processes run the same two cells; SPL starts each at the copy, the MOV copies through the
; A-field postincrement (}) and the B-field one (>) of the SPL's own cell, one cell a process.
        SPL.B   $1, #0
        SPL.B   $1, #0
        SPL.B   $1, #0
silk    SPL.B   @0, }2731
        MOV.I   }silk, >silk
        MOV.I   $bomb, >2000
bomb    DAT.F   #0, #0
