;redcode-94b
;name impring (hand-written reference)
;assert 1
step    EQU    2667
        SPL.B  $2,      #0
        SPL.B  $1,      #0
        JMP.B  <vec,    #0
        JMP.B  imp+2*step
        JMP.B  imp+step
        JMP.B  imp
vec     DAT.F  #0,      #0
imp     MOV.I  #0,      step
