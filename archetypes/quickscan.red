;redcode-94nop
;name quickscan (hand-written reference)
;assert CORESIZE==8000
step    EQU     400
gap     EQU     100
first   SEQ.I   first+1*step, first+1*step+gap
        JMP     hits+0
        SEQ.I   first+2*step, first+2*step+gap
        JMP     hits+2
        SEQ.I   first+3*step, first+3*step+gap
        JMP     hits+4
        SEQ.I   first+4*step, first+4*step+gap
        JMP     hits+6
        SEQ.I   first+5*step, first+5*step+gap
        JMP     hits+8
        SEQ.I   first+6*step, first+6*step+gap
        JMP     hits+10
        SEQ.I   first+7*step, first+7*step+gap
        JMP     hits+12
        SEQ.I   first+8*step, first+8*step+gap
        JMP     hits+14
        SEQ.I   first+9*step, first+9*step+gap
        JMP     hits+16
        SEQ.I   first+10*step, first+10*step+gap
        JMP     hits+18
        SEQ.I   first+11*step, first+11*step+gap
        JMP     hits+20
        SEQ.I   first+12*step, first+12*step+gap
        JMP     hits+22
        SEQ.I   first+13*step, first+13*step+gap
        JMP     hits+24
        SEQ.I   first+14*step, first+14*step+gap
        JMP     hits+26
        SEQ.I   first+15*step, first+15*step+gap
        JMP     hits+28
        SEQ.I   first+16*step, first+16*step+gap
        JMP     hits+30
        JMP     clear
hits    MOV.AB  #first+1*step-ptr, ptr
        JMP     found
        MOV.AB  #first+2*step-ptr, ptr
        JMP     found
        MOV.AB  #first+3*step-ptr, ptr
        JMP     found
        MOV.AB  #first+4*step-ptr, ptr
        JMP     found
        MOV.AB  #first+5*step-ptr, ptr
        JMP     found
        MOV.AB  #first+6*step-ptr, ptr
        JMP     found
        MOV.AB  #first+7*step-ptr, ptr
        JMP     found
        MOV.AB  #first+8*step-ptr, ptr
        JMP     found
        MOV.AB  #first+9*step-ptr, ptr
        JMP     found
        MOV.AB  #first+10*step-ptr, ptr
        JMP     found
        MOV.AB  #first+11*step-ptr, ptr
        JMP     found
        MOV.AB  #first+12*step-ptr, ptr
        JMP     found
        MOV.AB  #first+13*step-ptr, ptr
        JMP     found
        MOV.AB  #first+14*step-ptr, ptr
        JMP     found
        MOV.AB  #first+15*step-ptr, ptr
        JMP     found
        MOV.AB  #first+16*step-ptr, ptr
        JMP     found
found   MOV.I   bomb, @ptr
        ADD.AB  #gap, ptr
        MOV.I   bomb, @ptr
clear   MOV.I   bomb, >ptr
        JMP     clear
ptr     DAT.F   #0, #300
bomb    DAT.F   #0, #0
