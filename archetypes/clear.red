;redcode-94b
;name core-clear (hand-written reference)
;assert 1
ptr     DAT    #0, #4
top     MOV.I  bomb, >ptr
        JMP    top
bomb    DAT    #0, #0
        end    top
