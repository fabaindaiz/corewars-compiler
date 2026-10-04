;redcode-94b
;name dwarf (hand-written reference)
;assert 1
top     ADD.AB #4, bomb
        MOV.I  bomb, @bomb
        JMP    top
bomb    DAT    #0, #0
