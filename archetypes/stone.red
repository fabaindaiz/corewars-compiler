;redcode-94b
;name stone (hand-written reference)
;assert 1
        SPL.B  #0,     #0
top     ADD.AB #3044,  bomb
        MOV.I  bomb,   @bomb
        JMP    top
bomb    DAT    #0,     #0
