;redcode-94b
;name scanner (hand-written reference)
;assert 1
scan    ADD.AB #10, ptr
        JMZ.B  scan, @ptr
        MOV.I  bomb, @ptr
        JMP    scan
ptr     DAT    #0, #20
bomb    DAT    #0, #0
