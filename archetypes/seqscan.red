;redcode-94b
;name seqscan (hand-written reference)
;assert 1
scan    ADD.F  inc,   probe
probe   SNE.I  100,   104
        JMP    scan
        MOV.I  bomb,  @probe
        JMP    scan
inc     DAT    #8,    #8
bomb    DAT    #0,    #0
