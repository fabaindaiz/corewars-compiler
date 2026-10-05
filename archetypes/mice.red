;redcode-94b
;name mice (hand-written reference)
;assert CORESIZE==8000
; Copy by index, from the idea of Mice (Chip Wendell, 1986), with constants of its own: the counter
; sits before the code, the loop copies the seven cells after it through the counter, a process
; starts the copy, the target moves on.
count   DAT.F   #0, #0
entry   MOV.AB  #7, $count
more    MOV.I   @count, <target
        DJN.B   $more, $count
        SPL.B   @target, #0
        ADD.AB  #2903, $target
        JMZ.B   $entry, $count
target  DAT.F   #0, #1500
        END     entry
