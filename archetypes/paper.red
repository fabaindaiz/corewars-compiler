;redcode-94b
;name paper (hand-written reference)
;assert 1
top     MOV.AB #7,     #7
copy    MOV.I  <top,   <dest
        JMN.B  copy,   top
        SPL.B  @dest,  #0
        ADD.AB #2365,  dest
        JMP    top
dest    DAT    #0,     #2000
