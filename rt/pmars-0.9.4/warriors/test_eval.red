;some assertions to test the maths parser of pmars (found in eval.c)
;
;assert 1
;assert 2 == 2 ;basic sanity
;assert 123456789012345678901 ;should yield warning because number has more than 20 digits 
;assert 0-2*2+11 == 7
;assert 2 + -1 == 1
;assert 2+(2+2*(2+2*3-1)-1) == 17 
;assert 1 && 3 -2*1 - 1 || 1
;assert !(1 && 3-2*1-1) 
;assert 1 && 2 || 1
;assert (a = 2)*a == 4 ;the weird register facility of the evaluator
mov 1 && 3 -2*1 - 0 || 1, 0-2*2+11
mov 0, 0-2/2+11
