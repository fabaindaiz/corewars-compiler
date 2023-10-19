/* pMARS -- a portable Memory Array Redcode Simulator
 * Copyright (C) 1993-1996 Albert Ma, Na'ndor Sieben, Stefan Strack and Mintardjo Wangsawidjaja
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
 * pos.c: RNG and positioning functions
 * $Id: pos.c,v 1.1.1.1 2000/08/20 13:29:42 iltzu Exp $
 */
#include "global.h"
#include "sim.h"

#ifdef NEW_STYLE
int     posit(void);
void    npos(void);
S32_T   rng(S32_T seed);
static void init_rc5(U32_T *s);
static void rc5_crypt(U32_T *a, U32_T *b, const U32_T *schedule, unsigned int r);
static void rc5_decrypt(U32_T *a, U32_T *b, const U32_T *schedule, unsigned int r);
static void rc5_schedule(U32_T *schedule, U32_T keya, U32_T keyb, unsigned int r);
#endif

/* minimal standard random number generator; integer version 2
 * Communications of the ACM, 31:10 (1988)
 * returns 1 <= seed <= 2^31-2, cycle: 2^32, tested and approved
 *
 * or, if useExtRNG is set, RC5-32/8/12 in counter crypting mode.
 */
S32_T
rng(seed)
  S32_T   seed;
{
  register S32_T temp = seed;
  if (!useExtRNG) {
      temp = 16807 * (temp % 127773) - 2836 * (temp / 127773);
      if (temp < 0)
	  temp += 2147483647;
      return temp;
  } else {
      static U32_T counter = 0;
      static U32_T schedule[2*(12+1)];
      if (!counter) {
	  init_rc5(&schedule[0]);
      }
      {
	  U32_T a = 0, b = counter;
	  rc5_crypt(&a, &b, &schedule[0], 12);
	  counter++;
	  return b & 0x7fffFFFF;
      }
  }
}



#define RETRIES1 20                /* how many times to try generating one
                                 * position */
#define RETRIES2 4                /* how many times to start backtracking */
int
posit()
{
  int     pos = 1, i, retries1 = RETRIES1, retries2 = RETRIES2;
  int     diff;

  do {
    /* generate */
    warrior[pos].position =
      ((seed = rng(seed)) % (coreSize - 2 * separation + 1)) + separation;
    /* test for overlap */
    for (i = 1; i < pos; ++i) {
      /* calculate positive difference */
      diff = (int) warrior[pos].position - warrior[i].position;
      if (diff < 0)
        diff = -diff;
      if (diff < separation)
        break;                        /* overlap! */
    }
    if (i == pos)                /* no overlap, generate next number */
      ++pos;
    else {                        /* overlap */
      if (!retries2)
        return 1;                /* exceeded attempts, fail */
      if (!retries1) {                /* backtrack: generate new sequence starting
                                 * at an */
        pos = i;                /* arbitrary position (last conflict) */
        --retries2;
        retries1 = RETRIES1;
      } else                        /* generate new current number (pos not
                                 * incremented) */
        --retries1;
    }
  } while (pos < warriors);
  return 0;
}

void
npos()
{
  int     i, j;
  unsigned int temp;
  unsigned int room = coreSize - separation * warriors + 1;
  for (i = 1; i < warriors; i++) {
    temp = (seed = rng(seed)) % room;
    for (j = i - 1; j > 0; j--) {
      if (temp > warrior[j].position)
        break;
      warrior[j + 1].position = warrior[j].position;
    }
    warrior[j + 1].position = temp;
  }
  temp = separation;
  for (i = 1; i < warriors; i++) {
    warrior[i].position += temp;
    temp += separation;
  }
  for (i = 1; i < warriors; i++) {
    j = (seed = rng(seed)) % (warriors - i) + i;
    temp = warrior[j].position;
    warrior[j].position = warrior[i].position;
    warrior[i].position = temp;
  }
}

static void
init_rc5(U32_T *schedule)
{
    U32_T a = 0, b = 0;
    int i, j;
    unsigned char *p = SWITCH_F;
    while (*p) {
	for (i=0; *p && i<4; i++, p++)  a ^= *p << (i*8);
	for (i=0; *p && i<4; i++, p++)  b ^= *p << (i*8);
    }
    rc5_schedule(schedule, a, b, 12);
}


#define ROL(x,y) (((U32_T)(x) << ((y)&31)) | ((U32_T)(x) >> ((y)&31)))
#define ROR(x,y) (((U32_T)(x) >> ((y)&31)) | ((U32_T)(x) << ((y)&31)))

#define _P 0xb7e15163UL
#define _Q 0x9e3779b9UL

static
void
rc5_crypt( U32_T *a, U32_T *b, const U32_T *C, unsigned int rounds)
{
  U32_T A,B;
  unsigned int k;

  A = *a; B = *b;
  A = A + C[0];
  B = B + C[1];
  for (k=2; k<2*rounds+2; ) {
    A = A^B;
    A = ROL(A,B);
    A = A+C[k];

    B = A^B;
    B = ROL(B,A);
    B = B+C[k+1];
    k = k+2;
  }
  *a = A;
  *b = B;
}


static
void
rc5_decrypt( U32_T *a, U32_T *b, const U32_T *C, unsigned int rounds )
{
  U32_T A,B;
  unsigned int k;

  A = *a;
  B = *b;
  for (k=2*rounds; k!=0; ) {
    B = B - C[k+1];
    B = ROR(B,A);
    B = B^A;

    A = A - C[k];
    A = ROR(A,B);
    A = B^A;
    k = k-2;
  }
  B = B - C[1];
  *b = B;
  A = A - C[0];
  *a = A;
}


static void
rc5_schedule( U32_T *S, U32_T LA, U32_T LB, unsigned int r)
{
  unsigned int i, k, NKEYS = 2*(r+1);
  U32_T A=0;

  A = S[0]  = ROL( _P,	3);
  	LA  = ROL( LA	+A,	A );
  A = S[1]  = ROL( _P+_Q +A+LA,	3);
  	LB  = ROL( LB	+A+LA,	A+LA );

  for ( S[2]=_P+2*_Q, i=3; i<NKEYS; i++) S[i] = S[i-1]+_Q;

  k = 0;
  i = 2;
  do {
    do
    {
      A = S[i]  = ROL( S[i]  +A+LB, 3);
            LA  = ROL( LA    +A+LB, A+LB );
      A = S[i+1]= ROL( S[i+1]+A+LA, 3);
	    LB  = ROL( LB    +A+LA, A+LA );
      i+=2;
    } while (i<NKEYS);
    k++;
    i = 0;
  } while (k < 3);
}
