#include "incs.h"
#include "tests.h"
#include "k.h"

//  Note:
//  
//  To test out-of-memory handling on determininistic calls, execute the string
//  and record the number of allocations n. Then simulate out of memory,
//  running the string each time for each i < n. Allocating functions such as
//  malloc, mmap, newK(), etc. will need to be overridden with debug versions.
//  This will be significantly slower.
//

Z I testsIO();
Z I tests02();
Z I tests01();
Z I testsBook();

#define TC_(x, y ) {S s=ts(tp(tc(x, y))); if(test_print) fprintf(stderr, "%s:%u: TC( %s , %s ) ... %s\n", __FILE__, __LINE__, x, y, s); test_print=0; }
#define TC(x,...) TC_(#x,#__VA_ARGS__)

I passed=0, skipped=0, failed=0;
I tests=0;
I test_print=0;
F testtime;

S ts(I x){ switch(x){CS(0,R "fail";) CS(1,R "OK")  CS(2,R "skipped")}; R 0;}
I tp(I x){ switch(x){CS(0,failed++)  CS(1,passed++)CS(2,skipped++)} tests++; R x;} //process the test

I tc(S a, S b) //test comparison .  R 0,1,2
{
  if(!(tests % 50)) O("t:%lld\n",tests); //commenting this causes an error. no idea why. fflush? macro stuff? >2 args bc of "skip" ?
  if(!SC("skip",a)) R 2;

  kreci=0;

  KTREE=Kd();
  K x = X(a);
  K y = X(b);
  I m=matchI(x,y);

  if(!m)
  {
    fprintf(stderr,"\nFailed. These are not equal:\n");
    fprintf(stderr,"%s , %s\n", a,b);
    fprintf(stderr,"********************************\n");
    show(x); fflush(stdout);
    fprintf(stderr,"--------------------------------\n");
    show(y); fflush(stdout);
    fprintf(stderr,"\n");
  }
  cd(x); cd(y);

  cd(KTREE);
  I c=0; DO(kreci, if(krec[i]) c++)
  if(!c) R m;

  fprintf(stderr,"Failed: Memory Leak - %s, %s \nAllocated K: %lld\nUnfreed K  : %lld\nLeak %%     : %f\n", a,b,kreci, c, c/(F)kreci);
  I j=-1;
  DO(c, do j++; while(!krec[j] && j < kreci); if(j>=kreci) break; K k=krec[j]; if(k){O("c:%lld t:%lld n:%lld | k:%p\n",k->c,k->t,k->n,k); show(k);} )
  R 0;
}

I test()
{ testtime=clock();

  testsBook();
  tests01();
  tests02();
  testsIO();  //could become slow - in the future may not want to test by default
  K x; x=_(567);if(!tp(x && *kI(x)==567))fprintf(stderr,"\n\nK string execution broken\n\n"); cd(x);

//done:
  testtime=(clock()-testtime)/CLOCKS_PER_SEC;
  F rate=passed/((F)tests-skipped);
  O("Test pass rate: %.4f, Total: %lld, Passed: %lld, Skipped: %lld, Failed: %lld, Time: %fs\n", rate,tests,passed,skipped,failed,testtime);
  I r=1==rate;
  O("%s\n", ts(r));
  testtime=0; lineB=0; kerr("undescribed");
  R r;
}

Z I testsIO()
{
  return 0;
  //binary - 1: 2:
  TC(1,t:`testfile00; a:(1;1.0;"c";`d;1 2;3.0 4.0;"ef";`g`h;();(1;`z)); t 1: a;  &/ a ~/: (1:t;2:t)) //leaves a file
}
 
Z I tests02()
{
  TC(`b,(`a`b)[1])
  TC(2, {1+1} 0)
  TC(2, {a:1;a+a} _n )
  TC(_n, {a:1;a+a}0;a)
  TC(99#1, {1}/:!99)
  TC(1 2, 2#(1;2;3.0))
  TC(1 2, -1_(1;2;3.0))
  TC({x^2}, f:{x^2};.`f)
  TC(1, (1+)~(1+))
  TC(0, (1+)~(2+))
  TC(1, {x} ~ {x})
  TC(0, {x} ~ {x })
  TC(1+2+3, {b:1;c:2;d:3;b+c+d} 0)
  TC(_n, {1;;;} 0 )
  TC({_f}, {_f} 0)
  TC(2 6 24, {:[x<2;1;_f[x-1]*x]}/: 2 3 4)  //paired with below
  TC(2 6 24, {:[x<2;1;x* _f[x-1]]}/: 2 3 4) //regression
  TC(60.0,   {:[(x<2)&(y<2);2;(x^2)+(y^2)+_f[x-1;y-1]]}[4;4]) //tests _f cache handled correctly
  TC(skip, (1;"stack") , .[{_f _f};0;:] ) //this test works but it takes 3x the other 600 tests
  TC(_n , _f) //nice not necessary
  TC(7, {[a;b;cc]q:0;z:1; a+b*cc}[1;2;3])
  TC(1, {x} 1)
  TC(2, {x+y}[1;1])
  TC(3, {z+y+x}[1;1;1])
  TC(12, f:{x+y}; f[1;2];f[8;4])
  TC(4, {4} 0)
  TC(5, {[] 2+3} 0)
  TC(1 2, a: 1 2;{a} 0) //Context is visible
  TC(1 2, a: 1 2;{a:3} 0;a)//Local changes are not applied to context
  TC(3 4, a: 1 2;{a+:2} 0; a)
  TC(_n, b:9;{b:b} 0)
  TC((2 4) , ((1+)[1]; (1+|+)[1;2]) )
  TC((),=!0) // =: Group had a bug with one element lists 
  TC((,,0),=,1)
  TC((,0;,1),=1 2)
  TC((,0;,1),=2 1)
  TC((2#,(,0;,1)), =:/:(1 2;2 1)) //Nice extension to K3.2, which errs on both =/: and =:/:

  //Projections
  TC((1;"valence"),@[.:;"{x}[1;]";:])
  TC((1;"valence"),@[.:;"+[1;2;;;]";:])
  TC(skip, (1;"valence"), .[+[1;2;;;]; 5;:] ) //fill in error trap //this has a problem?  +[1;2;;;;] causing valence when shouldn't?

  TC({x+y}[1;],{x+y}[1;])
  TC({x+y}[;1],{x+y}[;1])
  TC(!3, {x,y,z}[;;2][;1][0])
  TC(1 2 3 4, {x[z;y]}[,][3 4;1 2])
  TC(!3, f:{[a;b;c]a,b,c}; g:f 0; h:g 1; h 2)
  TC(skip, {[a;b;c;d;e]a+b+c+d+e}[;;1;1;1]'[;1] , {[a;b;c;d;e]a+b+c+d+e}[;;1;1;1]'[;1])
  TC((1 0;1 3) , a:(1 0;3 2); f:{a[x;y]}; (a[]0;f[]0)) //Cross-sectional indexing inconsistent with simple composition/projection
  TC(2 1, f:![1;]; f 1 2)
  TC(3, f:+[;]; g:f 1; g 2)
  TC(8.0, f:^[;]; g:f[;3]; g 2)
  TC(4 5, +[3;]/:1 2)
  TC_("7 8", "+[3;]\'[4 5]")

  //Global Assignment
  TC(7, {a::7} 0; a)
  TC(7, a::[1;7];a) //Mix with conditionals
  TC(7, {a:::[1;7]}0;a)
  TC(7, {a::[1;7];a} 0)

  //Subfunctions
  TC( 3, {b:3; g:{b}; b:4; g[]}0)       // inheritance: same var in prnt&child  child res
  TC( 4, {b:3; g:{b}; b:4; b}0)         // inheritance: same var in prnt&child  prnt res
  TC(_n, {b:3; g:{a}; b:4; g[]}0)       // inheritance: diff var in prnt/child, child res
  TC( 4, {b:3; g:{a}; b:4; b}0)         // inheritance: diff var in prnt/child, prnt res
  TC( 1, {g:{b:1}; g[]}0)               // :: subf-asgn, no prnt-asgn
  TC(12, {g:{b:1}; b::12; g[]}0;b)      // :: gl-asgn, gl-res (no prnt-asgn)
  TC( 1, {g:{b:1}; b::12; g[]}0)        // :: gl-asgn, child-res (no prnt-asgn)
  TC( 1, {g:{b:1}; c::12; g[]}0)        // :: gl-asgn, child-res (diff gl variable)
  TC( 4, {g:{b:1}; b::12; b:4; b}0)     // :: gl-asgn, prnt-res (prnt-asgn after gl-asgn)
  TC(_n, {g:{b:1}; b::12; b:4; b}0;b)   // :: gl-asgn, glbl-res (prnt-asgn after gl-asgn)
  TC(12, {g:{b:1}; b::12; b}0)          // :: gl-asgn, prnt-res (no prnt-asgn)
  TC(12, {g:{b:1}; b:4; b::12; b}0)     // :: gl-asgn, prnt-res (prnt-asgn before gl-asgn)
  TC(_n, {g:{b:1}; b:4; b::12; b}0;b)   // :: gl-asgn, glbl-res (prnt-asgn before gl-asgn)
  TC(12, b:3;{g:{b:1};b::12;g[]} 0;b)   // :: gl-asgn, pre-existing gl
  TC( 1, {g:{b:1;b}; b:12; g[]}0)       // :  prnt-asgn, child res
  TC( 3, {b:3; {b:4; b}0; b}0)          // :  prnt-asgn, prnt res
  TC( 3, {[a;b]{[c;d]c+d}[a;b]}[1;2])   // 2 explicit bracket args
  TC( 2, {[a;a]a+a}[1;9])               // oddly enough (no subfunction)
  TC( 1 10, {a:10; {x,a}x}1)            // 1 implicit arg: x
  TC( 1 2 10, {a:10;{x,y,a}[x;y]}[1;2]) // 2 implicit args x,y
  TC( 7, {{x+y+z}[x;y;z]+1}[1;2;3])     // 3 implict args: x,y,z
  TC( 8, {x+{b:3;g:{b};b:4;g[]}0}5)     // Depth-2 and inheritance
  TC( 4, {x-{x+{b:3;g:{b};b:4;g[]}0}5}12)  // Depth-3 and inheritance
  TC(3 5, {x+y,{x-{x+{b:3;g:{b};b:4;g[]}0}5}12}[1;2])  // Depth-4
  TC( 9, {+/x,{x+y,4}[1;2]}1)           // 2-level implicit args
  TC( 9, {+/x,{x+y,{x-{x+{b:3; g:{b}; b:4; g[]}0}5}12}[1;2]}1) // Depth-5
  TC( 5, {+/x,{x+y,{x-{x+{b:3; g:{b}; b:4; :7; g[]}0}5}12}[1;2]}1) // early-return in level-5
  TC( 7, {+/x,{x+y,{x-{x+{b:3; g:{b}; b:4; g[]}0}5}12}[1;2]; :7}1) // early-return in level-1
  TC( 3, {{b:3; g:{b}; b:4; g[]}0}0)    // unbuffered double braces
  TC( 4, {1+{b:3; g:{b}; b:4; g[]}0}0)  // buffered double braces 
  TC( 8, {({b:3;g:{b};b:4;g[]}0) + {b:5;g:{b};b:6;g[]}0}0) //double independent subfuncs
  TC( 3, b:9; {b:3;g:{b};b:4;g[]}0)     // pre-existing K-Tree entry
  TC(25, {{+/x,{x+y,{x-{x+{b:3;g:{b};b:4;g[]}0}5}12}[1;2]}1 + {x+{b:3;g:{b};b:4;g[]}0}5 + {x-{x+{b:3;g:{b};b:4;g[]}0}5}12 + x}4)
         // 4 stacks: depth-5, depth-2, depth-3, and implicit
  TC(1 3 10 3 10, g:1; do[2;{a:10; g::g,{x,a}x}3]; g) // do[2;] loop with subfunction (implicit arg) 
  TC( 7, f:{x+y+z}; g:f[1;;3]; {b:3; h:{b}; b:4; g[h[ ] ] }0) // projection and subfunction
  TC( 3, {:[x<2;1;_f[x-1]*x]}/: 2 3 4; {b:3; g:{b}; b:4; g[]}0) // muti-statement combo test
  TC(120, {b:3; g:{b}; a::{:[x<2; 1; _f[x-1]*x]}5; g[ ]}0; a) // embedded _f with atom-arg
  TC(2 6 24, {b:3; g:{b}; a::{:[x<2; 1; _f[x-1]*x]}/:2 3 4; g[ ]}0; a) // embedded _f with list-arg
  TC( 5, {x + {[a] a+a} y}[1;2])        // Leon Baum test
  TC( 5, {[a;b] a + {x+x} b}[1;2])      // Leon Baum test-2
  TC_("(7 0;7 1;7 2)", "f:{(7;x)};{[n]a:n;f'[!a]}[3]") // Variable scope
  TC_("(7 0;7 1;7 2)", "{a:5;{(7;x)}'[!3]}[]")         // More complicated variable scope
  TC(1 3, a:{x/y}; b:{a[{(x;#y)};x]}; b[(1;1 2 3)])    // Subfunctions and juxtaposition
  TC_("2 2#!4" , "a:{[f;c]f/'c}; c:2 2#!4; {d:a[{(x;y)};x];d}c") // Subfunction args & local variables
  TC(1 2, {a:x;{a,x}y}[1;2])  // Outer scope shadowed depending on argument passing syntax: issue #221
  TC(1 2, {a:x;{[b]a,b}y}[1;2])  // Outer scope shadowed depending on argument passing syntax: issue #221
  TC_("(1 0;3 2)", "g:{[x;f]l:x[;0];r:x[;1];l f' r}; d:2 2#!4; g[d;{y,x}]")      // issue #221
  TC_("(1 0;3 2)", "g:{[x;f]l:x[;0];r:x[;1];l f' r}; a:{y,x}; d:2 2#!4; g[d;a]") // issue #221
  TC_("(0 3;1 3;2 3;3 3;4 3;5 3)","{{[x;t](t,x)}[x]'!y+x}[3;3]") // s0 from issue #221 comments
  TC_("(0 3;1 3;2 3;3 3;4 3;5 3)","{{[t](t,x)}'!y+x}[3;3]")      // s1 from issue #221 comments
  TC_("(0 3;1 3;2 3;3 3;4 3;5 3)","{a:x;{[t](t,x)}'!y+x}[3;3]")  // s2 from issue #221 comments
  TC_("0 0 0 1 0 0 0 0 0 0", "f:{a:!x;{3=x!10}'a};f 10")         // issue #232
  TC(15 20 25, f:{a:x; 1 2 3{a+x*y}\\:5} ;f 10)   // issue #234 (extra escape needed for C)
  TC(99, f:{a:x; 2{a+x+y}/42}; f 55)              // issue #234
  TC(29, g:{a:x; 2{a+x+y}/!6}; g 1; g 2)                    // issue #235 case 1
  TC_("1 2", "{a:x;{a}()}'1 2")                             // issue #235 case 2
  TC(123, f:{a:x; {a+x}}; g:f 123; h:f 456; g 0)            // issue #235 case 3a
  TC(456, f:{a:x; {a+x}}; g:f 123; h:f 456; h 0)            // issue #235 case 3b
  TC(10, f:{a:x; g::{a+x}; a+:a; h::{a+x}}; f 10; g 0)      // issue #235 case 4a
  TC(20, f:{a:x; g::{a+x}; a+:a; h::{a+x}}; f 10; g 0; h 0) // issue #235 case 4b
  TC(_n, f:{a}; {a:x; f()}1)                      // issue #236
  TC(1, f: {x {x}/ 1}; f 1; f 1)                  // issue #237
  TC(1, f:{x}; g:{x f/1}; g 1; g 1)               // issue #238 case 1
  TC(1, f:{x}; g:{x f/1}; g 2)                    // issue #238 case 2
  TC(1, f:{x}; g:{x f/1}; g 1; g 2)               // issue #238 case 3
  TC(22, {[e]{[x]e}1}@22)                         // issue #239
  TC_( "(7 0;7 1;7 2)", "f:{(7;x)}; {[n]a:n;f'[!a]}[3]; {[n]a:n;f'[!a]}[3]")  // issue #243
  TC(456 123, f:{{[a]a,x}}123; f 456)             // issue #220
  TC(456 123, x:999; f:{{[a]a,x}}123; f 456)      // issue #220
  TC(3, {{[a]a+x}}[1][2])                         // issue #220
  TC(1 2, {a:x;{[b](a;b)}y}[1;2])                 // issue #220
  TC(3 1 2, f:{{[a]a,x,y}}[1]2; f 3)              // issue #244
  TC(3 1 2, {{[a]a,x,y}}[1][2]3)                  // issue #244
  TC(4 1 2 3, f:{{[a]a,x,y,z}}[1][2]3; f 4)       // issue #247
  TC(4 1 2 3, {{[a]a,x,y,z}}[1][2][3]4)           // issue #247
  TC(2 1, a:1;{a:2;{a}[]}[],a)                    // issue #287
  TC(2 1, a:1;{a:2;{a+x}[0]}[],a)                 // issue #287

  //Error trap: {[a;b][c;d] a+b} -> parse error ; { {[a][b] }} -> parse error
  TC(.[*; (3;4); :], (0;12) )
  TC(.[*;3 4 5;:], (1;"valence"))
  TC((.[-1 -2 _; ,!9; :]) , (1;"domain"))
  TC((.[0 10 100 _; ,!9; :]) , (1;"length"))
  //TC(.[=; 0; :] , (1;"valence") ) // ignore: better to return =[0;] than valence error
  TC((.[.:;,"{_foo[x]}";:]),(1;"reserved"))
  TC((@[.:;"{_foo[x]} 0";:]),(1;"reserved"))//.[NULL;...] errors inside bracket should not be interpreted as projections
  TC(a:1+`d, )

  TC(-9131, _jd 20100101)

  //Degenerate uses of : colon verb
  TC(1, (::)[1])  //monadic
  TC(2, (:)[0;2]) //dyadic
  
  //Conditionals
  TC(_n,:[1;])
  TC(_n,:[0;])
  TC(_n,:[0;1])
  TC(1,:[1;1])
  TC(2,:[0;1;2])
  TC(2,:[1;2;3])
  TC(2,:[1+1;2;3])
  TC(3,:[0;2;3])
  TC(2,:[1;2;3;4;5;6])
  TC(7,:[0;1;0;3;0;5;1;7;0;9;10])
  TC(3,:[0+0;1;1-1;2;1;3])
  TC(4,:[0+0;1;1-1;2;-1+1;3;4])
  TC(2,:[0;;0;;0;;1;2;0;])
  TC(_n,:[0;;0;;0;;0;2;0;])
  TC(_n,:[0;;0;;0;;0;2;0;;])
  TC(1,:[0;;0;;0;;0;2;0;; 1 ])
  TC(_n,:[,0;1])
  TC(1,:[,1;1;2])
  TC(2,:[,,0;1;2])
  TC(1,:[,,1;1;2])
  TC((1;"type"),@[.:;":[,!0;1;2]";:])
  TC(2 3, f:{[a] :[a;2;3]}; (f 1;f 0))
  TC(skip,(1;"parse"),:[1])
  TC(_n,if[3<4;a:20])
  TC(40,a:10;if[5<6;a:40];a)
  TC(30,a:30;if[5>6;a:20];a)
  TC(30,a:10;if[1;a:20;a:30;];a)
  TC(20,a:10;if[2>,1;a:20];a)
  TC(20,a:10;if[2>,,,1;a:20];a)
  TC((1;"type"),@[.:;"a:10;if[,!0;a:20];a";:])
  TC(_n,i:0;while[7>i;0+0;i+:1])
  TC(7,i:0;while[7>i;0+0;i+:1];i)
  TC(5,i:0;while[5>i+:1];i)
  TC_(",5","i:,0;while[5>i+:1];i")


  TC(_n,do[5;0])
  TC(7,i:0;do[7;0+0;i+:1;];i)
  TC(5,i:0;do[,5;i+:1];i)

  //Commands
  TC((1;"parse"), @[.:;"\b";:]) //trap parse errors  (\b  backspace)

  TC(51, a:101#"1+"; . a) //In debug mode repeating . a;. a; causes execution time to build

  TC(6, (+/)[1 2 3])
  TC(7, (+/)[1; 1 2 3])

  TC(1,~0.0)
  TC(1,~ -0.0)

  TC((1;"valence"), @[.:;",\\: 1 2 3";:])

  TC(((0 2;0 3);(1 2;1 3)),  0 1 ,/:\\:2 3 )
  TC(((0 2;0 3);(1 2;1 3)),  0 1 ,/:\\:\\:2 3 )

  TC(((0 2;1 2);(0 3;1 3)),  0 1 ,\\:/: 2 3 )
  TC( ((0;1;2 3;4 5);(0;1;6 7;8 9)) ,   0 1 ,/: ((2 3;4 5);(6 7;8 9)))
  TC( ((0 1 2 3;0 1 4 5);(0 1 6 7; 0 1 8 9)) ,   0 1 ,/:/: ((2 3;4 5);(6 7;8 9)))
  TC( (((0 1 2;0 1 3);(0 1 4; 0 1 5));((0 1 6; 0 1 7);(0 1 8; 0 1 9))) , 0 1 ,/:/:/: ((2 3;4 5);(6 7;8 9)))

  //_bd _db
  TC(1,a:(1;1.0;"c";`d;1 2;3.0 4.0;"ef";`g`h;();(1;`z)); &/{x~_db _bd x}/:a,,a)
  TC(1,a:(!11)#\\:`a`b; &/{x~_db _bd x}/:a)
  TC(1,a:(!11)#\\:"cd"; &/{x~_db _bd x}/:a)
  TC(1,a:(!11)#\\:(`a;"c";1); &/{x~_db _bd x}/:a)
  TC(1,a:(!11)#\\:,(`a;"c";1); &/{x~_db _bd x}/:a)
  //TC((1;"parse"), .[.:;,"_db_bd 1";:] ) //we handle this case now instead of error
  TC(2,  _db_bd_db_bd 2) //should be able to handle this case
  
  TC(skip, 1,&/{x~_db _bd x}/: (+;+:;-;-:)) // handle unreserved monadic,dyadic verbs

  TC(0 2, @[.:;"1+1";:])

  TC(32, (+/ *)[1 2 3;4 5 6])
  TC(32,  1 2 3 _dot 4 5 6)

  TC((0.5 0 0;0 0.5 0;0 0 0.5), _inv (2 0 0;0 2 0; 0 0 2)  )
  TC((0.1 0 0;0 0.1 0;0 0 0.1), _inv (10 0 0;0 10 0; 0 0 10)  )
  TC(400 400, 10 10 _mul 20 20)

  //_sv _vs
  TC(13, 11(_+)\\2)
  TC(11 13 16, 11(_+)\\2 3)
  TC( 2010,  10 _sv 2 0 1 0)
  TC( 11,     2 _sv 1 0 1 1 )

  TC(1 2 3 4 _bin/: 1 2 3 4 5 0 0.5 1.5 2.5 3.5 4.5, 0 1 2 3 4 0 0 1 2 3 4)


  // \p
  TC( 4:."\\p",1)

  TC(3999, #5:2000#1) //5:monadic should not be subject to "..." display eliding (before displaying it anyway)
  //TC(33599997, #5:16799999#1) // Forces r>KP_MAX in kexpander for 32-bit-Linux or OSX
  //TC(33999997, #5:16999999#1) // Forces r>KP_MAX in kexpander for Cygwin
  TC(5:(+),   (,"+")) 
  TC(5:(|/), "|/") 
  TC(5:(_acos;_tanh;_abs;_size;_bin;_vs;_ssr), "(_acos;_tanh;_abs;_size;_bin;_vs;_ssr)")
  TC_("a:.'(+;-);a@\\:1 2", "3 -1")

  TC(3 3#!0, (0 0 0;0 0 0; 0 0 0))
  TC(3 3#0#0.0, (0 0 0.0;0 0 0.0; 0 0 0.0))
  TC(2 2#0#"", ("  ";"  ") )
  TC(1 1#0#`, ,,` )
  TC(2 2#(), ((;);(;)))

  TC({x+y}/1 2 3 4, 10)
 
  //Radix _vs (unbounded)
  //TC(!0, 2 _vs 0) //an artifact of writing K in K (K3.2), but sensible if you intend 30000 and 30 to have the same length _vs[2;] (leading zeroes)
  TC((,0), 2 _vs 0)
  TC((,1), 2 _vs 1)
  TC(1 0, 2 _vs 2)
  TC(1 0 1 1,  2 _vs 11)
  TC(1 1 0 0,  2 _vs 12)
  TC(2 0 1 0, 10 _vs 2010)
  TC(1 2 3 4 5, 10 _vs 12345)
  //TC(((2;2 2);(0;0 0);(1; 1 1);(0;1 2)), 10 _vs (2010;2011 2012)) //Unclear to me whether this level of generalization makes sense
  //TC(skip,  2 _vs (2; 4 8;(16 32;64 128))) //see above
  //Clock arithmetic _vs (bounded)
  TC(1 6 40, 24 60 60 _vs 4000)
  TC(0 0 0, 1 2 3 _vs 0)
  TC(23 59 0, 24 60 60 _vs -60)
  TC(13 20 0, 24 60 60 _vs -6000000)
  TC((0 0 10;0 16 40;13 46 40) , 24 60 60 _vs/: 10 1000 1000000) // /: is redundant for us
  TC(0 0, 1 1 _vs 0)
 
  TC(skip, 0, (_+[2;]) 11) //segfault
  TC((1;"parse"),  @[.:;"if[1; `0:“bad unicode quotes”]";:]) 

  //f'[x;y;z;...]
  TC_("6", "f:{x+y+z}; f'[1;2;3]")
  TC_("6", "f:{x+y+z}; f''[1;2;3]")
  TC_("()", "f:{x+y+z}; f'[();2;3]")
  TC_("_n","{x}'[]"); 
  TC_("12 15 18", "f:{x+y+z}; f'[1 2 3;4 5 6;7 8 9]")
  TC_("22 25 28", "f:{[a;b;c;d] a+b+c+d}; f'[1 2 3;4 5 6;7 8 9;10]")
  TC_("22 26 30", "f:{[a;b;c;d] a+b+c+d}; f'[1 2 3;4 5 6;7 8 9;10 11 12]")
  TC_("12 14 16", "f:{x+y+z}; f'[1;4 5 6;7 8 9]")
  TC_("12 14 16", "{x+y+z}'[1;4 5 6;7 8 9]")
  TC_("(9 12;9 12)", "{x+y+z}'[(1 2;1 2);(3 4;3 4);(5 6;5 6)]")
  TC_("9 12", "{x+y+z}'[1 2;3 4;5 6]")
  TC_("9 12", "{x+y+z}''''''[1 2;3 4;5 6]")
  TC_("12 15 18", "f:{x+y+z}; f''[1 2 3;4 5 6;7 8 9]")

  //f/[x;y;z;...]
  TC(16 17 18, {x+y}/[1 2 3;4 5 6] )
  TC(16 17 18, (+)/[1 2 3;4 5 6] )
  TC((), {x+y+z}/[();1 2 3;4 5 6])
  TC(1 4 2 5 3 6, {x,y,z}/[();1 2 3;4 5 6])
  TC((,7), @/[,1;0 0 0;+;1 2 3]) 
  TC((,5), @/[,1;4#0;+;1]) 
  TC(3 1,  ./[1 1;0 0;+; 1 1])
  TC(2,+/[1 1])
  TC(3 3,+/[1 1;1 1])
  TC(_n, +/[])
  
  //f\[x;y;z;...]
  TC((1 2 3;5 6 7;10 11 12;16 17 18), (+)\\[1 2 3;4 5 6] )
  TC((1 2 3;5 6 7;10 11 12;16 17 18), +\\[1 2 3;4 5 6] )
  TC((1 2;1 2 3 5;1 2 3 5 4 6)  , {x,y,z}\\[1 2;3 4;5 6])
  TC(3 , {x+y+z}\\[1;1;1])
  TC((1 1;3 3;5 5) , {x+y+z}\\[1 1;1 1;1 1])
  TC((,1), {x,y,z}\[1;();()])
  TC(1 3 6, +\\[1 2 3])
  TC(1 3 6, (+\\)[1 2 3])
  TC((1 1;2 2;3 3), +\\[1 1;1 1])
  TC((1 1;2 2;3 3), (+\\)[1 1;1 1])
  TC((1 1;10 10;19 19) ,{[a;b;c;d]a+b+c+d}\\[1 1;2 2;3 3;4 4])

  //f'/\...[x;y;z;...]
  TC_("(33 34;39 40)", "{x+y+z}'/[(1 2;3 4);(5 6;7 8);(9 10;11 12)]")
  TC_("(31 32;41 42)", "{x+y+z}/'[(1 2;3 4);(5 6;7 8);(9 10;11 12)]")

  TC( (@[0$;0 1;:])   , (1;"type"))
  TC( (@[0$;0 1.0;:]) , (1;"type"))
  TC( (@[0$;`a`a;:])  , (1;"type"))

  TC( (@[.:;"a:(1 2)[0]:3";:] ), (1;"parse") )  
  TC( (@[.:;"a:(0 1;2 3); a[0][1]:9";:] ), (1;"parse") )  
  TC( (@[.:;"(1):2";:] ), (0;2) )    //optional, differs from K3.2
  TC( (@[.:;"a: 1 1 1; a/[0] +: 10";:] ), (1;"valence") ) //not clear here. think ... +:10 is ... index flip 10. Should be parse err?
  TC( (@[.:;"5 (a:5)/1";:] ), (1;"type") )  

  TC(13, ({x(|+\\)\\1 1} 5)[5;0])
  TC( (.[.:;,"@[a-b]";:]) , (1;"type") ) //specific err not important

  TC( 5, _ceiling 4.6)
  TC(-4, _ceiling -4.6)
  TC( 5, _ceiling 5.0)
  TC( 2, _ceiling 1.001)
  TC( 2, _ceiling 1.0000001)
  TC( 1, _ceiling 1.00000000000000000001)
  TC( 1, _ceiling 0.00000000000000000001) //per defn. as -_-

  TC(1 2, @[1 2;0;0])
  TC(1 2, .[1 2;0;0])
  TC(6 2, @[1 2;0;5 6 7])
  TC(6 2, .[1 2;0;5 6 7])

  TC_("(1;\"index\")", "f:4 4#!16;i:(0;1 2;3 4 5;6 7 8 9); @[f;i;:]")
  TC_("(1;\"index\")", "f:4 4#!16;i:(0;1 2;3 4 5;6 7 8 9); @[.:;\"f@'i\";:]")

  TC_("(0;1 2)", "@[.:;\"1 2(+\\:)'1 2\";:]")
  TC_("(1;\"valence\")", "@[.:;\"1 2(+\\\\:)'1 2\";:]")
  TC_("(1;\"type\")", "@[.:;\"(2=\\\"2\\\") 2\";:]")


  TC_("(0;6 7 8)", "a:{x+y};@[.:;\"a/:[5;1 2 3]\";:]")
  TC_("(0;6 7 8)", "a:{x+y};@[.:;\"a\\\\:[5 6 7;1]\";:]")
  TC_("(\"ab\"\n \"ac\")", "{x,y}/:[\"a\";\"bc\"]")
  TC(-1 0 1, flip:{[f;x;y]f[y;x]};flip[-\\:][2;1 2 3])

  TC_("10#2", "{x+1}'10#1")
  TC_("c:{&:'y(?,/(1!)\\'1,')/,&x-y}; {x@<x}@c[4;2]","(0 1;0 2;0 3;1 2;1 3;2 3)")

  TC(:,:)
  TC(::,::)
  TC(::::,::::)
  TC(5:(:), ,":")
  TC(5:(::), "::")
  TC(5:(::::), "::::") 
  TC_("(1;\"parse\")", "@[.:;\":::\";:]")  //:::, :::::, and so on return parse error (monadic colons ending in dyadic) with
  TC_("(1;\"parse\")", "@[.:;\":::::\";:]")//more elborate code you could handle these cases, but that seems pointless
  TC_("(1;\"parse\")", "@[.:;\":::5::::\";:]") 
  TC_("(1;\"parse\")", "@[.:;\"a _abs: \";:]") //more generally PE for anything not assignment ending in dyadic colon (except a solitary dyadic colon)
  TC_("(1;\"parse\")", "@[.:;\"4:::\";:]")

  TC(2,a:2 3;a@*:0) 
  TC(0=#:,a:0;a=#:)

  TC(_n, do[3;."\" \"_sv 1"]) //bv_ex error mishandling was causing crash
  TC(_n, do[3;."1_sv \" \""]) //^^
  
  TC(6 7 8, (21>+/)(2+)/!3)
  TC((1;"wsfull"),@[.:;"0I#0";:])
  TC((1;"wsfull"),@[.:;"&0I";:])
  #if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
  TC((1;"limit"),@[.:;"!0I";:])
  #else
  TC((1;"wsfull"),@[.:;"!0I";:])
  #endif

  TC(1 3    ,  +\\ 1 2)
  TC(1 3.0  ,  +\\ 1.0 2.0)
  TC(1 2 4.0,  1.0 +\\ 1 2)
  TC(1 2 4.0,  1.0 +\\ 1.0 2.0)
  TC(1 2 4  ,  1 +\\ 1 2)     //These change in K4
  TC(1 2 4.0,  1 +\\ 1.0 2.0) //We break with K3's (1;2.0;4.0). K3 bug?

  TC(8 1, ."10 -'': 2 3")
  TC(8 1, ."10 -': 2 3")
  TC(8, ."10 -': 2")
  TC(12, ."10 +': 2")
  TC(12 5, ."10 +': 2 3")

  TC(0 12345 1406932606 654583775 1449466924, 4{((1103515245*x)+12345)!(_2^31)}\\0 )

  //bv_ex subtriadic
  //bv_ex 1. doesn't seem to handle projectons correcly
  //      2. doesn't let / \ etc handle themselves correctly in the dyadic (monadic, niladic?) case
  TC(+/[1;],+/[1;])    // valid as test for memory leak (not for correct result)
  TC(10 , +/[1;]2 3 4)
  TC(10 , (+/)[1;]2 3 4)
  TC(1 3 6 10 , +\\[1;]2 3 4)
  TC(+\\[1;],+\\[1;])    // valid as test for memory leak leak (not for correct result)
  TC(+/[;1],+/[;1])    // valid as test for memory leak leak (not for correct result)
  TC_("+'[;1]","+'[;1]")  // valid as test for memory leak leak (not for correct result)
  TC_("3 4 5","+'[1;]2 3 4")
  TC({x+y}/[1;], {x+y}/[1;])  // valid as test for memory leak leak (not for correct result)
  TC(10 , {x+y}/[1;]2 3 4)
  TC(10 , ({x+y}/)[1;]2 3 4)
  TC(1 3 6 10 , {x+y}\\[1;]2 3 4) //k4 deviates from k3 and gives 3 6 10
  TC({x+y}\\[1;],{x+y}\\[1;])  // valid as test for memory leak leak (not for correct result)
  TC({x+y}/[;1], {x+y}/[;1])  // valid as test for memory leak leak (not for correct result)
  TC_("{x+y}'[1;]","{x+y}'[1;]")  // valid as test for memory leak leak (not for correct result)
  TC_("3 4 5","{x+y}'[1;]2 3 4")
  TC_("skip","(,':)[1 2;3 4] ~ (1 2;4 3)")

  TC(("ab";,"ab";("ab";"cd")), (2 2 # "abcd")[(0;,0;0 1)]) // indexing scalars & singletons 
  TC(("a";),"a",:[1;;"b"])  // concatenating with "nothing"

  //COW: Copy On Write
  TC(0 1 2 3, a:b:!4; a[1]:9; b)     // update integer vector w integer atom
  TC(0 1 2 3, a:b:!4; a[1 2]:8 9; b) // update integer vector w integer vector
  TC((.1;.2;.3;.4), a:b:(.1;.2;.3;.4); a[1]:.9; b)         // update float vector w float atom
  TC((.1;.2;.3;.4), a:b:(.1;.2;.3;.4); a[1 2]:(.8;.9); b)  // update float vector w float vector
  TC(d:.((`a;1;);(`b;2;));d[!d]:d, (.((`a;1;);(`b;2;));.((`a;1;);(`b;2;)))  ) //update dictionaries
  TC(x:2 2#!4;x[0]:x;x, ((0 1;2 3);2 3)) //update list
  TC(x:!10;y:x;y[1]:100;x, !10) //cross-variable assignment
  TC(x:(1;1.0;"1");y:x;z:x;z[0]:2;y, (1;1.0;"1")) //demonstrate need for recursive ci/cd
  TC(1, x:.+(`a`b;1 2); y:x; ."y.a:11; x.a") //dict references
  TC(x:.+(`a`b;1 2); y:x; y.a:11;x, .+(`a`b;1 2) )  // Bakul comments #205, case 1
  TC(d:.+(`a`b;1 2);d[`a]:d, .+(`a`b;1 2) )         // Bakul comments #205, case 2a
  TC(d:.+(`a`b;1 2);d.a:d, .+(`a`b;1 2) )         // Bakul comments #205, case 2b
  TC(d:.+(`a`b;1 2); d[!d]:d, (.((`a;1;);(`b;2;));.((`a;1;);(`b;2;))) )   // #188
  TC(.((`a;(9 9 9;9 9 9;9 9 9););(`b;2;)), d:.+(`a`b;(3 3#9;2));e:d;d.a[1;2]:e) //case 3
  TC( (.((`a;(9 9;9 9););(`b;2;));.((`a;(9 9;9 9););(`b;2;))), d:.+(`a`b;(2 2#9;2));d[]:d)  // case 4
  TC((2;(9 9 9;9 9 9;9 9 9)),  d:.+(`a`b;(3 3#9;2));d[]:|d[])                   //case 5
  TC(x:.((`a;(0 1 2;3 4 5;6 7 8););(`b;10;));(x;x;x), d:.+(`a`b;(3 3#!9; 10));d.a[2]:(d;d;d)) // #43 case 3
  TC_(".,(`d;;)", "d:.k")  // #43 cases 1 & 2
  TC_(".,(`d;.,(`d;;);)", "d:.k;d:.k;d")  // #43 case 4
  TC_(".((`a;;);(`b;;))", "a:b:.k;a")    // #43 case 5a
  TC_(".((`a;;);(`b;;))", "a:b:.k;b")    // #43 case 5b
  TC_(".((`a;.,(`c;;););(`b;.,(`c;;);))", "c:.+(`a`b;(.k;.k));c") // #43 case 6
  TC_("(.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;)))", "a:b:c:(.k;.k;.k);a") // #43 case 7a
  TC_("(.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;)))", "a:b:c:(.k;.k;.k);b") // #43 case 7b
  TC_("(.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;));.((`a;;);(`b;;);(`c;;)))", "a:b:c:(.k;.k;.k);c") // #43 case 7c
  TC_("(.((`x;;);(`y;;));.((`x;;);(`y;;)))", "x:y:(.k;.k);x") // #43 case 8x
  TC_("(.((`x;;);(`y;;));.((`x;;);(`y;;)))", "x:y:(.k;.k);y") // #43 case 8y
  TC(3 4 5, x:2 3#!6; y:x; z:y[1]; y[1;1]:99; z)
  TC(3.1 4.1 5.1, x:.1+2 3#!6; y:x; z:y[1]; y[1;1]:99.1; z)

  TC(1 0, ("a.c";"bc") _sm "*.[ch]")
  TC_("0 1", "($`one;$`two) _sm $`two")
  TC_("0 1", "`one `two  _sm $`two")
  TC_("$`xxx", "`hostname _setenv $`xxx; _getenv `hostname")
  TC(".k", ."\\d a"; ."\\d .k"; $_d)  // check for memory leaks
  TC(1 2, a:.((`b;1);(`c;2)); `a[`b`c])
  TC(3, a:1; \\b:2; c:3) // check: \b n
  TC(5, a[5])
  TC(5 6, a[5 6])
  TC(5, a 5)
  TC(5 6, a 5 6)
  TC(a:{a[x]}; a 5, )  

  R 0; 
}

Z I tests01()
{
  //Backslashes must be represented as \\, percent-signs as %%
  //For ' and // use TC_("string","string")
  //TC(skip,1,1) //How to skip a test that used to look like TC(1,1)
  //First argument is everything before the first exposed comma. Second argument is everything after.
  TC(sizeofS:sizeof(S),sizeofI:sizeof(I))
  TC(1,1)
  TC(1 1, 1 1)
  TC(1 1,1,1) 
  TC(2,1+1)   
  TC((),())
  TC((),1 2 3@())
  TC((), (1 2 3)())
  TC(1 2 3, (1 2 3).())
  TC(3, 3 4[0])
  TC(10+1+, 10 + 1 2[0] +)
  TC(7 6,1+1+1+1+1+|||1 2)
  TC(_n,;)
  TC((;),(;))
  TC((;;),(;;))
  TC(1, (((((1))))))
  TC(5, 1+(1+(1+(1+1))))
  TC(2, 1+(((((1))))))
  TC(2 2, (2 2;3 4)[0])
  TC((1 2;3 4),  (1 2;3 4)[0 1] )
  TC(1 3, (1 2;3 4)[0 1;0])
  TC(1 2, (1 2;3 4)[0;0 1])
  TC(10,  (10 20;30 40)[0;0])
  TC(10,  (10 20;30 40)[0][0])
  TC(2,  (1 2;3 4)[0;1])
  TC(20,  (10 20;30 40)[0][1])
  TC(30,  ((10 20;30 40);50)[0;1;0])
  TC(30,  ((10 20;30 40);50)[0][1][0])
  TC(10 20 30, m:(1 2 3;10 20 30;100 200 300); m[1;])
  TC(2 20 200, m:(1 2 3;10 20 30;100 200 300); m[;1])
  TC((0 1;1 2 3) , a:(!a)+!:/:2+!a:4; a[0 1;]) //Extension: !:/:2+!4 works. K3.2 valence err for !, !:, or (!:)
  TC(a:2 2 2#1+!10, a[0 1;;0 1])
  TC(2 , # (+;+))
  TC(7, 4:(`a 1 \\ ))
  TC(1, 7=4:(`.))
  TC(+,+)
  TC(+-|, ||;+-|)
  TC(1 , 1 2 . 0)
  TC(11, .(`a;();:;11);a;a;a;a)
  TC(`a, .(`a;();:;22))
  TC(33, .(`a;();:;33);a)
  TC(1,a:1)
  TC(2,a:2)
  TC(3,+[1;2])
  TC(`b, .[`b;();:;33])
  TC(33,..[`b;();:;33])
  TC(1+2 3,..[`b;();:;3 4])
  TC(2, a:1;b:4;b:5;c:1;d:2)
  TC(30,  ((10 20;30 40);50)[0;1;0])
  TC(`b,b:1;.[`b;();-:]) 
  TC(33,b:33;..[`b;()]) 
  TC(-33,b:33; ..[`b;();-:]) 
  TC(10,a:10)
  TC(1 2 3,a:1 2 3)
  TC(3,a:1+a:1+a:1)
  TC(1 1 1 1, a:b:c:d:1;(a,b,c,d))
  TC(+, a:+)
  TC(11+, 11+)
  TC(1-1+, 1-1+)
  TC(0, (10+1+) ~ (11+)) //Interesting case
  TC(_n, a:) //In K3.2 this is parse error
  TC(4,#(+;+:;0:;:))
  TC(-10, a:0;a:a-10)
  TC(-12, .(10;();:;-12))
  TC(-3, z:1;z+:2;z-:)
  TC(7 8, a:1 2 3;1+a[0 1]:6 7)
  TC(`a`b, a:1 2 3;a[0 1]:`a`b)
  TC(`a`b, a:(1 2 3;1.0 2.0 3.0); a[0 1]:`a`b)
  TC(1, a:(((((1))))))
  TC(6, a:2;f:(1+a+);a:10; f 3) //Phrases ending in verb/adverb that get converted to 7-types need to "finally resolve" variables
  TC(0,a:b:10;a:14;b~14)
  TC(-10, a:0; a -: 10) 
  TC(-1 -2 -3, a:1 2 3; a -:) 
  TC(6, a: 1 2 3; 1+a[0]+:4)
  TC(5, a: 1 2 3; 1+a[0]+:4;a[0])
  TC(2, a: 1 2 3; 1-a[0]-:)
  TC(-1, a: 1 2 3; 1-a[0]-:;a[0])
  TC(3, a:2; z:1 2 3; z[a])
  TC(1, a:0; z:(1 2; 3 4); z[a][a]) 
  TC(1   , a:0;z:((1 2;3 4);(5 6;7 8);(9 10;11 12));z[a][a][a])
  TC(8   , a:0;z:((1 2;3 4);(5 6;7 8);(9 10;11 12));z[a][a][a:1])
  TC(1 2, a:_n; 1 2[a][a][a][a][a][a][a][a][][][][][][][a][][][][][][a][][][][])
  TC((1 2;3 4) , ( 1 2 \n 3 4))
  TC(0 1 2,! 3)
  TC(4 ,2*2 ) 
  TC(0,+/())
  TC(0,|/())
  TC(1,*/())
  TC(1,&/())
  TC(3,+/1 2)
  TC(1,+/1)
  TC(1,+/,1)
  TC(1 3,+\\1 2)
  TC(1,+\\1)
  TC(14, 2+/3 4 5)
  TC(a:(1 2 3;"CCC";`x`y`z), ++a)
  TC(1 2 4 7, 1+\\1 2 3)
  TC((.,(`k;)), .,(`k;))
  TC(0 8 , a:0;z:((1 2;3 4);(5 6;7 8);(9 10;11 12));a,(z+a:0)[a][a][a:1])
  TC(0 0 0 1, 1   < -1 0 1 2  )
  TC(0 0 0 1, 1   < -1 0 1 2.0)
  TC(0 0 0 1, 1.0 < -1 0 1 2  )
  TC(0 0 0 1, 1.0 < -1 0 1 2.0)
  TC(1 1 0 0, 1   > -1 0 1 2  )
  TC(1 1 0 0, 1   > -1 0 1 2.0)
  TC(1 1 0 0, 1.0 > -1 0 1 2  )
  TC(1 1 0 0, 1.0 > -1 0 1 2.0)
  TC(0 0 0 1, "c" < "abcd")
  TC(1 1 0 0, "c" > "abcd")
  TC(0 0 0 1, `c < `a`b`c`d)
  TC(1 1 0 0, `c > `a`b`c`d)
  TC(6 ,f:|/0(0|+)\\; f 2 1 -4 2 -1 5)
  TC(5#45 , f:|/0(0|+)\\; a:!10; (f (!10); f[!10]; f a; f @ a; f .,a) )
  TC(2,."0+0\n1+1")
  TC_("4",".\"1+1\\n2+2\"")
  R 0;
}


Z I testsBook()
{
  //K Manual 2.0 compliance
  TC(1 0 1 0 0 2 0 1 2 1,A.I:1 2 3;A.F:2 5 7;B.F: 5 2 5 2 2 7 2 5 7 5; A.F ?/: B.F )
  //TC((2 1 2 1 1 3 1 2 3 2;5 2 5 2 2 7 2 5 7 5) ,A.I:1 2 3;A.F:2 5 7;B.F: 5 2 5 2 2 7 2 5 7 5; A[;A.F?/:B.F] )
  TC(10,+/1 2 3 4)
  TC(26, 16 +/ 1 2 3 4)
  TC(3.5, (1;"a";3.5;`xyz) 2)
  TC(25 20 5 ,x:10; (x+5;x:20;x-5))
  TC((98 54;2.5;3.5 -1; 54; 24)  ,  x:99 55; (x-1;3.5-1;3.5 -1;x[1]-1;(12+13)- 1) )
  TC(-3 -4 -5, - 3 4 5)
  TC((-5 -2;-3;8 0 -2), - (5 2; 3; -8 0 2))
  TC(5 9, 2 6 + 3)
  TC(5 -6, 2 + 3 -8)
  TC(5 -2, 2 6 + 3 -8)
  TC(((7 8;9 10 11);(13;15 16)), (2;3 4) + ((5 6;7 8 9);(10;11 12))) 
  TC((2 7;-23), 2 4 -23 8 7 @ (0 4; 2))
  TC(1, @"a")
  TC( (1;(+;102)) , f:+; (@f;(f;102)))
  TC((1;;2), (1; _n ; 2))
  TC(4 3, ^ (1 2 3; "abc"; `x `y `z; 5.4 1.2 -3.56)) 
  TC(2 3 2, ^ ((1 2; `a `b; "AB"); ("CD"; 3 4; `c `d)))
  TC(2 3, ^ ((0 1 2; `a; "AB"); ("CD"; 3 4; `c `d)))
  TC(3, # 1 -2 3)
  TC(-2, 1 -2 3[1])
  TC(4, # 3 4 5.721 1.023e10)
  TC(5 , #"hello")
  //End of "Terminology" section, Begin Verbs A-F
  TC(9 14 19 6 5 4 3 5 7 0, d: 9 8 7 6 5 4 3 2 1 0;i: 2 7 1 8 2 8; y: 5 3 6 2 7 4;@[d;i;+;y])
  TC(((9;"a";"b";"c");(8;1.5;`xyz);(7;100;3.76;`efgh)),d:9 8 7; i:(0;(1;2 2)); y: ("abc";((1.5;`xyz);(100;(3.76;`efgh)))); @[d;i;,;y] )
  TC(9 60 70 6 5 4 3 30 40 0, d:9 8 7 6 5 4 3 2 1 0;i:2 7 1 8 2 8; y: 50 30 60 20 70 40; @[d;i;:;y])
  TC((5 2.14;("a";"b";"cxyz")), .[(5 2.14; "abc");1 2; ,; "xyz"])
  TC( ((1 2 3 400 600;4 5 6 7 500);(8 9;10;11 12);(13 14 100 300;15 16 17 18 200;19 20)), 
       d:((1 2 3;4 5 6 7);(8 9;10; 11 12);(13 14;15 16 17 18; 19 20)); i:(2 0;0 1 0); y:(100 200 300; 400 500 600); .[d;i;,;y])
  TC((600 500;(8 9;10;11 12);(300;200;19 20)), d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20)); i: (2 0; 0 1 0); y:(100 200 300; 400 500 600);.[d;i;:;y])
  TC(((1 2 3;-4 -5 -6 -7);(8 9;10;11 12);(13 14;-15 -16 -17 -18;19 20))  , d:((1 2 3; 4 5 6 7);(8 9;10; 11 12);(13 14; 15 16 17 18;19 20)); i: (2 0; 0 1 0); y: (100 200 300; 400 500 600); .[d;i;-:])
  TC(2 3 4 5 6, .[2 3;();,;4 5 6])
  TC((2 4;3 5), .[2 3; ,_n;,;4 5])
  TC("c" , (5 2.14;"abc") . 1 2)
  TC(1,-2~4:0.0 0i)
  TC(0.0 0i,0.0 0I)
  TC(1, 0i = 0I)
  TC(0, 0i ~ 0I) 
  TC(1, @1)
  TC(0, @2 3 )
  TC(1, @"Z" )
  TC(0, @"" )
  TC(1, @{x+y} )
  TC(0, @(+;-) )
  TC(1, @ `symbol )
  TC(1, @ .((`a;2);(`b;3)) )
  TC(1, @ _n)
  TC(4 , #3 1 4 2 )
  TC(3 , #(8 1 6;3 7;4 9 2) )
  TC(1 , #"A" )
  TC(1 , #,"A" )
  TC(5 , #"count" )
  TC("tares", 1 _ "stares")
  TC("star", -2 _ "stares")
  TC(7, 0 _ 7)
  TC("", 88 _ "")
  TC(!0 , 9 _ !6)
  TC((0 1 2;3 4 5), 0 3 _ 0 1 2 3 4 5)
  TC((0 1 2 3;4 5), 0 4 _ 0 1 2 3 4 5)
  TC(("seas";"hells"), 0 4 _ "seashells")
  TC(("try";" to";" cut";" into";" words"), a:"try to cut into words"; (0,(&a=" ")) _ a)
  TC((!0;1 2;3 4 5), 1 1 3 _ !6)
  TC((,3), ^ 1 2 3)
  TC(1 3, ^,1 2 3)
  TC(0 1 2 3 4, !5)
  TC(1, 3=3)
  TC(0, 3= -3)
  TC(0 1 1, "cat" = "rat")
  TC(0, `abc = `abcdefg)
  TC(0, 3.0=3.1)
  TC(0, 3.0 = 3.0001)
  TC(1, 3.0 = 3.0000000000000001)
  TC(2, 9 8 7 6 5 4 3 ? 7)
  TC(7, 9 8 7 6 5 4 3 ? 1)
  TC(2 , (8 16; "abcdef"; 4 9 2; `x`y`z`w; 4 9 2) ? 4 9 2)
  TC(2 6, words:("canoe";"tug";"raft";"rowboat";"ark";"liner"); (words?"raft";words?"submarine"))
  TC("a",*"abc")
  TC("abc", *("abc";"defg";"hijkl"))
  TC((,`pqr), *,,`pqr)
  TC(`pqr, *`pqr)
  TC(0,*!0)
  TC(0.0, *0#0.0)
  TC(" ",*0#"")
  TC(`, *0#`)
  TC(_n, *())
  //End of 'First', Begin 'Flip'
  TC((0 4 8;1 5 9;2 6 10;3 7 11) , +3 4#!12)
  TC(((0 1 2;4 5;10 11);(3;6 7 8 9;12)), a:((0 1 2;3);(4 5;6 7 8 9);(10 11;12)); +a )
  TC(((1;"C";`x);(2;"C";`y);(3;"C";`z)), +(1 2 3;"C";`x`y`z))
  TC(((1;"C";`x);(2;"C";`y);(3;"C";`z)), +(1 2 3;"CCC";`x`y`z))
  TC(`abc,+`abc)
  TC(67,+67)
  TC({x*y},+{x*y})
  TC(`a`b`c,+`a`b`c)
  TC(4, _ 4.6)
  TC(-5, _ -4.6)
  TC(0, 2=1.999)
  TC(1, 2=1.9999999999999999999)
  TC(0, 1=1.001)
  TC(1, 1=1.00000000000000000001)
  TC(1, _ 1.999)
  TC(2, _ 1.999999999999999999)
  TC((,"0";,"9") , $ 0 9)
  TC("123", $"123")
  TC("**", 2$2.345)
  TC("   abcd",  7$`abcd)
  TC("   2.35", 7.2 $ 2.345)
  TC(" 714.00", 7.2 $ 714)
#ifdef WIN32
  TC("1.2e-034", $ 1.2e-34) 
  TC("2.35e+000" , -9.2 $ 2.345)
  TC("7.14e+002" , -9.2 $ 714)
#else
  TC("1.2e-34", $ 1.2e-34) 
  TC("2.35e+00 " , -9.2 $ 2.345)
  TC("7.14e+02 " , -9.2 $ 714)
#endif
  TC(27, 0$"27")
  TC(3.4, 0.0$"3.4")
  TC(27.0 , 0.0$"27")
  TC("ab", " "$"ab")
  TC(`abc , `$"abc")
  TC((23;(23.4;`abc)), (0;(0.0;`)) $ ("23";("23.4";"abc")))
  TC( 4#$ _log 2 , 4#$ (_exp) ? 2)
  TC( 4#$_sqrt 2, 4#$ {x^2} ? 2)
  TC( 2 0 3 1, > 3 1 4 2)
  TC( 4 3 2 1 , 3 1 4 2[2 0 3 1])
  TC(0 2 1, > (8 1 6;3 5 7; 3 9 2))
  TC("zoned", a:"dozen"; a[>a])
  TC(2 1 0, > `aaa `bb `c)
  //End of 'Grade Down', Begin 'Grade Up'
  TC(1 3 0 2, < 3 1 4 2)
  TC(1 2 3 4, 3 1 4 2[1 3 0 2])
  TC("adept",a:"taped"; a[<a])
  TC((1 2 0;(3 5 7;4 9 2;8 1 6)),ms:(8 1 6;3 5 7;4 9 2); (<ms;ms[<ms]) )
  TC((0 2 3;1 4 5), = 2 1 2 2 1 1 )
  TC((,0;1 2 4;,3;,5;,6),="weekend")
  TC((0 2 5;1 4;,3),=(9 2 3;4 5;9 2 3;6 7 8;4 5;9 2 3))
  TC("e","abcdefg"@4)
  TC("faded","abcdefg"@ 5 0 3 4 3)
  TC(("fa";("d";,"ed")),"abcdefg"@(5 0;(3;,4 3)))
  TC(((7 4 9 2;3 5);(3 5;7 4 9 2;8 1 6)), (8 1 6;3 5;7 4 9 2) @ ( 2 1; 1 2 0))
  TC((2 3 4;("abcdefg";2 3 4)), d:.((`a;2 3 4);(`b;"abcdefg")); (d @`a; d @ `b`a))
  TC(((8 9;10;11 12);11 12;11) , d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20) ); (d . 1; d . 1 2; d . 1 2 0) )
  TC(11 11, d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20) ); ((((d @ 1)@2)@0);(d @/1 2 0)) )
  TC(((13 14;15 16 17 18);(1 2 3;4 5 6 7)), d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20) ); d . (2 0;0 1)  ) 
  TC((1 2 3;8 9;13 14), d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20) ); d . (;0)  ) 
  TC(((2 1;5 4);(14 13;16 15;20 19)), d:((1 2 3;4 5 6 7);(8 9;10;11 12);(13 14;15 16 17 18;19 20)); d . (0 2;;1 0) )
  TC(30,(1;.((`a; 2 3 4);(`b; 10 20 30 40))) . (1; `b; 2))
  TC(1 2 3, 1 2 3 . ())
  TC(10, 10 . ())
  TC(1 4 5 6 7, 1 , 4 5 6 7)
  TC(1 2 3 4, 1 2 3 , 4)
  TC(1 2 3 4 5 6 7, 1 2 3 , 4 5 6 7)
  TC((1;2;3;8 1 6;3 5 7;4 9 2), 1 2 3, (8 1 6;3 5 7;4 9 2))
  TC(0 0 0 1, 1 < -1 0 1 2)
  TC(1, "a" < "z")
  TC(0 1, "aA" < "Z")
  TC(0 1, `inch`mile < `foot`yard)
  TC(1,1 < 1.000000000001)
  TC(0,1 < 1.0000000000000000001)
  TC(1,0.999999999999 < 1)
  TC(0,0.999999999999999999 < 1)
  TC(x:.((`a;4 4);(`b;"aa"));x, .(. x))
  TC(1, 2 3 ~ 2 3)
  TC(0, () ~ !0)
  TC(0, "a" ~ ,"a")
  TC(8, 3|8)
  TC(3, 3| -8)
  TC(987.65, 123.45 | 987.65)
  TC(123.45, 123.45 | -987.65)
  TC(0 1 1 1, 0 0 1 1 | 0 1 0 1)
  TC(3, 3 & 8)
  TC(-8, 3 & -8)
  TC(123.45, 123.45 & 987.65)
  TC(-987.65, 123.45 & -987.65)
  TC(0 0 0 1, 0 0 1 1 & 0 1 0 1)
  TC(1 1 0 0, 1 > -1 0 1 2)
  TC(0, "a" > "z")
  TC(1 0, "aA" > "Z")
  TC(1 0, `inch`mile > `foot`yard)
  TC(1, 1.000000000001 > 1)
  TC(0, 1.0000000000000000001 > 1)
  TC(1,1 > 0.999999999999)
  TC(0,1 > 0.999999999999999999)
  TC(0 1,~ 1 0 )
  TC(0 1 0, ~ 4.6 0 -4.6)
  TC( ~`c , `c.) // Attribute in Not/Attribute
  //End of 'Not/Attribute', begin 'Plus'
  TC(8.0, 2^3)
  TC(-8.0, -2^3)
  TC(1.0, 0^0)
  TC(4.0,-2.0^2)
  TC(3#$1.414214,3#$ 2.0^0.5)
  TC(0i,10^1000)
  TC(9 6 8 7, ? 9 6 8 6 9 7 8 9 6)
  TC("strange",?"strange")
  TC("racon",?"raccoon")
  TC((9 2 3;4 5;6 7 8), ? (9 2 3;4 5;9 2 3;6 7 8;4 5;9 2 3))
  TC(2.0 , %% 2) //Kind of cheating here. C parser complains on single %
  TC(1.0, %%1)
  TC(0.0, %%0)
  TC(0i, %0)
  TC(1, 0i>3.13)  // test for issue #230
  TC(0N, 0%0)             
  TC(2 4 1 3, |3 1 4 2)
  TC(m:(8 1 6;3 5 7;4 9 2); |m , (4 9 2;3 5 7;8 1 6))
  TC(`three, *|`one`two`three)
  TC("a",|"a")
  TC(!0, |!0)
  TC((,3 1 4 2), |,3 1 4 2)
  TC(2,5 ! 3)
  //TC(0.0, 2.4 ! 0.4) //yields 0.4 in K4
  TC(0.0,2.0 ! 1.0)
  TC(2.0,5.0 ! 3)
  TC(("  0";"0.1") , 3 $  1.8 -2.7 ! 0.2) 
  TC(-1,5 ! -3)
  TC(-1.0, 5.0 ! -3.0)
  TC(-3 0 -1, -3 4 -17 ! -4)
  TC(-3 0 -1.0, -3.0 4 -17 ! -4)
  TC("fghabcde", 5 ! "abcdefgh")
  TC("fghabcde", 21 ! "abcdefgh")
  TC("defghabc", -5 ! "abcdefgh")
  TC("defghabc", -21 ! "abcdefgh")
  TC(!0, ^ 3.14)
  TC(4 3 2, r:(("ab";"cd";"ef");("gh";"ij";"kl");("mn";"op";"qr");("st";"uv";"wx"));^r) //see refactor below
  TC(4 3, s:(("aby";"cd";"ef");("gh";"i";"kl1");("mn";"opz";"qr");("st";"uv";"w"));^s)
  TC((,4), t:(("ab";"cd";"ef");("gh";"ij");("kl";"mn";"op";"qr");("st";"uv";"wx"));^t)
  TC(4 5 6 , 3 # 4 5 6 7 8 9)
  TC(7 8 9, -3 # 4 5 6 7 8 9)
  TC(4 5 6 7 8 9 4 5, 8 # 4 5 6 7 8 9)
  TC(9 4 5 6 7 8 9 4 5 6 7 8 9, -13 # 4 5 6 7 8 9)
  TC(1, r:(("ab";"cd";"ef");("gh";"ij";"kl");("mn";"op";"qr");("st";"uv";"wx")); r ~ 4 3 2 # "abcdefghijklmnopqrstuvwxyz")
  TC(2 3 4 0, ^ 2 3 4 0 7 8 # "abc")
  TC(("ab";"cd"), 2 -1 # "abcd" )
  TC(("abcd";"efgh"), 2 -1 # "abcdefgh")
  TC(2 2,^ 2 -1 # "abcd" )
  TC(2 4,^ 2 -1 # "abcdefgh")
  TC((,"abc";,"def"), 2 -1 3 # "abcdef")
  TC((("abc";"def");("ghi";"jkl")), 2 -1 3 # "abcdefghijkl")
  TC(2 1 3, ^ 2 -1 3 # "abcdef")
  TC(2 2 3, ^ 2 -1 3 # "abcdefghijkl")
  TC(14, ."2+3*4")
  TC(2 4 7, & 0 0 1 0 1 0 0 1)
  TC( 0 0 0 2 2 2 2, & 3 0 4)
  TC(0 0 0, &3)
  //End 'Where'. End Verbs.
  //Begin Adverbs. Begin 'Each'
  TC_("(!6;!4;!5)","!:' 6 4 5")
  TC_("6 4 5"," #:'!:'6 4 5")
  TC_("(1 0;2 1 0)", "|:'!:'2 3")
  TC_("15 6 10", "+/' !:'6 4 5")
  TC_("\"ab\"", "\"a\" ,' \"b\"")
  TC_("1", "(\"a\",'\"b\") ~ \"a\" , \"b\"")
  TC_("(\"ab\";\"ac\";\"ad\")"," \"a\" ,' \"bcd\"")
  TC_("(\"ad\";\"bd\";\"cd\")"," \"abc\" ,' \"d\"")
  TC_("(\"ad\";\"be\";\"cf\")"," \"abc\" ,' \"def\"")
  TC_("1", "((,\"a\") ,' (,\"b\")) ~ ,\"ab\"")
  //Begin 'Each Left'
  TC((2 5 6 7;3 5 6 7;4 5 6 7), 2 3 4 ,\\: 5 6 7)
  TC_("3 5 5 11 11", "-': 1 4 9 14 25 36")
  TC((2 3 4 5;2 3 4 6;2 3 4 7) , 2 3 4 ,/:  5 6 7)
  TC(3, 1 7 2 4 6 10 3 ? 4)
  TC(7, 1 7 2 4 6 10 3 ? 4 3)
  TC(3 6, 1 7 2 4 6 10 3 ?/: 4 3)
  TC(16,10+/1 2 3)
  TC(6,+/1 2 3)
  TC(1, +/1)
  TC(1, +/,1)
  TC(9, |/1 4 -6 9 1 3)
  TC(-6 , &/ 1 4 -6 9 1 3)
  TC_("(\"a\";1;2;`bc;\"x\";\"y\";\"z\";2.35)",",//(\"a\";(1 2;`bc;(\"xyz\";2.35)))") //TC_ for raze ",//" (not comment)
  TC_("25.0", "|/,//(1;(2.3 25.0;(6 7 -9;10)))" )
  TC_("-9.0", "&/,//(1;(2.3 25;(6 7 -9;10)))" )
  TC(1 4 9 16, +\\ 1 3 5 7)
  //End Adverbs
  //Amend, Index, Apply & Assign
  TC(6 7 8, a:1 2 3;a+:5; a)
  TC((0 2 4;1 3 5) , b: 3 2 # ! 6; r: b +:; b ) //manual has error
  TC(((101 2 103;103 4 105);(103 105;101 103))  , a:(1 2 3;3 4 5); r: a[1 0;0 2] +: 100; (a;r) ) 
  TC((10 11 12 13;(`ab;"cde");-8 -9 -10 -11), a:3 4#!12; a[1]:(`ab;"cde"); a[0]+:10;a[2] -:;a  ) 
  TC((101 2 103 4;101 103 ) ,a:1+!4;r: a[0 2] +: 100; (a;r) ) 
  TC( (.,(`a;(`b;"c"))) , d:.,(`a;1 2 3); d[`a]: (`b;"c"); d)
  TC_(".((`a;1;)\n  (`b;1;)\n  (`c;1;))","a[`a`b`c]:1;a")
  TC(10 20 30, a:3 3#!9; a[;1]:10 20 30)
  TC(10 20 30, a:3 3#!9; a[1;]:10 20 30)
  TC(3 4, d:.((`a;1);(`b;2)); d[!d]:3 4)
  TC(3 4, d:.((`a;1);(`b;2)); .[`d;_n;:;3 4]; d[!d])
  TC(3 4, d:.((`a;1);(`b;2)); @[`d;_n;:;3 4]; d[!d])
  TC(3 4, d:.((`a;1);(`b;2)); d[]:3 4; d[!d])

  //System Functions
  TC(2#1.0, _abs - _cos 0 0)

  //Names
  TC( a.b_c.d:1;a.b_c.d, 1)

  TC_("#:'(1;,1;1 2;1 2 3)","1 1 2 3") 
  TC_("skip", "#'(1;1 2)"  "(#[1;];#[1 2;])")  //This is true, and will pass, but set to skip until projections are actually compared correctly
  TC_("a:(#'3 4 6)[0]; a 9", "9 9 9")
  TC_(".'\"98\"", ".'\"98\"") // .'"98" was causing crash bug. now projects  

  TC(1, 4 _in 1 7 2 4 6 3)
  TC(0, 4 3 _in 1 7 2 4 6 3)
  TC(1 1, 4 3 _in\\: 1 7 2 4 6 3)
  TC(skip,("abcdefg";"bdf"), dir:.((`a;2 3 4);(`b;"abcdefg")); (`dir . `b; `dir . (`b;1 3 5)) )  //dename without path creation
  TC(skip,(1 2 3;"2+3";`button), c:.((`a;1 2 3);(`b;"2+3";.,(`c;`button))); (c.a;c.b;c.b..c))

  TC(skip, 1 2, d:_n;d. 1 2) // d . or d.  ?
  TC(skip, _n, ."r:2+3*4")
  //TC_("\"..\"" , "a:\"The quick. Brown fox.\"; b: & {(x=\" \") & y=\".\"}': a; a[b]")
  TC(3, {x-2} 5)
  TC(9.0, {x^2} @ 3)
  TC(skip, 15, f:{a:10; :x+a; a:20}; f[5])
  TC( 12 23, a:2 3;b:10 20; {a+b} . ,_n)
  TC(skip, "{[x;y] x+y}", ${[x;y] x+y})
  TC(6, {x+y+z}[1;2;3])
  TC(6, g:{x+y+z}[1;;3]; g[2])
  TC(6, h:{x+y+z}[1]; h[2;3])
  TC(6, e:{x+y+z}[;2]; e[1;3])
  TC(skip, (,5; 12 6 3) , f:{:[x ! 2; x; _ x %% 2]} ; (f\\ 5; f\\ 12) )
  TC(skip, 640640 320320 160160 80080, b:{x>100000}; b f\\ 640640)
  TC(skip, 2, {}$"1+1")
  TC(("canoe";`dinghy;"kayak";66545;{x+y}) , ("canoe";`dinghy),("kayak";66545;{x+y})   )
  TC(skip, 1++, 1+a:+)  //Should seven_types merge sub-seven_types ?
  TC(skip, +\\(!2;!3))  //error (adverbs check intermediate values)
  TC((1;"index"), @[.:;"1+/() 1 2 3";:]) 

  TC(640640, f:{:[x!2;x;_ x*0.5]}; (640640<) f/640640)
  TC(320320, f:{:[x!2;x;_ x*0.5]}; (640639<) f/640640)
  TC(5005,   f:{:[x!2;x;_ x*0.5]}; (10000<) f/640640)
  TC(5005,   f:{:[x!2;x;_ x*0.5]}; {x>10000} f/640640)

  TC((,640640), f:{:[x!2;x;_ x*0.5]}; (640640<) f\\640640)
  TC(640640 320320, f:{:[x!2;x;_ x*0.5]}; (640639<) f\\640640)
  TC(640640 320320 160160 80080 40040 20020 10010 5005,f:{:[x!2;x;_ x*0.5]}; (10000<) f\\640640)
  TC(640640 320320 160160 80080 40040 20020 10010 5005,f:{:[x!2;x;_ x*0.5]}; {x>10000} f\\640640)

  TC(1, a:`a`b`c!3 3#!9;b:.((`a;0 1 2);(`b;3 4 5);(`c;6 7 8));a~b )

  TC((1;"type") , @[.:;"_sin _sin (;)";:])
  TC((1;"type") , @[.:;"_sin _sin (0;)";:])

  TC((1;"domain") , @[.:;".`\"1\"";:])
  TC((1;"domain") , @[.:;".`\" \"";:])
  TC((1;"domain") , @[.:;".`\"Ŕ̡̢͘͝x̀2́̀H̨̀͘l͘͞͞͠v̶̡͘͡k̷̡͞͝ ̛(̨͏͏͞Q̧͏y̵̴͘͠͡,̶̴̴̕͞\"";:])

  TC(_n,.`"f") 
  TC(_n,.`".f") 
  TC(_n,.`".k.xyz") 
  TC(_n,.`".k.xyz.") 
  TC(_n,.`".k.xyz.b") 
  TC(_n,.`".k.xyz..b") 

  TC(_ssr[1], _ssr 1)
  TC(_ssr[1;2], (_ssr 1) 2)
  TC(_ssr[()], _ssr ())
  TC(_ssr[("this";"is";,"at")], _ssr ("this";"is";,"at"))
  TC(_ssr[("this";"is";,"at")], _ssr ("this";"is";"at"))
  TC("that", _ssr["this";"is";,"at"])
  TC("that", _ssr["this";"is";"at"])
  TC("asdf", _ssr["gsdf";"g";,"a"])
  TC("asdf", _ssr["gsdf";"g";"a"])

  //Regressions
  TC(2, _2.5)

  //Early Return
  TC(:, :)
  TC(1, :1)
  TC(1, :1;2;3)
  TC(2, 1;:2;3)
  TC(1 2 3, (1;2;3))
  TC(2, 0 5 9[:2])
  TC(10, 0 5 9[:10])
  TC(20, 0 5 9[:10;:20])
  TC(20, 0 5 9[(:10;:20)])
  TC(40, 0 5 9[(:10;:20);(:30;:40)])
  TC(3, (;1;:2;:3))
  TC(3, :[(:1;:2;:3);:4])
  TC(4 5 6 7, :4,5,6,7)
  TC(4, (:1\n:2\n:3\n:4))
  TC(1, {:1\n:2\n:[:4;:5]\n:6}0)
  TC(4, {:[:4;:5]}0)
  TC(5, while[:5;:6])
  TC(6, while[5;:6])
  TC(6, do[5;:6])
  TC(5, do[:5;:6])
  TC(6, if[5;:6;:7;:8])
  TC(6, if[5;(:3;:2;:6);:7;:8])
  TC(4, while[1;if[5;do[6;:4]]])
  TC({:1}, :[5;{:1}])
  TC(5, :[5;:[6;5;{1+4;(:1;:2;:3)}0]])
  TC(3, {1+{1;:2;1+x}0}0)
  TC(0, {1+{1;:2;1+x}0;7;0}0)
  TC(7, {1+{1;:2;1+x}0;:7;0}0)
  TC(32.0, {1;:4*2^3;1}0)
  TC(2, a:1;b:2;c:3;a;:b;c)
  TC(2, {[d]d:4;e:5;f:6;a:1;b:2;c:3;a;:b;c}0)
  TC(4, {[d]d:4;e:5;f:6;a:1;b:2;c:3;a;:d;c}0)
  TC(2, {x;4;e:5;f:6;a:1;b:2;c:3;a;:b;c}0)
  TC(4, {x:4;e:5;f:6;a:1;b:2;c:3;a;:x;c}0)
  TC(4, f:{x:4;e:5;f:6;a:1;b:2;c:3;a;:x;c};f 0;f 0)
  TC(2, f:{x;4;e;5;f:t;a:1;b:2;c:3;a;:b;c};f 0;f 0)
  TC(5, {b::4; b:5; b}0)   // verify that global assign not interpreted as early return
  TC(5, {b:4; b::5; b}0)

  //overMonad
  TC("abcd" ,("abcd";"efgh")/0)
  TC("efgh" ,("abcd";"efgh")/1)
  TC("b" ,("abcd";"efgh")/0 1)
  TC("g" ,("abcd";"efgh")/1 2)
  TC(0, (%[;2])/9999)
  TC(0.0, (%[;2.0])/9999.0)

  //stack counter
  TC(skip, 500, {:[x>0;1+_f[x-1];0]}500)                    // works but is slow
  TC(skip, stack error, {:[x>0;1+_f[x-1];0]}500)            // fails with stack error
  TC(skip, 500, rcr:{:[x>0;1+_f[x-1];0]}; rcr 1000)         // works but is slow 
  TC(skip, stack error, rcr:{:[x>0;1+_f[x-1];0]}; rcr 1001) // fails with stack error
  TC(skip, stack error, . t : ". t")                        // fails with stack error

  TC((1;"syntax") , @[.:;"0-8^-4&1/::=-";:]) //if we ever handle this, then won't be error

  TC((0 0;0 1;1 0;1 1) ,!2 2)
  TC(a:2 1 3; a _vs/:!*/a, !2 1 3)
  TC(3 2, 1 2 3 4 (2 1))
  TC(2 1, x:!10; x 2 1)
  TC(3 2, y:2 1; 1 2 3 4 y)

  R 0;
}


//K x; K zero=Ki(0); K a=Ki(9);
//K b = enumerate(a); K c = times(b,b); K d = plus(b,c); K e = times(enlist(promote(a)), promote(a));
//K f=newK(0,2); kK(f)[0]=b; kK(f)[1]=e; K empty=newK(0,0);
//K g=newK(0,4); kK(g)[0]=f; kK(g)[1]=empty; kK(g)[2]=f; kK(g)[3]=enumerate(zero);
//K h=enlist(enlist(enlist(enumerate(Ki(5)))));
//K i=plus(Ki(2),enumerate(Ki(3))); K j=take_reshape(reverse(plus(Ki(3),enumerate(Ki(3)))),enumerate(Ki(10)));
//K k=join(join(Ki(2),Kf(3.0)),Kc('c')); K l=take_reshape(Ki(3),Kc('d'));

//TODO: add a test to make sure wordfuncs (,/:) and (,\:) work and have valence 2
 /*
 show(enumerate(Ki(10)));
 show(format_dyadic(Ki( 10),Kf(123.45678)));
 show(format_dyadic(Ki(-10),Kf(123.45678)));
 show(format_dyadic(Ki(3),Kf(123.45678)));
 printf("%.*f\n",9, 123.123456789012345 );
 */
 /*
 show(Kf(1.2));
 show(format(Kc('c')));
 show(format(Kf(1.123456789)));
 show(format(Ks(sp("what"))));
 show(count(format(Ks(sp("ok"))))); 
 show(count(format(Ks(sp("o"))))); 
 show(count(format(Ks(sp(""))))); 
 */

 /*
 K z=newK(0,3);
 DO(z->n, kK(z)[i]=enumerate(Ki(10))) 
 K e2=plus(Ki(1),enumerate(Ki(2)));
 K je=join(enlist(join(enlist(e2),enlist(e2))),enlist(e2));
 show(je);
 show(dot(z,je));
 */

  /*
  //Dictionaries
  K dict=newK(0,2);
  kK(dict)[0]=newK(0,2);
  kK(dict)[1]=newK(0,2);
  x=kK(dict)[0];
  kK(x)[0]=Ks(sp("a"));
  kK(x)[1]=Ki(9);
  x=kK(dict)[1];
  kK(x)[0]=Ks(sp("b"));
  kK(x)[1]=Kf(4.5);
  //show(dict);
  K gd=dot_monadic(dict);
  //show(gd); P("%d\n", isKey(gd,sp("a"))); P("%d\n", isKey(gd,sp("b"))); P("%d\n", isKey(gd,sp("c")));
  
  K d2=newK(0,2);
  kK(d2)[0]=cl0ne(kK(dict)[0]);
  K b2=newK(0,3);
  kK(b2)[0]=Ks(sp("f"));
  kK(b2)[1]=Kc('r');;
  kK(b2)[2]=dot_monadic(dict);
  kK(d2)[1]=b2;

  K d3=newK(0,2);
  kK(d3)[0]=join(Ks(sp("a")),Ks(sp("b")));
  kK(d3)[1]=join(Ks(sp("c")),Ks(sp("d")));

  //show(d2);
  //show(dot_monadic(d2));
  //show(dot_monadic(dot_monadic(d2)));
  //show(dot_monadic(drop_cut(Ki(-1),d2)));
  show(dot_monadic(d3));//Also a valid dictionary
  show(at_verb(dot_monadic(dict), Ks(sp("a"))));
  show(at_verb(dot_monadic(d2), Ks(sp("f"))));
  show(at_verb(dot_monadic(d2), Kn()));

  show(dot_monadic(d2));
  show(dot(dot_monadic(d2),Kn()));
  */

  //DO(10,dd(rp2(i)))
  //printf("%d\n", rp2(-1)); printf("%d\n", rp2(0)); printf("%d\n", rp2(1)); printf("%d\n", rp2(2)); printf("%d\n", rp2(3)); printf("%d\n", rp2(4));
  //printf("%d\n", rp2(5)); printf("%d\n", rp2(6)); printf("%d\n", rp2(7)); printf("%lld\n", rp2(8));
  //printf("%lld\n", rp2(8000000000)); printf("%lld\n", rp2(5223372036854775805));
  /*
  //Not/Attribute
  K ps = Ks(sp("ok"));
  K pt = Ks(sp("duh"));
  K pu = join(join(ps,pt),pt);
  show(pu);
  K pv=not_attribute(pu);
  show(pv);
  P("%d %d\n", kS(pv)[1], kS(pv)[2]);
  K pw=not_attribute(plus(Kf(-1.0),enumerate(Ki(10))));
  show(not_attribute(pw));
  show(not_attribute(join(pu,pw)));
  show(not_attribute(not_attribute(pw)));
  */

  //show(range(l));
  //show(range(b));
  //show(range(g));

  /*
  K fc=newK(-3,-1);
  C* fcs="weekender";
  fc->n=strlen(fcs);//don't do this
  ke(fc)=fcs;//or this
  show(fc);
  show(group(fc));  
  //run grade_up on this to test low/high chars: "abcdefghi\001\002\xff\xfe";
  K ic=newK(-1,6);
  kI(ic)[0]=2; kI(ic)[1]=1; kI(ic)[2]=2; kI(ic)[3]=2; kI(ic)[4]=1; kI(ic)[5]=1;
  show(ic);
  show(group(ic));
  K gsc=enlist(Kc('c')); show(sc); show(group(sc));
  */

  /*
  //Grades
  I di=10,dj=5;
  K ds=(plus(Ki(5),enumerate(Ki(di))));
  ds = join(ds,reverse(ds));
  show(ds);
  show(distributionGrade(ds,0,0,di+dj));
  show(mergeGrade(ds,0));
  show(mergeGrade(ds,1));
  show(grade_up(ds));
  show(grade_down(ds));
  //Character Vector Grade
  K cc=join(Kc('c'),Kc('d'));
  cc = take_reshape(Ki(20),cc);
  show(cc);
  show(charGrade(cc,1));
  show(distributionGrade(newK(-1,0),0,0,0));//Empties
  show(mergeGrade(newK(0,0),0));
  show(charGrade(newK(-3,0),0));
  */
  

  //show(Ks(sp("gg"))); show(Ks(sp("xx"))); show(Ks(sp("aa"))); show(Ks(sp("bb")));
  /*
  S z; z=""; P("%s - %s\n", z, sp(z)); 
  #include <time.h>
  time_t seconds;
  time(&seconds);
  srand((unsigned int) seconds);
  char ok[255];
  I y;
  for(y=0;y<999888;y++)
  {
    I x;
    sprintf(ok,"%d\0", (x=rand()) % 100);
    if(0 == (y % 100000))P("%d\n",y);
    sp(ok);
    if(strcmp(ok,sp(ok))){P("VERY BAD!\n");break;}
  }
  pt(SYMBOLS);
  */

 /*
  show(minus(d,c)); show(divide(d,c)); show(divide(zero,a)); show(divide(a,zero));
  show(atom(zero)); show(atom(enlist(zero)));
  show(join(b,divide(d,c))); show(join(a,b));
  show(enlist(enumerate(Ki(1))));
  show(negate(a));show(negate(zero));show(reverse(negate(g)));
  show(where(zero)); show(where(a)); show(reverse(where(b))); show(where(d));
  show(reciprocal(negate(b)));
  show(drop_cut(Ki(9),enumerate(Ki(15)))); show(drop_cut(Ki(-9),enumerate(Ki(15)))); show(drop_cut(Ki(2),enumerate(Ki(15)))); 
  show(drop_cut(Ki(-2),enumerate(Ki(15)))); show(drop_cut(Ki(15),enumerate(Ki(15)))); show(drop_cut(Ki(15),enumerate(Ki(0))));
  show(drop_cut(join(Ki(2),join(Ki(2),Ki(8))),enumerate(Ki(10))));
  show(drop_cut(join(Ki(2),join(Ki(10),Ki(12))),join(enumerate(Ki(10)),Kf(3.4) )));//Error. 12>b->n
  show(rotate_mod(enumerate(Ki(0)),Kf(0)));
  show(rotate_mod(enumerate(Ki(5)),Ki(0)));
  show(rotate_mod(enumerate(Ki(5)),Ki(3)));
  show(rotate_mod(negate(enumerate(Ki(5))),Ki(3)));
  show(rotate_mod(enumerate(Ki(5)),Kf(0.0)));
  show(rotate_mod(enumerate(Ki(5)),Kf(1.333)));
  show(rotate_mod(negate(enumerate(Ki(5))),Kf(2.5)));
  show(equal(Ki(3),enumerate(Ki(10)))); show(equal(enumerate(Ki(9)),Kf(4.000000000000000000001)));
  show(equal(enumerate(Ki(10)),enumerate(Ki(10)))); show(equal(Kc('a'),Kc('s'))); show(equal(Kc('s'),Kc('s')));
  show(floor_verb(enlist(join(Kf(1.99999999),Kf(-1.000000000000000001)))));
  show(shape(enumerate(Ki(3)))); show(shape(enlist(enumerate(Ki(3))))); 
  show(shape(join(join(h,h),h)));
  show(shape(join(join(h,h),enlist(enlist(b)))));
  show(take_reshape(Ki(0),Kf(9)));
  show(take_reshape(Ki(10),Kf(9)));
  show(take_reshape(Ki(0),newK(0,0)));
  show(take_reshape(Ki(5),newK(-3,0)));
  show(take_reshape(Ki(-4),enumerate(Ki(15))));
  show(take_reshape(Ki(-4),first(enumerate(Ki(15)))));
  show(take_reshape(enlist(Ki(0)),enumerate(Ki(10))));
  show(take_reshape(enumerate(Ki(0)),enumerate(Ki(10))));
  show(take_reshape(join(i,Ki(0)),Kc('c')));
  show(take_reshape(join(i,Ki(0)),Ki(0)));
  show(take_reshape(join(i,Ki(0)),Kn()));
  show(take_reshape(i,Kn()));
  show(join(Kn(),Kn()));
  show(join(take_reshape(Ki(2),Kn()),join(Kn(),Kn())));
  show(plus(j,j)); show(times(j,Ki(3)));
  show(max_or(j,Kf(3.1))); show(min_and(j,Ki(1)));
  show(shape(h));
  show(join(enumerate(Ki(10)),enlist(enlist(Ki(3)))));
  */
  //show(flip(flip(join(join(enlist(k),enlist(k)),enlist(k)))));
  //show(flip(join(join(join(enlist(k),enlist(k)),enlist(k)),l)));
  //show(flip(join(join(join(enlist(k),enlist(k)),enlist(k)),Kc('d'))));
  //show(match(zero,zero));
  //show(match(enumerate(Ki(3)),plus(Ki(1),enumerate(Ki(3)))));
  //show(match(b,b)); show(match(plus(Ki(1),b),plus(Kf(1.0),b)));
  //show(find(b,Kf(3.0))); show(find(b,Ki(20))); show(find(g,f));
  //show(less(reverse(b),b)); show(less(reverse(b),Kf(4.5)));
  //show(more(reverse(b),b)); show(more(reverse(b),Kf(4.5)));

  //S ga="asasaa"; S gb="asasaa"; S gc="cccccc"; printf("%d %d %d", ga, gb, gc);//GCC sourcefile string interning
  //show(less(sa,sb)); show(more(sa,sb));

  //V wsomesyms[] = {Ks(sp("a")),join,Ks(sp("b")),join,Ks(sp("c")),'\0'};
  //K somesyms = ex(wsomesyms);
  //K verb = newK(7,1);
  //kV(verb)[0] = dot_monadic;
  //show(verb);
  //V words[] = {i,minus,i,plus,i,'\0'};
  //V words[] = {i,minus,i,plus,enumerate,Ki(3),'\0'};
  //V words[] = {i,join,each,i,'\0'};
  //V words[] = {i,join,eachleft,eachright,i,'\0'};
  //V words[] = {somesyms,join,eachright,eachleft,i,'\0'};
  //V words[] = {enumerate,each,i,'\0'};
  //V words[] = {d,Ki(-18),plus,first,reverse,first,Ki(2),plus,flip,flip,enlist,enumerate,Ki(10),times,i,Ki(0),'\0'};
  //V words[] = {where,each,Ki(10),'\0'};
  //V words[] = {Ki(5),minus,eachpair,enumerate,Ki(0),'\0'};
  //V words[] = {Ki(5),minus,eachpair,enumerate,Ki(1),'\0'};
  //V words[] = {Kf(5),minus,eachpair,reverse,Kf(2),power,enumerate,Ki(6),'\0'};
  //V words[] = {Kf(5),plus,over,Ki(1),plus,enumerate,Ki(3),'\0'};
  //V words[] = {Kf(5),plus,over,newK(0,0),'\0'};
  //V words[] = {Kf(5),plus,over,enlist,Ki(1),'\0'};
  //V words[] = {Kf(5),plus,over,Ki(1),'\0'};
  //V words[] = {newK(-4,0),plus,over,enumerate,Ki(3),'\0'};//should error
  //V words[] = {newK(0,0),plus,over,enumerate,Ki(3),'\0'};

  //V words[] = {Kf(5),plus,scan,Ki(1),plus,enumerate,Ki(3),'\0'};
  //V words[] = {Kf(5),plus,scan,newK(0,0),'\0'};
  //V words[] = {Kf(5),plus,scan,enlist,Ki(1),'\0'};
  //V words[] = {Kf(5),plus,scan,Ki(1),'\0'};
  //V words[] = {newK(-4,0),plus,scan,enumerate,Ki(3),'\0'};//should error
  //V words[] = {newK(0,0),plus,scan,enumerate,Ki(3),'\0'};


  //V words[] = {negate,over,Ki(10),'\0'};
  //V words[] = {negate,over,Ki(10),'\0'};
  //V words[] = {flip,over,Ki(10),'\0'};
  //V words[] = {negate,scan,Ki(1),plus,enumerate,Ki(3),'\0'};
  //V words[] = {flip,scan,Ki(10),'\0'};
  //V words[] = {negate,scan,Ki(1),plus,enumerate,Ki(3),'\0'};
  //V words[] = {Ki(5),plus,scan,enlist,Ki(1),'\0'};
  //V words[] = {max_or,over,Ki(0), '\0'};
  //V words[] = {Ki(10),take_reshape,Ki(1),'\0'};
  //V words[] = {max_or,over,Ki(0),take_reshape,Ki(1),'\0'};
  //V words[] = {plus,over,Ki(0),take_reshape,Ki(1),'\0'};
  //V words[] = {min_and,over,Ki(0),take_reshape,Ki(1),'\0'};
  //V words[] = {times,over,Ki(0),take_reshape,Ki(1),'\0'};
  //V words[] = {max_or,over,Ki(0),take_reshape,Kf(1),'\0'};
  //V words[] = {plus,over,Ki(0),take_reshape,Kf(1),'\0'};
  //V words[] = {min_and,over,Ki(0),take_reshape,Kf(1),'\0'};
  //V words[] = {times,over,Ki(0),take_reshape,Kf(1),'\0'};
  //V words[] = {max_or,over,Ki(1),join,Ki(4),join,Ki(-6),join,Ki(9),join,Ki(1),join,Ki(3),'\0'};
  //V words[] = {Ki(10),max_or,over,Ki(1),join,Ki(4),join,Ki(-6),join,Ki(9),join,Ki(1),join,Ki(3),'\0'};
  //V words[] = {Ki(10),max_or,over,Ki(0),take_reshape,Ki(0),'\0'};
  //V words[] = {join(plus(Ki(2),enumerate(Ki(3))),take(Ki(2),Ki(0))),over,Ki(0),'\0'};
  //V words[] = {join(plus(Ki(2),enumerate(Ki(3))),take(Ki(2),Ki(0))),scan,Ki(0),'\0'};
  //V words[] = {join,over,over,join(enlist(join(enlist(join(Ki(1),Ki(2))),enlist(join(Ki(3),Ki(4))))),enlist(join(enlist(join(Ki(5),Ki(6))),enlist(join(Ki(7),Ki(8)))))), '\0'};

  //show(ex(words));

  //V words[] = {enumerate(Ki(6)),plus,eachpair,enumerate(Ki(3)), '\0'};

  //show(ex(sw("1+2+3")));
  //show(ex(sw("-1+2+3")));
  //show(ex(sw("6---1+2+3")));
  //show(ex(sw("|\\1,4,0,9,1,3")));
  //show(ex(sw("3|\1,4,0,9,1,3")));
  //show(ex(sw("1,,,,,,2,,,,,,3,,,,,,,4,,,,,,,,5,,,,,6")));
  //show(ex(sw(",/1,,,,,,2,,,,,,3,,,,,,,4,,,,,,,,5,,,,,6")));
  //show(ex(sw(",//1,,,,,,2,,,,,,3,,,,,,,4,,,,,,,,5,,,,,6")));
  //show(ex(sw("+\\1,2,3,4,5")));
  //show(ex(sw("4+\\1,2,3,4,5")));
  //show(ex(sw("3,//1,,,,,,2,,,,,,3,,,,,,,4,,,,,,,,5,,,,,6")));
  //show(ex(sw("3-:\\1")));
  //show(ex(sw("-:\\1")));

  //K mf= Kv(); kV(mf)[2] = calloc(10,sizeof(V)); kW(mf)[0] = reciprocal; kW(mf)[1] = reverse; kW(mf)[2] = negate;
  //K mf2= Kv(); kV(mf2)[2] = calloc(10,sizeof(V)); kW(mf2)[0] = reciprocal; kW(mf2)[1] = negate; kW(mf2)[2] = plus; 
  //K mo3 = Kv(); kV(mo3)[2] = calloc(10,sizeof(V)); kW(mo3)[0] = negate; kW(mo3)[1] = negate; kW(mo3)[2] = negate;
  //K mf3= Kv(); kV(mf3)[2] = calloc(10,sizeof(V)); kW(mf3)[0] = reverse; kW(mf3)[1] = mo3; kW(mf3)[2] = over; kW(mf3)[3] = negate; kW(mf3)[4] = plus;

  //V words[] = {mf,ex(sw("1,2,3")),'\0'};
  //V words[] = {mf,enumerate(Ki(2)),'\0'};
  //V words[] = {enumerate(Ki(5)),negate,mf3,over,enumerate(Ki(3)),'\0'};
  //V words[] = {Ki(1),plus,mf2,scan,Ki(1),plus,enumerate(Ki(2)),'\0'};
  //V words[] = {mo3,scan,enumerate(Ki(3)),'\0'};

  //V words[] = {mf3,scan,enumerate(Ki(3)),'\0'};
  //show(ex(words));

  //show(ex(sw("1+,,,///:")));
  //K mf4= Kv(); kV(mf4)[2] = calloc(10,sizeof(V)); kW(mf4)[0] = ex(sw("1+2+3+//,/:/")); kW(mf4)[1] = mo3; kW(mf4)[2] = over; kW(mf4)[3] = negate; kW(mf4)[4] = plus;
  //show(mf4);

//show(what(ex(sw("3*")),ex(sw("1,2,3"))));
//show(what(ex(sw("3*1-")),ex(sw("1,2,3"))));
//show(what(ex(sw("3*1-")),ex(sw("1,2,3"))));

//show(vf_ex(what,join(ex(sw("3+")), ex(sw("7,2")))));
//show(vf_ex(what,join(ex(sw("1+2+3+")), ex(sw("1,4")))));
//show(vf_ex(what,join(ex(sw("1+2+3+")), ex(sw("1,,4,4")))));

//show(what_triadic(ex(sw("3+")),ex(sw("0")),ex(sw("1")) ));
//show(what_triadic(ex(sw("3+")),ex(sw("0")),ex(sw("1,2")) ));
//show(what_triadic(ex(sw("3+")),ex(sw("0,1")),ex(sw("1")) ));
//show(what_triadic(ex(sw("3+")),ex(sw("0,1")),ex(sw("1,2")) ));
//show(what_triadic(ex(sw("3+")),ex(sw("0,1")),newK(0,0)));
//show(what_triadic(ex(sw("3+")),ex(sw("0,1")),newK(-1,0)));

  //I wn = 3; V* words = malloc(1+wn*sizeof(V*)); words[wn]=0; free(words);

  //V words[] = {sp("a"),,i,'\0'};
  //preventing us from making words a K: variables (a:5+a:2), verbs.
  //maybe there is a way: raw symbol pointer in type7 is implicit symtree/ktree lookup?

  //show(ex(sw("1")));
  //show(dot(ex(sw("+")),ex(sw("1,2"))));
  //show(KTREE);show(_d);


  //all this glue/isGlobalHandle/unhand business was replaced with dename functions
  //dd(isGlobalHandle(Ks(sp("a."))))
  //show(glue(Ks(sp("")),Ks(sp("aaa"))));
  //show(glue(Ks(sp(".k")),Ks(sp("aaa.m"))));
  //show(glue(Ks(sp(".k.m.n")),Ks(sp("aaa"))));
  
  //K* kz = unhand(Ks(sp(".k"))); *kz=ex(sw("1+1")); show(*kz); show(KTREE); //WARNING - no free; wrecks .`.k

  //unhandOrCreate(Ks(sp(".a.b")));
  //unhandOrCreate(Ks(sp(".a.b")));
  //unhandOrCreate(Ks(sp(".a.c")));
  //unhandOrCreate(Ks(sp(".a.b.d")));
  //unhandOrCreate(Ks(sp(".a.c.e.f.g.h")));
  //show(KTREE);

  //show(at_tetradic(_(1,2),_(0),_(-:),0));  
  //show(at_tetradic(_(1,2),_(0,1),_(-:),0));  
  //show(at_tetradic(_(1,2),_(0,0),_(-:),0));  
  //show(at_tetradic(Ks(sp("")),Ks(sp("abc")),_(:),_(1)));  show(KTREE);
  //show(at_tetradic(make(newK(0,0)),Ks(sp("a")),_(:),_(1)));  
  //show(at_tetradic(_(!5),_(0,1,2,3,0),_(:),_(9)));  
  //show(at_tetradic(_(!5),_(0,1,2,3,0),_(:),_(9,8,7,6,5)));  
  //show(at_tetradic(_(!5),_(0,1,2,3,0),_(+),_(9,8,7,6,5)));  
  //show(at_tetradic(_(!3),_(,!3),_(+),_(2)));  

  //show(at_tetradic(Ks(sp(".k")),Ks(sp("kxyz")),_(:),_(1)));  show(KTREE);
  //show(at_tetradic(Ks(sp(".k")),Ks(sp("a")),_(:),_(1)));  show(KTREE);
  //show(at_tetradic(Ks(sp(".k")),Ks(sp("a")),_(+),_(2)));  show(KTREE);
  //show(at_tetradic(Ks(sp(".k")),Ks(sp("a")),_(*),_(3)));  show(KTREE);
  //show(at_tetradic(Kn(),Ks(sp("kxyz")),_(:),_(1)));

  //show(dot_tetradic(_(1,2,3),newK(0,0), _(-:), 0 ));
  //show(dot_tetradic(_(1,2,3),newK( 0,0),_(:),_(1)));
  //show(dot_tetradic(_(1,2,3),newK(-1,0),_(:),_(2)));
  //show(dot_tetradic(_(1,2,3),newK(-2,0),_(:),_(3)));
  //show(dot_tetradic(_(1,2,3),newK(-4,0),_(:),_(4)));
  //show(dot_tetradic(Ks(sp("a")),newK(0,0),_(:),_(7)));show(KTREE);

  //show(dot_tetradic(Kn(),_(`a,`b,`c),_(*:),0));
  //dot_tetradic(_(`),_(`a,`b,`c),_(*:),0);
  //dot_tetradic(_(`),_(`a,`b,`d),_(:),Ki(7));
  //show(KTREE);

  //K aa = join(_(,1,2),_(,3,4));
  //K bb = join(enlist(aa),enlist(plus(Ki(10),aa)));
  //show(dot_tetradic(aa,_(0),_(:),_(8)));
  //show(dot_tetradic(aa,_(0,0),_(:),_(8)));
  //show(dot_tetradic(aa,_(0,0),_(:),_(8)));
  //show(dot_tetradic(bb,newK(0,0),_(:),0));

  //show(dot_tetradic(bb,_(0),_(-:),0));     show(Kn());
  //show(dot_tetradic(bb,_(1),_(-:),0));     show(Kn());
  //show(dot_tetradic(bb,_(0,0),_(-:),0));   show(Kn());
  //show(dot_tetradic(bb,_(0,1),_(-:),0));   show(Kn());
  //show(dot_tetradic(bb,_(1,0),_(-:),0));   show(Kn());
  //show(dot_tetradic(bb,_(1,1),_(-:),0));   show(Kn());
  //show(dot_tetradic(bb,_(0,0,1),_(-:),0)); show(Kn());
  //show(dot_tetradic(bb,_(0,1,0),_(-:),0)); show(Kn());
  //show(dot_tetradic(bb,_(0,1,1),_(-:),0)); show(Kn());
  //show(dot_tetradic(bb,_(1,0,0),_(-:),0)); show(Kn());
  //show(dot_tetradic(bb,_(0,1,,1,1),_(+),Ki(100))); show(Kn());
  
  //show(vf_ex(at, join(_(,!5),join(_(1),_(-:)))));
  //show(vf_ex(at, join(_(,!5),join(_(0),join(_(:),_(7))) ) ));
  //show(vf_ex(dot, join(_(,!5),join(_(1),_(-:)))));
  //show(vf_ex(dot, join(_(,!5),join(_(0),join(_(:),_(7))) ) ));

  //K dc=newK(-3,-1); C* dcs="12.34"; dc->n=strlen(dcs); ke(dc)=dcs;//don't do this
  //show(dollar(Ki(0),dc));
  //show(dollar(Kf(0.0),dc)); //Test on 12.34, a12.34, 12.34a, +-inf etc.
  //show(dollar(Kc('a'),dc));
  //show(dollar(Ks(sp("")),dc));
  //show(dollar(_(9),Kn()));
  //show(dollar(_(0),Kc('\0')));
  //show(dollar(Kf(7.2),Kf(2.345))); 
  //show(dollar(Kf(7.2),Ki(714))); 
  //show(dollar(Kf(-9.2),Kf(2.345))); 
  //show(dollar(Kf(-9.2),Kf(714))); 
  //Under Linux A+ had a problem with strtod on string "00" (0.0 $ "00")

  //show(_abs  (enumerate(Ki(10))));
  //show(_acos (enumerate(Ki(10))));
  //show(_asin (enumerate(Ki(10))));
  //show(_atan (enumerate(Ki(10))));
  //show(_cos  (enumerate(Ki(10))));
  //show(_cosh (enumerate(Ki(10))));
  //show(_exp  (enumerate(Ki(10))));
  //show(_floor(enumerate(Ki(10))));
  //show(_log  (enumerate(Ki(10))));
  //show(_sin  (enumerate(Ki(10))));
  //show(_sinh (enumerate(Ki(10))));
  //show(_sqr  (enumerate(Ki(10))));
  //show(_sqrt (enumerate(Ki(10))));
  //show(_tan  (enumerate(Ki(10))));
  //show(_tanh (enumerate(Ki(10))));

  // also  "1+ * :'(3 0;5 2;7 4)" works (monad colon not flush with *)
  
  //K x = newK(0,0), K k;
  //DO(1000, k = Ki(i); kap(&x,&k); show(x)  )
  //dd(x->n)

  //show(make(newK(0,0))); //empty dict works .()

  //K *g=denameS("dfsf",".f.hi");
  //*g=enumerate(Ki(10));
  //show(KTREE);


  //  empty symbol ` is global/absolute like `.a might be.
  //  so it needs to be treated as a global by the variable parser
  //  in both terminal input and inside functions? verify cases

  //show(_(_size `h.h));
  //show(_(_size "h.h"));

  //(0;0.0) $\: ("0N";"-0I";"0I";"0n";"-0i";"0i")
 
  // .(`;();:;1)  --> Bug / reference error, adds empty dictionary to .k, empties other dictionaries

  //K k; k=_("ab"); DO(5000, kap(&k,"c")) cd(k); k=_(1 2 3); DO(5000, kap(&k,&i); ) cd(k); //Make sure kap works and is speedy


