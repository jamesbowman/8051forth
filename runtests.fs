\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

CR .( Running ANS Forth and Forth 2012 test programs, version 0.11) CR

include tester.fr

include core.fr
marker XX
include coreplustest.fth
include errorreport.fth
include coreexttest.fth
include doubletest.fth
include exceptiontest.fth
\ XX
\ marker XX
\ include facilitytest.fth
\ XX
\ \ \ include filetest.fth
\ \ \ include localstest.fth
\ \ include memorytest.fth
\ marker XX
\ include toolstest.fth
\ XX
\ \ \ include searchordertest.fth
\ marker XX
\ include stringtest.fth
\ XX
\ REPORT-ERRORS

decimal
CR CR .( Forth tests completed ) CR CR

