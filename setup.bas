REM setup.exe Updated January 10 2020 at 9:00 AM
REM
REM

_FULLSCREEN
start:
CLS
PRINT
PRINT "Lotto Max Lottery Simulator RC7 Version 2020 "
PRINT "Program Parameters Setup File"
PRINT "Released on January 10 2020 at 9:00 AM "
PRINT "Written By Russ Campbell of Kitchener, Ontario, Canada"
PRINT "------------------------------------------------------"
PRINT
INPUT "Do want colour screens [default Yes]       : ", cs$
IF cs$ = "y" THEN
    cs$ = "Y"
ELSEIF cs$ = "n" THEN
    cs$ = "N"
END IF
IF cs$ = "" THEN
    cs$ = "Y"
END IF
INPUT "Do you have a printer online and do you want to use it [default NO]  : ", printer$
IF printer$ = "y" THEN
    printer$ = "Y"
ELSEIF printer$ = "n" THEN
    printer$ = "N"
END IF
IF printer$ = "" THEN
    printer$ = "N"
END IF
INPUT "Do you wish to display help files on the on the screen [default [Y]   : ", answer$
IF answer$ = "N" OR answer$ = "n" THEN
    helpfile$ = "N"
ELSE
    helpfile$ = "Y"
END IF
INPUT "Do you wish to display text from draws on the screen -- [default [Y]  : ", answer$
IF answer$ = "N" OR answer$ = "n" THEN
    display$ = "N"
ELSE
    display$ = "Y"
END IF
INPUT "Which speed do you wish to display [R]eadable or  [F]ast [default [R] : ", answer$
IF answer$ = "F" OR answer$ = "f" THEN
    speed$ = "Fast"
ELSE
    speed$ = "Readable"
END IF

CLS
PRINT
PRINT "Graphics Mode For Lotto Max"
PRINT
PRINT "[A]  -  No Graphics Card"
PRINT "[B]  -  VGA Mode"
PRINT "[C]  -  HDMI Mode"
PRINT
PRINT "[Return] defaults to HDMI Mode ..."
PRINT
INPUT "Enter [A B C] : ", answer$
IF answer$ = "a" OR answer$ = "A" THEN
    stype = 0
    stype$ = "None"
ELSEIF answer$ = "b" OR answer$ = "B" THEN
    stype = 1
    stype$ = "VGA"
ELSEIF answer$ = "c" OR answer$ = "C" THEN
    stype = 2
    stype$ = "HDMI"
ELSEIF answer$ = "" THEN
    stype = 2
    stype$ = "HDMI"
END IF
qqqq:

PRINT
INPUT "How many Numbers in the Wheeling System [14 to 42] [defaults to 14] : ", qqq
IF qqq < 14 OR qqq > 42 THEN
    qqq = 14
END IF
PRINT
CLS
PRINT
PRINT "Challenge Level of Lotto Max RC6 Lottery Simulator"
PRINT
PRINT " [1]  Easy Skill Game"
PRINT " [2]  Medium Skill Game"
PRINT " [3]  Hard Skill Game"
PRINT " [4]  Real Time Game"
PRINT " [5]  Impossible Game"
PRINT
INPUT " Skill Level : ", skill
IF skill = 1 THEN money = 1000000: skill$ = "Easy"
IF skill = 2 THEN money = 50000: skill$ = "Medium"
IF skill = 3 THEN money = 10000: skill$ = "Hard"
IF skill = 4 THEN money = 25000: skill$ = "Real Time"
IF skill = 5 THEN money = -25000: skill$ = "Impossible"
PRINT
CLS
PRINT
PRINT "Lotto Max RC6 Lottery Simulator System Configuration"
PRINT
PRINT "Printer  Online                               :  "; printer$
PRINT "Color Screens                                 :  "; cs$
PRINT "Graphics Mode                                 :  "; stype$
PRINT "Wheeling System                               :  "; qqq; " Numbers"
PRINT "Starting Money                                :  "; "$"; money; "Dollars."
PRINT "Skill Level                                   :  "; skill$
PRINT "Display text output on screen                 :  "; display$
PRINT "Display help files on screen                  :  "; helpfile$
PRINT "Display Speed [R]eadable or [F]ast            :  "; speed$
PRINT
INPUT "Is the above correct [Y]es or [N]o : ", answer$
IF answer$ = "n" OR answer$ = "N" OR answer$ = "no" OR answer$ = "NO" THEN
    GOTO start
END IF


OPEN "grmode.dat" FOR OUTPUT AS #1
PRINT #1, printer$
PRINT #1, cs$
PRINT #1, stype
PRINT #1, qqq
PRINT #1, skill$
PRINT #1, display$
PRINT #1, helpfile$
PRINT #1, speed$
CLOSE #1
money:
OPEN "totalbalance.dat" FOR OUTPUT AS #2
PRINT #2, money
PRINT #2, 0
PRINT #2, 0
PRINT #2, money
CLOSE #2
chainit:
CHAIN "lotto_max_RC7"
finish:
END

