
REM *** Lotto Max Simulator Version 2020 RC7 Working Version With Automatic Draw Date Update
REM *** Fully 99.999 % working copy Most up to date.
REM *** Written by: Russ Campbell
REM
REM *** Last Update : January 10 2020 9:00 AM
REM
_FULLSCREEN _STRETCH
CLEAR , , 600000
DIM temp(7): TIMER ON
DIM temp2(7)
DIM temp3(50)
DIM temp4(50)
DIM entry(50)
DIM month$(12)
DIM numb(50)
DIM chart(100000, 13)
DIM bonus(50)
DIM outpt(50)
DIM outpt1(100)
DIM outpt2(50)
DIM m AS INTEGER
DIM y AS INTEGER
DIM day2 AS INTEGER
DIM SHARED draw2$(10)
DIM SHARED VirtualYear AS INTEGER


REM $STATIC
RANDOMIZE TIMER
REM ON ERROR GOTO errorhandle
GOSUB READDAYMONTHYEAR
GOSUB grcheck
GOSUB grinit
GOSUB buildchart
GOSUB convert
GOSUB betaversion
beginning$ = "Y"
GOSUB startingandending
IF finish$ = "Y" THEN GOTO finish:
GOSUB rules
IF crs$ = "Y" THEN COLOR 1, 7
CLS
IF crs$ = "Y" THEN COLOR 11, 1
REM build outer top corner at pos 3,6
LOCATE 3, 6
PRINT CHR$(201)
REM build outer top line at line 3
FOR i = 2 TO 66
    LOCATE 3, 5 + i
    PRINT CHR$(205)
NEXT i
REM build outer end corner at pos 3,72
LOCATE 3, 72
PRINT CHR$(187)
REM build outer box vertical lines on both sides
FOR i = 4 TO 17
    LOCATE i, 6
    PRINT CHR$(186)
    LOCATE i, 72
    PRINT CHR$(186)
NEXT i
LOCATE 18, 6
PRINT CHR$(200)
REM build outer box bottom line
FOR i = 2 TO 66
    LOCATE 18, 5 + i
    PRINT CHR$(205)
NEXT i
REM build outer box right bottom corner
LOCATE 18, 72
PRINT CHR$(188)
IF crs$ = "Y" THEN COLOR 7, 1
REM build white speckled box  top line
FOR i = 8 TO 70
    LOCATE 4, i
    PRINT CHR$(178)
NEXT i
REM build white speckled box bottom line
FOR i = 8 TO 70
    LOCATE 17, i
    PRINT CHR$(178)
NEXT i
REM build white speckled box bot vertical lines
FOR i = 4 TO 17
    LOCATE i, 7
    PRINT CHR$(178)
    LOCATE i, 71
    PRINT CHR$(178)
NEXT i
IF crs$ = "Y" THEN COLOR 3, 1
LOCATE 5, 8
PRINT CHR$(201)
FOR i = 9 TO 69
    LOCATE 5, i
    PRINT CHR$(205)
NEXT i
LOCATE 5, 70
PRINT CHR$(187)
FOR i = 9 TO 69
    LOCATE 16, i
    PRINT CHR$(205)
NEXT i
REM build inner box vertical lines
FOR i = 5 TO 16
    LOCATE i, 8
    IF i = 5 THEN
        PRINT CHR$(201) REM correct
    ELSEIF i = 8 THEN
        PRINT CHR$(204) REM correct
        REM build inner box left corner
    ELSEIF i = 15 THEN
        PRINT CHR$(200)
    ELSE
        PRINT CHR$(186)
    END IF
    LOCATE i, 70
    IF i = 5 THEN
        PRINT CHR$(187)
    ELSEIF i = 8 THEN
        PRINT CHR$(185)
        REM build inner box lower  right corner
    ELSEIF i = 16 THEN
        PRINT CHR$(188)
    ELSE
        PRINT CHR$(186)
    END IF
NEXT i
REM build first middle line at location 8,9
FOR i = 9 TO 69
    LOCATE 8, i
    PRINT CHR$(205)
NEXT i

REM build second middle line at location  15,9
FOR i = 9 TO 69
    LOCATE 15, i
    PRINT CHR$(205)
NEXT i
LOCATE 15, 70
PRINT CHR$(187)
IF crs$ = "Y" THEN COLOR 10, 5
LOCATE 6, 9
PRINT " Lotto Max 2020 RC7 Version Lotto Max Simulator Game  Vers   "
LOCATE 7, 9
PRINT " You start off with variable amounts decided by skill level  "
IF crs$ = "Y" THEN COLOR 2, 14
LOCATE 9, 9
PRINT "                                                             "
LOCATE 10, 9
PRINT "                                                             "
LOCATE 11, 9
PRINT "                                                             "
LOCATE 12, 9
PRINT "                                                             "
LOCATE 13, 9
PRINT "                                                             "
LOCATE 14, 9
PRINT "                                                             "
IF crs$ = "Y" THEN COLOR 10, 5
LOCATE 10, 14
PRINT "Written By    : Russ Campbell                      "
LOCATE 11, 14
PRINT "Release Date  : Jan 10  2020  -  9:00 AM           "
LOCATE 12, 14
PRINT "Registered To :                                    "
LOCATE 12, 30
PRINT reg$
LOCATE 13, 14
PRINT "Serial Number : "; reg2$; "      "
IF crs$ = "Y" THEN COLOR 3, 1
FOR i = 9 TO 70
    LOCATE 15, i
    PRINT CHR$(205)
NEXT i
IF crs$ = "Y" THEN COLOR 2, 12
LOCATE 16, 8
PRINT " Graphics Mode : "
LOCATE 16, 25
PRINT "["
LOCATE 16, 30
PRINT "]"
IF crs$ = "Y" THEN COLOR 4, 2
LOCATE 16, 26
PRINT "    "
LOCATE 16, 26
PRINT grmode$
LOCATE 16, 45
IF crs$ = "Y" THEN COLOR 15, 3
LOCATE 16, 31
PRINT
IF crs$ = "Y" THEN COLOR 15, 3
LOCATE 16, 63
PRINT "       "
IF crs$ = "Y" THEN COLOR 11, 6
LOCATE 16, 31
PRINT "     Press any key to continue...       "
IF crs$ = "Y" THEN COLOR 3, 1
LOCATE 15, 70
PRINT CHR$(188)
DO WHILE INKEY$ = ""
LOOP
GOTO main:
READDAYMONTHYEAR:
RESTORE monthread
FOR monthdata = 1 TO 12
    READ month$(monthdata)
NEXT monthdata
RETURN

betaversion:
CLS
PRINT
PRINT "Welcome to the Lotto Max 2020 Simulator. This program"
PRINT "has been written to show you that if you gamble a lot"
PRINT "of real money recklessly on ANY lottery, you will loose"
PRINT "BIG TIME. But for those of you who still think that by"
PRINT "spending a lot of money on ANY lottery, it will allow"
PRINT "to do just that for free, and will keep track of every"
PRINT "dollar spent , and every dollar won. This program will"
PRINT "try to follow the rules exactly, but to make it interesting"
PRINT "the payouts have been increased substantionally, in other"
PRINT "words the prizes are bigger."
PRINT
PRINT "Here is where I give a disclaimer: This is just a game for"
PRINT "Entertainment purposes. I understand that I am not using"
PRINT "any real money in this game, nor will I win any real money"
PRINT "in this game. Any losses incurred from this program are my"
PRINT "own resposibility. Not the author of this game or anyone"
PRINT "Involved in any way at all."
PRINT
PRINT "To continue using this Entertainment Only Game, please "
PRINT "Press a key to continue..."
DO WHILE INKEY$ = ""
LOOP
RETURN
picker:
IF helpfile$ = "N" THEN GOTO help2
CLS
PRINT
PRINT "This part of the program is a simulation of playing"
PRINT "the actual lottery, Lotto Max. You start off with "
PRINT "A selectable value , and the tickets are at real"
PRINT "market value, Every ticket you buy is added up,"
PRINT "the date of purchase is logged, and the next draw"
PRINT "date is logged. You have at your disposal just about"
PRINT "every possible strategy that you could use but"
PRINT "Its real time, you are not actually spendin any money"
PRINT "or winning any money, but the program will keep track"
PRINT "of it all. You are to enter each draw as a set of winning"
PRINT "numbers The program will not allow you to enter the draw"
PRINT "until it has happened, if you try to enter false numbers"
PRINT "before the draw , it will tell you that the draw has not"
PRINT "yet happened, and it will not let you enter numbers."
PRINT
PRINT "Disclaimer: You are not actually spending any money or"
PRINT "winning any money , if you do it for real I am not"
PRINT "responsible for the consequences, in fact I advise"
PRINT "against doing so foolishly. "
PRINT
PRINT "This program is for entertainment purposes only!"
PRINT
PRINT "Press any key to continue...."
PRINT
DO WHILE INKEY$ = ""
LOOP
help2:
PRINT
CLS
IF randome$ = "no" THEN
    PRINT ""
    PRINT "This section will prompt you for 14 numbers of your choosing"
    PRINT "then create sets of numbers based on your chosen numbers."
    PRINT ""
ELSEIF randome$ = "yes" THEN
    PRINT ""
    PRINT "This section will pick random numbers for you"
    PRINT "then create as many sets of numbers as you desire."
    PRINT
END IF
PRINT
OPEN "totalbalance.dat" FOR INPUT AS #1
INPUT #1, newbalance
INPUT #1, money
INPUT #1, winningstotal
INPUT #1, totalbalance
CLOSE #1

money:
CLS
PRINT
INPUT "Enter date of draw for your tickets in MM-DD-YYYY format : ", draw2$
IF MID$(draw2$, 3, 1) <> "-" OR MID$(draw2$, 6, 1) <> "-" THEN
    PRINT "Date Incorrect !!!!!! [Use MM-DD-YYYY format !!!!"
    GOTO money
END IF
IF DATE$ > draw2$ THEN
    PRINT
    PRINT "Draw is in the past !!! The system will not allow"
    PRINT "you to buy any tickets please try again"
    PRINT
    PRINT "Press a key to continue ....."
    PRINT
    DO WHILE INKEY$ = ""
    LOOP
    counters = counters + 1
    IF counters > 3 THEN
        counters = 0
        GOTO main
    END IF
    GOTO money
END IF
PRINT
PRINT "How many sets of tickets do you wish to buy ? "
INPUT "[At $5.00 per 3 sets of numbers  ]  : ", sets
PRINT
PRINT "Disclaimer: This program and all of its contents"
PRINT "            is only for entertainment purposes."
PRINT
PRINT "You are not spending any money nor will you win"
PRINT "any money unless you actually buy a tivket from "
PRINT "any of the OLG agents. I do not condone any "
PRINT "any reckless gambling of real money. This is "
PRINT "just a game and all money won or lost is only"
PRINT "funny money, not real or negotiable anywhere."
PRINT
PRINT "By playing this game you agree to this......"
PRINT
PRINT "Press any key to continue ......"
DO WHILE INKEY$ = ""
LOOP
GOSUB banker
IF randome$ = "N" THEN
    PRINT "Using a : "; qqq; " Wheeling system."
END IF
PRINT "Press any key to continue...."
PRINT
DO WHILE INKEY$ = ""
LOOP

diskcop:
diskcopy$ = "Y"
CLS
PRINT
PRINT "Enter your filename for output to disk "
PRINT "For checking for winnerrs later on"
PRINT "after the next draw of: ";
PRINT
INPUT "Filename [Without extension : "; file$
IF file$ = "" OR file$ = " " THEN
    file$ = "lottomax2020"
END IF
file$ = file$ + ".dat"
OPEN file$ FOR OUTPUT AS #2
PRINT "Filename is : "; file$
IF display$ = "Y" THEN
    IF speed$ = "Readable" THEN speed = 2300000
END IF

IF randome$ = "yes" THEN
    GOTO start
ELSEIF randome$ = "no" THEN
    GOTO previously
END IF
previously:
INPUT "Do you wish to use a previously saved set of numbers  [Y] or [N] : ", answer$
IF answer$ = "y" OR answer$ = "Y" THEN
    OPEN "numbers.dat" FOR INPUT AS #1
    DO UNTIL EOF(1)
        counter = counter + 1
        INPUT #1, number
        entry(counter) = number
    LOOP
ELSE
    GOTO entry
END IF
CLOSE #1
IF counter < qqq THEN
    qqq = counter
ELSEIF counter > qqq THEN
    qqq = counter
END IF
GOTO start
pickthedrawnumber:
IF randome$ = "yes" THEN
    raaa = 35
END IF
IF randome$ = "no" THEN
    raaa = 0
END IF
rzzz = INT((qqq + raaa) * RND(1) + 1)
temp(y) = rzzz
temp3(rzzz) = temp3(rzzz) + 1
GOSUB checkforduplicates
RETURN
cleararrays:
ERASE temp3
RETURN
checkforduplicates:
IF y > 1 THEN
    IF temp3(rzzz) > 1 THEN
        GOSUB pick
    END IF
END IF
RETURN


banker:

PRINT


OPEN "Ispentthismuch.dat" FOR APPEND AS #1
PRINT #1, DATE$
PRINT #1, moneyspent
CLOSE #1
RETURN

entry:
newbalance = newbalance = -money
IF randome$ = "yes" THEN GOTO start
CLS
FOR zq = 1 TO qqq
    1 PRINT "Entry #"; zq; " ";: INPUT ":", entry(zq)
    IF entry(zq) = 0 THEN
        IF diskcopy$ = "Y" THEN
            CLOSE #2
        END IF
        GOTO main
    END IF
    IF entry(zq) < 1 OR entry(zq) > 50 THEN
        PRINT "Number Out of Range ! , Try Again !!"
        GOTO 1
    END IF
    IF zq > 1 THEN
        FOR i = 1 TO (zq - 1)
            IF entry(zq) = entry(i) THEN
                PRINT "Number already entered, pick another!"
                GOTO 1
            END IF
        NEXT i
    END IF
NEXT zq

PRINT
CLS
PRINT
PRINT "You have picked the following numbers: "
PRINT
FOR i = 1 TO qqq
    PRINT "Entry # "; i; " "; entry(i)
NEXT i
PRINT
INPUT "Is this all correct [Y]es or [N]o  : ", answer$
IF answer$ = "n" OR answer$ = "N" THEN GOTO entry
PRINT
INPUT "Save Numbers to disk [Y] or [N] : ", answer$
IF answer$ = "Y" OR answer$ = "y" THEN
    OPEN "numbers.dat" FOR OUTPUT AS #1
    FOR i = 1 TO qqq
        PRINT #1, entry(i)
    NEXT i
    CLOSE #1
END IF
start:

IF hardcopy$ = "Y" OR hardcopy$ = "y" THEN
    LPRINT
    LPRINT "Lotto Max Number List"
    LPRINT "---------------------"
    LPRINT
END IF

FOR x = 1 TO sets
    FOR zzzr = 1 TO 3
        FOR y = 1 TO 7
            GOSUB pick
        NEXT y
        IF randome$ = "no" THEN
            FOR i = 1 TO 7
                temp(i) = entry(temp(i))
            NEXT i
        END IF
        FOR i = 1 TO 7
            temp2(i) = temp(i)
        NEXT i
        FOR j = 1 TO 7
            marker = 99
            FOR i = 1 TO 7
                IF temp2(i) < marker THEN
                    marker = temp2(i)
                    marker2 = i
                END IF
            NEXT i
            temp2(marker2) = 99
            temp(j) = marker
        NEXT j

        a = temp(1): b = temp(2): C = temp(3): d = temp(4): e = temp(5): f = temp(6): g = temp(7)
        IF display$ = "Y" THEN
            PRINT USING "      ## ## ## ## ## ## ##       "; a, b, C, d, e, f, g
            FOR aaaa = 1 TO speed: NEXT aaaa
        END IF



        IF diskcopy$ = "Y" THEN
            PRINT #2, USING "## ## ## ## ## ## ##"; a, b, C, d, e, f, g
        END IF
        IF hardcopy$ = "Y" OR hardcopy$ = "y" THEN
            LPRINT USING "## ## ## ## ## ## ##"; a, b, C, d, e, f, g
        END IF

        GOSUB cleararrays
    NEXT zzzr
    IF diskcopy$ = "Y" THEN

    END IF
    IF hardcopy$ = "Y" THEN
        LPRINT ""
    END IF
    PRINT ""
NEXT x
CLS
PRINT "You have bought"; sets; "sets of tickets at $5.00 per set."
PRINT "This has cost you about $"; sets * 5; " dollars worth of tickets."
PRINT
PRINT "The filename used to save this set of numbers to disk is "; file$
PRINT
PRINT "Press any key to Continue......"
DO WHILE INKEY$ = ""
LOOP
GOSUB winningsandloosings
PRINT "Your numbers have been saved to disk for checking"
PRINT "When you enter the draw data for the next draw."

PRINT
PRINT "Program Written by : Russ Campbell"
IF hardcopy$ = "Y" AND randome$ = "no" THEN
    LPRINT
    LPRINT "The numbers that you have chosen are as follows: "
    LPRINT
    FOR i = 1 TO qqq
        LPRINT entry(i); " ";
    NEXT i
    PRINT
END IF
IF hardcopy$ = "Y" THEN
    LPRINT
    LPRINT "Program written by : Russ Campbell"
    LPRINT

END IF
IF diskcopy$ = "Y" THEN
    CLOSE #2
END IF
PRINT
PRINT "Press any key to continue"
DO WHILE INKEY$ = ""
LOOP

OPEN "totalbalance.dat" FOR OUTPUT AS #1
PRINT #1, newbalance
PRINT #1, money
PRINT #1, moneytotal
PRINT #1, winningstotal
totalbalance = newbalance - money
PRINT #1, totalbalance
CLOSE #1
GOTO main
pick:
grandprizechange = grandprizechange + 1
IF grandprizechancge > 25 THEN
    GOSUB payouts
END IF
IF randome$ = "yes" THEN
    raaa = 35
END IF
IF randome$ = "no" THEN
    raaa = 0
END IF
rzzz = INT((qqq + raaa) * RND(1) + 1)
temp(y) = rzzz
temp3(rzzz) = temp3(rzzz) + 1
GOSUB checkforduplicates
RETURN
cleararrays2:
ERASE temp3
RETURN
checkforduplicates2:
IF y > 1 THEN
    IF temp3(rzzz) > 1 THEN
        GOSUB pick
    END IF
END IF
RETURN
winningsandloosings:
newbalance = newbalance - money + winningstotal
money = sets * 5
wininngstotal = grandtotal

CLS
PRINT
PRINT "You have spent the grand total of $ "; money
PRINT "You have bought "; sets * 3; "sets of 7 numbers from 1 to 50"
PRINT
PRINT "|--------------------------------------------------------------------------|"
PRINT "| Previous Balance| Tickets Cost    |    Winnings       |  Balance         |"
PRINT "|-----------------|-----------------|-------------------|------------------|"
pt$ = "|$ #,###,###,###  | $ #,###,###,### |   $ #,###,###,### |  $ #,###,###,### |"

PRINT USING pt$; newbalance; money; winningstotal; -money + newbalance + winnings
PRINT "|--------------------------------------------------------------------------|"
PRINT
PRINT
PRINT "Press any key to continue..."
DO WHILE INKEY$ = ""
LOOP
RETURN

main:

IF crs$ = "Y" THEN COLOR 1, 7
CLS
IF crs$ = "Y" THEN COLOR 3, 8
LOCATE 4, 7
PRINT CHR$(201)
FOR i = 8 TO 73
    LOCATE 4, i
    PRINT CHR$(205)
NEXT i
LOCATE 4, 74
PRINT CHR$(187)
FOR i = 5 TO 13
    LOCATE i, 7
    IF i = 6 OR i = 11 OR i = 13 THEN
        PRINT CHR$(204)
    ELSE
        PRINT CHR$(186)
    END IF
    LOCATE i, 74
    IF i = 6 OR i = 11 OR i = 13 THEN
        PRINT CHR$(185)
    ELSE
        PRINT CHR$(186)
    END IF
NEXT i
FOR i = 8 TO 73
    LOCATE 6, i
    PRINT CHR$(205)
NEXT i
FOR i = 8 TO 73
    LOCATE 11, i
    PRINT CHR$(205)
NEXT i
FOR i = 8 TO 73
    LOCATE 13, i
    PRINT CHR$(205)
NEXT i
LOCATE 14, 7
PRINT CHR$(186)
LOCATE 14, 74
PRINT CHR$(186)
LOCATE 15, 7
PRINT CHR$(200)
FOR i = 8 TO 73
    LOCATE 15, i
    PRINT CHR$(205)
NEXT i
LOCATE 15, 74
PRINT CHR$(188)
IF crs$ = "Y" THEN COLOR 15, 3
LOCATE 14, 8
PRINT "                                                                  "
LOCATE 14, 8

m = month
dy = day
y = year

GOSUB getdate
PRINT USING "  Winning Numbers Database Updated On : \ \  \ \  ##-#### "; GetDay$(m, (dy + LASTDAY), y); month$(m); (dy + LASTDAY); y

IF crs$ = "Y" THEN COLOR 10, 5
LOCATE 5, 8
PRINT "        Lotto Max Simulator RC7 Version 2020  Main Menu           "
IF crs$ = "Y" THEN COLOR 4, 7
FOR i = 7 TO 10
    LOCATE i, 13
    PRINT "["
    LOCATE i, 15
    PRINT "]"
    LOCATE i, 42
    PRINT "["
    LOCATE i, 44
    PRINT "]"
NEXT i
IF crs$ = "Y" THEN COLOR 0, 7
LOCATE 7, 14
PRINT "A"
LOCATE 7, 43
PRINT "B"
LOCATE 8, 14
PRINT "C"
LOCATE 8, 43
PRINT "D"
LOCATE 9, 14
PRINT "E"
LOCATE 9, 43
PRINT "F"
LOCATE 10, 14
PRINT "G"
LOCATE 10, 43
PRINT "H"
IF crs$ = "Y" THEN COLOR 1, 7
LOCATE 7, 18
PRINT "Enter Winning Numbers"
LOCATE 8, 18
PRINT "Use"; qqq; "Number System"
LOCATE 9, 18
PRINT "About this program"
LOCATE 10, 18
PRINT "Special Date Bargraphs"
LOCATE 7, 47
PRINT "Search and Print Menus"
LOCATE 8, 47
PRINT "Pick random Numbers"
LOCATE 9, 47
PRINT "Bargraph Menu"
LOCATE 10, 47
PRINT "Frequency Calculator"
IF crs$ = "Y" THEN COLOR 4, 7
LOCATE 12, 31
PRINT "["
LOCATE 12, 33
PRINT "]"
LOCATE 12, 42
PRINT "Escape Key Exits Program"
IF crs$ = "Y" THEN COLOR 1, 7
LOCATE 12, 13
PRINT "Your choice is : "
main2:
LOCATE 12, 32
answer$ = INPUT$(1)
PRINT answer$
IF answer$ = CHR$(27) THEN
    finish$ = "Y"
    GOSUB startingandending
    GOTO finish
ELSEIF answer$ = "A" OR answer$ = "a" THEN
    GOTO enterwinningnumbers
ELSEIF answer$ = "B" OR answer$ = "b" THEN
    GOTO searchmenu
ELSEIF answer$ = "C" OR answer$ = "c" THEN
    randome$ = "no"
    GOTO picker
ELSEIF answer$ = "D" OR answer$ = "d" THEN
    randome$ = "yes"
    GOTO picker
ELSEIF answer$ = "E" OR answer$ = "e" THEN
    GOSUB aboutthisprogram
ELSEIF answer$ = "F" OR answer$ = "f" THEN
    spec$ = "N"
    GOTO bargraph
ELSEIF answer$ = "G" OR answer$ = "g" THEN
    spec$ = "Y"
    GOTO enquiremonth
ELSEIF answer$ = "H" OR answer$ = "h" THEN
    GOTO chart2
ELSEIF answer$ = "%" THEN
    CLS
    INPUT "Enter Password: ", pass2$
    owner$ = "Russ Campbell"
    registered$ = reg$
    distribute$ = "QB64.ORG"
    serial$ = reg2$
    version$ = "2020 RC7"
    release$ = "January 10 2020 - 9:00 AM"
    pass$ = "Password"
    IF pass2$ = pass$ THEN
        CLS
        PRINT
        PRINT "Program Written by : "; owner$
        PRINT "Registered to      : "; registered$
        PRINT "Distributed by     : "; distribute$
        PRINT "Serial #           : "; serial$
        PRINT "Version #          : "; version$
        PRINT "Release date       : "; release$
        PRINT
        PRINT "Press any key to continue ... "
        DO WHILE INKEY$ = ""
        LOOP
    END IF
    GOTO main
END IF
count = -1
GOTO main
chart:
CLS
PRINT "Number Pattern"
PRINT "--------------"
PRINT
PRINT "This sections displays the number pattern for winning #s that have"
PRINT "been entered. The amount of times each number has been picked is "
PRINT "displayed as in the chart below."
PRINT
IF bonus$ = "Y" THEN
    title2$ = "With Bonus Numbers"
ELSEIF bonus$ = "N" THEN
    title2$ = "Without Bonus Numbers"
END IF
FOR j = 1 TO 50
    outpt(j) = 0
    outpt1(j) = 0
    outpt2(j) = 0
NEXT j
FOR j = 1 TO 50
    outpt(j) = numb(j)
NEXT j

total = 0
average = 0
high = 90
low = 75
IF bonus$ = "Y" THEN
    title3$ = "With Bonus Numbers  - All Numbers Drawn to Date"
ELSEIF bonus$ = "Y" THEN
    title3$ = "Without Bonus Numbers - All numbers Drawn to Date"
END IF

PRINT
PRINT "Number Chart "; title2$
PRINT
FOR j = 1 TO 10
    PRINT USING "Num ## : ###"; j; outpt(j);
    PRINT USING "  Num ## : ###"; j + 10; outpt(j + 10);
    PRINT USING "  Num ## : ###"; j + 20; outpt(j + 20);
    PRINT USING "  Num ## : ###"; j + 30; outpt(j + 30);
    PRINT USING "  Num ## : ###"; j + 40; outpt(j + 40)
NEXT j
PRINT: PRINT "Press any key to continue"
DO WHILE INKEY$ = ""
LOOP

chart4:

CLS
PRINT "Low Number Pattern"
PRINT "------------------"
PRINT ""
PRINT "This section displays all of the low numbers that have been"
PRINT "been entered. The amount of times each number has been picked is "
PRINT "displayed as in the chart below. Note that if the number is"
PRINT "not in the low area then it will display a zero for that number."
PRINT
IF bonus$ = "Y" THEN
    title3$ = "With Bonus Numbers - only numbers that are low"
ELSEIF bonus$ = "N" THEN
    title3$ = "Without Bonus Numbers - only numbers that are low"
END IF
FOR j = 1 TO 50
    IF bonus$ = "Y" AND numb(j) < low THEN
        outpt1(j) = numb(j) + bonus(j)
    ELSEIF bonus$ = "N" AND numb(j) < low THEN
        outpt1(j) = numb(j)
    END IF
NEXT j
PRINT
PRINT "Number Chart "; title3$
PRINT
FOR j = 1 TO 10
    PRINT USING "Num ## : ###"; j; outpt1(j);
    PRINT USING "  Num ## : ###"; j + 10; outpt1(j + 10);
    PRINT USING "  Num ## : ###"; j + 20; outpt1(j + 20);
    PRINT USING "  Num ## : ###"; j + 30; outpt1(j + 30);
    PRINT USING "  Num ## : ###"; j + 40; outpt1(j + 40)
NEXT j
DO WHILE INKEY$ = ""
LOOP

chart5:
CLS
PRINT "High Number Pattern"
PRINT "------------------"
PRINT ""
PRINT "This section displays all of the high numbers that have been"
PRINT "been entered. The amount of times each number has been picked is "
PRINT "displayed as in the chart below. Note that if the number is not"
PRINT "in the high area then it will display a zero for that number."
PRINT
IF bonus$ = "Y" THEN
    title4$ = "With Bonus Numbers - only numbers that are high"
ELSEIF bonus$ = "N" THEN
    title4$ = "Without Bonus Numbers - only numbers that are high"
END IF
FOR j = 1 TO 50
    IF bonus$ = "Y" AND numb(j) > high THEN
        outpt2(j) = numb(j) + bonus(j)
    ELSEIF bonus$ = "N" AND numb(j) > high THEN
        outpt2(j) = numb(j)
    END IF
NEXT j
PRINT
PRINT "Number Chart "; title4$
PRINT
FOR j = 1 TO 10
    PRINT USING "Num ## : ###"; j; outpt2(j);
    PRINT USING "  Num ## : ###"; j + 10; outpt2(j + 10);
    PRINT USING "  Num ## : ###"; j + 20; outpt2(j + 20);
    PRINT USING "  Num ## : ###"; j + 30; outpt2(j + 30);
    PRINT USING "  Num ## : ###"; j + 40; outpt2(j + 40)
NEXT j
IF hardcopy$ = "Y" THEN
    LPRINT
    LPRINT "Number Chart "; title2$
    LPRINT
    LPRINT
    FOR j = 1 TO 10
        LPRINT USING "Num ## : ###"; j; outpt(j);
        LPRINT USING "  Num ## : ###"; j + 10; outpt(j + 10);
        LPRINT USING "  Num ## : ###"; j + 20; outpt(j + 20);
        LPRINT USING "  Num ## : ###"; j + 30; outpt(j + 30);
        LPRINT USING "  Num ## : ###"; j + 40; outpt(j + 40)
    NEXT j
    LPRINT
    LPRINT
    LPRINT "Number Chart "; title3$
    LPRINT
    LPRINT
    FOR j = 1 TO 10
        LPRINT USING "Num ## : ###"; j; outpt1(j);
        LPRINT USING "  Num ## : ###"; j + 10; outpt1(j + 10);
        LPRINT USING "  Num ## : ###"; j + 20; outpt1(j + 20);
        LPRINT USING "  Num ## : ###"; j + 30; outpt1(j + 30);
        LPRINT USING "  Num ## : ###"; j + 40; outpt1(j + 40)
    NEXT j
    LPRINT
    LPRINT
    LPRINT "Number Chart "; title4$
    LPRINT
    LPRINT
    FOR j = 1 TO 10
        LPRINT USING "Num ## : ###"; j; outpt2(j)
        LPRINT USING "  Num ## : ###"; j + 10; outpt2(j + 10);
        LPRINT USING "  Num ## : ###"; j + 20; outpt2(j + 20);
        LPRINT USING "  Num ## : ###"; j + 30; outpt2(j + 30);
        LPRINT USING "  Num ## : ###"; j + 40; outpt2(j + 40)
    NEXT j
    LPRINT
END IF
PRINT: PRINT "Press any key to continue"
DO WHILE INKEY$ = ""
LOOP
GOTO graphmenu

enquiremonth:
CLS
PRINT
PRINT "View Graphs from specified date"
PRINT
INPUT "Enter Year to start from  : ", yearin
IF yearin < 2009 THEN
    yearin = 2009
END IF
INPUT "Enter Month to start from : ", monthin
INPUT "Enter Year to finish at   : ", yearend
IF yearend < 2009 THEN
    yearend = 2009
END IF
INPUT "Enter Month to finish at  : ", monthend
bargraph:
FOR i = 1 TO 50
    numb(i) = 0
    bonus(i) = 0
    outpt(i) = 0
NEXT i
GOSUB convert
viewpoint$ = ""
graphmenu:
IF crs$ = "Y" THEN COLOR 1, 7
CLS
IF crs$ = "Y" THEN COLOR 3, 8
LOCATE 2, 6
PRINT CHR$(201)
FOR i = 7 TO 73
    LOCATE 2, i
    PRINT CHR$(205)
NEXT i
LOCATE 2, 74
PRINT CHR$(187)
FOR i = 3 TO 21
    LOCATE i, 6
    IF i = 4 OR i = 7 OR i = 12 OR i = 14 OR i = 20 THEN
        PRINT CHR$(204)
    ELSE
        PRINT CHR$(186)
    END IF
    LOCATE i, 74
    IF i = 4 OR i = 7 OR i = 12 OR i = 14 OR i = 20 THEN
        PRINT CHR$(185)
    ELSE
        PRINT CHR$(186)
    END IF
NEXT i
LOCATE 22, 6
PRINT CHR$(200)
LOCATE 22, 74
PRINT CHR$(188)
FOR i = 7 TO 73
    LOCATE 22, i
    PRINT CHR$(205)
NEXT i
FOR i = 7 TO 73
    LOCATE 7, i
    PRINT CHR$(205)
NEXT i
FOR i = 7 TO 73
    LOCATE 12, i
    PRINT CHR$(205)
NEXT i
FOR i = 7 TO 73
    LOCATE 20, i
    PRINT CHR$(205)
NEXT i
FOR i = 7 TO 73
    LOCATE 14, i
    PRINT CHR$(205)
NEXT i
FOR i = 7 TO 73
    LOCATE 4, i
    PRINT CHR$(205)
NEXT i
LOCATE 4, 37
PRINT CHR$(203)
LOCATE 5, 37
PRINT CHR$(186)
LOCATE 6, 37
PRINT CHR$(186)
LOCATE 7, 37
PRINT CHR$(206)
LOCATE 8, 37
PRINT CHR$(186)
LOCATE 9, 37
PRINT CHR$(186)
LOCATE 10, 37
PRINT CHR$(186)
LOCATE 11, 37
PRINT CHR$(186)
LOCATE 12, 37
PRINT CHR$(202)
IF crs$ = "Y" THEN COLOR 4, 7
FOR i = 8 TO 11
    LOCATE i, 12
    PRINT "["
    IF crs$ = "Y" THEN COLOR 0, 7
    LOCATE i, 13
    PRINT CHR$(57 + i)
    IF crs$ = "Y" THEN COLOR 4, 7
    LOCATE i, 14
    PRINT "]"
NEXT i
FOR i = 8 TO 11
    LOCATE i, 42
    PRINT "["
    IF crs$ = "Y" THEN COLOR 0, 7
    LOCATE i, 43
    PRINT CHR$(61 + i)
    IF crs$ = "Y" THEN COLOR 4, 7
    LOCATE i, 44
    PRINT "]"
NEXT i
FOR i = 15 TO 19
    LOCATE i, 20
    PRINT "["
    IF crs$ = "Y" THEN COLOR 0, 7
    LOCATE i, 21
    PRINT CHR$(58 + i)
    IF crs$ = "Y" THEN COLOR 4, 7
    LOCATE i, 22
    PRINT "]"
NEXT i
IF crs$ = "Y" THEN COLOR 10, 5
LOCATE 3, 7
PRINT "     Lotto Max Simulator RC7 Version 2020 Barchart Menu            "
LOCATE 13, 7
PRINT "                     Other Options Available                       "
LOCATE 5, 7
IF crs$ = "Y" THEN COLOR 15, 3
PRINT "         Barcharts            "
LOCATE 6, 7
PRINT "     With Bonus Numbers       "
LOCATE 5, 38
PRINT "         Barcharts                  "
LOCATE 6, 38
PRINT "    Without Bonus Numbers           "
IF crs$ = "Y" THEN COLOR 4, 7
LOCATE 21, 38
PRINT "["
LOCATE 21, 40
PRINT "]"
IF crs$ = "Y" THEN COLOR 1, 7
LOCATE 8, 17
PRINT "Low Numbers"
LOCATE 8, 47
PRINT "Low Numbers"
LOCATE 9, 17
PRINT "Average Numbers"
LOCATE 9, 47
PRINT "Average Numbers"
LOCATE 10, 17
PRINT "High Numbers"
LOCATE 10, 47
PRINT "High Numbers"
LOCATE 11, 17
PRINT "All Numbers"
LOCATE 11, 47
PRINT "All Numbers"
LOCATE 15, 25
PRINT "View Bonus Number Barchart"
LOCATE 16, 25
PRINT "View Normal Charts (No Bonus Numbers)"
LOCATE 17, 25
PRINT "View Normal Charts (With Bonus Numbers)"
LOCATE 18, 25
PRINT "Change any features of this program ....."
LOCATE 19, 25
PRINT "Return To Main Menu"
LOCATE 21, 20
PRINT "Your Choice Is : "
LOCATE 21, 39
answer$ = ""
answer$ = INPUT$(1)
PRINT answer$
IF answer$ = "" OR answer$ = CHR$(27) THEN
    spec$ = "N"
    GOTO main
ELSEIF answer$ = "a" OR answer$ = "A" THEN
    viewpoint$ = "low"
    bonus$ = "Y"
    GOTO start3
ELSEIF answer$ = "b" OR answer$ = "B" THEN
    viewpoint$ = "average"
    bonus$ = "Y"
    GOTO start3
ELSEIF answer$ = "c" OR answer$ = "C" THEN
    viewpoint$ = "high"
    bonus$ = "Y"
    GOTO start3
ELSEIF answer$ = "d" OR answer$ = "D" THEN
    viewpoint$ = "all"
    bonus$ = "Y"
    GOTO start3
ELSEIF answer$ = "e" OR answer$ = "E" THEN
    viewpoint$ = "low"
    bonus$ = "N"
    GOTO start3
ELSEIF answer$ = "f" OR answer$ = "F" THEN
    viewpoint$ = "average"
    bonus$ = "N"
    GOTO start3
ELSEIF answer$ = "g" OR answer$ = "G" THEN
    viewpoint$ = "high"
    bonus$ = "N"
    GOTO start3
ELSEIF answer$ = "h" OR answer$ = "H" THEN
    viewpoint$ = "all"
    bonus$ = "N"
    GOTO start3
ELSEIF answer$ = "i" OR answer$ = "I" THEN
    viewpoint$ = "all"
    bonus$ = "Y"
    normal$ = "N"
    GOTO start3
ELSEIF answer$ = "j" OR answer$ = "J" THEN
    bonus$ = "N"
    GOSUB start2

    GOTO chart
ELSEIF answer$ = "k" OR answer$ = "K" THEN
    bonus$ = "Y"
    GOSUB start2:
    GOTO chart
ELSEIF answer$ = "l" OR answer$ = "L" THEN
    CHAIN "setup"
ELSEIF answer$ = "m" OR answer$ = "M" OR answer$ = "" THEN
    spec$ = "no"
    GOTO main
END IF
GOTO graphmenu
choice:
RETURN
PRINT
PRINT
start2:
IF bonus$ = "N" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = numb(i)
    NEXT i
    low = 63: high = 76
    FOR i = 1 TO 50
        IF numb(i) < low THEN
            low = numb(i)
        END IF
        IF numb(i) > high THEN
            high = numb(i)
        END IF
    NEXT i
    spread = INT(high * .00564)

ELSEIF bonus$ = "Y" AND normal$ = "" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = numb(i) + bonus(i)
    NEXT i
    low = 83: high = 76
    FOR i = 1 TO 50
        IF outpt(i) < low THEN
            low = outpt(i)
        END IF
        IF outpt(i) > high THEN
            high = outpt(i)
        END IF
    NEXT i
    spread = INT(high * .0564)
ELSEIF bonus$ = "Y" AND normal$ = "N" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = bonus(i)
    NEXT i
END IF

RETURN
start3:
IF bonus$ = "N" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = numb(i)
    NEXT i
    low = 63: high = 76
    FOR i = 1 TO 50
        IF numb(i) < low THEN
            low = numb(i)

        END IF
        IF numb(i) > high THEN
            high = numb(i)

        END IF
    NEXT i
    spread = INT(high * .0564)

ELSEIF bonus$ = "Y" AND normal$ = "" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = numb(i) + bonus(i)
    NEXT i
    low = 63: high = 0
    FOR i = 1 TO 50
        IF outpt(i) < low THEN
            low = outpt(i)

        END IF
        IF outpt(i) > high THEN
            high = outpt(i)
        END IF
    NEXT i
    spread = INT(high * .10)
ELSEIF bonus$ = "Y" AND normal$ = "N" THEN
    FOR i = 1 TO 50
        outpt(i) = 0
    NEXT i
    FOR i = 1 TO 50
        outpt(i) = bonus(i)
    NEXT i
END IF
IF grtype = 0 THEN
    CLS
    GOSUB asciidisplay
    GOTO graphmenu
END IF

SCREEN smode
CLS
IF smode = 12 THEN
    PALETTE 2, 12
END IF
counter = 96
IF grmode$ = "HDMI" THEN
    counter = 96
END IF

FOR i = 1 TO 50
    cl = cl + 1: IF cl > 12 THEN cl = 1
    IF cl = 2 THEN cl = 8
    length = outpt(i) * barl
    IF viewpoint$ = "low" THEN
        title$ = " Numbers Least Drawn"
        IF outpt(i) > low + spread THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "average" THEN
        title$ = " Numbers Average Drawn"
        IF outpt(i) < low + spread OR outpt(i) > high - spread THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "high" THEN
        title$ = " Numbers Most Drawn"
        IF outpt(i) < high - spread THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "all" THEN
        title$ = " All Numbers Drawn"
        length = outpt(i) * barl
    END IF
    IF bonus$ = "Y" AND normal$ = "" THEN
        title2$ = "   With Bonus Numbers"
    ELSEIF bonus$ = "N" AND normal$ = "" THEN
        title2$ = "   Without Bonus Numbers"
    ELSEIF bonus$ = "Y" AND normal$ = "N" THEN
        title2$ = "   Just Bonus Numbers"
    END IF
    LINE (counter, wl)-(counter, wl - length), cl
    LINE (counter + 1, wl)-(counter + 1, wl - length), cl
    LINE (counter + 2, wl)-(counter + 2, wl - length), cl
    LINE (counter + 3, wl)-(counter + 3, wl - length), cl
    LINE (counter + 4, wl)-(counter + 4, wl - length), cl
    LINE (counter + 5, wl)-(counter + 5, wl - length), cl
    LINE (counter + 5, wl)-(counter + 5, wl - length), cl
    IF grmode$ = "Herc" THEN
        LINE (counter + 6, wl)-(counter + 6, wl - length), cl
    END IF
    IF grmode$ = "herc" THEN
        counter = counter + 9
    ELSE
        counter = counter + 8
    END IF
NEXT i
PRINT
LOCATE 3, 4
PRINT "Frequency Distribution Chart  " + title$ + title2$
LOCATE 4, 4
PRINT "Covering the period from  "; month$(monthin); yearin; " to  "; month$(m); y
IF grmode$ = "vga" THEN
    LOCATE 27, 13
    PRINT "00000000011111111112222222222333333333344444444445"
    LOCATE 28, 13
    PRINT "12345678901234567890123456789012345678901234567890"
ELSEIF grmode$ = "ega" THEN
    LOCATE 21, 13
    PRINT "00000000011111111112222222222333333333344444444440"
    LOCATE 22, 13
    PRINT "12345678901234567890123456789012345678901234567890"
ELSEIF grmode$ = "cga" THEN
    LOCATE 20, 13
    PRINT "00000000011111111112222222222333333333344444444445"
    LOCATE 21, 13
    PRINT "12345678901234567890123456789012345678901234567890"
ELSEIF grmode$ = "HDMI" THEN
    LOCATE 27, 13
    PRINT "00000000011111111112222222222333333333344444444445"
    LOCATE 28, 13
    PRINT "12345678901234567890123456789012345678901234567890"

END IF
LOCATE 7 - offset, 7
PRINT "200"
LOCATE 7 - offset, 63
PRINT "200"
LOCATE 10 - offset, 7
PRINT "150"
LOCATE 10 - offset, 63
PRINT "150"
LOCATE 15 - offset, 7
PRINT "100"
LOCATE 15 - offset, 63
PRINT "100"
LOCATE 20 - offset, 7
PRINT "50"
LOCATE 20 - offset, 63
PRINT "50"
LOCATE 25 - offset, 7
PRINT "0"
LOCATE 25 - offset, 63
PRINT "0"
IF grmode$ = "vga" THEN
    LOCATE 29, 20
ELSEIF grmode$ = "HDMI" THEN
    LOCATE 29, 20
END IF
PRINT "Press any key to continue ...."
DO WHILE INKEY$ = ""
LOOP
viewpoint$ = "": normal$ = "": bonus$ = ""
SCREEN 0
GOTO graphmenu

graphit:
_FULLSCREEN _STRETCH
FOR i = 1 TO 50
    cl = cl + 1: IF cl > 12 THEN cl = 1
    IF cl = 2 THEN cl = 8
    length = outpt(i) * barl
    IF viewpoint$ = "low" THEN
        title$ = " Numbers Least Drawn"
        IF outpt(i) > low THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "average" THEN
        title$ = " Numbers Average Drawn"
        IF outpt(i) < low + spread OR outpt(i) > high - spread THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "high" THEN
        title$ = " Numbers Most Drawn"
        IF outpt(i) < high - spread THEN
            length = 4
        END IF
    ELSEIF viewpoint$ = "all" THEN
        title$ = " All Numbers Drawn"
        length = outpt(i) * barl
    END IF
    searchmenu:
    CLS
    IF crs$ = "Y" THEN COLOR 3, 8
    LOCATE 3, 10
    PRINT CHR$(201)
    FOR i = 11 TO 68
        LOCATE 3, i
        PRINT CHR$(205)
    NEXT i
    LOCATE 3, 69
    PRINT CHR$(187)
    FOR i = 4 TO 12
        LOCATE i, 10
        IF i = 5 OR i = 11 THEN
            PRINT CHR$(204)
        ELSE
            PRINT CHR$(186)
        END IF
        LOCATE i, 69
        IF i = 5 OR i = 11 THEN
            PRINT CHR$(185)
        ELSE
            PRINT CHR$(186)
        END IF
    NEXT i
    FOR i = 11 TO 68
        LOCATE 5, i
        PRINT CHR$(205)
    NEXT i
    FOR i = 11 TO 68
        LOCATE 11, i
        PRINT CHR$(205)
    NEXT i
    FOR i = 11 TO 68
        LOCATE 13, i
        PRINT CHR$(205)
    NEXT i
    LOCATE 13, 10
    PRINT CHR$(200)
    LOCATE 13, 69
    PRINT CHR$(188)
    IF crs$ = "Y" THEN COLOR 10, 5
    LOCATE 4, 11
    PRINT "Lotto Max Simulator RC7 Version 2020   Search & Print Menu"
    IF crs$ = "Y" THEN COLOR 4, 7
    FOR i = 6 TO 10
        LOCATE i, 17
        PRINT "["
        LOCATE i, 19
        PRINT "]"
    NEXT i
    IF crs$ = "Y" THEN COLOR 0, 7
    FOR i = 6 TO 10
        LOCATE i, 18
        PRINT CHR$(59 + i)
    NEXT i
    IF crs$ = "Y" THEN COLOR 1, 7
    LOCATE 6, 22
    PRINT "Search For Winning Numbers By Date"
    LOCATE 7, 22
    PRINT "Print Out List of Winning Numbers"
    LOCATE 8, 22
    PRINT "Check List of Numbers for Winners"
    LOCATE 9, 22
    PRINT "Scan Database for a series of Numbers"
    LOCATE 10, 22
    PRINT "Print lists from Frequency Calculator"
    LOCATE 12, 17
    PRINT "Your Choice Is: "
    LOCATE 12, 33
    answer$ = INPUT$(1)
    PRINT answer$
    IF answer$ = "a" OR answer$ = "A" THEN
        GOTO search
    ELSEIF answer$ = "b" OR answer$ = "B" THEN
        GOTO printmenu
    ELSEIF answer$ = "c" OR answer$ = "C" THEN
        GOTO checknumbers
    ELSEIF answer$ = "d" OR answer$ = "D" THEN
        GOTO specialcheck
    ELSEIF answer$ = "e" OR answer$ = "E" THEN
        GOTO freqprint
    ELSE
        GOTO main
    END IF
NEXT i
GOTO main
freqprint:
OPEN "next.txt" FOR INPUT AS #1
DO UNTIL EOF(1)
    INPUT #1, a$
    LPRINT a$
LOOP
CLOSE #1

OPEN "freq.txt" FOR INPUT AS #1
DO UNTIL EOF(1)
    INPUT #1, a$
    LPRINT a$
LOOP
CLOSE #1
GOTO main

GOTO graphmenu
GOTO main

OPEN "Lottomax.dat" FOR INPUT AS #1
checkforwinners:
zaaa = entry2(1): zbbb = entry2(2): zccc = entry2(3): zddd = entry(4): zeee = entry2(5): zfff = entry2(6): zggg = entry2(7)
PRINT "Press any key to start search ...."
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT
PRINT "Searching , please stand bye ............."
PRINT
DO UNTIL EOF(1)
    FOR i = 1 TO 8
        IF entry(i) = entry2(j) THEN
            match = match + 1
        END IF
        IF entry(i) = entry2(j) AND match = 6 THEN
            match = match + 1
        END IF
    NEXT i

    counterr = 0
    IF match > 2 THEN
        PRINT "Match for : ";
        PRINT USING "\    \"; month$(chart(i, 1));
        PRINT USING "## ## ## ## ## ## ## ##"; entry(1); entry(2); entry(3); entry(4); entry(5); enntry(6); entry(7); entry(8)
        PRINT "  > "; match; " Correct."
        IF hardcopy$ = "Y" THEN
            LPRINT "Match for : ";
            LPRINT USING "\    \"; month$(chart(i, 1));
            LPRINT USING "## #### ## ## ## ## ## ## ## ##"; chart(i, 2); chart(i, 3); chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9); chart(i, 10); chart(i, 11);
            LPRINT "  > "; match; " Correct."
        END IF

    END IF
LOOP

IF match > 2 THEN

    found = found + 1
END IF
match = 0
sczzz:

IF found = 0 THEN

    PRINT
    PRINT "No Match found for your numbers."
    PRINT
    PRINT "Your numbers were : ";
    IF hardcopy$ = "Y" THEN
        LPRINT "Your Numbers were : ";
        FOR i = 1 TO 7
            IF entry(i) = 0 THEN GOTO nopr1
            LPRINT entry(i);
        NEXT i
        nopr1:
        LPRINT
    ELSE
        PRINT "Your Numbers were : ";
        FOR i = 1 TO 7
            IF entry(i) = 0 THEN GOTO nopr3
            PRINT entry(i);
        NEXT i
    END IF
END IF
nopr3:
PRINT
PRINT found; " Matches found for your numbers."
PRINT
PRINT "Your numbers were : ";
FOR i = 1 TO 7
    IF entry(i) = 0 THEN
        GOTO nopr2
    END IF
    PRINT entry(i);
NEXT i
nopr2:
PRINT

IF hardcopy$ = "Y" THEN
    LPRINT
    LPRINT "Program written by : Russ Campbell."
    LPRINT
END IF
PRINT "Program written by: Russ Campbell."
found = 0
PRINT
PRINT "Press any key to continue ..."

DO WHILE INKEY$ = ""
LOOP
found = 0
GOTO main

specialcheck:
bypass$ = ""
CLS
PRINT "This section checks for patterns in winning numbers. You can choose"
PRINT "to search either the last 3 months of the database, or the whole "
PRINT "database for any sets of numbers that you input."
PRINT
PRINT "Any matches will be printed out as the program runs ...."
PRINT
INPUT "Do you wish to load numbers from a saved file [ Enter defaults to Yes ]: ", saved$
IF saved$ = "" THEN
    saved$ = "Y"
END IF
IF saved$ = "n" OR saved$ = "N" THEN
    saved$ = "N"
    GOTO search2
END IF
filename:
INPUT "Enter filename [without extension] [Enter Defaults to Demo File] : ", file$
IF file$ = "numbers2" THEN
    PRINT
    PRINT "Incorrect filename !"
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
    GOTO filename
ELSEIF file$ = "grmode" THEN
    PRINT
    PRINT "Incorrect filename !"
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
    GOTO filename
ELSEIF file$ = "" THEN
    file$ = "numbers"
    PRINT "Loading Demo File"
END IF
PRINT
file$ = file$ + ".dem"
PRINT file$
IF saved$ = "N" THEN GOTO savednumbers
IF saved$ = "Y" OR saved$ = "y" THEN
    saved$ = "Y"
    OPEN file$ FOR INPUT AS #1
    DO UNTIL EOF(1)
        INPUT #1, entry(zqa)
        zqa = zqa + 1
    LOOP
END IF
CLOSE #1
search2:
PRINT "Search [L]ast 3 months data or [E]ntire list of data "
PRINT
INPUT "[L] or [E] <return defaults to [L] > : ", answer$
IF answer$ = "e" OR answer$ = "E" THEN
    bypass$ = "Y"
    GOTO bypass
END IF
IF month > 3 THEN
    firstmonth = month - 3
    lastmonth = month
    firstyear = year
    lastyear = year
ELSEIF month < 4 THEN
    firstyear = year - 1
    lastyear = year
    monthadj = monthadj + 12
    firstmonth = monthadj - 3
    lastmonth = month
END IF
bypass:
IF saved$ = "Y" THEN GOTO savednumbers
savednumbers:
IF saved$ = "Y" OR saved$ = "y" THEN GOTO sc
INPUT "Do you wish to save the following numbers to disk : ", numsave$
IF numsave$ = "Y" OR numsave$ = "y" THEN
    numsave$ = "Y"
END IF
filename2:
INPUT "Enter filename [without extension] [Enter Defaults to Demo File]: ", file$

IF file$ = "numbers2" THEN
    PRINT "Incorrect filename !"
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
    GOTO filename2
ELSEIF file$ = "grmode" THEN
    PRINT
    PRINT "Incorrect filename !"
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
ELSEIF file$ = "" THEN
    file$ = "numbers"
END IF

PRINT file$
OPEN file$ FOR OUTPUT AS #1


IF numsave$ = "N" THEN GOTO sc
zzzzza:
za = 0: zb = 0: zc = 0: zd = 0: ze = 0: zf = 0: zg = 0
INPUT "How many Numbers [Maximum 7]: ", numbrs
IF numbrs > 7 THEN PRINT "Too Many Numbers !": GOTO zzzzza
FOR zqa = 1 TO numbrs
    rd2:
    IF saved$ = "Y" THEN
        GOTO szc
    END IF
    PRINT "Entry #"; zqa; " ";: INPUT ":", entry(zqa)
    IF entry(zqa) < 1 OR entry(zqa) > 50 THEN PRINT "Number Out of Range ! , Try Again !!": GOTO rd2
    IF zqa > 1 THEN
        FOR i = 1 TO (zqa - 1)
            IF entry(zqa) = entry(i) THEN PRINT "Number already entered, pick another": GOTO rd2
        NEXT i
    END IF
    IF numsave$ = "Y" THEN
        PRINT #1, entry(zqa)
    END IF
NEXT zqa
CLOSE #1

sc:
szc:
za = entry(1): zb = entry(2): zc = entry(3): zd = entry(4): ze = entry(5): zf = entry(6): zg = entry(7)
PRINT "Press any key to start search ...."
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT
PRINT "Searching , please stand bye ............."
PRINT
FOR i = 1 TO linecounter
    IF bypass$ = "Y" THEN
        GOTO bypass20
    END IF
    IF chart(i, 3) < firstyear THEN GOTO sczzz
    IF chart(i, 1) < firstmonth AND chart(i, 3) = firstyear THEN GOTO sczzz
    bypass20:
    FOR j = 4 TO 11
        IF chart(i, j) = za OR chart(i, j) = zb OR chart(i, j) = zc OR chart(i, j) = zd OR chart(i, j) = ze OR chart(i, j) = zf OR chart(i, j) = zg THEN
            match = match + 1
        END IF
        IF chart(i, j) = zh AND match = 7 THEN
            match = match + 1
        END IF
    NEXT j
NEXT i
counterr = 0
IF match > 2 THEN
    PRINT "Match for : ";
    PRINT USING "\    \"; month$(chart(i, 1));
    PRINT USING "## #### ## ## ## ## ## ## ## ##"; chart(i, 2); chart(i, 3); chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9); chart(i, 10); chart(i, 11);
    PRINT "  > "; match; " Correct."
    IF hardcopy$ = "Y" THEN
        LPRINT "Match for : ";
        LPRINT USING "\    \"; month$(chart(i, 1));
        LPRINT USING "## #### ## ## ## ## ## ## ## ##"; chart(i, 2); chart(i, 3); chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9); chart(i, 10); chart(i, 11);
        LPRINT "  > "; match; " Correct."
    END IF

END IF
IF match > 2 THEN
    found = found + 1
END IF
match = 0
GOTO main
checknumbers:
CLS
PRINT
PRINT "Winning Number Check"
PRINT
PRINT "This section of the program will take a file that you have written"
PRINT "out from option C or D and check to see if you have any winning "
PRINT "numbers in the file."
PRINT
PRINT "You can either enter the numbers manually or read them in from"
PRINT "the winning numbers database."
PRINT
INPUT "Enter filename [without extension] : "; file$
IF file$ = "numbers2" THEN
    PRINT
    PRINT "Incorrect filename !"
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
    GOTO checknumbers
ELSEIF file$ = "" THEN
    GOTO searchmenu
END IF

PRINT
PRINT
INPUT "Get numbers Manually or Read from database [M] or [R] : "; answer$
IF answer$ = "r" OR answer$ = "R" THEN
    GOTO inq
ELSEIF answer$ = "M" OR answer$ = "m" THEN
    GOTO manent
ELSE
    GOTO checknumbers
END IF
inq:
INPUT "Enter year of draw  : "; yearinp
IF yearinp < 2000 THEN
    GOTO main
END IF
INPUT "Enter month of draw : "; monthinp
INPUT "Enter day of draw   : "; dayinp
PRINT
PRINT "Date of Draw        : "; month$(monthinp); dayinp; yearinp
PRINT
INPUT "Is this correct ??? "; answer$
PRINT
IF answer$ = "N" OR answer$ = "n" OR answer$ = "no" OR answer$ = "NO" OR answer$ = "nO" OR answer$ = "No" THEN
    GOTO inq
END IF
IF answer$ = "" THEN
    GOTO searchmenu
END IF
FOR i = 1 TO linecounter
    IF chart(i, 3) <> yearinp THEN
        GOTO nextitem2
    ELSEIF chart(i, 1) <> monthinp THEN
        GOTO nextitem2
    ELSEIF chart(i, 2) <> dayinp THEN
        GOTO nextitem2
    ELSEIF chart(i, 3) = yearinp AND chart(i, 1) = monthinp AND chart(i, 2) = dayinp THEN
        search = 1
        PRINT
        PRINT "Winning Numbers : "; chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9), chart(i, 10); chart(i, 11)
        PRINT
        INPUT "Is this Correct <return defaults yes> [Y]es or [N]o : "; answer$
        IF answer$ = "N" OR answer$ = "n" OR answer$ = "NO" OR answer$ = "no" OR answer$ = "No" OR answer$ = "nO" THEN
            GOTO inq
        END IF
        ac = chart(i, 4)
        bc = chart(i, 5)
        cc = chart(i, 6)
        dc = chart(i, 7)
        ec = chart(i, 8)
        fc = chart(i, 9)
        gc = chart(i, 10)
        hc = chart(i, 11)
    END IF
    nextitem2:
NEXT i
IF search = 0 THEN
    PRINT
    PRINT "Draw not found"
    PRINT
    PRINT "Either use manual entry or ensure that correct data"
    PRINT "has been entered with main menu option A"
    PRINT
    PRINT "Press any key to Continue ...."
    DO WHILE INKEY$ = ""
    LOOP
    CLOSE #2
    GOTO searchmenu
END IF
search = 0
GOTO reedfiles
manent:
CLS
PRINT
PRINT "Winning Numbers Entry"
PRINT
PRINT "This option is only for checking winning numbers, it does NOT"
PRINT "update the numbers database. Use option A from the main menu"
PRINT "if you want to update the database."
PRINT
FOR zqa = 1 TO 8
    rd1:
    IF zqa = 8 THEN
        PRINT "Bonus #"; "  "; " ";: INPUT entry(zqa)
    ELSE
        PRINT "Entry #"; zqa; " ";: INPUT entry(zqa)
    END IF
    IF entry(zqa) = 0 THEN GOTO searchmenu
    IF entry(zqa) < 1 OR entry(zqa) > 50 THEN PRINT "Number Out of Range ! , Try Again !!": GOTO rd1
    IF zqa > 1 THEN
        FOR i = 1 TO (zqa - 1)
            IF entry(zqa) = entry(i) THEN PRINT "Number already entered, pick another": GOTO rd1
        NEXT i
    END IF
NEXT zqa

ac = entry(1): bc = entry(2): cc = entry(3): dc = entry(4): ec = entry(5): fc = entry(6): gc = entry(7): hc = entry(8)
reedfiles:
file$ = file$ + ".dat"
OPEN file$ FOR INPUT AS #2

CLS
linecount = 0
DO UNTIL EOF(2)
    INPUT #2, a, b, C, d, e, f, g
    IF a = ac OR a = bc OR a = cc OR a = dc OR a = ec OR a = ac OR a = gc THEN
        winnings = winnings + 1
    END IF
    IF b = ac OR b = bc OR b = cc OR b = dc OR b = ec OR b = fc OR b = gc THEN
        winnings = winnings + 1
    END IF
    IF C = ac OR cc = bc OR e = cc OR C = dc OR C = ec OR C = fc OR C = gc THEN
        winnings = winnings + 1
    END IF
    IF d = ac OR d = bc OR d = cc OR d = dc OR d = ec OR d = fc OR d = gc THEN
        winnings = winnings + 1
    END IF
    IF e = ac OR e = bc OR e = cc OR e = dc OR e = ec OR e = fc OR e = gc THEN
        winnings = winnings + 1
    END IF
    IF f = ac OR f = bc OR f = cc OR f = dc OR f = ec OR f = fc OR f = gc THEN
        winnings = winnings + 1
    END IF
    IF g = ac OR g = bc OR g = cc OR g = dc OR g = ec OR g = cc OR g = gc THEN
        winnings = winnings + 1
    END IF
    IF g = ac OR g = bc OR g = cc OR g = dc OR g = ec OR g = fc OR g = gc THEN
        winnings = winnings + 1
    END IF
    IF a = hc OR b = hc OR C = hc OR d = hc OR e = hc OR f = hc OR g = fc THEN
        bonus = bonus + 1
    END IF

    IF winnings = 7 THEN
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct. You have  $ "; "$"; 700000000
        payout7 = payout7 + 1
        cash7 = 70000000
    END IF
    IF winnings = 6 AND bonus = 1 THEN
        winnings = winnings + 1
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout6b = payout6b + 1
        cash6b = 197679.90
    END IF
    IF winnings = 6 THEN
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout6 = payout6 + 1
        cash6 = 3822
    END IF
    IF winnings = 5 AND bonus = 1 THEN
        winnings = winnings + 1
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout5b = payoutb + 1
        cash5b = 791.50
    END IF
    IF winnings = 5 THEN
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout5 = payout5 + 1
        cash5 = 95.5
    END IF
    IF winnings = 4 AND bonus = 1 THEN
        winnings = winnings + 1
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout4b = payout4b + 1
        cash4b = 44.30

    END IF
    IF winnings = 4 THEN
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout4 = payout4 + 1
        cash4 = 20
    END IF

    IF winnings = 3 AND bonus = 1 THEN
        winnings = winnings + 1
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct."
        payout3b = payout3b + 1
        cash3b = 20
    END IF
    IF winnings = 3 THEN
        PRINT "Line # "; linecount; ">= ";
        PRINT USING "## ## ## ## ## ## ##"; a; b; C; d; e; f; g
        PRINT "    > "; winnings; "  Correct.";
        payout3 = payout3 + 1
        cash3 = 5
    END IF
    IF payout3 > 0 THEN
        PRINT "Cash for 3/7  Numbers Matched = Free Ticket"
    END IF
    IF payout3b > 0 THEN
        PRINT "Cash for 3/7  Numbers Matched and Bonus matched = $20.00"
    END IF
    IF payout4 > 0 THEN
        PRINT "Cash for 4/7  Numbers Matched  = $20.00"
    END IF
    IF payout4b > 0 THEN
        PRINT "Cash for 4/7  Numbers Matched and Bonus matched = $44.30"
    END IF
    IF payout5 > 0 THEN
        PRINT "Cash for 5/7  Numbers Matched = 96.50"
    END IF
    IF payout5b > 0 THEN
        PRINT "Cash for 5/7  Numbers Matched and Bonus matched = $791.50"
    END IF
    IF payout6 > 0 THEN
        PRINT "Cash for 6/7  Numbers Matched = $3,822.00"
    END IF
    IF payout6b > 0 THEN
        PRINT "Cash for 6/7  Numbers Matched and Bonus matched = $197,679.00"
    END IF
    IF payout7 > 0 THEN
        PRINT "Cash for 7/7  Numbers Matched  = $"; grandprize
    END IF
    cash3 = 0
    cash3b = 0
    cash4 = 0
    cash4b = 0
    cash5 = 0
    cash5b = 0
    cash6 = 0
    cash6b = 0
    cash7 = 0
    winnings = 0

LOOP
PRINT "Press any key to continue......"
DO WHILE INKEY$ = ""
LOOP


GOSUB winningsandloosings
GOSUB payouts
GOTO main

payouts:
grandprize = INT((70) * RND(0)) + 1 + 1000000
IF grandprize < 10000000 THEN grandprize = 10000000
CLS
PRINT
PRINT "Total of 3/7              Payouts of free tickets =  "; payout3
PRINT "Total of 3/7      + Bonus Payouts of $20.00 Cash is $"; payout3b * 20
PRINT "Total of 4/7              Payouts of $20.00 Cash is $"; payout4 * 20
PRINT "Total of 4/7 +      Bonus Payouts of $44.30 Cash is $"; payout4b * 44.3
PRINT "Total of 5/7              Payouts of $96.50 Cash is $"; payout5 * 96.5
PRINT "Total of 5/7 +     Bonus Payouts of $791.50 Cash is $"; payout5b * 791.5
PRINT "Total of 6/7            Payouts of $3822.00 Cash is $"; payout6 + 3822
PRINT "Total of 6/7 + Bonus Payouts of $197,679.00 Cash is $"; payout6b * 197679
PRINT "Total of 7/7      Payouts of $"; grandprize; " Cash is $"; payout7 * grandprize
PRINT
grandtotalpayouts = payout3 + payout3b + payout4 + payout4b + payout5 + payout5b + payout6 + payout6b + payout7
PRINT "Total Payouts from the above is :  grandtotalpayouts"
PRINT ""
PRINT "You had a total of "; correctnum; " possible winners in that file."
PRINT "Check your printout, diskfile or tickets for more info."
IF hardcopy$ = "Y" AND correctnum > 3 THEN
    LPRINT
    LPRINT "Program Written by : Russ Campbell"
    LPRINT

END IF
correctnum = 0
PRINT
PRINT "The winning numbers were : "; ac; bc; cc; dc; ec; fc; gc; " Bonus # "; hc
PRINT
PRINT "Press any key to continue ....."
PRINT
DO WHILE INKEY$ = ""
LOOP
CLOSE #2
PRINT "Press any key to continue....."
DO WHILE INKEY$ = ""
LOOP
RETURN



enterwinningnumbers:
OPEN "lottomax.dat" FOR APPEND AS #1
beginning:
CLS
PRINT
PRINT "Lotto Max Package Version 2020 RC7 Winning Numbers Entry"
PRINT "--------------------------------------------------------"
PRINT
PRINT "This section allows you to enter winning numbers from each draw."
PRINT "It then stores the numbers in a database for use with the chart"
PRINT "used in section B. I now have automatic next draw date entry."
PRINT
PRINT "Winning Numbers Updated on : ";
PRINT "Last numbers entered : "; a; b; C; d; e; f; g; h
PRINT "Next Draw Date "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
PRINT
IF d1 < __GETDAY THEN
    PRINT "The draw has not happened yet !!!"
END IF
enternumbers:
PRINT
first:
PRINT "Enter 1st number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry] : ", a
IF a = 0 THEN CLOSE #1: count = -1: GOTO main
IF __GETDAY < d1 THEN
    PRINT
    PRINT "The draw has not happened yet !!!"
    PRINT "Press any key to Continue..."
    DO WHILE INKEY$ = ""
    LOOP
    CLOSE #1
    count = -1
    GOTO main
END IF
IF a = -1 THEN PRINT "You are already at entry # 1 !!! You cant go back another entry !!!": GOTO first
IF a < 1 OR a > 50 THEN PRINT "Number Out of range !": GOTO first
second:
PRINT "Enter 2nd number for the draw of : "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", b
IF b = -1 THEN GOTO first
IF b < 1 OR b > 50 THEN PRINT "Number Out of Range !": GOTO second
IF b = a THEN PRINT "Number already picked!": GOTO second
third:
PRINT "Enter 3rd number for the draw of : "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", C
IF C = -1 THEN GOTO second
IF C < 1 OR C > 50 THEN PRINT "Number Out of Range !": GOTO third
IF C = b OR C = a THEN PRINT "Number already picked!": GOTO third
fourth:
PRINT "Enter 4th number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", d
IF d = -1 THEN GOTO third
IF d < 1 OR d > 50 THEN PRINT "Number Out of Range !": GOTO fourth
IF d = C OR d = b OR d = a THEN PRINT "Number already picked!": GOTO fourth
fifth:
PRINT "Enter 5th number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", e
IF e = -1 THEN GOTO fourth
IF e < 1 OR e > 50 THEN PRINT "Number Out of Range !": GOTO fifth
IF e = d OR e = C OR e = b OR e = a THEN PRINT "Number already picked!": GOTO fifth
sixth:
PRINT "Enter 6th number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", f
IF f = -1 THEN GOTO fifth
IF f < 1 OR f > 50 THEN PRINT "Number Out of Range !": GOTO sixth
IF f = e OR f = d OR f = C OR f = b OR f = a THEN PRINT "Number already picked!": GOTO sixth
seventh:
PRINT "Enter 7th number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", g
IF g = -1 THEN GOTO sixth
IF g < 1 OR f > 50 THEN PRINT "Number Out of Range !": GOTO seventh
IF f = e OR f = d OR f = C OR f = b OR f = a THEN PRINT "Number already picked!": GOTO seventh
eighth:
PRINT "Enter bonus number for the draw of "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
INPUT "[if you make a mistake type -1 on the next number to go back 1 entry ] : ", h
IF h = -1 THEN GOTO seventh
IF h < 1 OR g > 50 THEN PRINT "Number Out of Range !": GOTO eighth
IF h = g OR h = f OR h = e OR h = d OR h = C OR h = b OR h = a THEN PRINT "Number already picked !": GOTO eighth
PRINT
PRINT "You Have Entered for : "; GetDay$(m, (d1), y); " "; month$(m); (d1); y
PRINT a; " "; b; " "; C; " "; d; " "; e; " "; f; " "; g; " Bonus  :"; h
PRINT

PRINT "Is the above correct [Y]es  No  Quit [Y default] : "
answer$ = INPUT$(1)

IF answer$ = "" OR answer$ = "y" THEN answer$ = "Y"
IF answer$ = "n" OR answer$ = "N" OR answer$ = "no" OR answer$ = "NO" THEN count = -1: GOTO beginning
IF answer$ = "Q" OR answer$ = "q" THEN CLOSE #1: GOTO main
IF answer$ = "Y" THEN

    PRINT #1, USING "## ## ####  ## ## ## ## ## ## ## ##"; nextdraw; d1; y; a; b; C; d; e; f; g; h

END IF
PRINT
PRINT "Any more winning numbers ??? [Y] or No [N default] "
answer$ = INPUT$(1)
IF answer$ = "" OR answer$ = "Y" OR answer$ = "y" THEN
    count = 1: CLOSE #1: GOSUB buildchart: GOSUB convert: GOSUB getdate: GOTO enterwinningnumbers
ELSE
    CLOSE #1
    GOSUB buildchart
    GOSUB convert
    GOSUB getdate
    count = 0: GOTO main
END IF
GOTO main

GOTO main
buildchart:
chartbuild = chartbuild + 1
chartcount = 100
linecounter = 0
ERASE chart
OPEN "lottomax.dat" FOR INPUT AS #1
DO UNTIL EOF(1)
    INPUT #1, month, day, year, a, b, C, d, e, f, g, h
    chart(linecounter, 1) = month
    chart(linecounter, 2) = day
    chart(linecounter, 3) = year
    chart(linecounter, 4) = a
    chart(linecounter, 5) = b
    chart(linecounter, 6) = C
    chart(linecounter, 7) = d
    chart(linecounter, 8) = e
    chart(linecounter, 9) = f
    chart(linecounter, 10) = g
    chart(linecounter, 11) = h
    linecounter = linecounter + 1
LOOP
CLOSE #1

convert:

FOR i = 1 TO linecounter
    IF spec$ = "Y" THEN
        IF chart(i, 3) < yearin THEN GOTO zqzq
        IF chart(i, 1) < monthin AND chart(i, 3) = yearin THEN GOTO zqzq
        IF chart(i, 3) > yearend THEN GOTO zqzq
        IF chart(i, 1) > monthend AND chart(i, 3) = yearend THEN GOTO zqzq
    END IF
    numb(chart(i, 4)) = numb(chart(i, 4)) + 1
    numb(chart(i, 5)) = numb(chart(i, 5)) + 1
    numb(chart(i, 6)) = numb(chart(i, 6)) + 1
    numb(chart(i, 7)) = numb(chart(i, 7)) + 1
    numb(chart(i, 8)) = numb(chart(i, 8)) + 1
    numb(chart(i, 9)) = numb(chart(i, 9)) + 1
    numb(chart(i, 10)) = numb(chart(i, 10)) + 1
    bonus(chart(i, 11)) = bonus(chart(i, 11)) + 1

    zqzq:
NEXT i
IF spec$ = "N" THEN
    yearin = chart(1, 3)
    yearend = chart(linecounter, 3)
    monthin = chart(1, 1)
    monthend = chart(linecounter, 3)
END IF
RETURN
printmenu:
CLS
IF crs$ = "Y" THEN COLOR 10, 5

LOCATE 4, 18
PRINT " Lotto Max Version 2020 RC7  Print Menu  "
IF crs$ = "Y" THEN COLOR 3, 8
LOCATE 3, 17
PRINT CHR$(201)
FOR i = 18 TO 57
    LOCATE 3, i
    PRINT CHR$(205)
NEXT i
LOCATE 3, 58
PRINT CHR$(187)
FOR i = 18 TO 57
    LOCATE 5, i
    PRINT CHR$(205)
NEXT i
FOR i = 18 TO 57
    LOCATE 9, i
    PRINT CHR$(205)
NEXT i
FOR i = 18 TO 57
    LOCATE 11, i
    PRINT CHR$(205)
NEXT i
LOCATE 11, 17
PRINT CHR$(200)
LOCATE 11, 58
PRINT CHR$(188)
FOR i = 4 TO 10
    LOCATE i, 17
    IF i = 5 OR i = 9 THEN
        PRINT CHR$(204)
    ELSE
        PRINT CHR$(186)
    END IF
    LOCATE i, 58
    IF i = 5 OR i = 9 THEN
        PRINT CHR$(185)
    ELSE
        PRINT CHR$(186)
    END IF
NEXT i
IF crs$ = "Y" THEN COLOR 4, 7
FOR i = 6 TO 8
    LOCATE i, 27
    PRINT "["
    LOCATE i, 28
    IF crs$ = "Y" THEN COLOR 0, 7
    PRINT CHR$(59 + i)
    IF crs$ = "Y" THEN COLOR 4, 7
    LOCATE i, 29
    PRINT "]"
NEXT i
LOCATE 10, 45
PRINT "["
LOCATE 10, 47
PRINT "]"
IF crs$ = "Y" THEN COLOR 1, 7
LOCATE 6, 31
PRINT "Print Entire List"
LOCATE 7, 31
PRINT "Print Partial List"
LOCATE 8, 31
PRINT "Goto Search Menu"
LOCATE 10, 27
PRINT "Your Choice is:"
LOCATE 10, 46
answer$ = INPUT$(1)
PRINT answer$
IF answer$ = "a" OR answer$ = "A" THEN
    monthin = 1
    monthend = 12
    yearin = 2000
    yearend = 3000
    GOTO printentirelist
ELSEIF answer$ = "b" OR answer$ = "B" THEN
    monthin = 1
    monthend = 12
    yearin = 2000
    yearend = 3000
    GOTO printpartial
ELSEIF answer$ = "c" OR answer$ = "C" OR answer$ = "" THEN
    GOTO searchmenu
ELSE
    GOTO searchmenu
END IF
printpartial:
CLS
PRINT
PRINT "Printing Partial List of winning numbers."
partial = 1
PRINT
INPUT "Start  Month: ", monthin
IF monthin = 0 THEN GOTO main
IF monthin < 1 OR monthin > 12 THEN
    GOTO printpartial
END IF
INPUT "Start  Year [All 4 digits] : ", yearin
IF yearin = 0 THEN
    GOTO main
END IF
INPUT "Finish Month: ", monthend
IF monthend = 0 THEN
    GOTO main
END IF
IF monthend < 1 OR monthend > 12 THEN
    GOTO printpartial
END IF
INPUT "Ending Year [All 4 digits] : ", yearend
IF yearend = 0 THEN
    GOTO main
END IF
printentirelist:
IF partial <> 0 THEN GOTO lister
PRINT
PRINT "Printing Entire List of winning numbers."
PRINT
lister:
IF hardcopy$ = "Y" THEN
    LPRINT
    LPRINT "List of Lotto Max Winning Numbers"
    LPRINT "---------------------------------"
    LPRINT
    LPRINT "Month | Day | Year |  1st | 2nd | 3rd | 4th | 5th | 6th | 7th | Bonus"
    LPRINT "----- | --- | ---- |  --- | --- | --- | --- | --- | --- | --- | -----"
    LPRINT
    pat$ = "   ## |  ## | #### |   ## |  ## |  ## |  ## |  ## |  ## |  ## |  ##  "
    FOR i = 1 TO linecounter
        IF chart(i, 3) < yearin THEN GOTO qzqz
        IF chart(i, 1) < monthin AND chart(i, 3) = yearin THEN GOTO qzqz
        IF chart(i, 3) > yearend THEN GOTO qzqz
        IF chart(i, 1) > monthend AND chart(i, 3) = yearend THEN GOTO qzqz
        LPRINT USING pat$; chart(i, 1); chart(i, 2); chart(i, 3); chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9); chart(i, 10); chart(i, 11)
        qzqz:
    NEXT i
END IF
CLS
PRINT
PRINT "List of Lotto Max Winning Numbers"
PRINT "---------------------------------"
PRINT
PRINT "Month | Day | Year |  1st | 2nd | 3rd | 4th | 5th | 6th | 7th | Bonus"
PRINT "----- | --- | ---- |  --- | --- | --- | --- | --- | --- | --- | -----"
PRINT
pat$ = "   ## |  ## | #### |   ## |  ## |  ## |  ## |  ## |  ## |  ## | ##  "
FOR i = 1 TO linecounter
    linecounter = linecounter + 1
    IF chart(i, 3) < yearin THEN GOTO qzqzqz
    IF chart(i, 1) < monthin AND chart(i, 3) = yearin THEN GOTO qzqzqz
    IF chart(i, 3) > yearend THEN GOTO qzqzqz
    IF chart(i, 1) > monthend AND chart(i, 3) = yearend THEN GOTO qzqzqz
    PRINT USING pat$; chart(i, 1); chart(i, 2); chart(i, 3); chart(i, 4); chart(i, 5); chart(i, 6); chart(i, 7); chart(i, 8); chart(i, 9); chart(i, 10); chart(i, 11)
    FOR zz = 1 TO 5000000: NEXT zz
    qzqzqz:
NEXT i
DO WHILE INKEY$ = ""
LOOP
GOTO printmenu
errorhandle:
CLS
PRINT
PRINT "There is something wrong with what you have input. Either you said"
PRINT "that you have a printer online, when in fact you don't or you have"
PRINT "tried to read a non-existant file."
PRINT
PRINT "In the above case if you do have a printer online, make sure that"
PRINT "it is turned on, is online and has paper in it."
PRINT
PRINT "If you are trying to read the chart, and get this error, then make "
PRINT "sure that the 'lottomax.dat' file exists.  "
PRINT
PRINT "If you are using the barchart menu, and are trying to display a chart"
PRINT "when this error occurs, then it is most likely that you have the "
PRINT "wrong graphics mode set. Try reseting your graphics mode."
PRINT
PRINT "If all else fails, and the error keeps coming up, then it is obviously"
PRINT "a program error. Please let me know about it and I will try to fix it."
PRINT
PRINT
PRINT "Press any key to continue ..."
DO WHILE INKEY$ = ""
LOOP
GOTO main
asciidisplay:
FOR j = 1 TO 50
    IF viewpoint$ = "low" THEN
        title$ = "Numbers Least Drawn"
        IF outpt(j) > low + spread THEN
            markz = 1
        END IF
    ELSEIF viewpoint$ = "average" THEN
        title$ = " Numbers Average Drawn"
        IF outpt(j) < low + spread OR outpt(i) > high - spread THEN
            markz = 1
        END IF
    ELSEIF viewpoint$ = "high" THEN
        title$ = " Numbers Most Drawn"
        IF outpt(j) < high - spread THEN
            markz = 1
        END IF
    ELSEIF viewpoint$ = "all" THEN
        title$ = "All Numbers Drawn"
    END IF
    IF bonus$ = "Y" AND normal$ = "" THEN
        title2$ = "  With Bonus Numbers"
    ELSEIF bonus$ = "N" AND normal$ = "" THEN
        title2$ = "  Without Bonus Numbers"
    ELSEIF bonus$ = "Y" AND normal$ = "N" THEN
        title2$ = "  Just Bonus Numbers"
    END IF
    IF markz > 0 THEN
        GOTO disp
    END IF
    countz = (outpt(j) / 16.247255256274392)
    countz = 30 - countz
    IF ca > 2 THEN
        ca = 0
    END IF
    FOR i = 20 TO countz STEP -1
        LOCATE i, j + 11
        PRINT CHR$(176 + ca)
    NEXT i
    disp:
    markz = 0
    ca = ca + 1
NEXT j
LOCATE 3, 4
PRINT "Frequency Distribution Chart  " + title$ + title2$
LOCATE 4, 4
PRINT "Covering the period from "; month$(monthin); yearin; " to "; month$(monthend); yearend
LOCATE 21, 12
PRINT "00000000011111111112222222222333333333344444444445"
LOCATE 22, 12
PRINT "12345678901234567890123456789012345678901234567890"
xyxty = 4 * 2
FOR i = 20 TO 6 STEP -1
    LOCATE i, 6
    PRINT xyxty
    LOCATE i, 63
    PRINT xyxty
    xyxty = xyxty + 48
NEXT i
LOCATE 24, 20
PRINT "Press any key to continue ..."
DO WHILE INKEY$ = ""
LOOP
viewpoint$ = "": normal$ = "": bonus$ = ""
RETURN
GOTO main

rules:
IF helpfile$ = "N" THEN GOTO help4
IF crs$ = "Y" THEN COLOR 4, 1

CLS
PRINT
PRINT "Lotto Max 2020 RC5 Simulation Program With Real Time Game Play Rules."
PRINT "---------------------------------------------------------------------"
PRINT
PRINT "This program has been written as a simulator of the"
PRINT "popular Lotto Max lottery and is written to be played"
PRINT "as a game in real time.. But not by spending real money"
PRINT "for real prizes by matching 7 out of 50 numbers."
PRINT
PRINT "The program starts off by giving you $40,000 to spend"
PRINT "and will automatically keep track of ticket"
PRINT "purchases, and winning tickets. Every time you buy (not for"
PRINT "real), sets of tickets the program first charges you the"
PRINT "market rate for your tickets, then saves the random or"
PRINT "wheeled system sets of 7 numbers from 1 to 50."
PRINT
PRINT "This program is designed to show you that if you play the"
PRINT "game responibly , you will have a lot of fun, but if you"
PRINT "gamble recklessly, you will in most cases loose a lot of money."
PRINT
PRINT "Press any key to continue..."
DO WHILE INKEY$ = ""
LOOP
help4:
RETURN
basicrules:
IF helpfile$ = "N" THEN GOTO help5
IF crs$ = "Y" THEN COLOR 5, 1
CLS
PRINT
PRINT "Basic Rules and object of this Lotto Max 2020 RC7 Lottery Simulator"
PRINT "-------------------------------------------------------------------"
PRINT
PRINT "You start out with $40,000, and the object"
PRINT "of the game is to win $70 Million dollars by matching"
PRINT "all 7 numbers from 1 to 50. "
PRINT
PRINT "Prize Payout (Not in real dollars but funny money)."
PRINT "---------------------------------------------------"
PRINT
PRINT "Cash for 3/7  Numbers Matched                   = Free Ticket"
PRINT "Cash for 3/7  Numbers Matched and Bonus matched = $20.00"
PRINT "Cash for 4/7  Numbers Matched                   = $20.00"
PRINT "Cash for 4/7  Numbers Matched and Bonus matched = $44.30"
PRINT "Cash for 5/7  Numbers Matched                   = $96.50"
PRINT "Cash for 5/7  Numbers Matched and Bonus matched = $791.50"
PRINT "Cash for 6/7  Numbers Matched                   = $3,822.00"
PRINT "Cash for 6/7  Numbers Matched and Bonus matched = $197,679.00"
PRINT "Cash for 7/7  Numbers Matched                   = $70,000,000.00"
PRINT
PRINT "These payouts are at the maximun for the benefit of the game here."
PRINT
PRINT "Press any  key to continue...."
DO WHILE INKEY$ = ""
LOOP
help5:
RETURN
worthit:
IF helpfile$ = "N" THEN GOTO help6
IF crs$ = "Y" THEN COLOR 8, 1
CLS
PRINT
PRINT "Is it worth playing the lotteries in the long run ???"
PRINT
PRINT "You decide by playing this simulator as if it were real,"
PRINT "almost every method of tracking the database of winning"
PRINT "of winning numbers and winning this game has been"
PRINT "provided to you in the main menu."
PRINT "I have written this program as a retro DOS style program,"
PRINT "everything has been done with the program to make it seem"
PRINT "like an 80s style program , with some modern ideas of how"
PRINT "to track the lotteries. I hope the you enjoy this simulator"
PRINT "and treat it as a learning tool, not a way of trying to"
PRINT "actually beat the lottery systems that are out there."
PRINT "All of the winning numbers have been faithfully recorded"
PRINT "in a simple database and each draw date is automatically"
PRINT "calculated by my coding of this program."
PRINT "Basically if you play responsibly , you will have a lot of"
PRINT "fun, but if you gamble recklessly , you will loose a lot of"
PRINT "money, in most cases everything you have."
PRINT
PRINT "Lets just say that now I support OLG and I used to never play"
PRINT "the games, butI will now, responsibly."
PRINT
PRINT "Press any key to continue ...."
DO WHILE INKEY$ = ""
LOOP
help6:
RETURN
startingandending:
IF helpfile$ = "N" THEN GOTO help7
IF crs$ = "Y" THEN COLOR 6, 1
CLS
PRINT
PRINT "Lotto Max Simulation Program In Real Time."
PRINT "-------------------------------------------"
PRINT
PRINT "The reason that I call this program"
PRINT "Lotto Max Simulation Program is because it"
PRINT "is all about the game play, not gambling with"
PRINT "a lot of money, and in this simulator you can"
PRINT "play with a lot of funny money, and watch the"
PRINT
PRINT "I hope that you will enjoy playing this"
PRINT "Lotto Max Lottery Simulator in real time,"
PRINT "I will make a disclaimer here that you"
PRINT "are not playing for real money."
PRINT "If you decide to play for real, I take no "
PRINT "responsibility for the consequences."
PRINT
PRINT "I do not condone trying to beat the lottery"
PRINT "system in any way shape or form. I do provide"
PRINT "ways of tracking the outcome of the Lotto Max"
PRINT "Lottery, but this information is for educational"
PRINT "purposes only."
PRINT
PRINT "Press any key to continue......"
DO WHILE INKEY$ = ""
LOOP
help7:
RETURN
aboutthisprogram:
IF crs$ = "" THEN COLOR 3, 1
CLS
PRINT
PRINT "This program called Lotto Max Simulator has been written by"
PRINT "Russ Campbell of Kitchener Ontario, Canada."
PRINT
PRINT "My email address to contact me about this program, or bugs in it,"
PRINT "or game ideas or positive suggestions is"
PRINT "rcamp48 AT rogers dot com. PLEASE !!!!!! NO SPAM !!!!!!!"
PRINT
PRINT "This program has been written in a time period of over 35 years"
PRINT "and developed initially as a way of beating the Lottery system,"
PRINT "however it is no longer designed to do that now."
PRINT
PRINT "You can however use this program to play resposively, and you will"
PRINT "have a lot of fun, its not really about gambling a lot of money"
PRINT "on what could in most cases if not 99 percent of them, cost you"
PRINT "almost everything you own. Remember, have a lot of fun playing "
PRINT "the game, but gamble responibly. This program will show you what"
PRINT "what will happen if you do not gamble responsibly."
PRINT
PRINT "Press any key to continue..."
DO WHILE INKEY$ = ""
LOOP
RETURN
getdate:
d1 = __GETDAY - 1

VirtualYear = y

FOR l = 1 TO 366 * 40
    d1 = d1 + 1
    IF ISLEAPYEAR(VirtualYear) AND d1 > 366 THEN VirtualYear = VirtualYear + 1: d1 = d1 - 366
    IF ISLEAPYEAR(VirtualYear) = 0 AND d1 > 365 THEN VirtualYear = VirtualYear + 1: d1 = d1 - 365
    today$ = ""
    DO UNTIL today$ = "Friday" OR today$ = "Tuesday"
        today$ = GETDAYNAME(VirtualYear, 1, d1)
        d1 = d1 + 1
    LOOP
    d1 = d1 - 1
    day = GETDAYNR(VirtualYear, 1, d1)
    realdate$ = GETDATE$(day, VirtualYear)
    IF today$ = "Friday" AND count = -1 THEN LASTDAY = -3
    IF today$ = "Tuesday" AND count = -1 THEN LASTDAY = -4
    IF today$ = "Friday" OR today$ = "Tuesday" THEN EXIT FOR
NEXT
RETURN


chart2:
CLS
PRINT
PRINT "This section will check each number from 1 to 50 and see how often"
PRINT "it occurs. It takes a few seconds to run however."
PRINT
PRINT "It will then give you a saved report that tells when the numbers came"
PRINT "up last, when they may come up again by, and the average days between"
PRINT "occurances. Note that your winning numbers database has the be up to"
PRINT "date completely for this to work properly."
PRINT
PRINT "Press any key to continue ....."
DO WHILE INKEY$ = ""
LOOP
CLS
x1 = VAL(MID$(DATE$, 1, 2))
x2 = VAL(MID$(DATE$, 4, 5))
x3 = VAL(MID$(DATE$, 7, 10))
x1 = x1 * 30
x3 = x3 * 365
now2 = x1 + x2 + x3
OPEN "freq.txt" FOR OUTPUT AS #1 LEN = 80
OPEN "next.txt" FOR OUTPUT AS #2 LEN = 80

FOR j = 1 TO 50
    FOR i = 1 TO linecounter
        IF chart(i, 4) = j OR chart(i, 5) = j OR chart(i, 6) = j OR chart(i, 7) = j OR chart(i, 8) = j OR chart(i, 9) = j OR chart(i, 10) = j THEN
            count = count + 1
            k = i
            IF count = 1 THEN
                day3 = chart(i, 2)
                month1 = chart(i, 1)
                year1 = chart(i, 3)
            END IF
            IF count > 1 THEN
                day4 = chart(i, 2)
                month2 = chart(i, 1)
                year2 = chart(i, 3)
                now = (year2 * 365) + (month2 * 30) + day4
                thn = (year1 * 365) + (month1 * 30) + day3
                balance = now - thn
                temp4(j) = temp4(j) + balance
                year1 = year2
                month1 = month2
                day3 = day4
            END IF
        END IF
    NEXT i
    temp4(j) = INT(temp4(j) / count)
    count = 0
    PRINT #1, USING "Number ## comes up every ## days."; j; temp4(j)
    PRINT #1, USING "Number ## last came up ## ## ####."; j; chart(k, 1); chart(k, 2); chart(k, 3)
    day5 = chart(k, 2) + temp4(j)
    IF day5 > 30 THEN
        day5 = day5 - 30
        month3 = chart(k, 1) + 1
    ELSE
        month3 = chart(k, 1)
    END IF
    IF month3 > 12 THEN
        month3 = month3 - 12
        year3 = chart(k, 3) + 1
    ELSE
        year3 = chart(k, 3)
    END IF
    PRINT #1, USING "Number ##   due by:    ## ## ####."; j; month3; day5; year3
    z1 = month3 * 30
    z2 = day3
    z3 = year3 * 365
    thn2 = z1 + z2 + z3
    IF thn2 < (now2 - 20) THEN
        PRINT #2, USING "Number  ##  is a good bet as it was due by : ## ## ####"; j; month3; day3; year3
        PRINT USING "Number ## is a good bet, it was due by : ## ## ####"; j; month3; day3; year3
    END IF
    k = 0
    PRINT #1, ""
NEXT j
PRINT
PRINT "Press any key to continue ..."
DO WHILE INKEY$ = ""
LOOP
CLOSE #1
CLOSE #2
GOTO main
search:
CLS
PRINT
PRINT "Search for Winning Numbers by Date"
PRINT "----------------------------------"
PRINT
month2:
INPUT "Enter Month: ", month2
IF month2 = 0 THEN
    GOTO searchmenu
END IF
IF month2 < 1 OR month2 > 12 THEN
    PRINT "Invalid Month !": GOTO month2
END IF
day2:
INPUT "Enter Day: ", day2
IF day2 < 1 OR day2 > 31 THEN
    PRINT "Invalid Month !": GOTO day2
END IF
INPUT "Enter Year [Last 2 digits] : ", year2
year2 = year2 + 2000
FOR i = 1 TO linecounter
    IF chart(i, 3) <> year2 THEN
        GOTO nextitem
    ELSEIF chart(i, 1) <> month2 THEN
        GOTO nextitem
    ELSEIF chart(i, 2) <> day2 THEN
        GOTO nextitem
    ELSEIF chart(i, 3) = year2 AND chart(i, 1) = month2 AND chart(i, 2) = day2 THEN
        search = 1
        CLS
        PRINT
        PRINT "Winning Numbers List"
        PRINT "--------------------"
        PRINT
        PRINT "Month : ", chart(i, 1)
        PRINT "Day   : ", chart(i, 2)
        PRINT "Year  : ", chart(i, 3)
        PRINT
        PRINT "1st   : ", chart(i, 4)
        PRINT "2nd   : ", chart(i, 5)
        PRINT "3rd   : ", chart(i, 6)
        PRINT "4th   : ", chart(i, 7)
        PRINT "5th   : ", chart(i, 8)
        PRINT "6th   : ", chart(i, 9)
        PRINT "7th   : ", chart(i, 10)
        PRINT "Bonus : ", chart(i, 11)
        PRINT
        PRINT "Press any key to continue ... "
        PRINT
        DO WHILE INKEY$ = ""
        LOOP
    END IF
    nextitem:
NEXT i
IF search = 0 THEN
    PRINT
    PRINT "There was no draw on : "; month2; day2; year2
    PRINT
    PRINT "Press any key to continue ...."
    DO WHILE INKEY$ = ""
    LOOP
END IF
search = 0
GOTO searchmenu
grcheck:
OPEN "grmode.dat" FOR INPUT AS #1
INPUT #1, hardcopy$
INPUT #1, crs$
INPUT #1, grtype
INPUT #1, qqq
INPUT #1, skill$
INPUT #1, display$
INPUT #1, helpfile$
INPUT #1, speed$
CLOSE #1
OPEN "dname.dat" FOR INPUT AS #1
INPUT #1, reg$
FOR i = 1 TO LEN(reg$)
    counter4 = counter4 - 5.406713
    z2 = ASC(MID$(reg$, i, i))
    MID$(reg$, i, i) = CHR$(z2 + counter4)
NEXT i
CLOSE #1
counter4 = 0
OPEN "reg.dat" FOR INPUT AS #1
INPUT #1, reg2$
FOR i = 1 TO LEN(reg2$)
    counter4 = counter4 - 5.435713
    z2 = ASC(MID$(reg2$, i, i))
    MID$(reg2$, i, i) = CHR$(z2 + counter4)
NEXT i
realname$ = reg$
realreg$ = reg2$
CLOSE #1
RETURN

grinit:
IF hardcopy$ = "y" THEN
    hardcopy$ = "Y"
END IF
IF grtype = 0 THEN
    grmode$ = "none"
ELSEIF grtype = 1 THEN
    grmode$ = "vga"
    wl = 400
    smode = 12
    barl = 1.84762357837476237
    offset = 0
ELSEIF grtype = 2 THEN
    grmode$ = "HDMI"
    wl = 400
    smode = 12
    barl = 1.84235735763546337
    offset = 0
END IF
RETURN
monthread:
DATA "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
month$ = month$(month)
RETURN
finish:
END

'function GETDAYNAME parameters: YEAR, MONTH, DAY
'listing of Tuesdays and Thursdays for 40 years

FUNCTION GETDAYNR (y1, m1, d1) 'insert year, month and day and function calculate which day in year it is
    FOR month = 1 TO m1
        IF ISLEAPYEAR(y1) = 0 THEN
            SELECT CASE month
                CASE 1: m1 = 31
                CASE 2: m1 = 28
                CASE 3: m1 = 31
                CASE 4: m1 = 30
                CASE 5: m1 = 31
                CASE 6: m1 = 30
                CASE 7: m1 = 31
                CASE 8: m1 = 31
                CASE 9: m1 = 30
                CASE 10: m1 = 31
                CASE 11: m1 = 30
                CASE 12: m1 = 31
            END SELECT
        ELSE
            SELECT CASE month
                CASE 1: m1 = 31
                CASE 2: m1 = 29
                CASE 3: m1 = 31
                CASE 4: m1 = 30
                CASE 5: m1 = 31
                CASE 6: m1 = 30
                CASE 7: m1 = 31
                CASE 8: m1 = 31
                CASE 9: m1 = 30
                CASE 10: m1 = 31
                CASE 11: m1 = 30
                CASE 12: m1 = 31
            END SELECT
        END IF
        GETDAYNR = GETDAYNR + m1
    NEXT
    GETDAYNR = GETDAYNR - (m1 - d1)
END FUNCTION

FUNCTION __GETDAY
    __GETDAY = VAL(MID$(DATE$, 4, 2))
END FUNCTION


FUNCTION GETDATE$ (NrOfTheDayInYear, y) 'return date 'YYYYMMDD in year y

    'spocitat mesic
    FOR month = 1 TO 12
        IF ISLEAPYEAR(y) = 0 THEN
            SELECT CASE month
                CASE 1: m1 = 31
                CASE 2: m1 = 28
                CASE 3: m1 = 31
                CASE 4: m1 = 30
                CASE 5: m1 = 31
                CASE 6: m1 = 30
                CASE 7: m1 = 31
                CASE 8: m1 = 31
                CASE 9: m1 = 30
                CASE 10: m1 = 31
                CASE 11: m1 = 30
                CASE 12: m1 = 31
            END SELECT
        ELSE
            SELECT CASE month
                CASE 1: m1 = 31
                CASE 2: m1 = 29
                CASE 3: m1 = 31
                CASE 4: m1 = 30
                CASE 5: m1 = 31
                CASE 6: m1 = 30
                CASE 7: m1 = 31
                CASE 8: m1 = 31
                CASE 9: m1 = 30
                CASE 10: m1 = 31
                CASE 11: m1 = 30
                CASE 12: m1 = 31
            END SELECT
        END IF
        oldm = om
        om = om + m1
        IF om >= NrOfTheDayInYear AND oldm < NrOfTheDayInYear THEN EXIT FOR
    NEXT
    day = NrOfTheDayInYear - om + m1
    day$ = STR$(day): IF LEN(day$) < 3 THEN day$ = "0" + LTRIM$(day$)
    month$ = STR$(month): IF LEN(month$) < 3 THEN month$ = "0" + LTRIM$(month$)

    GETDATE$ = LTRIM$(STR$(y)) + "-" + LTRIM$(month$) + "-" + LTRIM$(day$)
END FUNCTION




FUNCTION ISLEAPYEAR (year)
    IF year MOD 4 = 0 AND year MOD 100 THEN ISLEAPYEAR = 1
    IF year MOD 100 = 0 AND year MOD 400 = 0 THEN ISLEAPYEAR = 1
END FUNCTION

FUNCTION GETDAYNAME$ (year, month, day) 'otestovano brutalne, fuguje skutecne spravne!
    '  IF year < 1900 THEN GETDAYNAME$ = "Invalid Year ( <1900 ).": EXIT FUNCTION
    'spocitat pocet mesicu od ledna 1900:
    DIM om AS LONG, m1 AS LONG, days AS LONG
    days = day
    FOR yyr = 1 TO year
        IF yyr = year THEN monthend = month ELSE monthend = 12
        FOR mont = 1 TO monthend
            IF ISLEAPYEAR(yyr) = 0 THEN
                SELECT CASE mont
                    CASE 1: m1 = 31
                    CASE 2: m1 = 28
                    CASE 3: m1 = 31
                    CASE 4: m1 = 30
                    CASE 5: m1 = 31
                    CASE 6: m1 = 30
                    CASE 7: m1 = 31
                    CASE 8: m1 = 31
                    CASE 9: m1 = 30
                    CASE 10: m1 = 31
                    CASE 11: m1 = 30
                    CASE 12: m1 = 31
                END SELECT
            ELSE
                SELECT CASE mont
                    CASE 1: m1 = 31
                    CASE 2: m1 = 29
                    CASE 3: m1 = 31
                    CASE 4: m1 = 30
                    CASE 5: m1 = 31
                    CASE 6: m1 = 30
                    CASE 7: m1 = 31
                    CASE 8: m1 = 31
                    CASE 9: m1 = 30
                    CASE 10: m1 = 31
                    CASE 11: m1 = 30
                    CASE 12: m1 = 31
                END SELECT
            END IF
            om = m1
            days = days + m1
        NEXT
    NEXT
    days = days - m1 - 1
    '  PRINT "Dnu:"; Days
    a = (days MOD 7) '0 = pondeli
    RESTORE nms
    FOR r = 0 TO a
        READ GETDAYNAME$
    NEXT
    nms:
    DATA Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday
END FUNCTION


FUNCTION GetDay$ (mm, dd, yyyy) 'use 4 digit year
    'From Zeller's congruence: https://en.wikipedia.org/wiki/Zeller%27s_congruence
    IF mm < 3 THEN mm = mm + 12: yyyy = yyyy - 1
    century = yyyy MOD 100
    zerocentury = yyyy \ 100
    result = (dd + INT(13 * (mm + 1) / 5) + century + INT(century / 4) + INT(zerocentury / 4) + 5 * zerocentury) MOD 7
    SELECT CASE result
        CASE 0: GetDay$ = "Saturday"
        CASE 1: GetDay$ = "Sunday"
        CASE 2: GetDay$ = "Monday"
        CASE 3: GetDay$ = "Tuesday"
        CASE 4: GetDay$ = "Wednesday"
        CASE 5: GetDay$ = "Thursday"
        CASE 6: GetDay$ = "Friday"
    END SELECT
END FUNCTION


