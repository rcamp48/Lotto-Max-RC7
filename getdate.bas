
'function GETDAYNAME parameters: YEAR, MONTH, DAY
'listing of Tuesdays and Thursdays for 40 years
'FUNCTION GETDAYNR is upgraded and must be copyed to your program for correct





getdate:
d1=10
Mnth=1
VirtualYear=y
'this is repaired     ------------------------------------------------------------------------------------------
FOR l = 1 TO 366 * 40
    d1 = d1 + 1

    today$ = ""
    DO UNTIL today$ = "Friday" OR today$ = "Tuesday"
        today$ = GETDAYNAME(VirtualYear, Mnth, d)
        d1 = d1 + 1

    LOOP
    d1 = d1 - 1

    day = GETDAYNR(VirtualYear, Mnth, d)

    IF ISLEAPYEAR(VirtualYear) AND day > 366 THEN VirtualYear = VirtualYear + 1: day = day - 366: d1 = d1 - 366
    IF ISLEAPYEAR(VirtualYear) = 0 AND day > 365 THEN VirtualYear = VirtualYear + 1: day = day - 365: d1 = d1 - 365

    realdate$ = GETDATE$(day, VirtualYear)

    PRINT "Next date is "; realdate$; " "; GETDAYNAME(VirtualYear, Mnth, d); ", Week:"; 1 + day \ 7
    SLEEP

    IF today$ = "Friday" AND count = -1 THEN LASTDAY = -3
    IF today$ = "Tuesday" AND count = -1 THEN LASTDAY = -4
NEXT
'this is repaired     ------------------------------------------------------------------------------------------
RETURN


FUNCTION GETDAYNR (y, mn, d) 'insert year, month and day and function calculate which day in year it is
    FOR month = 1 TO mn
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
        GETDAYNR = GETDAYNR + m
    NEXT
    GETDAYNR = GETDAYNR - (m - d)
END FUNCTION

FUNCTION __GETDAY
    __GETDAY = VAL(MID$(DATE$, 4, 2))
END FUNCTION


FUNCTION GETDATE$ (NrOfTheDayInYear, y) 'return date 'YYYYMMDD in year y
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
        oldm1 = om
        om1 = om + m
        IF om >= NrOfTheDayInYear AND oldm < NrOfTheDayInYear THEN EXIT FOR
    NEXT
    day = NrOfTheDayInYear - om + m
    day$ = STR$(day): IF LEN(day$) < 3 THEN day$ = "0" + LTRIM$(day$)
    month$ = STR$(month): IF LEN(month$) < 3 THEN month$ = "0" + LTRIM$(month$)

    GETDATE$ = LTRIM$(STR$(y)) + "-" + LTRIM$(month$) + "-" + LTRIM$(day$)
END FUNCTION




FUNCTION ISLEAPYEAR (year)
    IF year MOD 4 = 0 AND year MOD 100 THEN ISLEAPYEAR = 1
    IF year MOD 100 = 0 AND year MOD 400 = 0 THEN ISLEAPYEAR = 1
END FUNCTION

FUNCTION GETDAYNAME$ (year, mmonth, day) 'otestovano brutalne, fuguje skutecne spravne!
    DIM om AS LONG, m AS LONG, days AS LONG
    days = day
    FOR yyr = 1 TO year
        IF yyr = year THEN monthend = mmonth ELSE monthend = 12
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
            om1 = m1
            days = days + m1
        NEXT
    NEXT
    days = days - m - 1
    a = (days MOD 7)
    RESTORE nms
    FOR r = 0 TO a
        READ GETDAYNAME$
    NEXT
    nms:
    DATA Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday
END FUNCTION


FUNCTION GetDay$ (mm, dd, yyyy) 'use 4 digit year
    'From Zeller's congruence: https://en.wikipedia.org/wiki/Zeller%27s_congruence
    IF mm < 3 THEN mm1 = mm + 12: yyyy = yyyy - 1
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


