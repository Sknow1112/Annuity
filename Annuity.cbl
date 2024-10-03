       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANNUITY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TOTAL-LOAN       PIC Z(9).99 VALUE ZERO.
       01 TOTAL-LOAN-CALC  PIC 9(9)V99 VALUE ZERO.
       01 INTEREST-RATE  PIC ZZZ.999 VALUE ZERO. 
       01 INTEREST-RATE-CALC PIC 999V999 VALUE ZERO.
       01 INTEREST-RATEP PIC 999V99  VALUE ZERO.
       01 MONTHS           PIC 999     VALUE ZERO.
       01 YEARS            PIC ZZZ     VALUE ZERO.
       01 YEARS-CALC       PIC 999     VALUE ZERO.
       01 MONTHLY-PAYMENT  PIC Z(8)9.99     VALUE ZERO.
       01 MONTHLY-PAYMENT-CALC   PIC 9(9)V99 .
       01 COUNTER       PIC 999     VALUE ZERO.
       01 INPUT-FLAG       PIC X       VALUE 'N'.
       PROCEDURE DIVISION.
      *    MOVE 10000 TO TOTAL-LOAN
      *    MOVE 5.0 TO INTEREST-RATE
      *    MOVE 5 TO YEARS
           DISPLAY 'Welcome to Sergios annuity'
           DISPLAY 'Enter the total loan amount: '
           PERFORM GET-LOAN-AMOUNT
           DISPLAY 'Enter the interest rate (%): '
           PERFORM GET-INTEREST-RATE
           DISPLAY 'Enter the number of years: '
           PERFORM GET-LOAN-YEARS
           PERFORM REPORT-ANNUITY

           GOBACK.

       GET-LOAN-AMOUNT.
           PERFORM UNTIL INPUT-FLAG = 'Y'
              ACCEPT TOTAL-LOAN
              IF TOTAL-LOAN > 0
              COMPUTE TOTAL-LOAN-CALC = FUNCTION NUMVAL(TOTAL-LOAN)
               IF TOTAL-LOAN-CALC IS NUMERIC
                 MOVE 'Y' TO INPUT-FLAG
                ELSE
                     DISPLAY 'Please enter a valid number'
               END-IF
              ELSE
                 DISPLAY 'Invalid input. Please enter a valid number'
              END-IF
           END-PERFORM.

           MOVE 'N' TO INPUT-FLAG.
           

       GET-INTEREST-RATE.
           PERFORM UNTIL INPUT-FLAG = 'Y'
               ACCEPT INTEREST-RATE
               IF FUNCTION TEST-NUMVAL(INTEREST-RATE) = 0
                   COMPUTE INTEREST-RATE-CALC =
                    FUNCTION NUMVAL(INTEREST-RATE)
                   IF INTEREST-RATE-CALC > 0 AND 
                   INTEREST-RATE-CALC <= 100
                       MOVE 'Y' TO INPUT-FLAG
                   ELSE
                       DISPLAY 'Interest rate must be a valid %.'
                       DISPLAY 'Please enter a valid interest rate: '
                   END-IF
               ELSE
                   DISPLAY 'Invalid input. Please enter a valid number'
               END-IF
           END-PERFORM.
           MOVE 'N' TO INPUT-FLAG.

       GET-LOAN-YEARS.
           PERFORM UNTIL INPUT-FLAG = 'Y'
              ACCEPT YEARS
              IF YEARS > 0 
              COMPUTE YEARS-CALC = 
              FUNCTION NUMVAL(YEARS)
                 IF YEARS-CALC  IS NUMERIC
                 MOVE 'Y' TO INPUT-FLAG
                ELSE
                     DISPLAY 'Please enter a valid number'
               END-IF
              ELSE
                 DISPLAY 'Invalid input. Please enter a valid number'
              END-IF
           END-PERFORM.
           
           MOVE 'N' TO INPUT-FLAG.

       REPORT-ANNUITY.
           DISPLAY 'Version 5.0'
           DISPLAY 'TOTAL-LOAN: ' TOTAL-LOAN ' USD - INTEREST-RATE: ' 
           INTEREST-RATE '% YEARS: ' YEARS 
           DISPLAY ' Y   M        AMOUNT '
           DISPLAY '--- --- -------------'
           MOVE YEARS-CALC TO COUNTER
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL 
           COUNTER > YEARS-CALC
              COMPUTE MONTHS = COUNTER * 12
              COMPUTE MONTHLY-PAYMENT-CALC ROUNDED =
              TOTAL-LOAN-CALC * FUNCTION ANNUITY((INTEREST-RATE-CALC
               / 100 / 12), MONTHS)
              MOVE MONTHLY-PAYMENT-CALC TO MONTHLY-PAYMENT
              DISPLAY COUNTER " " MONTHS
              " $" MONTHLY-PAYMENT " USD"
           END-PERFORM.
