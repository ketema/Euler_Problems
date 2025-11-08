       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISTINCT-POWERS.
      *> Project Euler Problem 29: Distinct Powers
      *> Find distinct values of a^b for 2 <= a,b <= 100

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-A                PIC 999.
           05  WS-B                PIC 999.
           05  WS-MAX              PIC 999 VALUE 100.
           05  WS-IDX              PIC 9(5).
           05  WS-COUNT            PIC 9(5) VALUE 0.
           05  WS-DISTINCT         PIC 9(5) VALUE 0.
           05  WS-LOG-VALUE        COMP-2.
           05  WS-PREV-LOG         COMP-2 VALUE -1.
           05  WS-EPSILON          COMP-2 VALUE 0.0000001.
           05  WS-DIFF             COMP-2.

       01  WS-POWERS-TABLE.
           05  WS-POWER OCCURS 9801 TIMES.
               10  WS-LOG-A-B      COMP-2.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *>   Generate all log(a^b) = b * log(a) values
           PERFORM VARYING WS-A FROM 2 BY 1
                   UNTIL WS-A > WS-MAX
               PERFORM VARYING WS-B FROM 2 BY 1
                       UNTIL WS-B > WS-MAX
                   ADD 1 TO WS-COUNT
                   COMPUTE WS-LOG-VALUE = WS-B *
                           FUNCTION LOG(WS-A)
                   MOVE WS-LOG-VALUE TO
                        WS-LOG-A-B(WS-COUNT)
               END-PERFORM
           END-PERFORM.

      *>   Sort the array
           SORT WS-POWER ON ASCENDING KEY WS-LOG-A-B.

      *>   Count distinct values (those that differ by more than epsilon)
           MOVE 1 TO WS-DISTINCT
           MOVE WS-LOG-A-B(1) TO WS-PREV-LOG

           PERFORM VARYING WS-IDX FROM 2 BY 1
                   UNTIL WS-IDX > WS-COUNT
               COMPUTE WS-DIFF = WS-LOG-A-B(WS-IDX) - WS-PREV-LOG
               IF WS-DIFF > WS-EPSILON
                   ADD 1 TO WS-DISTINCT
                   MOVE WS-LOG-A-B(WS-IDX) TO WS-PREV-LOG
               END-IF
           END-PERFORM.

           DISPLAY "PROBLEM 29: DISTINCT POWERS".
           DISPLAY "ANSWER: " WS-DISTINCT.

           STOP RUN.
