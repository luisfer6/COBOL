      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-BURBUJEO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  WS-AUX                           PIC X(03) VALUE ' '.
       77  WS-I                             PIC 9(02) VALUE 0.
       77  WS-HUBO-CAMBIO                   PIC X(02) VALUE ' '.
           88 88-HUBO-CAMBIO-SI                       VALUE 'SI'.
           88 88-HUBO-CAMBIO-NO                       VALUE 'NO'.

       01  TABLA-INICIAL.
           03 FILLER                        PIC X(03) VALUE '001'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '230'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '198'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '340'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE '002'.
           03 FILLER                        PIC X(01) VALUE ' '.
           03 FILLER                        PIC X(03) VALUE X'FFFFFF'.
           03 FILLER                        PIC X(01) VALUE ' '.

       01  TABLA REDEFINES TABLA-INICIAL.
           03 CAMPO-TABLA-V      OCCURS 6 TIMES.
              05 CAMPO-TABLA              PIC X(03).
              05 FILLER                   PIC X(01).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Hello world"

           DISPLAY 'TABLA-INICIAL: ' TABLA-INICIAL.

           SET 88-HUBO-CAMBIO-SI    TO TRUE.
           PERFORM UNTIL 88-HUBO-CAMBIO-NO

               SET 88-HUBO-CAMBIO-NO    TO TRUE
               PERFORM VARYING WS-I FROM 1 BY 1
                 UNTIL WS-I > 5
                 IF CAMPO-TABLA(WS-I) > CAMPO-TABLA(WS-I + 1)
                    MOVE CAMPO-TABLA(WS-I + 1) TO WS-AUX
                    MOVE CAMPO-TABLA(WS-I)     TO CAMPO-TABLA(WS-I + 1)
                    MOVE WS-AUX                TO CAMPO-TABLA(WS-I)
                    SET 88-HUBO-CAMBIO-SI      TO TRUE
                    DISPLAY 'TABLA: ' TABLA
                  END-IF
                END-PERFORM
                DISPLAY ' '
           END-PERFORM.
           DISPLAY 'TABLA-FINAL: ' TABLA.


           STOP RUN.
       END PROGRAM PGM-BURBUJEO.
