      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJE3.
      *--------------------------------------
        ENVIRONMENT DIVISION.
      *--------------------------------------

       CONFIGURATION SECTION.
      *--------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *--------------------------------------
       77   BAL-PROM  PIC 9(06)V99.
       01  LINEA-1.
           05 FILER       PIC X(49) VALUE ' '.
           05 FILLER      PIC X(132) VALUE 'RESUMEN ESTADISTICA'.
       01  LINEA-2.
           05 FILLER      PIC X(14) VALUE 'PROMEDIO BAL '.
           05 BAL-PROM-ED PIC $ZZZ.Z99,99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 785312,77    TO BAL-PROM.
            DISPLAY "RESUMEN ESTADISTICA"
            DISPLAY " PROMEDIO BAL : " BAL-PROM.

            MOVE BAL-PROM     TO BAL-PROM-ED.

            DISPLAY 'Linea-1 :' LINEA-1.
            DISPLAY 'Linea-2 :' LINEA-2.

            DISPLAY ' --------------------------- '.
            DISPLAY LINEA-1.
            DISPLAY LINEA-2.
            STOP RUN.
       END PROGRAM EJE3.
