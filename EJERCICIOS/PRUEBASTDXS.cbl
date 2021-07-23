      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Eje2A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-REGISTRO.
           05 FILLER                  PIC X(16) VALUE ALL SPACES.
           05 TOTAL                   PIC 9(09) VALUE 0.
           05 FILLER                  PIC X(10) VALUE ALL SPACES.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM Eje2A.
