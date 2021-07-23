      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCU1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  NUM1     PIC 9 VALUE 0.
       01  NUM2     PIC 9 VALUE 0.
       01  RESULTADO     PIC 9 VALUE 0.
       01  OPERADOR PIC X VALUE ' '.
       PROCEDURE DIVISION.
       CALCULADORA.
           PERFORM 3 TIMES
           DISPLAY 'Ingrese el primer digito     : '
           ACCEPT NUM1
           DISPLAY 'Ingrese el segundo digito    : '
           ACCEPT NUM2
           DISPLAY 'Ingrese el operador (+ or *) : '
           ACCEPT OPERADOR
           IF OPERADOR = '+' THEN
              ADD NUM1,NUM2 GIVING RESULTADO
           END-IF
           IF OPERADOR = '*' THEN
                MULTIPLY NUM1 BY NUM2 GIVING RESULTADO
           END-IF
           DISPLAY 'RESULTADO ES = ', RESULTADO
           END-PERFORM.
            STOP RUN.
       END PROGRAM CALCU1.
