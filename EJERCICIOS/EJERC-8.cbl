      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      *8)    Suponga que en todos los casos se aplican las siguientes dos
      * instrucciones.
      *   MOVE A TO B    (copia el contenido de A en B)
      *   MOVE B TO C    (copia el contenido de B en C)
      *   Donde C se ha definido como: 01    C     PIC     x(11).
      *   Muestre el resultado contenido en C
      *CONTENIDO DE A     PICTURE DE B     RESULTADO EN C
      *        10.125       999V99
      *      10000.00     Z.ZZZ.ZZ
      *        900.15    $$,$$Z.99
      *          0.08    $$,$ZZ.ZZ
      *         50.50    $***,99DB
      *        -25.25        ++.99
      *         25.25      $$$$.99
      *       PASCUAS      XXXBXXX
      ******************************************************************
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EJERCICIO-8.
       ENVIRONMENT DIVISION.
      *---------------------
       CONFIGURATION SECTION.
      *----------------------
      *     SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *-------------
       WORKING-STORAGE SECTION.
      *------------------------

       01  WS-CONTENIDO-A.
           03  COPY-A-1      PIC 9(03)V999.
           03  COPY-A-2      PIC Z.ZZZ,ZZ.
           03  COPY-A-3      PIC $$,$$9.99.
           03  COPY-A-4      PIC $$,$$9.99.
           03  COPY-A-5      PIC $***.99DB.
           03  COPY-A-6      PIC ++,99.
           03  COPY-A-7      PIC $$$$,99.
           03  COPY-A-8      PIC XXXBXXX.

       01  WS-CONTENIDO-B.
           03  COPY-A-EN-B-1 PIC 9(03)V999.
           03  COPY-A-EN-B-2 PIC 9(05)V99.
           03  COPY-A-EN-B-3 PIC 9(04)V99.
           03  COPY-A-EN-B-4 PIC 9(06)V99.
           03  COPY-A-EN-B-5 PIC 9(04)V99.
           03  COPY-A-EN-B-6 PIC --9V99.
           03  COPY-A-EN-B-7 PIC $$$$V99.
           03  COPY-A-EN-B-8 PIC XXXBXXX.

       01  WS-RESULT-C.
           03  RESULT-A-B-1     PIC 9(03)V999.
           03  RESULT-A-B-2     PIC 9(05)V99.
           03  RESULT-A-B-3     PIC 9(04)V99.
           03  RESULT-A-B-4     PIC 9(06)V99.
           03  RESULT-A-B-5     PIC 9(04)V99.
           03  RESULT-A-B-6     PIC --9V99.
           03  RESULT-A-B-7     PIC $$$$V99.
           03  RESULT-A-B-8     PIC XXXBXXX.
           03  CASO-C           PIC X(11).

       PROCEDURE DIVISION.
      *-------------------
       MAIN-PROCEDURE.

      *    MOVER COPY A EN B
           MOVE 10.125            TO COPY-A-EN-B-1.
           MOVE 10000.00          TO COPY-A-EN-B-2
           MOVE 900.15            TO COPY-A-EN-B-3
           MOVE 0.08              TO COPY-A-EN-B-4
           MOVE 50.50             TO COPY-A-EN-B-5
           MOVE -25.25            TO COPY-A-EN-B-6
           MOVE 25.25             TO COPY-A-EN-B-7
           MOVE 'PASCUAS'         TO COPY-A-EN-B-8
      *    MOVER COPY B EN RESULT

           MOVE COPY-A-EN-B-1     TO  RESULT-A-B-1
           MOVE COPY-A-EN-B-2     TO  RESULT-A-B-2
           MOVE COPY-A-EN-B-3     TO  RESULT-A-B-3
           MOVE COPY-A-EN-B-4     TO  RESULT-A-B-4
           MOVE COPY-A-EN-B-5     TO  RESULT-A-B-5
           MOVE COPY-A-EN-B-6     TO  RESULT-A-B-6
           MOVE COPY-A-EN-B-7     TO  RESULT-A-B-7
           MOVE COPY-A-EN-B-8     TO  RESULT-A-B-8

      *    IMPRESIONES
            DISPLAY 'RESULT-C-1: ' RESULT-A-B-1.
            DISPLAY 'RESULT-C-2: ' RESULT-A-B-2.
            DISPLAY 'RESULT-C-3: ' RESULT-A-B-3.
            DISPLAY 'RESULT-C-4: ' RESULT-A-B-4.
            DISPLAY 'RESULT-C-5: ' RESULT-A-B-5.
            DISPLAY 'RESULT-C-6: ' RESULT-A-B-6.
            DISPLAY 'RESULT-C-7: ' RESULT-A-B-7.
            DISPLAY 'RESULT-C-8: ' RESULT-A-B-8.
            STOP RUN.
       END PROGRAM EJERCICIO-8.
