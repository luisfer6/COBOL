      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
       PROGRAM-ID.   UTLVNUM.
      *AUTHOR.       EDUARDO A. PALMEYRO
      *DATE-WRITTEN. 01-10-96.

      *---------------------------------------------------------------*
      *         RUTINA DE VALIDACION NUMERICA                         *
      *---------------------------------------------------------------*
      * OBJETIVO:                                                     *
      * ---------                                                     *
      *                                                               *
      * VALIDACION DE CAMPOS NUMERICOS EDITADOS O NO, CON O SIN DECI- *
      * MALES.                                                        *
      *                                                               *
      * ADAPTACION DE RUTINA DUTLVNUM PARA PRUEBAS DESDE BATCH CON    *
      * CALL                                                          *
      *---------------------------------------------------------------*
      * PROGRAMA   :  UTLVNUM COBOL II                                *
      * COPY       :  WUTLVNUM                                        *
      * INVOCAR    :  CALL 'UTLVNUM' USING WUTLVNUM.                 *
      *                                                               *
      *---------------------------------------------------------------*

      *****************************************************************
       ENVIRONMENT DIVISION.
      *****************************************************************
       CONFIGURATION SECTION.
      *---------------------*
       SOURCE-COMPUTER. IBM-HOST.
       OBJECT-COMPUTER. IBM-HOST.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT  SECTION.
      *---------------------*
       FILE-CONTROL.
      *------------*

      *****************************************************************
       DATA DIVISION.
      *****************************************************************

       WORKING-STORAGE SECTION.
      *-----------------------*

       COPY WUTLVNUM.

       LINKAGE SECTION.
      *---------------*

       01  DFHCOMMAREA                      PIC X(090).

      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************

      *---------------------------------------------------------------*
      * ATENCION: DAR A LA LINKAGE LA DIMENSION EXACTA DEL COPY       *
      *           POR QUE SINO DESTRUYE WORKING DEL PGM. PRINCIPAL.   *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      * DEFAULT VALUE FOR DECIMALS IN NUMERIC FIELDS = ZERO           *
      * ELSE DEFAULT VALUE CAN BE CHANGED PERMANENTLY ('DEF-DEC')     *
      * ELSE 'DEC-ALLOWED' CAN BE SER TEMPORARILY BEFORE ENTERING     *
      * THIS ROUTINE. FIELD EDITDEC IN ENV-TABLE COULD BE USED AS     *
      * INPUT (COST & PRICE FIELDS).                                  *
      * DEFAULT VALUE FOR TOTAL DIGITS IN NUMERIC FIELDS = 7          *
      * ELSE DEFAULT VALUE CAN BE CHANGED PERMANENTLY ('DEF-TOT')     *
      * ELSE 'TOT-ALLOWED' CAN BE SER TEMPORARILY BEFORE ENTERING     *
      * THIS ROUTINE.                                                 *
      *---------------------------------------------------------------*

           DISPLAY 'ESTO ES NUEVO:' DFHCOMMAREA.
           MOVE DFHCOMMAREA                  TO WUTLVNUM.

           SET IND1, IND2                    TO 18.
           MOVE ZERO                         TO DECIMALS  TOTALS
                                                PARTIALS  W-CHECK-OUT
                                                W-ENTABS-OUT.
           MOVE +1                           TO MULTIPLIER.
           MOVE SPACES                       TO SW-CHECK-DIGIT.

       CHECK-DIGIT.
      *------------

           IF  DIGIT (IND1)                  = DECDELIM
               PERFORM IS-COMMA
           ELSE
           IF  DIGIT (IND1)                  = EDTDELIM
               PERFORM IS-POINT
           ELSE
           IF  DIGIT (IND1)                  = '+'
               PERFORM IS-SIGN
           ELSE
           IF  DIGIT (IND1)                  = '-'
               PERFORM IS-SIGN
           ELSE
           IF  DIGIT (IND1)                  = ' '
               PERFORM IS-SPACE
           ELSE
           IF  DIGIT (IND1)                 IS NUMERIC
               PERFORM IS-DIGIT
           ELSE
               MOVE 'YES'                   TO SW-ERR-FLD.

       END-CHECK-DIGIT.

           PERFORM CHECK-DIGIT
                   VARYING IND1 FROM 17 BY -1
                     UNTIL IND1 < 1.

           COMPUTE DELTA-DEC                  = DEC-ALLOWED - DECIMALS.

       LOOP-DEC.
      *--------*

           IF  DELTA-DEC                  NOT = ZEROS
           AND DELTA-DEC                     IS NEGATIVE
               MOVE 'YES'                    TO SW-ERR-FLD
           ELSE
               IF  DELTA-DEC              NOT = ZEROS
               AND DELTA-DEC                 IS POSITIVE
                   MULTIPLY  10              BY W-CHECK-OUT
                   SUBTRACT   1            FROM DELTA-DEC
                   GO TO LOOP-DEC
           END-IF.

           MULTIPLY MULTIPLIER               BY W-CHECK-OUT.
           MOVE DEF-MAXIMUM                  TO CTR-MAXIMUM.

           IF  FLD-OK
               MOVE W-CHECK-OUT              TO W-ENTABS-OUT.

           MOVE WUTLVNUM                     TO DFHCOMMAREA.

           GOBACK.

       END-NUM-FIELDS.

       IS-COMMA.
      *--------*

           IF  COMMA-INHIBITED
               MOVE 'YES'                    TO SW-ERR-FLD
           ELSE
               MOVE PARTIALS                 TO DECIMALS
               MOVE 'YES'                    TO SW-COMMA
               MOVE ZERO                     TO PARTIALS
           END-IF.

       IS-POINT.
      *--------*

           IF  POINT-INHIBITED
           OR  PARTIALS                   NOT = 3
               MOVE 'YES'                    TO SW-ERR-FLD
           ELSE
               MOVE ZERO                     TO PARTIALS
           END-IF.

       IS-SIGN.
      *--------*

           IF  SIGN-INHIBITED
               MOVE 'YES'                    TO SW-ERR-FLD
           ELSE
               MOVE 'YES'                    TO SW-SIGN
               IF  DIGIT (IND1)               = '-'
                   MOVE -1                   TO MULTIPLIER
               END-IF
           END-IF.

       IS-SPACE.
      *--------*

           IF  TOTALS                     NOT = ZERO
               MOVE 'YES'                    TO SW-COMMA
                                                SW-POINT
                                                SW-DIGIT.

       IS-DIGIT.
      *--------*

           IF  DIGIT-INHIBITED
               MOVE 'YES'                    TO SW-ERR-FLD
           ELSE
               ADD  1                        TO PARTIALS , TOTALS
               MOVE DIGIT (IND1)             TO W-DIGIT (IND2)
           END-IF.

           SET IND2 DOWN BY 1.
