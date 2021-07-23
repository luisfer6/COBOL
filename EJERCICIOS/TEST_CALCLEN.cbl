       PROGRAM-ID. TEST_CALCLEN AS "TEST_CALCLEN".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY WCALCLEN.

       PROCEDURE DIVISION.

           INITIALIZE WCALCLEN.

           MOVE '123456XXXXXXXXXXXXXXXXXXXXXXc*ccccccccccccccccccccccccc
      -    'xxxxxxxxq1 *' TO WCALCLEN-CAMPO.

           DISPLAY WCALCLEN-CAMPO.

      *     PERFORM VARYING TALLY FROM 1 BY 1
      *       UNTIL TALLY > LENGTH OF WCALCLEN-CAMPO
      *          IF WCALCLEN-CAMPO(TALLY:1) = '*'
      *            THEN MOVE '$'           TO WCALCLEN-CAMPO(TALLY:1)
      *          END-IF
      *     END-PERFORM.

      *     DISPLAY WCALCLEN-CAMPO.

      *     STOP RUN.


           PERFORM CALL-CALCLEN.


           DISPLAY 'WCALCLEN-MSG ENTRADA :' WCALCLEN-MSG IN
                                            WCALCLEN-ENTRADA.
           DISPLAY 'WCALCLEN-MSG SALIDA  :' WCALCLEN-MSG IN
                                            WCALCLEN-SALIDA.

           DISPLAY 'WCALCLEN-LEN: ' WCALCLEN-LEN.
           DISPLAY 'WCALCLEN-RC : ' WCALCLEN-RC.
           DISPLAY 'WCALCLEN-MSG: ' WCALCLEN-MSG IN WCALCLEN-SALIDA.

           STOP RUN.

           PERFORM VARYING TALLY FROM 1 BY 1
             UNTIL TALLY > 4093
                   MOVE '*'         TO WCALCLEN-CAMPO(TALLY:1)
           END-PERFORM.

           PERFORM CALL-CALCLEN.

           INITIALIZE WCALCLEN-CAMPO.

           PERFORM CALL-CALCLEN.

           STOP RUN.

       CALL-CALCLEN.

           DISPLAY ' '.
           DISPLAY 'LONGITUD DE WCALCLEN-CAMPO: '
                    LENGTH OF WCALCLEN-CAMPO.

           CALL "CALCLEN" USING WCALCLEN.

           DISPLAY ' '.
           DISPLAY 'VOLVIO DE CALCLEN CON LONGITUD: ' WCALCLEN-LEN.
           DISPLAY 'WCALCLEN-RC                   : ' WCALCLEN-RC.
           DISPLAY 'WCALCLEN-MSG                  : ' WCALCLEN-MSG
                                                   IN WCALCLEN-SALIDA.

       FIN-CALL-CALCLEN.
           EXIT.

       END PROGRAM TEST_CALCLEN.
