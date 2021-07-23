      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMPERFORM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  WS-CONTADOR                      PIC 99  VALUE 0.

       77  WS-I                             PIC S99 VALUE +0.
       77  WS-J                             PIC S99 VALUE +0.
       77  WS-MARCA                         PIC X   VALUE ' '.
           88 88-FIN-MARCA                          VALUE 'S'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              SET 88-FIN-MARCA TO TRUE.

           PERFORM WITH TEST AFTER
             UNTIL 88-FIN-MARCA
                   DISPLAY 'ENTRO A LA DEMO 88-FIN-MARCA'
           END-PERFORM.

           DISPLAY 'AQUI YA ESTAMOS'

           DISPLAY '** EJECUTO 100-INICIO SIN THRU'.
           PERFORM 100-INICIO.
           DISPLAY 'VOLVIO DE 100-INICIO SIN THRU'.

           DISPLAY ' '.
           DISPLAY '** EJECUTO 100-INICIO CON THRU'.
           PERFORM 100-INICIO THRU 100-FIN.
           DISPLAY 'VOLVIO DE 100-INICIO CON THRU'.

           DISPLAY ' '.
           DISPLAY '** EJECUTO 200-PROCESO UNTIL WS-J > LITERAL'
           PERFORM 200-PROCESO WITH TEST BEFORE
           VARYING WS-J FROM 1 BY 1
             UNTIL WS-J > 2.
           DISPLAY 'VOLVIO DE 200-PROCESO'.

           DISPLAY ' '.
           DISPLAY '----------'
           DISPLAY 'SALTO DEL VARYING WS-J'
           DISPLAY 'WS-CONTADOR: ' WS-CONTADOR.
           DISPLAY 'WS-J       : ' WS-J.

           DISPLAY ' '.
           DISPLAY '** EJECUTO 300-FINALIZO'.
           PERFORM 300-FINALIZO.
           DISPLAY 'VOLVIO DE 300-FINALIZO'.

           STOP RUN.

       100-INICIO.

           DISPLAY '100-INICIO'.
           DISPLAY '100-DISPLAY-1'.

       100-INICIO-BIS.

           DISPLAY 'WS-I : ' WS-I
           DISPLAY '100-INICIO-BIS'.
           DISPLAY '100-DISPLAY-2'.


       100-FIN.
           EXIT.

       200-PROCESO.

           DISPLAY '200-PROCESO'.
           ADD 1 TO WS-CONTADOR.

           DISPLAY 'WS-CONTADOR: ' WS-CONTADOR.

           DISPLAY '1ER BUCLE DENTRO DE 200 PROCESO'

      *    PERFORM VARYING WS-I FROM 10 BY -3
           PERFORM VARYING WS-I FROM 1  BY 1
             UNTIL WS-I > 3
      *      UNTIL WS-I NOT > 0
                   DISPLAY 'WS-I : ' WS-I
                   PERFORM 100-INICIO-BIS
           END-PERFORM.

           DISPLAY '2DO BUCLE DENTRO DE 200 PROCESO'

           PERFORM 100-INICIO-BIS
           VARYING WS-I FROM 1  BY 1
             UNTIL WS-I > 3.

           DISPLAY '3ER BUCLE DENTRO DE 200 PROCESO'
           PERFORM WS-I TIMES
                   DISPLAY 'WS-I : ' WS-I
           END-PERFORM.


       200-FIN.
           EXIT.

       300-FINALIZO.


           PERFORM UNTIL WS-CONTADOR = 0

              DISPLAY 'ESTO ESTA DENTRO DEL PERFORM'
              SUBTRACT 1 FROM WS-CONTADOR
              DISPLAY 'WS-CONTADOR' WS-CONTADOR

              PERFORM  2 TIMES
                  DISPLAY  'N TIMES'
              END-PERFORM

           END-PERFORM.

           DISPLAY '300-FINALIZO'.

       300-FIN.
           EXIT.
