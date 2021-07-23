       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.   PGMINSPECT.
      *AUTHOR.       EDUARDO PALMEYRO.

      *-------------------------------------------------------------*
      *    OBJETIVO:                                                *
      *             DEMOSTRACION                                    *
      *             INSPECT, STRING, UNSTRING                       *
      *-------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  WS-PROGRAMA                     PIC X(10) VALUE 'PGMINSPECT'.

       77  WS-RESULTADO                    PIC 9(9) COMP VALUE 0.
       77  WS-RESULTADO-2                  PIC 9(9) COMP VALUE 0.
       77  WSC-ASTERISCO                   PIC X    VALUE '*'.
       77  WSC-EQUIS                       PIC X    VALUE 'X'.
       77  WSC-A                           PIC X    VALUE 'A'.
       77  WSC-PUNTO                       PIC X    VALUE '.'.

       77  WSC-APOSTROFE                   PIC X    VALUE QUOTE.
       77  WSC-BARRA                       PIC X    VALUE '/'.

       01  WS-CADENA                       PIC  X(80) VALUE SPACES.



       PROCEDURE DIVISION.

       0000-TRONCO-PRINCIPAL.

           MOVE ALL '*'                TO WS-CADENA(20:1)
                                          WS-CADENA(30:1)
                                          WS-CADENA(40:5).

           DISPLAY 'WS-CADENA           : ' WS-CADENA.

           INSPECT WS-CADENA TALLYING WS-RESULTADO
               FOR ALL WSC-ASTERISCO.

           DISPLAY 'ENCONTRO LA CANTIDAD DE ' WS-RESULTADO
                   ' '
                   'CARACTERES ' WSC-ASTERISCO
           END-DISPLAY.

      *-----------------------------------------------------------------
           DISPLAY ' '.
           DISPLAY 'WS-CADENA ANTES     : ' WS-CADENA.

           INSPECT WS-CADENA
             REPLACING ALL WSC-ASTERISCO
                        BY WSC-EQUIS.

           DISPLAY 'WS-CADENA DESPUES   : ' WS-CADENA.

           INITIALIZE WS-RESULTADO.
           INSPECT WS-CADENA TALLYING WS-RESULTADO
               FOR ALL WSC-EQUIS.


           DISPLAY 'ENCONTRO LA CANTIDAD DE ' WS-RESULTADO
                   ' '
                   'CARACTERES ' WSC-EQUIS
           END-DISPLAY.

           DISPLAY '----------------------------------------------'.

           MOVE WSC-PUNTO                TO WS-CADENA(49:1).
           MOVE WSC-BARRA                TO WS-CADENA(56:1).

           MOVE ALL 'A'                  TO WS-CADENA(50:6)
                                            WS-CADENA(01:01).

           DISPLAY ' '.
           DISPLAY 'WS-CADENA ANTES     : ' WS-CADENA.

           INITIALIZE     WS-RESULTADO
                          WS-RESULTADO-2.

           INSPECT WS-CADENA TALLYING
                 WS-RESULTADO   FOR ALL WSC-A BEFORE  WSC-PUNTO.
      *          WS-RESULTADO-2 FOR ALL WSC-A BEFORE  '/'.

           INSPECT WS-CADENA TALLYING
      *          WS-RESULTADO   FOR ALL WSC-A BEFORE  WSC-PUNTO
                 WS-RESULTADO-2 FOR ALL WSC-A BEFORE  '/'.

           DISPLAY 'WS-CADENA DESPUES   : ' WS-CADENA.
           DISPLAY 'WS-RESULTADO        : ' WS-RESULTADO.
           DISPLAY 'WS-RESULTADO-2      : ' WS-RESULTADO-2.

           DISPLAY '----------------------------------------------'.
           DISPLAY 'INICIO INSPECT'.

           INITIALIZE     WS-RESULTADO
                          WS-RESULTADO-2.
           MOVE X'C1'          TO WS-CADENA(3:1).
           MOVE X'C2'          TO WS-CADENA(5:1).
           DISPLAY 'WS-CADENA ANTES     : ' WS-CADENA.
      *-----ARRANCANDO PRUEBAS CON INSPECT*****
           INSPECT WS-CADENA
               TALLYING WS-RESULTADO      FOR ALL WSC-A
                        WS-RESULTADO-2    FOR ALL WSC-PUNTO
               REPLACING ALL X'C1' BY X'41'
                             X'C2' BY X'42'

      *     INSPECT TALLY FROM 1 BY 1
      *         UNTIL TALLY > LENGTH OF WS-CADENA
      *                  WHEN WSC-A
      *                       ADD 1 TO WS-RESULTADO
      *                  WHEN WSC-PUNTO
      *                       ADD 1 TO WS-RESULTADO-2
      *                  WHEN X'C1'
      *                       MOVE X'41'TO WS-CADENA(TALLY:1)
      *                  WHEN X'C2'
      *                       MOVE X'42'TO WS-CADENA(TALLY:1)
      *                  WHEN OTHER
      *                       CONTINUE
      *              END-EVALUATE
      *     END-PERFORM.

           DISPLAY 'WS-CADENA DESPUES   : ' WS-CADENA.
           DISPLAY 'WS-RESULTADO (A)    : ' WS-RESULTADO.
           DISPLAY 'WS-RESULTADO-2 (.)  : ' WS-RESULTADO-2.


           STOP RUN.
