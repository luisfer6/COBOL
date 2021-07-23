       PROGRAM-ID.   CALCLEN.
      *AUTHOR. EDUARDO PALMEYRO
      *INSTALLATION. EDUSAM.
      *DATE-COMPILED.
      *DATE WRITTEN. XX/XX/XX.
      *-------------------------------------------------------------*
      * RUTINA PARA CALCULO DE LONGITUD DE UN CAMPO
      *-------------------------------------------------------------*

       ENVIRONMENT DIVISION.
      *-------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
      *-------------------------------------------------------------*

       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
       77  WS-FIN-CALCLEN                       PIC X     VALUE ' '.
           88 88-FIN-CALCLEN                              VALUE '1'.

       01  KTE-CAMPO-VACIO                      PIC X(50) VALUE
           'EL CAMPO *WCALCLEN-CAMPO* ESTA VACIO'.

      *-------------------------------------------------------------*
       LINKAGE SECTION.
      *-------------------------------------------------------------*
      * COPY DE AREA DE COMUNICACION CON ESTA RUTINA

       COPY WCALCLEN.

      *-------------------------------------------------------------*
       PROCEDURE DIVISION USING WCALCLEN.
      *-------------------------------------------------------------*

       0000-CUERPO-PRINCIPAL SECTION.
      *-----------------------------

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO.

           PERFORM 3000-TERMINO.

       1000-INICIO SECTION.
      *--------------------

           INITIALIZE WCALCLEN-SALIDA.
           MOVE '00'     TO WCALCLEN-RC.

       2000-PROCESO SECTION.
      *---------------------

           INITIALIZE WS-FIN-CALCLEN.

           PERFORM VARYING WCALCLEN-LEN
              FROM LENGTH OF WCALCLEN-CAMPO BY -1
             UNTIL 88-FIN-CALCLEN
                OR WCALCLEN-LEN = +0
                   EVALUATE  WCALCLEN-CAMPO-BYTE(WCALCLEN-LEN)
                       WHEN  ' '
                       WHEN  LOW-VALUE
                             CONTINUE
                       WHEN  OTHER
                             SET 88-FIN-CALCLEN TO TRUE
                             ADD +1 TO WCALCLEN-LEN
                   END-EVALUATE
           END-PERFORM.

           EVALUATE WCALCLEN-LEN
               WHEN +0
                     MOVE KTE-CAMPO-VACIO  TO WCALCLEN-MSG OF
                                              WCALCLEN-SALIDA
                     MOVE 'ESTE MENSAJE ES PARA EL DE ENTRADA'
                                           TO WCALCLEN-MSG OF
                                              WCALCLEN-ENTRADA
               WHEN  OTHER
                     CONTINUE
           END-EVALUATE.


       3000-TERMINO SECTION.
      *---------------------
           DISPLAY 'LONGITUD OBTENIDA EN RUTINA CALCLEN: ' WCALCLEN-LEN.
           DISPLAY 'RC EN RUTINA CALCLEN           : ' WCALCLEN-RC.
           GOBACK.

       END PROGRAM CALCLEN.
