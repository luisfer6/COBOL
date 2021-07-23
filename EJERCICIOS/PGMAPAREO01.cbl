      ******************************************************************
      * PROGRAMA DE INDUCCION DE LOGICA DE APAREO DE ARCHIVOS
      * ACTUALIZACION DE ARCHIVO MAESTRO A PARTIR DE UN ARCHIVO DE
      * NOVEDADES
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMAPAREO01.

       ENVIRONMENT DIVISION.
      *--------------------

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
      *--------------------

       FILE-CONTROL.
      *------------

           SELECT MAESTRO            ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\Maestro.
      -    'txt'
                                     ORGANIZATION IS LINE SEQUENTIAL
                                     FILE STATUS  IS FS-MAESTRO.

           SELECT NOVEDAD            ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\Novedad.
      -    'txt'
                                     ORGANIZATION IS LINE SEQUENTIAL
                                     FILE STATUS  IS FS-NOVEDAD.

           SELECT MAEACT            ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\MaeAct.t
      -    'xt'
                                     ORGANIZATION IS LINE SEQUENTIAL
                                     FILE STATUS  IS FS-MAEACT.


       DATA DIVISION.
      *-------------
       FILE SECTION.
      *------------

       FD  MAESTRO
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-MAESTRO-FD                 PIC X(80).

       FD  NOVEDAD
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-NOVEDAD-FD                 PIC X(80).

       FD  MAEACT
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-MAEACT-FD                  PIC X(80).

       WORKING-STORAGE SECTION.
      *-----------------------
       77  WS-PROGRAMA                    PIC X(11) VALUE 'PGMAPAREO01'.
       77  FS-MAESTRO                     PIC X(02) VALUE ' '.
       77  FS-NOVEDAD                     PIC X(02) VALUE ' '.
       77  FS-MAEACT                      PIC X(02) VALUE ' '.
       77  WS-CANT-LEIDOS-MAE             PIC 9(09) VALUE 0.
       77  WS-CANT-LEIDOS-NOV             PIC 9(09) VALUE 0.
       77  WS-CANT-GRABADOS-MAEACT        PIC 9(09) VALUE 0.
       77  WS-CANT-GRABADOS-ALTA          PIC 9(09) VALUE 0.
       77  WS-CANT-GRABADOS-MODIF         PIC 9(09) VALUE 0.
       77  WS-CANT-BAJAS                  PIC 9(09) VALUE 0.
       77  WS-APENOM                      PIC X(50) VALUE ' '.

      *****************************************************************
      *    REGISTRO ARCHIVO MAESTRO
      *****************************************************************
       01  MAE-REG.
           05  MAE-CLAVE.
               07  MAE-CODIGO             PIC X(03).
           05  MAE-DATO                   PIC X(03).
           05  FILLER                     PIC X(74).
      *****************************************************************
      *              REGISTRO ARCHIVO DE NOVEDAD
      *****************************************************************
       01  NOV-REG.
           05  NOV-CLAVE.
               07  NOV-CODIGO             PIC X(03).
           05  NOV-CODNOV                 PIC X.
               88 88-NOV-ES-ALTA          VALUE 'A'.
               88 88-NOV-ES-BAJA          VALUE 'B'.
               88 88-NOV-ES-MODI          VALUE 'M'.
           05  NOV-DATO                   PIC X(03).
           05  FILLER                     PIC X(73).

      *****************************************************************
      *    REGISTRO ARCHIVO MAESTRO ACTUALIZADO
      *****************************************************************
       01  MAEACT-REG.
           05  MAEACT-CLAVE.
               07  MAEACT-CODIGO             PIC X(03).
           05  MAEACT-DATO                   PIC X(03).
           05  FILLER                        PIC X(74).


       PROCEDURE DIVISION.
      *-------------------
       MAIN-PROCEDURE.

           PERFORM 100-INICIO.

           PERFORM 200-PROCESO
             UNTIL MAE-CLAVE = HIGH-VALUES
               AND NOV-CLAVE = HIGH-VALUES.

           PERFORM 300-FINALIZO.

           STOP RUN.

       100-INICIO.

           PERFORM 1000-ABRO-ARCHIVOS.

           PERFORM 1100-LEO-MAESTRO.

           PERFORM 2100-LEO-NOVEDAD.

       100-FIN.
           EXIT.

       200-PROCESO.

           DISPLAY 'EL CODIGO DE NOV ES: ' NOV-CODNOV

           IF  MAE-CLAVE = NOV-CLAVE
               IF  MAE-CLAVE NOT = HIGH-VALUES
                  DISPLAY 'ESTAMOS EN PAREO, VALIDOS CODNOV = M / B'
                  EVALUATE TRUE
                   WHEN 88-NOV-ES-BAJA
                        PERFORM 10000-TRATO-BAJA

                   WHEN 88-NOV-ES-MODI
                        PERFORM 11000-TRATO-MODI

                   WHEN 88-NOV-ES-ALTA
                        DISPLAY 'SE INFORMA ALTA PARA  '
                        DISPLAY 'UNA CLAVE EXISTENTE EN MAE'
                        DISPLAY 'SE DESCARTA EL REGISTRO DE NOV'
                        DISPLAY 'SE GRABA MAE SIN CAMBIOS'
                        PERFORM 13000-GRABO-MAE-SINMODIF

                   WHEN OTHER
                        DISPLAY 'CODIGO DE NOVEDAD INVALIDO PARA '
                        DISPLAY 'UNA CLAVE EXISTENTE EN MAE'
                        DISPLAY 'SE DESCARTA EL REGISTRO DE NOV'
                        DISPLAY 'SE GRABA MAE SIN CAMBIOS'
                        PERFORM 13000-GRABO-MAE-SINMODIF
                END-EVALUATE
                PERFORM 1100-LEO-MAESTRO
                PERFORM 2100-LEO-NOVEDAD
               ELSE
                DISPLAY 'FIN DEL PROCESO - AMBAS CLAVES SON HV'
               END-IF
           ELSE
                IF MAE-CLAVE > NOV-CLAVE
                   DISPLAY 'ESTAMOS EN ALTA, VALIDO CODNOV = A'
                   EVALUATE TRUE
                       WHEN 88-NOV-ES-ALTA
                            PERFORM 12000-TRATO-ALTA

                       WHEN OTHER
                            DISPLAY 'CODIGO DE NOVEDAD INVALIDO PARA '
                            DISPLAY 'UN ALTA '
                            DISPLAY 'SE DESCARTA EL REGISTRO DE NOV'
                   END-EVALUATE
                   PERFORM 2100-LEO-NOVEDAD
                 ELSE
                   DISPLAY 'ESTAMOS ANTE MAESTRO SIN NOVEDAD'
                   PERFORM 13000-GRABO-MAE-SINMODIF
                   PERFORM 1100-LEO-MAESTRO
                END-IF
           END-IF.
       200-FIN.
           EXIT.


       300-FINALIZO.

           PERFORM 5000-CIERRO-ARCHIVOS.

           PERFORM 6000-TOTALES-CONTROL.

       300-FIN.
           EXIT.

       1000-ABRO-ARCHIVOS.

           OPEN INPUT MAESTRO.

           IF FS-MAESTRO = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO MAESTRO'
              DISPLAY 'FS-MAESTRO: ' FS-MAESTRO
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           OPEN INPUT NOVEDAD.

           IF FS-NOVEDAD = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO NOVEDAD'
              DISPLAY 'FS-NOVEDAD: ' FS-NOVEDAD
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           OPEN OUTPUT MAEACT.

           IF FS-MAEACT = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO MAEACT'
              DISPLAY 'FS-MAEACT: ' FS-MAEACT
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.

       1000-FIN.
           EXIT.


       1100-LEO-MAESTRO.

           INITIALIZE MAE-REG.

           READ MAESTRO INTO MAE-REG
                AT END
                   DISPLAY 'ENCONTRO FIN DE ARCHIVO MAESTRO'
                NOT AT END
                   DISPLAY 'NO ES FIN DE ARCHIVO MAESTRO'
           END-READ.

           EVALUATE FS-MAESTRO
               WHEN '00'
                    ADD 1 TO WS-CANT-LEIDOS-MAE

               WHEN '10'
                    MOVE HIGH-VALUES TO MAE-CLAVE

               WHEN OTHER
                    DISPLAY 'ERROR LECTURA MAESTRO'
                    DISPLAY 'SE FINALIZA EL PROGRAMA'
                    STOP RUN

           END-EVALUATE.

       1100-FIN.
           EXIT.

       2100-LEO-NOVEDAD.

           INITIALIZE NOV-REG.

           READ NOVEDAD INTO NOV-REG
            AT END
                   DISPLAY 'ENCONTRO FIN DE ARCHIVO NOVEDAD'
                NOT AT END
                   DISPLAY 'NO ES FIN DE ARCHIVO NOVEDAD'
           END-READ.

           EVALUATE FS-NOVEDAD
               WHEN '00'
                    ADD 1 TO WS-CANT-LEIDOS-NOV

               WHEN '10'
                    MOVE HIGH-VALUES TO NOV-CLAVE

               WHEN OTHER
                    DISPLAY 'ERROR LECTURA NOVEDAD'
                    DISPLAY 'SE FINALIZA EL PROGRAMA'
                    STOP RUN

           END-EVALUATE.

       1100-FIN.

       5000-CIERRO-ARCHIVOS.

           CLOSE MAESTRO.

           IF FS-MAESTRO = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO MAESTRO'
              DISPLAY 'FS-MAESTRO: ' FS-MAESTRO
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           CLOSE NOVEDAD.

           IF FS-NOVEDAD = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO NOVEDAD'
              DISPLAY 'FS-NOVEDAD: ' FS-NOVEDAD
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           CLOSE MAEACT.

           IF FS-MAEACT = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO MAEACT'
              DISPLAY 'FS-MAEACT: ' FS-MAEACT
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.

       5000-FIN.
           EXIT.

       6000-TOTALES-CONTROL.

           DISPLAY '***********************************************'.
           DISPLAY '     TOTALES DE CONTROL PROGRAMA ' WS-PROGRAMA.
           DISPLAY '***********************************************'.
           DISPLAY 'REG. MAESTRO LEIDOS   : ' WS-CANT-LEIDOS-MAE.
           DISPLAY 'REG. NOVEDAD LEIDOS   : ' WS-CANT-LEIDOS-NOV.
           DISPLAY 'REG. MAEACT  GRABADOS : ' WS-CANT-GRABADOS-MAEACT.
           DISPLAY 'CANT. ALTAS           : ' WS-CANT-GRABADOS-ALTA.
           DISPLAY 'CANT. MODIF.          : ' WS-CANT-GRABADOS-MODIF.
           DISPLAY 'CANT. BAJAS           : ' WS-CANT-BAJAS.

       6000-FIN.
           EXIT.

       10000-TRATO-BAJA.

           DISPLAY 'PROCESO BAJA DE CLAVE: ' NOV-CLAVE.
           DISPLAY 'NOV-CODIGO           : ' NOV-CODIGO.
           DISPLAY 'NOV-CODNOV           : ' NOV-CODNOV.
           DISPLAY 'NOV-DATO             : ' NOV-DATO.

           ADD 1                     TO WS-CANT-BAJAS.

       10000-FIN.
           EXIT.

       11000-TRATO-MODI.

           INITIALIZE  MAEACT-REG.

           MOVE MAE-REG              TO MAEACT-REG.
           MOVE NOV-DATO             TO MAEACT-DATO.

           DISPLAY 'PROCESO MODIFICACION DE MAE-CLAVE : ' MAE-CLAVE.
           DISPLAY 'MAE-DATO-ANTERIOR                 : ' MAE-DATO.
           DISPLAY 'MAEACT-DATO NUEVO                 : ' MAEACT-DATO.

           PERFORM 11500-GRABO-MAEACT.

           ADD 1                    TO WS-CANT-GRABADOS-MODIF.

       11000-FIN.
           EXIT.

       11500-GRABO-MAEACT.

           WRITE REG-MAEACT-FD     FROM MAEACT-REG.

           EVALUATE FS-MAEACT
               WHEN '00'
                    ADD 1            TO WS-CANT-GRABADOS-MAEACT

               WHEN OTHER
                    DISPLAY 'ERROR EN 11500-GRABO-MAEACT'
                    DISPLAY 'ARCHIVO MAEACT'
                    DISPLAY 'FS-MAEACT: ' FS-MAEACT
                    DISPLAY 'SE CANCELA EL PROGRAMA'
                    STOP RUN
           END-EVALUATE.

       11500-FIN.
           EXIT.

       12000-TRATO-ALTA.

           INITIALIZE  MAEACT-REG.

           MOVE NOV-CODIGO           TO MAEACT-CODIGO.
           MOVE NOV-DATO             TO MAEACT-DATO.

           DISPLAY 'PROCESO ALTA DE MAEACT-CLAVE      : ' MAEACT-CLAVE.
           DISPLAY 'MAEACT-CODIGO                     : ' MAEACT-CODIGO.
           DISPLAY 'MAEACT-DATO                       : ' MAEACT-DATO.

           PERFORM 11500-GRABO-MAEACT.

           ADD 1                     TO WS-CANT-GRABADOS-ALTA.

       12000-FIN.
           EXIT.

       13000-GRABO-MAE-SINMODIF.

           INITIALIZE  MAEACT-REG.

           MOVE MAE-REG              TO MAEACT-REG.

           DISPLAY 'PROCESO SIN MODIFICACION DE MAE-CLAVE : ' MAE-CLAVE.
           DISPLAY 'MAEACT-CODIGO                     : ' MAEACT-CODIGO.
           DISPLAY 'MAEACT-DATO                       : ' MAEACT-DATO.

           PERFORM 11500-GRABO-MAEACT.

       13000-FIN.
           EXIT.
