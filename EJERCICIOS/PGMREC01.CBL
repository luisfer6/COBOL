       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMRECAU.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RECDIAR       ASSIGN        TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\RECDIAR.
      -    'txt'
                                 ORGANIZATION IS LINE SEQUENTIAL
                                 FILE STATUS  IS FS-RECDIAR.

           SELECT RESXDIA        ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\RESXDIA.
      -    'txt'

                                 ORGANIZATION IS LINE SEQUENTIAL
                                 FILE STATUS  IS FS-RESXDIA.

       DATA DIVISION.

       FILE SECTION.
       FD  RECDIAR
           RECORDING MODE IS F.
      *    BLOCK 0.
       01  RECAUDACION-FD             PIC X(28).

       FD  RESXDIA
           RECORDING MODE IS F.
      *    BLOCK 0.
       01  RESUMEN-RECAUDACION-FD     PIC X(37).

       WORKING-STORAGE SECTION.

       77  FS-RECDIAR                    PIC X(02)      VALUE ' '.
           88 88-FS-RECAUDACION-DIARIA-OK               VALUE '00'.
           88 88-FS-RECAUDACION-DIARIA-EOF              VALUE '10'.

       77  FS-RESXDIA                    PIC X(02)      VALUE ' '.
           88 88-FS-RESUMENXDIA-OK                      VALUE '00'.

       77  WS-LEIDOS-RECDIAR             PIC  9(13)V99  VALUE 0.
       77  WS-LEIDOS-RECDIAR-ED          PIC  Z.ZZZ.ZZ9.

       77  WS-GRABADOS-RESXADIA          PIC  9(13)V99  VALUE 0.
       77  WS-GRABADOS-RESXADIA-ED       PIC  Z.ZZZ.ZZ9.

       01  CLAVE-RECAUDACION-ANT.
           05 COD-CAJERO-ANT             PIC 9(03).
           05 FECHA-RECAUDACION-ANT.
              07 FECHA-AAAA-ANT          PIC X(04).
              07 FILLER                  PIC X(01).
              07 FECHA-MM-ANT            PIC X(02).
              07 FILLER                  PIC X(01).
              07 FECHA-DD-ANT            PIC X(02).

       01  WS-ACUMULADOR.
           03 ACUM-FECHA                 PIC 9(13)V99   VALUE 0.
           03 ACUM-CAJERO                PIC 9(13)V99   VALUE 0.
           03 ACUM-GENERAL               PIC 9(13)V99   VALUE 0.
           03 ACUM-ENTRADA               PIC 9(13)V99   VALUE 0.


       01  WCT-CONTADOR.
           03 CONTADOR-FECHA             PIC 9(05)      VALUE 0.
           03 CONTADOR-CAJERO            PIC 9(05)      VALUE 0.
           03 CONTADOR-GENERAL           PIC 9(05)      VALUE 0.

       01  WS-FECHA-MIN-E                PIC X(10)      VALUE
                                                           '9999-99-99'.

       01  WS-FECHA-MAX-E                PIC X(10)      VALUE
                                                           '0001-01-01'.

       COPY WRECAUDACION.

       COPY WRES-RECAUDACION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO
               UNTIL 88-FS-RECAUDACION-DIARIA-EOF.

           PERFORM 3000-FINALIZAR.

           STOP RUN.

       1000-INICIO.

           PERFORM 1100-ABRIR-ARCHIVOS.

           PERFORM 1110-READ-RECAUDACION-DIARIA.

           IF 88-FS-RECAUDACION-DIARIA-OK
              MOVE CLAVE-RECAUDACION IN RECAUDACION-REG
                                        TO CLAVE-RECAUDACION-ANT
           END-IF.

       1100-ABRIR-ARCHIVOS.

           OPEN INPUT RECDIAR

           EVALUATE TRUE
               WHEN 88-FS-RECAUDACION-DIARIA-OK
                    CONTINUE
               WHEN 88-FS-RECAUDACION-DIARIA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN RECAUDACION DIARIA '
                    DISPLAY 'FILE STATUS: ' FS-RECDIAR
                    DISPLAY 'SE CANCELA EL PROCESO '
                    STOP RUN
           END-EVALUATE

           OPEN OUTPUT RESXDIA

           EVALUATE TRUE
               WHEN 88-FS-RESUMENXDIA-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OUTPUT RESUMEN POR DIA '
                    DISPLAY 'FILE STATUS: ' FS-RESXDIA
                    DISPLAY 'SE CANCELA EL PROCESO '
                    STOP RUN
           END-EVALUATE.
      ******************************************************************
      *    Se hacen dos display para ver si hubo fallas  en ADD        *
      ******************************************************************
       1110-READ-RECAUDACION-DIARIA.

           INITIALIZE  RECAUDACION-REG

           READ RECDIAR INTO RECAUDACION-REG

           EVALUATE TRUE
               WHEN 88-FS-RECAUDACION-DIARIA-OK
                    DISPLAY 'IMPORTE LEIDO: ' IMPORTE IN RECAUDACION-REG
                    ADD 1    TO WS-LEIDOS-RECDIAR
                    DISPLAY 'IMPORTE GENERAL HASTA LA FECHA: '
                                     IMPORTE IN RESUMEN-RECAUDACION-REG
                    ADD IMPORTE IN RECAUDACION-REG
                                   TO ACUM-ENTRADA

                    IF FECHA-RECAUDACION IN RECAUDACION-REG
                                                      < WS-FECHA-MIN-E

                       MOVE FECHA-RECAUDACION IN RECAUDACION-REG
                                                 TO WS-FECHA-MIN-E
                    END-IF
                    IF FECHA-RECAUDACION IN RECAUDACION-REG
                                                      > WS-FECHA-MAX-E

                       MOVE FECHA-RECAUDACION IN RECAUDACION-REG
                                                 TO WS-FECHA-MAX-E
                    END-IF
               WHEN 88-FS-RECAUDACION-DIARIA-EOF
                    CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR EN READ RECDIAR FS: ' FS-RECDIAR
                    DISPLAY 'SE CANCELA EL PROCESO     '
                    STOP RUN
           END-EVALUATE.
      ******************************************************************
      *    Se hace un display Grabados para verlo al ejecutar CBL      *
      ******************************************************************

       1200-WRITE-RESUMENXDIA.

           WRITE RESUMEN-RECAUDACION-FD FROM  RESUMEN-RECAUDACION-REG

           EVALUATE TRUE
               WHEN 88-FS-RESUMENXDIA-OK
      * DISPLAY PARA VER SI HUBO UNA FALLA EN WS-GRABADOS-RESXADIA
                    DISPLAY 'GRABADOS RESXDIA: ' WS-GRABADOS-RESXADIA
                    ADD 1 TO WS-GRABADOS-RESXADIA
               WHEN OTHER
                    DISPLAY 'ERROR WRITE RESXDIA FS: ' FS-RESXDIA
                    DISPLAY 'SE CANCELA EL PROCESO   '
                    STOP RUN
           END-EVALUATE.

       2000-PROCESO.

           IF CLAVE-RECAUDACION EQUAL CLAVE-RECAUDACION-ANT
              PERFORM 2050-PROCESO-DETALLE
           ELSE
               IF COD-CAJERO IN RECAUDACION-REG NOT EQUAL COD-CAJERO-ANT
                  PERFORM 2100-CORTE-CAJERO
                  PERFORM 2050-PROCESO-DETALLE
               ELSE
                  PERFORM 2200-CORTE-FECHA
                  PERFORM 2050-PROCESO-DETALLE

               END-IF

           END-IF.

           PERFORM 1110-READ-RECAUDACION-DIARIA.

       2050-PROCESO-DETALLE.
           ADD    1    TO CONTADOR-FECHA
           ADD IMPORTE IN RECAUDACION-REG
                          TO ACUM-FECHA.

       2100-CORTE-CAJERO.

           PERFORM 2200-CORTE-FECHA.

           MOVE COD-CAJERO-ANT TO COD-CAJERO IN RESUMEN-RECAUDACION-REG

           MOVE SPACES  TO FECHA-RECAUDACION
                        IN RESUMEN-RECAUDACION-REG

           MOVE ACUM-CAJERO TO IMPORTE IN RESUMEN-RECAUDACION-REG

           MOVE CONTADOR-CAJERO TO CANTIDAD-CASOS
                                IN RESUMEN-RECAUDACION-REG

           PERFORM 1200-WRITE-RESUMENXDIA.

           ADD CONTADOR-CAJERO TO CONTADOR-GENERAL.
           ADD ACUM-CAJERO     TO ACUM-GENERAL.

           INITIALIZE CONTADOR-CAJERO.
           INITIALIZE ACUM-CAJERO.

           MOVE COD-CAJERO IN RECAUDACION-REG
                              TO COD-CAJERO-ANT.

       2200-CORTE-FECHA.

           MOVE COD-CAJERO-ANT TO COD-CAJERO IN RESUMEN-RECAUDACION-REG

           MOVE FECHA-RECAUDACION-ANT TO FECHA-RECAUDACION
                                      IN RESUMEN-RECAUDACION-REG

           MOVE ACUM-FECHA TO IMPORTE IN RESUMEN-RECAUDACION-REG

           MOVE CONTADOR-FECHA TO CANTIDAD-CASOS
                               IN RESUMEN-RECAUDACION-REG

           PERFORM 1200-WRITE-RESUMENXDIA.

           ADD CONTADOR-FECHA TO CONTADOR-CAJERO.
           ADD ACUM-FECHA     TO ACUM-CAJERO.


           INITIALIZE CONTADOR-FECHA.
           INITIALIZE ACUM-FECHA.

           MOVE FECHA-RECAUDACION IN RECAUDACION-REG
                                     TO FECHA-RECAUDACION-ANT.

       3000-FINALIZAR.

           PERFORM 2100-CORTE-CAJERO.

           PERFORM 3100-TOTALES-CONTROL.
           PERFORM 3200-CIERRO-ARCHIVOS.

       3100-TOTALES-CONTROL.

           MOVE WS-LEIDOS-RECDIAR        TO WS-LEIDOS-RECDIAR-ED.
           MOVE WS-GRABADOS-RESXADIA     TO WS-GRABADOS-RESXADIA-ED.


           DISPLAY ' '.
           DISPLAY '**************************************************'.
           DISPLAY '    TOTALES DE CONTROL PGM: PGM RECAUDACION 01    '.
           DISPLAY '**************************************************'.
           DISPLAY ' '.
           DISPLAY '* Total de Registros leidos RECDIAR    : '
                                                WS-LEIDOS-RECDIAR-ED.
           DISPLAY ' '.
           DISPLAY 'Importe : '
                                                ACUM-ENTRADA.
           DISPLAY ' '.
           DISPLAY '* Total de Registros grabados RESXADIA : '
                                                WS-GRABADOS-RESXADIA-ED.
           DISPLAY ' '.
           DISPLAY 'Importe : '
                                                ACUM-GENERAL.
           DISPLAY '*                                      *'.
           DISPLAY '**************************************************'.
           DISPLAY ' '.

           DISPLAY 'Periodo procesado Desde : ' WS-FECHA-MIN-E
                                    ' Hasta : ' WS-FECHA-MAX-E.
           DISPLAY '**************************************************'.

       3200-CIERRO-ARCHIVOS.

           CLOSE RECDIAR.

           EVALUATE TRUE
               WHEN 88-FS-RECAUDACION-DIARIA-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE RECDIAR FS: '
                                                   FS-RECDIAR
                    DISPLAY 'SE CANCELA EL PROCESO '
                    STOP RUN
           END-EVALUATE.

           CLOSE RESXDIA.

           EVALUATE TRUE
               WHEN 88-FS-RESUMENXDIA-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE RESXDIA FS: '
                                                   FS-RESXDIA
                    DISPLAY 'SE CANCELA EL PROCESO '
                    STOP RUN
           END-EVALUATE.

       END PROGRAM PGMRECAU.
