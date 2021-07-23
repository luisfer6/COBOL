      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER-REC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENTRADA ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\RECDIAR.
      -    'txt'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-ENTRADA.

       SELECT SALIDA ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\RESXDIA.
      -    'txt'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-SALIDA.
       DATA DIVISION.

       FILE SECTION.

       FD  ENTRADA
           RECORDING MODE IS F
           BLOCK 0.
       01  ENTRADA-FD                     PIC X(28).

       FD  SALIDA
           RECORDING MODE IS F
           BLOCK 0.
       01  SALIDA-FD                      PIC X(37).

       WORKING-STORAGE SECTION.

       77  FS-ENTRADA                     PIC X(02).
           88 88-ENTRADA-OK                            VALUE '00'.
           88 88-ENTRADA-EOF                           VALUE '10'.

       77  WS-ENTRADA-OPEN                PIC X(01).
           88 88-ENTRADA-OPEN-SI                       VALUE 'S'.

       77  FS-SALIDA                      PIC X(02).
           88 88-SALIDA-OK                             VALUE '00'.

       77  WS-SALIDA-OPEN                 PIC X(01).
           88 88-SALIDA-OPEN-SI                        VALUE 'S'.

       77  WS-FECHA-MAX                   PIC X(10)    VALUE LOW-VALUE.
       77  WS-FECHA-MIN                   PIC X(10)    VALUE HIGH-VALUE.

       77  WS-REG-LEIDOS                  PIC 9(10)    VALUE 0.
       77  WS-IMPORTE-LEIDO               PIC 9(13)V99 VALUE 0.

       77  WS-REG-GRABADOS                PIC 9(10)    VALUE 1.
       77  WS-IMPORTE-GRABADO             PIC 9(13)V99 VALUE 0.

       01  WS-AUX-CLAVES.
           03  WS-AUX-CAJERO                  PIC 9(03).
           03  WS-AUX-FECHA                   PIC X(10).


       77  WS-CORTE-IMP-FECHA             PIC 9(13)V99.
       77  WS-CASOS-FECHA                 PIC 9(09).


       77  WS-CORTE-IMP-CAJERO            PIC 9(13)V99.
       77  WS-CASOS-CAJERO                PIC 9(09).

       COPY WRECAUDACION.

       COPY WRES-RECAUDACION.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           PERFORM 1000-INICIO

           PERFORM 2000-PROCESO UNTIL 88-ENTRADA-EOF

           PERFORM 3000-FINALIZAR.

       1000-INICIO.

           PERFORM 1100-ABRIR-ARCHIVOS.

           PERFORM 1200-LECTURA-ENTRADA.

           IF 88-ENTRADA-EOF
              DISPLAY 'ARCHIVO VACIO'
              PERFORM 3000-FINALIZAR
           END-IF

           MOVE CLAVE-RECAUDACION IN RECAUDACION-REG TO WS-AUX-CLAVES.

       1100-ABRIR-ARCHIVOS.

           OPEN INPUT ENTRADA

           EVALUATE TRUE
               WHEN 88-ENTRADA-OK
                    SET 88-ENTRADA-OPEN-SI TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN DE ENTRADA'
                    DISPLAY 'FILE STATUS:' FS-ENTRADA
                    PERFORM 3000-FINALIZAR
           END-EVALUATE.

           OPEN OUTPUT SALIDA

           EVALUATE TRUE
               WHEN 88-SALIDA-OK
                    SET 88-SALIDA-OPEN-SI TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN DE SALIDA'
                    DISPLAY 'FILE STATUS:' FS-SALIDA
                    PERFORM 3000-FINALIZAR
           END-EVALUATE.

       1200-LECTURA-ENTRADA.

           INITIALIZE RECAUDACION-REG

           READ ENTRADA INTO RECAUDACION-REG

           EVALUATE TRUE
               WHEN 88-ENTRADA-OK
                   ADD 1 TO WS-REG-LEIDOS
      *             DISPLAY 'IMPORTE HASTA N VECES FECHA:'
      *                                        IMPORTE IN RECAUDACION-REG
                   DISPLAY 'IMPORTE GENERAL CAJERO: ' WS-IMPORTE-LEIDO
                   ADD IMPORTE IN RECAUDACION-REG TO WS-IMPORTE-LEIDO
               WHEN 88-ENTRADA-EOF
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERROR EN READ DE ENTRADA'
                   DISPLAY 'FILE STATUS:' FS-ENTRADA
                   PERFORM 3000-FINALIZAR
           END-EVALUATE.

       1300-FECHA-MAX-MIN.

           IF FECHA-RECAUDACION IN RECAUDACION-REG
                                            IS GREATER THAN WS-FECHA-MAX
              MOVE FECHA-RECAUDACION IN RECAUDACION-REG
                                           TO WS-FECHA-MAX
           END-IF

           IF FECHA-RECAUDACION IN RECAUDACION-REG
                                            IS LESS THAN WS-FECHA-MIN
              MOVE FECHA-RECAUDACION IN RECAUDACION-REG
                                           TO WS-FECHA-MIN
           END-IF.

       2000-PROCESO.

           PERFORM 1300-FECHA-MAX-MIN

           PERFORM 2200-CONTROL-REGISTROS.

       2200-CONTROL-REGISTROS.

           IF CLAVE-RECAUDACION IN RECAUDACION-REG EQUAL WS-AUX-CLAVES
               PERFORM 2500-DETALLE
           ELSE
               IF COD-CAJERO IN RECAUDACION-REG NOT EQUAL WS-AUX-CAJERO
                   PERFORM 2400-CORTE-CAJERO
               ELSE
                   PERFORM 2300-CORTE-FECHA
               END-IF
           END-IF

           PERFORM 1200-LECTURA-ENTRADA.

       2300-CORTE-FECHA.

           MOVE WS-AUX-CAJERO TO COD-CAJERO IN RESUMEN-RECAUDACION-REG
           MOVE WS-AUX-FECHA  TO FECHA-RECAUDACION IN
                                               RESUMEN-RECAUDACION-REG

           MOVE WS-CORTE-IMP-FECHA TO IMPORTE IN RESUMEN-RECAUDACION-REG

           MOVE WS-CASOS-FECHA TO CANTIDAD-CASOS

           ADD WS-CORTE-IMP-FECHA TO WS-CORTE-IMP-CAJERO

           ADD WS-CASOS-FECHA TO WS-CASOS-CAJERO

           MOVE FECHA-RECAUDACION IN RECAUDACION-REG TO WS-AUX-FECHA
           MOVE IMPORTE IN RECAUDACION-REG  TO WS-CORTE-IMP-FECHA
           MOVE 1                           TO WS-CASOS-FECHA


           PERFORM 2900-GRABAR-SALIDA.

       2400-CORTE-CAJERO.

           PERFORM 2300-CORTE-FECHA

           MOVE WS-AUX-CAJERO TO COD-CAJERO IN RESUMEN-RECAUDACION-REG

           MOVE ALL SPACES  TO FECHA-RECAUDACION IN
                                               RESUMEN-RECAUDACION-REG

           MOVE WS-CORTE-IMP-CAJERO TO
                                      IMPORTE IN RESUMEN-RECAUDACION-REG

           MOVE WS-CASOS-CAJERO TO CANTIDAD-CASOS

           MOVE COD-CAJERO IN RECAUDACION-REG TO WS-AUX-CAJERO

           INITIALIZE WS-CASOS-CAJERO WS-CORTE-IMP-CAJERO

           PERFORM 2900-GRABAR-SALIDA.

       2500-DETALLE.

           ADD 1 TO WS-CASOS-FECHA

           ADD IMPORTE IN RECAUDACION-REG TO WS-CORTE-IMP-FECHA.

       2900-GRABAR-SALIDA.

           WRITE SALIDA-FD FROM RESUMEN-RECAUDACION-REG

           EVALUATE TRUE
               WHEN 88-SALIDA-OK
      * DISPLAY PARA VER SI HUBO UNA FALLA EN WS-GRABADOS-RESXADIA-ED
                    DISPLAY 'GRABADOS RESXADIA:' WS-REG-GRABADOS
                    ADD 1           TO WS-REG-GRABADOS
      * DISPLAY PARA VER SI HUBO UNA FALLA EN LA SUMA IMPORTE(ACUM-E)
                    DISPLAY 'IMPORTE GENERAL HASTA LA FECHA:'
                                      IMPORTE IN RESUMEN-RECAUDACION-REG
                    ADD IMPORTE IN RESUMEN-RECAUDACION-REG
                                                   TO WS-IMPORTE-GRABADO
               WHEN OTHER
                    DISPLAY 'ERROR EN WRITE'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE

           INITIALIZE RESUMEN-RECAUDACION-REG.


       3000-FINALIZAR.

           PERFORM 2400-CORTE-CAJERO

           PERFORM 3400-TOTALIZADORES

           PERFORM 3100-CIERRE-ARCHIVOS

           STOP RUN.

       3100-CIERRE-ARCHIVOS.

           DISPLAY '****************CIERRE DE ARCHIVOS*****************'

           PERFORM 3200-CIERRE-ENTRADA

           PERFORM 3300-CIERRE-SALIDA

           DISPLAY '***************************************************'
           .
       3200-CIERRE-ENTRADA.

           IF 88-ENTRADA-OPEN-SI

               CLOSE ENTRADA

               IF 88-ENTRADA-OK
                   DISPLAY 'CIERRE EXITOSO DEL ARCHIVO ENTRADA'
               ELSE
                   DISPLAY 'ERROR EN CIERRE DEL ARCHIVO ENTRADA'
                   DISPLAY 'FILE STATUS:' FS-ENTRADA
               END-IF

           END-IF.


       3300-CIERRE-SALIDA.

           IF 88-SALIDA-OPEN-SI

               CLOSE SALIDA

               IF 88-SALIDA-OK
                   DISPLAY 'CIERRE EXITOSO DEL ARCHIVO SALIDA'
               ELSE
                   DISPLAY 'ERROR EN CIERRE DEL ARCHIVO SALIDA'
                   DISPLAY 'FILE STATUS:' FS-SALIDA
               END-IF

           END-IF.

       3400-TOTALIZADORES.
           DISPLAY '********************TOTALES************************'
           DISPLAY 'TOTAL DE REGISTROS LEIDOS RECDIAR   : '
                                                   WS-REG-LEIDOS
           DISPLAY 'IMPORTE: ' WS-IMPORTE-LEIDO
           DISPLAY 'TOTAL DE REGISTROS GRABADOS RESXADIA: '
                                                   WS-REG-GRABADOS
           DISPLAY 'IMPORTE: ' WS-IMPORTE-GRABADO

           DISPLAY ' '


           DISPLAY 'PERIODO PROCESADO DESDE:' WS-FECHA-MIN
                                    ' HASTA:' WS-FECHA-MAX
           DISPLAY '***************************************************'
           DISPLAY ' '.

       END PROGRAM EJER-REC.
