      ************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-5.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHEQUE  ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\CHEQUE.T
      -    'XT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-CHEQUE.

           SELECT MATI  ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\MATI.TXT
      -    ''
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-MATI.


           SELECT MATI2  ASSIGN TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\MATI2.TX
      -    'T'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-MATI2.

       DATA DIVISION.
       FILE SECTION.

       FD  CHEQUE.
           01  REG-CHEQUE-FD        PIC X(80).

       FD  MATI.
           01  REG-LISTA-FD         PIC X(132).

       FD  MATI2.
           01  REG-LISTA-FD-2       PIC X(132).


       WORKING-STORAGE SECTION.

       77  FS-CHEQUE          PIC X(02) VALUE ' '.
           88 88-CHEQUE-OK              VALUE '00'.
           88 88-CHEQUE-FIN             VALUE '10'.

       77  FS-MATI            PIC X(02) VALUE ' '.
           88 88-MATI-OK                VALUE '00'.

       77  FS-MATI2           PIC X(02) VALUE ' '.
           88 88-MATI2-OK               VALUE '00'.

       77  WS-CHEQUE-OPEN     PIC X     VALUE 'N'.
           88 88-CHEQUE-OPEN-SI         VALUE 'S'.
           88 88-CHEQUE-OPEN-NO         VALUE 'N'.

       77  WS-MATI-OPEN       PIC X     VALUE 'N'.
           88 88-MATI-OPEN-SI           VALUE 'S'.
           88 88-MATI-OPEN-NO           VALUE 'N'.

       77  WS-MATI2-OPEN      PIC X     VALUE 'N'.
           88 88-MATI2-OPEN-SI          VALUE 'S'.
           88 88-MATI2-OPEN-NO          VALUE 'N'.

       77  WS-AUX-LISTA       PIC X(132) VALUE ' '.

      *    ESTRUCTURA ARCHIVO CHEQUE LREC ES = 80
       01  REG-CHEQUE.
           05 CHE-NUMERO               PIC 9(05).
           05 CHE-DEBITO               PIC 9(6)V99.
           05 CHE-DESCUENTO            PIC 9(4)V99.
           05 CHE-EFECTIVO             PIC 9(6)V99.
           05 CHE-DATOS-VENDEDOR.
              07 CHE-V-NOMBRE          PIC X(15).
              07 CHE-V-NUMERO          PIC X(4).
           05 CHE-FECHA.
              07 CHE-MES               PIC 99.
              07 CHE-DIA               PIC 99.
              07 CHE-ANIO              PIC 9999.
           05 FILLER                   PIC X(26).

       01  WS-TITULO-1.
           05 FILLER          PIC X(13) VALUE 'NOMBRE DEL   '.
           05 FILLER          PIC X(13) VALUE 'NUMERO DEL   '.
           05 FILLER          PIC X(14) VALUE 'NUMERO DEL    '.
           05 FILLER          PIC X(06) VALUE 'FECHA '.
           05 FILLER          PIC X(10) VALUE ' '.
           05 FILLER          PIC X(07) VALUE 'DEBITO '.
           05 FILLER          PIC X(09) VALUE ' '.
           05 FILLER          PIC X(13) VALUE 'DESCUENTO    '.
           05 FILLER          PIC X(09) VALUE 'EFECTIVO '.
           05 FILLER          PIC X(50) VALUE ' '.

       01  WS-TITULO-2.
           05 FILLER          PIC X(13) VALUE 'VENDEDOR     '.
           05 FILLER          PIC X(13) VALUE 'VENDEDOR     '.
           05 FILLER          PIC X(07) VALUE 'CHEQUE '.
           05 FILLER          PIC X(102) VALUE ' '.

       01  WS-TITULO-3.
           05 FILLER          PIC X(132) VALUE ' '.

       01  WS-DETALLE.
           05 WS-DET-NOMBRE   PIC X(12).
           05 FILLER          PIC X     VALUE ' '.
           05 WS-DET-NUM-VEN  PIC X(05).
           05 FILLER          PIC X(08) VALUE ' '.
           05 WS-DET-NUM-CHE  PIC 9(06).
           05 FILLER          PIC X(06) VALUE ' '.
           05 WS-DET-FECHA.
              07 WS-DET-DIA   PIC 99.
              07 FILLER       PIC X     VALUE '/'.
              07 WS-DET-MES   PIC 99.
              07 FILLER       PIC X     VALUE '/'.
              07 WS-DET-AÑO   PIC 9999.
           05 FILLER          PIC X(08) VALUE ' '.
           05 WS-DET-DEBITO   PIC $$$$$9.99.
           05 FILLER          PIC X(07) VALUE ' '.
           05 WS-DET-DESC     PIC $$$9.99.
           05 FILLER          PIC X(06) VALUE ' '.
           05 WS-DET-EFEC     PIC $$$$$9.99.
           05 FILLER          PIC X     VALUE ' '.

       PROCEDURE DIVISION.

       0000-MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO.

           PERFORM 3000-FINALIZA.

      *-----------------------------------------------------------------
       1000-INICIO.

           OPEN INPUT CHEQUE

           DISPLAY "FS-CHEQUE: " FS-CHEQUE.

           IF FS-CHEQUE EQUAL '00'
              SET 88-CHEQUE-OPEN-SI TO TRUE
           ELSE
               DISPLAY "ERROR EN OPEN"
               DISPLAY "FS-CHEQUE      " FS-CHEQUE
               PERFORM 9000-CANCELO
           END-IF.

           OPEN OUTPUT MATI

           DISPLAY "FS-MATI: " FS-MATI.

           IF FS-MATI EQUAL '00'
              SET 88-MATI-OPEN-SI TO TRUE
           ELSE
               DISPLAY "ERROR EN OPEN"
               DISPLAY "FS-MATI      " FS-MATI
               PERFORM 9000-CANCELO
           END-IF.

           OPEN OUTPUT MATI2

           DISPLAY "FS-MATI: " FS-MATI.

           IF FS-MATI2 EQUAL '00'
              SET 88-MATI2-OPEN-SI TO TRUE
           ELSE
               DISPLAY "ERROR EN OPEN"
               DISPLAY "FS-MATI2     " FS-MATI2
               PERFORM 9000-CANCELO
           END-IF.

      *-----------------------------------------------------------------
       2000-PROCESO.

           READ CHEQUE INTO REG-CHEQUE

           DISPLAY "READ FS-CHEQUE : " FS-CHEQUE.

           IF FS-CHEQUE EQUAL '00'
              CONTINUE
           ELSE
               DISPLAY "ARCHIVO VACIO"
               DISPLAY "FS-CHEQUE      " FS-CHEQUE
               PERFORM 9000-CANCELO
           END-IF.

           MOVE WS-TITULO-1            TO WS-AUX-LISTA.
           PERFORM 2900-IMPRIMO-MATI.

           MOVE WS-TITULO-2            TO WS-AUX-LISTA.
           PERFORM 2900-IMPRIMO-MATI.

           MOVE WS-TITULO-3            TO WS-AUX-LISTA.
           PERFORM 2900-IMPRIMO-MATI.


           MOVE CHE-V-NOMBRE                  TO WS-DET-NOMBRE.
           MOVE CHE-V-NUMERO                  TO WS-DET-NUM-VEN.
           MOVE CHE-NUMERO                    TO WS-DET-NUM-CHE.
           MOVE CHE-DIA                       TO WS-DET-DIA.
           MOVE CHE-MES                       TO WS-DET-MES.
           MOVE CHE-ANIO                      TO WS-DET-AÑO.
           MOVE CHE-DEBITO                    TO WS-DET-DEBITO.
           MOVE CHE-DESCUENTO                 TO WS-DET-DESC.
           MOVE CHE-EFECTIVO                  TO WS-DET-EFEC.
           MOVE WS-DETALLE                    TO WS-AUX-LISTA.
           PERFORM 2900-IMPRIMO-MATI.

           MOVE "JUAN"                 TO WS-DET-NOMBRE.
           MOVE "0017"                 TO WS-DET-NUM-VEN.
           MOVE 54321                  TO WS-DET-NUM-CHE.
           MOVE 15                     TO WS-DET-DIA.
           MOVE 04                     TO WS-DET-MES.
           MOVE 2021                   TO WS-DET-AÑO.
           MOVE 12360.35               TO WS-DET-DEBITO.
           MOVE 140.96                 TO WS-DET-DESC.
           MOVE 10009.05               TO WS-DET-EFEC.

           MOVE WS-DETALLE             TO WS-AUX-LISTA.
           PERFORM 2900-IMPRIMO-MATI.

       2900-IMPRIMO-MATI.

           WRITE REG-LISTA-FD          FROM WS-AUX-LISTA.
           EVALUATE FS-MATI
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    PERFORM 9000-CANCELO
           END-EVALUATE.

           DISPLAY "FS-MATI: " FS-MATI.
      *-----------------------------------------------------------------
       3000-FINALIZA.

           PERFORM 9100-CIERRO-ARCHIVOS.
           STOP RUN.

       9000-CANCELO.

           DISPLAY "PROGRAMA CANCELADO"
           PERFORM 3000-FINALIZA.

       9100-CIERRO-ARCHIVOS.

           IF 88-CHEQUE-OPEN-SI
              PERFORM 9150-CIERRO-CHEQUE
           END-IF.

           IF 88-MATI-OPEN-SI
              PERFORM 9200-CIERRO-MATI
           END-IF.

           IF 88-MATI2-OPEN-SI
              PERFORM 9300-CIERRO-MATI2
           END-IF.

       9150-CIERRO-CHEQUE.

           DISPLAY "CIERRO CHEQUE"

           SET 88-CHEQUE-OPEN-NO TO TRUE

           CLOSE CHEQUE.

           DISPLAY "FS-CLOSE: " FS-CHEQUE.

           IF FS-CHEQUE EQUAL '00'
              CONTINUE
           ELSE
               DISPLAY "ERROR EN CLOSE"
               DISPLAY "FS-CHEQUE      " FS-CHEQUE
               PERFORM 9000-CANCELO
           END-IF.



       9200-CIERRO-MATI.

           DISPLAY "CIERRO MATI"

           SET 88-MATI-OPEN-NO TO TRUE

           CLOSE MATI.

           DISPLAY "FS-MATI: " FS-MATI.

           IF FS-MATI EQUAL '00'
              CONTINUE
           ELSE
               DISPLAY "ERROR EN CLOSE"
               DISPLAY "FS-MATI      " FS-MATI
               PERFORM 9000-CANCELO
           END-IF.




       9300-CIERRO-MATI2.

           DISPLAY "CIERRO MATI2"

           SET 88-MATI2-OPEN-NO TO TRUE

           CLOSE MATI2.

           DISPLAY "FS-MATI2: " FS-MATI2.

           IF FS-MATI2 EQUAL '00'
              CONTINUE
           ELSE
               DISPLAY "ERROR EN CLOSE"
               DISPLAY "FS-MATI2     " FS-MATI2
               PERFORM 9000-CANCELO
           END-IF.




       END PROGRAM EJERCICIO-5.
