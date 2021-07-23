       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. TSTABMCLI.
      *AUTHOR. EDUARDO PALMEYRO.
      *
      *-----------------------------------------------------------
      * ESTE PROGRAMA REALIZA ABM Y D DE CLIENTES
      * SE HACE LA LECTURA DEL ARCHIVO DE NOVEDADES
      * Y POR CADA REGISTRO LEIDO DE NOVEDAD SE ACTUALIZA EL
      * ARCHIVO MAESTRO DE CLIENTES.
      *-----------------------------------------------------------
      * POR CADA REGISTRO DE NOVEDAD LEIDO VALIDAR:
      *       ERRORES: * CLIENTE NO NUMERICO
      *                * COD. TIPO NOVEDAD INVALIDO (A,B,M,D)
      *                * COD. CAMPO NOVEDAD NO INFORMADO
      *                * COD. CAMPO NOVEDAD DEBE SER NUMERICO 01 A 12
      *                * COD. CAMPO NOVEDAD DEBE SER 01 A 12
      *                * ALTAS INF. CAMPO 01 LAST-NAME
      *                * INFORMAR CAMPO DATO
      *                * ALTA DE CLIENTE EXISTENTE
      *                * BAJA DE CLIENTE INEXISTENTE
      *                * MODIF. DE CLIENTE INEXISTENTE
      *                * ALTA DE CLIENTE EFECTUADA
      *                * MODIFICACION EFECTUADA
      *                * BAJA LOGICA EFECTUADA
      *                * BAJA FISICA EFECTUADA
      *
      * SI EL CODIGO DE NOVEDAD ES ALTA:
      *       DEBE SER INFORMADO EL CAMPO 01 EN EL ALTA
      *-----------------------------------------------------------

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

           SELECT CLINOV           ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\CLINOV.D
      -    'AT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-CLINOV.

           SELECT CLIENTES         ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\Clientes
      -    'GNU.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS RANDOM
                                   RECORD KEY   IS REG-CLIENTES-KEY-FD
                                   ALTERNATE KEY   REG-CLIENTES-KEY2-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-CLIENTES.

           SELECT LISTADO          ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\TSTABMCL
      -    'I.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-LISTADO.


       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  CLINOV
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CLINOV-FD                 PIC  X(080).


       FD  CLIENTES
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-CLIENTES-FD.
           03 REG-CLIENTES-KEY-FD        PIC  9(010).
           03 FILLER                     PIC  X(190).
           03 REG-CLIENTES-KEY2-FD       PIC  X(050).
           03 FILLER                     PIC  X(400).


       FD  LISTADO
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-LISTADO-FD                PIC  X(199).



       WORKING-STORAGE SECTION.
      *-----------------------
       77  CTE-PROGRAMA                  PIC  X(20) VALUE 'TSTABMCLI'.

       77  FS-CLINOV                     PIC  X(02) VALUE ' '.
           88 88-FS-CLINOV-OK                       VALUE '00'.
           88 88-FS-CLINOV-EOF                      VALUE '10'.

       77  WS-OPEN-CLINOV                PIC  X     VALUE 'N'.
           88 88-OPEN-CLINOV-SI                     VALUE 'S'.
           88 88-OPEN-CLINOV-NO                     VALUE 'N'.

       77  WS-LEIDOS-CLINOV              PIC  9(09) VALUE 0.
       77  WS-LEIDOS-CLINOV-ED           PIC  ZZZ.ZZZ.ZZ9.
       77  WS-CLINOV-ERROR               PIC  X VALUE 'N'.
           88 88-CLINOV-ERROR-SI                VALUE 'S'.
           88 88-CLINOV-ERROR-NO                VALUE 'N'.

       77  FS-LISTADO                    PIC  X(02) VALUE ' '.
           88 88-FS-LISTADO-OK                      VALUE '00'.

       77  WS-OPEN-LISTADO               PIC  X     VALUE 'N'.
           88 88-OPEN-LISTADO-SI                    VALUE 'S'.
           88 88-OPEN-LISTADO-NO                    VALUE 'N'.

       77  WS-GRABADOS-LISTADO           PIC  9(09) VALUE 0.
       77  WS-GRABADOS-LISTADO-ED        PIC  ZZZ.ZZZ.ZZ9.

       77  FS-CLIENTES                   PIC  X(02) VALUE ' '.
           88 88-FS-CLIENTES-OK                     VALUE '00'.
           88 88-FS-CLIENTES-EOF                    VALUE '10'.
           88 88-FS-CLIENTES-INVALIDKEY             VALUE '21'.
           88 88-FS-CLIENTES-DUPKEY                 VALUE '22'.
           88 88-FS-CLIENTES-NOKEY                  VALUE '23'.

       77  WS-OPEN-CLIENTES              PIC  X     VALUE 'N'.
           88 88-OPEN-CLIENTES-SI                   VALUE 'S'.
           88 88-OPEN-CLIENTES-NO                   VALUE 'N'.

       77  WS-LEIDOS-CLIENTES            PIC  9(09) VALUE 0.
       77  WS-LEIDOS-CLIENTES-ED         PIC  ZZZ.ZZZ.ZZ9.
       77  WS-GRABADOS-CLIENTES          PIC  9(09) VALUE 0.
       77  WS-GRABADOS-CLIENTES-ED       PIC  ZZZ.ZZZ.ZZ9.
       77  WS-REGRABADOS-CLIENTES        PIC  9(09) VALUE 0.
       77  WS-REGRABADOS-CLIENTES-ED     PIC  ZZZ.ZZZ.ZZ9.
       77  WS-CLI-ID-ED                  PIC   ZZZZZZZZZ9.

       77  WS-CANT-ALTAS                 PIC  9(09) VALUE 0.
       77  WS-CANT-BAJAS-LOGICAS         PIC  9(09) VALUE 0.
       77  WS-CANT-BAJAS-FISICAS         PIC  9(09) VALUE 0.
       77  WS-CANT-MODIFICACIONES        PIC  9(09) VALUE 0.
       77  WS-CANT-ERRONEOS              PIC  9(09) VALUE 0.
       77  WS-CANT-ED                    PIC  ZZZ.ZZZ.ZZ9.


       77  WS-HOJA                       PIC  9(05).
       77  WS-HOJA-ED                    PIC  ZZ.ZZZ.
       77  WS-LINEA                      PIC  9(02) VALUE 80.
       77  WCN-LINEAS-MAX                PIC  9(02) VALUE 66.
       77  WCN-MAX-KEYS                  PIC  9(02) VALUE 21.
       77  IDX-KEY                       INDEX      VALUE +0.

       77  WS-PARRAFO                    PIC  X(50) VALUE ' '.
       77  WS-MJE-ERROR                  PIC  X(80) VALUE ' '.

       01  WS-CURRENT-DATE.
           03 WS-CURRENT-DATE-DATE.
              05 WS-CURRENT-DATE-YYYY    PIC 9(04) VALUE 0.
              05 WS-CURRENT-DATE-MM      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-DD      PIC 9(02) VALUE 0.
           03 WS-CURRENT-DATE-TIME.
              05 WS-CURRENT-DATE-HS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-MS      PIC 9(02) VALUE 0.
              05 WS-CURRENT-DATE-SS      PIC 9(02) VALUE 0.


      *-----------------------------------------------
      * DEFINICION DEL ARCHIVO DE NOVEDADES DE CLIENTE
      *-----------------------------------------------
       COPY WCLINOV.

      *------------------------------------
      * DEFINICION DEL ARCHIVO DE CLIENTES
      *------------------------------------
       COPY WCLIENTE.

      *-----------------------------------------------
      * DEFINICION DE CAMPOS ANTERIORES DEL CLIENTE
      *-----------------------------------------------
       COPY WCLIENTE
            REPLACING REG-CLIENTES BY REG-ANT.

      *------------------------------------
      * DEFINICION DE LINEA DE IMPRESION
      *------------------------------------
       COPY WLINEA198.

      *---------------------------------------------------
      * DEFINICION DE COPY WORKING RUTINA DE CANCELACION
      *---------------------------------------------------
       COPY WCANCELA.

       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *-----------------------

           PERFORM 10000-INICIO.

           PERFORM 20000-PROCESO
             UNTIL 88-FS-CLINOV-EOF.

           PERFORM 30000-FINALIZO.

           STOP RUN.

       10000-INICIO.
      *-------------

           INITIALIZE WCANCELA.
           MOVE CTE-PROGRAMA               TO WCANCELA-PROGRAMA.

           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 10200-1RA-LECTURA-CLINOV.

       FIN-10000.
           EXIT.

       10100-ABRO-ARCHIVOS.
      *-------------------

           MOVE '10100-ABRO-ARCHIVOS'      TO WS-PARRAFO.

           OPEN INPUT   CLINOV.

           EVALUATE FS-CLINOV
               WHEN '00'
                    SET 88-OPEN-CLINOV-SI  TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLINOV  '        TO WCANCELA-RECURSO
                    MOVE 'OPEN INPUT'      TO WCANCELA-OPERACION
                    MOVE FS-CLINOV         TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.


           OPEN I-O CLIENTES.

           EVALUATE FS-CLIENTES
               WHEN '00'
                    SET 88-OPEN-CLIENTES-SI TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'OPEN I-O  '      TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.


           OPEN OUTPUT LISTADO.

           EVALUATE FS-LISTADO
               WHEN '00'
                    SET 88-OPEN-LISTADO-SI TO TRUE

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'LISTADO'         TO WCANCELA-RECURSO
                    MOVE 'OPEN OUTPUT'     TO WCANCELA-OPERACION
                    MOVE FS-LISTADO        TO WCANCELA-CODRET
                    MOVE 'ERROR EN OPEN'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-10100.
           EXIT.

       10200-1RA-LECTURA-CLINOV.
      *-------------------------

           PERFORM 11000-READ-CLINOV.

           IF 88-FS-CLINOV-EOF
              DISPLAY '************************************'
              DISPLAY '***    ARCHIVO CLINOV VACIO      ***'
              DISPLAY '************************************'
           END-IF.


       FIN-10200.
           EXIT.

       11000-READ-CLINOV.
      *-------------------
                                                                                                                                C
           MOVE '11000-READ-CLINOV  '      TO WS-PARRAFO.

           INITIALIZE REG-CLINOV.

           READ CLINOV   INTO REG-CLINOV.                                                                                       R

           EVALUATE TRUE
               WHEN 88-FS-CLINOV-OK
                    ADD 1                  TO WS-LEIDOS-CLINOV

               WHEN 88-FS-CLINOV-EOF
                    MOVE ALL '*'           TO REG-CLINOV

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLINOV'          TO WCANCELA-RECURSO
                    MOVE 'READ'            TO WCANCELA-OPERACION
                    MOVE FS-CLINOV         TO WCANCELA-CODRET
                    MOVE 'ERROR EN READ'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.
                                                                                                                                C

       FIN-11000.
           EXIT.

       11100-READ-CLIENTES.
      *-------------------
                                                                                                                                C
           MOVE '11100-READ-CLIENTES'      TO WS-PARRAFO.

           INITIALIZE REG-CLIENTES
                      REG-CLIENTES-FD.

           MOVE CLINOV-CLAVE               TO REG-CLIENTES-KEY-FD.

           DISPLAY ' '.
           DISPLAY 'READ CLIENTES CLINOV-ID  :' CLI-ID IN REG-CLIENTES.
           DISPLAY 'REG-CLIENTES-KEY-FD      :' REG-CLIENTES-KEY-FD.

           READ CLIENTES INTO REG-CLIENTES


           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1                  TO WS-LEIDOS-CLIENTES
                    MOVE REG-CLIENTES      TO REG-ANT

               WHEN 88-FS-CLIENTES-NOKEY
                    MOVE ALL '*'           TO REG-CLIENTES

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'READ'            TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN READ'   TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-11100.
           EXIT.

       20000-PROCESO.
      *-------------

           PERFORM 20100-VALIDO-NOVEDAD.

           IF 88-CLINOV-ERROR-NO
              PERFORM 20200-ACTUALIZO-CLIENTES
           END-IF.

           PERFORM 21100-IMPRIMO-DETALLE.

           PERFORM 11000-READ-CLINOV.

       FIN-20000.
           EXIT.


       20100-VALIDO-NOVEDAD.
      *---------------------
           MOVE ' '                      TO WS-MJE-ERROR.
           SET  88-CLINOV-ERROR-NO       TO TRUE.

           IF  CLINOV-ID     NOT NUMERIC
           AND 88-CLINOV-ERROR-NO
               SET  88-CLINOV-ERROR-SI   TO TRUE
               MOVE 'CLIENTE NO NUMERICO'
                 TO WS-MJE-ERROR
           END-IF.

           IF  88-CLINOV-ERROR-NO
               EVALUATE  TRUE
                   WHEN  88-COD-ES-ALTA
                   WHEN  88-COD-ES-BAJA-LOGICA
                   WHEN  88-COD-ES-BAJA-FISICA
                   WHEN  88-COD-ES-MODIFICACION
                         CONTINUE

                   WHEN  OTHER
                         SET  88-CLINOV-ERROR-SI TO TRUE
                         MOVE 'COD. TIPO NOVEDAD INVALIDO (A,B,M,D)'
                           TO WS-MJE-ERROR
               END-EVALUATE
           END-IF.

           IF  88-CLINOV-ERROR-NO
           AND NOT 88-COD-ES-BAJA-LOGICA
           AND NOT 88-COD-ES-BAJA-FISICA
           AND (CLINOV-COD-CAMPO = LOW-VALUES
             OR CLINOV-COD-CAMPO = HIGH-VALUES
             OR CLINOV-COD-CAMPO = SPACES      )
               SET  88-CLINOV-ERROR-SI TO TRUE
               MOVE 'COD. CAMPO NOVEDAD NO INFORMADO'
                 TO WS-MJE-ERROR
           END-IF.

           IF  88-CLINOV-ERROR-NO
           AND NOT 88-COD-ES-BAJA-LOGICA
           AND NOT 88-COD-ES-BAJA-FISICA
           AND CLINOV-COD-CAMPO NOT NUMERIC
               SET  88-CLINOV-ERROR-SI TO TRUE
               MOVE 'COD. CAMPO NOVEDAD DEBE SER NUMERICO 01 A 12'
                 TO WS-MJE-ERROR
           END-IF.

           IF  88-CLINOV-ERROR-NO
           AND NOT 88-COD-ES-BAJA-LOGICA
           AND NOT 88-COD-ES-BAJA-FISICA
           AND (CLINOV-COD-CAMPO < 01
             OR CLINOV-COD-CAMPO > 12          )
               SET  88-CLINOV-ERROR-SI TO TRUE
               MOVE 'COD. CAMPO NOVEDAD DEBE SER 01 A 12'
                 TO WS-MJE-ERROR
           END-IF.

           IF  88-CLINOV-ERROR-NO
           AND 88-COD-ES-ALTA
           AND NOT 88-MOD-LAST-NAME
               SET  88-CLINOV-ERROR-SI TO TRUE
               MOVE 'ALTAS INF. CAMPO 01 LAST-NAME'
                 TO WS-MJE-ERROR
           END-IF.

           IF  88-CLINOV-ERROR-NO
           AND NOT 88-COD-ES-BAJA-LOGICA
           AND NOT 88-COD-ES-BAJA-FISICA
           AND (CLINOV-DATO  = SPACES
             OR CLINOV-DATO  = LOW-VALUES
             OR CLINOV-DATO  = HIGH-VALUES )
               SET  88-CLINOV-ERROR-SI TO TRUE
               MOVE 'INFORMAR CAMPO DATO'
                 TO WS-MJE-ERROR
           END-IF.

       FIN-20100.
           EXIT.


       20200-ACTUALIZO-CLIENTES.
      *------------------------

           PERFORM   11100-READ-CLIENTES.

           EVALUATE TRUE
               WHEN 88-COD-ES-ALTA
                    IF 88-FS-CLIENTES-NOKEY
                       PERFORM 20300-ALTA-CLIENTES
                    ELSE
                       SET  88-CLINOV-ERROR-SI   TO TRUE
                       MOVE 'ALTA DE CLIENTE EXISTENTE'
                         TO WS-MJE-ERROR
                    END-IF

               WHEN 88-COD-ES-BAJA-LOGICA
               WHEN 88-COD-ES-MODIFICACION
                    IF 88-FS-CLIENTES-OK
                       PERFORM 20400-MODIFICA-CLIENTES
                    ELSE
                       IF   88-COD-ES-BAJA-LOGICA
                            SET  88-CLINOV-ERROR-SI   TO TRUE
                            MOVE 'BAJA DE CLIENTE INEXISTENTE'
                              TO WS-MJE-ERROR
                       ELSE
                            SET  88-CLINOV-ERROR-SI   TO TRUE
                            MOVE 'MODIF. DE CLIENTE INEXISTENTE'
                              TO WS-MJE-ERROR
                       END-IF
                    END-IF

               WHEN  88-COD-ES-BAJA-FISICA
                     PERFORM 20500-DELETE-CLIENTES

               WHEN  OTHER
                     DISPLAY 'ERROR EN CODIGO DE NOVEDAD'
                             CLINOV-COD-CAMPO
                     STOP RUN

           END-EVALUATE.

       FIN-20200.
           EXIT.

       20300-ALTA-CLIENTES.
      *-------------------

           INITIALIZE REG-CLIENTES.

           MOVE CLINOV-CLAVE      TO CLI-CLAVE     IN REG-CLIENTES.
           MOVE CLINOV-DATO       TO CLI-LAST-NAME OF REG-CLIENTES.

           PERFORM 20900-WRITE-CLIENTES.

           ADD  1                        TO WS-CANT-ALTAS.
           MOVE 'ALTA DE CLIENTE EFECTUADA'
             TO WS-MJE-ERROR.

       FIN-20300.
           EXIT.

       20400-MODIFICA-CLIENTES.
      *-----------------------

           EVALUATE TRUE
               WHEN 88-MOD-LAST-NAME
                    MOVE CLINOV-DATO TO CLI-LAST-NAME  IN REG-CLIENTES

               WHEN 88-MOD-FIRST-NAME
                    MOVE CLINOV-DATO TO CLI-FIRST-NAME IN REG-CLIENTES

               WHEN 88-MOD-SEX
                    MOVE CLINOV-DATO TO CLI-SEX        IN REG-CLIENTES

               WHEN 88-MOD-BIRTH
                    MOVE CLINOV-DATO TO CLI-BIRTH      IN REG-CLIENTES

               WHEN 88-MOD-DEATH
                    MOVE CLINOV-DATO TO CLI-DEATH      IN REG-CLIENTES

               WHEN 88-MOD-MARITAL-STATUS
                    MOVE CLINOV-DATO TO CLI-MARITAL-STATUS
                                                       IN REG-CLIENTES

               WHEN 88-MOD-HOME-ADDY
                    MOVE CLINOV-DATO TO CLI-HOME-ADDY  IN REG-CLIENTES

               WHEN 88-MOD-HOME-CITY
                    MOVE CLINOV-DATO TO CLI-HOME-CITY  IN REG-CLIENTES

               WHEN 88-MOD-HOME-ZIP-CODE
                    MOVE CLINOV-DATO TO CLI-HOME-ZIP-CODE
                                                       IN REG-CLIENTES

               WHEN 88-MOD-HOME-PHONE
                    MOVE CLINOV-DATO TO CLI-HOME-PHONE IN REG-CLIENTES

               WHEN 88-MOD-HOME-EMAIL
                    MOVE CLINOV-DATO TO CLI-HOME-EMAIL IN REG-CLIENTES

               WHEN 88-MOD-HOME-PROVINCE-NAME
                    MOVE CLINOV-DATO TO CLI-HOME-PROVINCE-NAME
                                                       IN REG-CLIENTES

               WHEN 88-COD-ES-BAJA-LOGICA
                    SET CLI-STATUS-INACTIVO IN REG-CLIENTES TO TRUE


           END-EVALUATE.

           PERFORM 20950-REWRITE-CLIENTES.

           IF 88-COD-ES-BAJA-LOGICA
              ADD 1                         TO WS-CANT-BAJAS-LOGICAS
              MOVE 'BAJA LOGICA EFECTUADA'  TO WS-MJE-ERROR
           ELSE
              ADD 1                         TO WS-CANT-MODIFICACIONES
              MOVE 'MODIFICACION EFECTUADA' TO WS-MJE-ERROR
           END-IF.

       FIN-20400.
           EXIT.

       20500-DELETE-CLIENTES.
      *---------------------

           MOVE '20500-DELETE-CLIENTES'      TO WS-PARRAFO.

           MOVE CLINOV-CLAVE                 TO REG-CLIENTES-KEY-FD.

           DELETE CLIENTES RECORD.

           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1                  TO WS-CANT-BAJAS-FISICAS
                    MOVE 'BAJA FISICA EFECTUADA'
                                           TO WS-MJE-ERROR
               WHEN 88-FS-CLIENTES-NOKEY
                    SET  88-CLINOV-ERROR-SI   TO TRUE
                    MOVE 'BAJA DE CLIENTE INEXISTENTE'
                                           TO WS-MJE-ERROR

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'DELETE'          TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE CLINOV-CLAVE      TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-20500.
           EXIT.


       20900-WRITE-CLIENTES.
      *---------------------

           MOVE '20900-WRITE-CLIENTES'       TO WS-PARRAFO.

           WRITE REG-CLIENTES-FD FROM REG-CLIENTES.

           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1                  TO WS-GRABADOS-CLIENTES

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'WRITE'           TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN WRITE'  TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-20900.
           EXIT.

       20950-REWRITE-CLIENTES.
      *---------------------

           MOVE '20950-REWRITE-CLIENTES'   TO WS-PARRAFO.

           REWRITE REG-CLIENTES-FD FROM REG-CLIENTES.

           EVALUATE TRUE
               WHEN 88-FS-CLIENTES-OK
                    ADD 1                  TO WS-REGRABADOS-CLIENTES

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'CLIENTES'        TO WCANCELA-RECURSO
                    MOVE 'REWRITE'         TO WCANCELA-OPERACION
                    MOVE FS-CLIENTES       TO WCANCELA-CODRET
                    MOVE 'ERROR EN REWRITE' TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-20950.
           EXIT.


       21100-IMPRIMO-DETALLE.
      *---------------------

           ADD 1                    TO WS-LINEA.
           PERFORM 21200-CONTROL-LINEA.

           PERFORM 21300-ARMO-DETALLE.
           PERFORM 21400-WRITE-LISTADO.

           IF 88-CLINOV-ERROR-SI
              ADD 1                 TO WS-CANT-ERRONEOS
           END-IF.

       FIN-21100.
           EXIT.


       21200-CONTROL-LINEA.
      *-------------------

           IF WS-LINEA          > WCN-LINEAS-MAX
              PERFORM 21210-IMPRIMO-TITULOS
           END-IF.

       FIN-21200.
           EXIT.

       21210-IMPRIMO-TITULOS.
      *----------------------

           ADD 1                           TO WS-HOJA.

      * TITULO-LINEA-1
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE '-'                        TO PCC.
           MOVE 'FECHA:'                   TO P1.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE.
           MOVE WS-CURRENT-DATE-DD         TO P9.
           MOVE '/'                        TO P11.
           MOVE WS-CURRENT-DATE-MM         TO P12.
           MOVE '/'                        TO P14.
           MOVE WS-CURRENT-DATE-YYYY       TO P15.
           MOVE 'LISTADO DE NOVEDADES A'   TO P87.
           MOVE 'CLIENTES              '   TO P110.
           MOVE 'HOJA:'                    TO P187.
           MOVE WS-HOJA                    TO WS-HOJA-ED.
           MOVE WS-HOJA-ED                 TO P193.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-2
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE 'HORA:'                    TO P1.
           MOVE WS-CURRENT-DATE-HS         TO P9.
           MOVE ':'                        TO P11.
           MOVE WS-CURRENT-DATE-MS         TO P12.
           MOVE ':'                        TO P14.
           MOVE WS-CURRENT-DATE-SS         TO P15.
           MOVE ALL '-'                    TO WLINEA(88:31).
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-3
      *---------------

           MOVE ' '                        TO WLINEA.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-4
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE 'ID CLIENTE'               TO P1.
           MOVE 'TN'                       TO P12.
           MOVE 'CPO'                      TO P15.
           MOVE 'DESCRIPCION CAMPO'        TO P19.
           MOVE 'VALOR NUEVO'              TO P40.
           MOVE 'VALOR ANTERIOR'           TO P91.
           MOVE 'MENSAJE'                  TO P142.
           PERFORM 21400-WRITE-LISTADO.

      * TITULO-LINEA-5
      *---------------

           MOVE ' '                        TO WLINEA.
           MOVE '----------'               TO P1.
           MOVE '--'                       TO P12.
           MOVE '---'                      TO P15.
           MOVE ALL '-'                    TO WLINEA(20:20).
           MOVE ALL '-'                    TO WLINEA(41:50).
           MOVE ALL '-'                    TO WLINEA(92:50).
           MOVE ALL '-'                    TO WLINEA(143:57).
           PERFORM 21400-WRITE-LISTADO.

           MOVE 5                          TO WS-LINEA.

       FIN-21200.
           EXIT.

       21300-ARMO-DETALLE.
      *-------------------

           MOVE ' '                        TO WLINEA.

           IF CLINOV-ID IS NUMERIC
              MOVE CLINOV-ID               TO WS-CLI-ID-ED
              MOVE WS-CLI-ID-ED            TO P1
           ELSE
              MOVE CLINOV-CLAVE            TO P1
           END-IF.

           MOVE CLINOV-COD-NOVEDAD         TO P13.
           MOVE CLINOV-COD-CAMPO           TO P16.

           EVALUATE TRUE
               WHEN 88-MOD-LAST-NAME
                    MOVE 'LAST-NAME'                          TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-LAST-NAME          IN REG-ANT    TO P91

               WHEN 88-MOD-FIRST-NAME
                    MOVE 'FIRST-NAME'                         TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-FIRST-NAME         IN REG-ANT    TO P91

               WHEN 88-MOD-SEX
                    MOVE 'SEX       '                         TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-SEX                IN REG-ANT    TO P91

               WHEN 88-MOD-BIRTH
                    MOVE 'BIRTH DATE'                         TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-BIRTH              IN REG-ANT    TO P91

               WHEN 88-MOD-DEATH
                    MOVE 'DEATH DATE'                         TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-DEATH              IN REG-ANT    TO P91

               WHEN 88-MOD-MARITAL-STATUS
                    MOVE 'MARITAL STATUS'                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-MARITAL-STATUS     IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-ADDY
                    MOVE 'HOME ADDY     '                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-ADDY          IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-CITY
                    MOVE 'HOME CITY     '                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-CITY          IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-ZIP-CODE
                    MOVE 'HOME ZIP CODE '                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-ZIP-CODE      IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-PHONE
                    MOVE 'HOME PHONE    '                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-PHONE         IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-EMAIL
                    MOVE 'HOME EMAIL    '                     TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-EMAIL         IN REG-ANT    TO P91

               WHEN 88-MOD-HOME-PROVINCE-NAME
                    MOVE 'HOME PROVINCE NAME'                 TO P19
                    MOVE CLINOV-DATO                          TO P40
                    MOVE CLI-HOME-PROVINCE-NAME IN REG-ANT    TO P91

               WHEN 88-COD-ES-BAJA-LOGICA
                    MOVE 'CLIENTE STATUS'                     TO P19
                    MOVE CLI-STATUS           IN REG-CLIENTES TO P40
                    MOVE CLI-STATUS           IN REG-ANT      TO P91
           END-EVALUATE.

           MOVE WS-MJE-ERROR                                  TO P142.

       FIN-21300.
           EXIT.

       21400-WRITE-LISTADO.
      *-------------------

           MOVE '21400-WRITE-LISTADO' TO WS-PARRAFO.

           WRITE REG-LISTADO-FD     FROM WLINEA.

           EVALUATE FS-LISTADO
               WHEN '00'
                    ADD 1             TO WS-GRABADOS-LISTADO

               WHEN OTHER
                    MOVE WS-PARRAFO        TO WCANCELA-PARRAFO
                    MOVE 'LISTADO'         TO WCANCELA-RECURSO
                    MOVE 'WRITE'           TO WCANCELA-OPERACION
                    MOVE FS-LISTADO        TO WCANCELA-CODRET
                    MOVE WLINEA            TO WCANCELA-MENSAJE
                    PERFORM 99999-CANCELO

           END-EVALUATE.

       FIN-21400.
           EXIT.

       30000-FINALIZO.
      *--------------

           PERFORM 30100-TOTALES-CONTROL.

           PERFORM 31000-CIERRO-ARCHIVOS.

           STOP RUN.

       FIN-30000.
           EXIT.

       30100-TOTALES-CONTROL.
      *---------------------

           MOVE WS-LEIDOS-CLINOV          TO WS-LEIDOS-CLINOV-ED.
           MOVE WS-LEIDOS-CLIENTES        TO WS-LEIDOS-CLIENTES-ED.
           MOVE WS-GRABADOS-CLIENTES      TO WS-GRABADOS-CLIENTES-ED.
           MOVE WS-REGRABADOS-CLIENTES    TO WS-REGRABADOS-CLIENTES-ED.
           MOVE WS-GRABADOS-LISTADO       TO WS-GRABADOS-LISTADO-ED.

           DISPLAY ' '.
           DISPLAY '****************************************'.
           DISPLAY 'TOTALES DE CONTROL PGM: TSTABMCLI       '.
           DISPLAY '****************************************'.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. LEIDOS CLINOV          : '
                                               WS-LEIDOS-CLINOV-ED.

           MOVE WS-CANT-ALTAS               TO WS-CANT-ED.
           DISPLAY '*       CANT. ALTAS                 : '
                                               WS-CANT-ED

           MOVE WS-CANT-BAJAS-LOGICAS       TO WS-CANT-ED.
           DISPLAY '*       CANT. BAJAS LOGICAS         : '
                                               WS-CANT-ED

           MOVE WS-CANT-BAJAS-FISICAS       TO WS-CANT-ED.
           DISPLAY '*       CANT. BAJAS FISICAS         : '
                                               WS-CANT-ED

           MOVE WS-CANT-MODIFICACIONES      TO WS-CANT-ED.
           DISPLAY '*       CANT. MODIFICACIONES        : '
                                               WS-CANT-ED
           MOVE WS-CANT-ERRONEOS            TO WS-CANT-ED.
           DISPLAY '*       CANT. ERRORES               : '
                                               WS-CANT-ED
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. LEIDOS CLIENTES        : '
                                               WS-LEIDOS-CLIENTES-ED.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. GRABADOS CLIENTES      : '
                                               WS-GRABADOS-CLIENTES-ED.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. RE-GRABADOS CLIENTES   : '
                                             WS-REGRABADOS-CLIENTES-ED.
           DISPLAY '*                                      *'.
           DISPLAY '* CANT. REG. GRABADOS LISTADO       : '
                                               WS-GRABADOS-LISTADO-ED.
           DISPLAY '*                                      *'.
           DISPLAY '****************************************'.
           DISPLAY ' '.

       FIN-30100.
           EXIT.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           MOVE '31000-CIERRO-ARCHIVOS'        TO WS-PARRAFO.

           IF 88-OPEN-CLINOV-SI
              SET 88-OPEN-CLINOV-NO            TO TRUE
              CLOSE CLINOV
              EVALUATE TRUE
                  WHEN 88-FS-CLINOV-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
                       MOVE 'CLINOV  '         TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-CLINOV          TO WCANCELA-CODRET
                       MOVE 'CIERRA CLINOV  '  TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-CLIENTES-SI
              SET 88-OPEN-CLIENTES-NO          TO TRUE
              CLOSE CLIENTES
              EVALUATE TRUE
                  WHEN 88-FS-CLIENTES-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
                       MOVE 'CLIENTES'         TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-CLIENTES        TO WCANCELA-CODRET
                       MOVE 'CIERRA CLIENTES'  TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-LISTADO-SI
              SET 88-OPEN-LISTADO-NO           TO TRUE
              CLOSE LISTADO
              EVALUATE TRUE
                  WHEN 88-FS-LISTADO-OK
                       CONTINUE
                  WHEN OTHER
                       MOVE WS-PARRAFO         TO WCANCELA-PARRAFO
                       MOVE 'LISTADO'          TO WCANCELA-RECURSO
                       MOVE 'CLOSE'            TO WCANCELA-OPERACION
                       MOVE FS-LISTADO         TO WCANCELA-CODRET
                       MOVE 'CIERRA LISTADO'   TO WCANCELA-MENSAJE
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

       FIN-31000.
           EXIT.

       99999-CANCELO.

           PERFORM 31000-CIERRO-ARCHIVOS.

           CALL 'CANCELA' USING WCANCELA.

           STOP RUN.

       FIN-99999.
           EXIT.
