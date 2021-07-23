******************************************************************
      * PROGRAMA DE INDUCCION DE LOGICA DE APAREO DE ARCHIVOS
      * ACTUALIZACION DE ARCHIVO MAESTRO A PARTIR DE UN ARCHIVO DE
      * NOVEDADES
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMAPAREO02.

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
           RECORDING MODE IS F.
       01  REG-MAESTRO-FD                 PIC X(80).
       01  MAE-REG.
           05  MAE-CLAVE.
               07  CLAVE1                 PIC X(10).
               07  NOMBRE-M               PIC X(15).
               07  APELLIDO-M             PIC X(15).
           05  FILLER                     PIC X(40).
       FD  NOVEDAD
           RECORDING MODE IS F.
       01  REG-NOVEDAD-FD                 PIC X(80).
       01  NOV-REG.
           05  NOV-CLAVE.
               07  CLAVE2                 PIC X(10).
               07  NOMBRE-N               PIC X(15).
               07  APELLIDO-N             PIC X(15).
           05  FILLER                     PIC X(40).
       FD  MAEACT
           RECORDING MODE IS F.
       01  REG-MAEACT-FD                  PIC X(80).
       01  MAEACT-REG.
           05  MAEACT-CLAVE-M             PIC X(10).
           05  MAEACT-CLAVE-N             PIC X(10).
           05  MAEACT-NOMBRE-M            PIC X(15).
           05  MAEACT-APELLIDO-M          PIC X(15).
           05  MAEACT-NOMBRE-N            PIC X(15).
           05  MAEACT-APELLIDO-N          PIC X(15).

       WORKING-STORAGE SECTION.
      *-----------------------
       77  WS-PROGRAMA                    PIC X(11) VALUE 'PGMAPAREO02'.
      *             FILE STATUS
       01  FS-MAESTRO                     PIC X(02) VALUE ' '.
       01  FS-NOVEDAD                     PIC X(02) VALUE ' '.
       01  FS-MAEACT                      PIC X(02) VALUE ' '.

       01  WS-APENOM                      PIC X(50) VALUE ' '.

      *           CONTADORES
       01  A000-LEIDOS-E1-MAE             PIC 9(09) VALUE ZEROES.
       01  A000-LEIDOS-E2-NOV             PIC 9(09) VALUE ZEROES.
       01  A000-ESCRITOS                  PIC 9(09) VALUE ZEROES.

      *           SWITCHES
       01  S000-FIN-E1-MAE                PIC X(01) VALUE 'N'.
           88 S000-FIN-E1-SI                        VALUE 'S'.
           88 S000-FIN-E1-NO                        VALUE 'N'.

       01  S000-FIN-E2-NOV                PIC X(01) VALUE 'N'.
           88 S000-FIN-E2-SI                        VALUE 'S'.
           88 S000-FIN-E2-NO                        VALUE 'N'.


       01  W000-ABEND.
           02 W000-ABEND-OBJETO           PIC X(15).
           02 W000-ABEND-CODIGO           PIC X(05).
           02 W000-ABEND-DESCRIPCION      PIC X(50).

       PROCEDURE DIVISION.
      *-------------------
       MAIN-PROCEDURE.

           PERFORM 100-INICIO.

           PERFORM 200-PROCESO
             UNTIL S000-FIN-E1-SI.

           PERFORM 300-FINALIZO.

           STOP RUN.

       100-INICIO.

           PERFORM 103-ABRO-ARCHIVOS.

           PERFORM 101-LEO-MAESTRO.
           IF FS-MAESTRO = '10'
              MOVE 'ARCHIVO VACIO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAESTRO      TO W000-ABEND-CODIGO
              MOVE 'MAESTRO'       TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF

           PERFORM 102-LEO-NOVEDAD.
           IF FS-NOVEDAD = '10'
              MOVE 'ARCHIVO VACIO' TO W000-ABEND-DESCRIPCION
              MOVE FS-NOVEDAD      TO W000-ABEND-CODIGO
              MOVE 'NOVEDAD'       TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF.

       100-FIN.
           EXIT.

       200-PROCESO.

            IF CLAVE1 = CLAVE2
                  PERFORM 102-LEO-NOVEDAD
                  PERFORM 201-ESCRIBIR-SALIDA
             ELSE
             IF CLAVE1 > CLAVE2
                PERFORM 102-LEO-NOVEDAD
             ELSE
                PERFORM 101-LEO-MAESTRO

             END-IF
               PERFORM 201-ESCRIBIR-SALIDA
               PERFORM 101-LEO-MAESTRO

           END-IF.
       200-FIN.
           EXIT.


       300-FINALIZO.

           PERFORM 301-CIERRO-ARCHIVOS.

           PERFORM 302-TOTALES-CONTROL.
           STOP RUN.

       300-FIN.
           EXIT.

       101-LEO-MAESTRO.

           READ MAESTRO
              AT END
                 SET S000-FIN-E1-SI TO TRUE
                 MOVE HIGH-VALUES TO CLAVE1
              NOT AT END
              ADD 1   TO A000-LEIDOS-E1-MAE.

       101-FIN.
           EXIT.

       102-LEO-NOVEDAD.

           READ NOVEDAD
            AT END
               SET S000-FIN-E2-SI TO TRUE
               MOVE HIGH-VALUES TO CLAVE2
            NOT AT END
              ADD 1   TO A000-LEIDOS-E2-NOV.

       102-FIN.

       103-ABRO-ARCHIVOS.

           OPEN INPUT MAESTRO

           IF FS-MAESTRO NOT = '00'
              MOVE 'ERROR AL ABRIR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAESTRO               TO W000-ABEND-CODIGO
              MOVE 'MAESTRO'                TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF


           OPEN INPUT NOVEDAD

           IF FS-NOVEDAD NOT = '00'
              MOVE 'ERROR AL ABRIR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-NOVEDAD TO W000-ABEND-CODIGO
              MOVE 'NOVEDAD'  TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF


           OPEN OUTPUT MAEACT

           IF FS-MAEACT NOT = '00'
              MOVE 'ERROR AL ABRIR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAEACT TO W000-ABEND-CODIGO
              MOVE 'MAEACT'  TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF.

       103-FIN.
           EXIT.

      ****************************************
      *         201-GRABADO-ESCRIBIR-SALIDA  *
      ****************************************
       201-ESCRIBIR-SALIDA.
           WRITE MAEACT-REG

            IF FS-MAEACT   NOT = '00'
              MOVE 'ERROR AL ESCRIBIR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAEACT   TO W000-ABEND-CODIGO
              MOVE 'MAEACT'    TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
            ELSE
              MOVE NOMBRE-M   TO MAEACT-NOMBRE-M
              MOVE APELLIDO-M TO MAEACT-APELLIDO-M
              MOVE NOMBRE-N   TO MAEACT-NOMBRE-N
              MOVE APELLIDO-N TO MAEACT-APELLIDO-N
              MOVE CLAVE1     TO MAEACT-CLAVE-M
              MOVE CLAVE2     TO MAEACT-CLAVE-N
           END-IF

            ADD 1 TO A000-ESCRITOS.

       301-CIERRO-ARCHIVOS.

           CLOSE MAESTRO.

           IF FS-MAESTRO NOT = '00'
              MOVE 'ERROR AL CERRAR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAESTRO TO W000-ABEND-CODIGO
              MOVE 'MAESTRO'  TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF.


           CLOSE NOVEDAD.

           IF FS-NOVEDAD NOT = '00'
              MOVE 'ERROR AL CERRAR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-NOVEDAD TO W000-ABEND-CODIGO
              MOVE 'NOVEDAD'  TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF.


           CLOSE MAEACT.

           IF FS-MAEACT NOT = '00'
             MOVE 'ERROR AL CERRAR ARCHIVO' TO W000-ABEND-DESCRIPCION
              MOVE FS-MAEACT TO W000-ABEND-CODIGO
              MOVE 'MAEACT'  TO W000-ABEND-OBJETO
              PERFORM 900-ABEND-PROCESO
           END-IF.

       301-FIN.
           EXIT.

       302-TOTALES-CONTROL.
           DISPLAY ' '.
           DISPLAY '***********************************************'.
           DISPLAY '     TOTALES DE CONTROL PROGRAMA ' WS-PROGRAMA.
           DISPLAY '***********************************************'.
           DISPLAY 'REG. LEIDOS DE NOMBRES  : ' A000-LEIDOS-E1-MAE.
           DISPLAY 'REG. LEIDOS DE EDADES   : ' A000-LEIDOS-E2-NOV.
           DISPLAY 'REG SALIDA. ESCRITOS    : ' A000-ESCRITOS.

       302-FIN.
           EXIT.

      ****************************************
      *         900-ABEND-PROCESO            *
      ****************************************
       900-ABEND-PROCESO.
           DISPLAY ' '
           DISPLAY '***********************************'
           DISPLAY '* ERROR EN EL SISTEMA              '
           DISPLAY '* OBJETO:            ' W000-ABEND-OBJETO
           DISPLAY '* CÓDIGO DE ERROR:   ' W000-ABEND-CODIGO
           DISPLAY '* DESCRIPCION:       ' W000-ABEND-DESCRIPCION
           DISPLAY '***********************************'
           STOP RUN.
