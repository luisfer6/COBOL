      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EjerPatente.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT AUTOS ASSIGN TO
            'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\vehicul
      -    'o.dat'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS DYNAMIC
                                   RECORD KEY   IS ID-KEY-FD
                                   ALTERNATE KEY   PATENTE-KEY-FD
                                   ALTERNATE KEY   MARCA-KEY-FD
                                                   WITH DUPLICATES
                                   FILE STATUS  IS FS-AUTOS.

       DATA DIVISION.
       FILE SECTION.

       FD  AUTOS
           RECORDING MODE IS F
           BLOCK 0.
           01 REG-AUTOS-FD.
              03 ID-KEY-FD         PIC X(05).
              03 PATENTE-KEY-FD    PIC X(09).
              03 MARCA-KEY-FD      PIC X(08).
              03 FILLER            PIC X(17).
       WORKING-STORAGE SECTION.

       77  FS-AUTOS                PIC XX VALUE '  '.
           88 88-AUTOS-OKEY               VALUE '00'.
           88 88-AUTOS-EOF                VALUE '10'.
           88 88-AUTOS-NOKEY              VALUE '23'.
           88 88-INVALIDKEY               VALUE '22'.

       77  WS-AUTOS-OPEN           PIC X  VALUE 'N'.
           88 88-AUTOS-OPEN-SI            VALUE 'S'.
           88 88-AUTOS-OPEN-NO            VALUE 'N'.



       77  OPCION                          PIC 9.
           88 88-OPC1                      VALUE 1.
           88 88-OPC2                      VALUE 2.
           88 88-OPC3                      VALUE 3.
           88 88-OPC4                      VALUE 4.
           88 88-OPC-OK                    VALUE 1 2 3 4.

       77  WS-KEY-BUSQUEDA         PIC 9(10).

       77  WS-INPUT                PIC X(09).
           88 88-INPUT-FIN                 VALUE 'FIN'.
       COPY WAUTOS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO UNTIL 88-OPC4.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIO.

           PERFORM 1100-OPEN-AUTOS.

       1100-OPEN-AUTOS.

           OPEN INPUT AUTOS.

           IF FS-AUTOS EQUALS '00'
              SET 88-AUTOS-OPEN-SI TO TRUE
           ELSE
              DISPLAY 'ERROR OPEN EN AUTOS'
              DISPLAY 'ERROR CODE: ' FS-AUTOS
              PERFORM 3000-FINALIZO
           END-IF.
      *----------------------------------------------------------------*
       2000-PROCESO.
           PERFORM 2050-MOSTRAR-OPCIONES.
           INITIALIZE WS-INPUT.
           EVALUATE TRUE
               WHEN 88-OPC1
                    PERFORM 2750-BUSCAR-ID      UNTIL 88-INPUT-FIN
               WHEN 88-OPC2
                    PERFORM 2400-BUSCAR-MARCA   UNTIL 88-INPUT-FIN
               WHEN 88-OPC3
                    PERFORM 2300-BUSCAR-PATENTE UNTIL 88-INPUT-FIN
               WHEN 88-OPC4
                    PERFORM 3000-FINALIZO
               WHEN OTHER
                    CONTINUE
           END-EVALUATE.

       2050-MOSTRAR-OPCIONES.
      *-----------------------
           DISPLAY '--------------------MENU--------------------'.
           DISPLAY '............................................'.
           DISPLAY '     1. BUSCAR AUTO POR CODIGO              '.
           DISPLAY '............................................'.
           DISPLAY '     2. BUSCAR AUTO/S POR MARCA             '.
           DISPLAY '............................................'.
           DISPLAY '     3. BUSCAR / MOSTRAR UNA PATENTE        '.
           DISPLAY '............................................'.
           DISPLAY '     4. SALIR                               '.
           DISPLAY '............................................'.
           DISPLAY 'INGRESE OPCION:                             '.
           ACCEPT OPCION.
           DISPLAY ' '.
           IF NOT 88-OPC-OK
              DISPLAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
              DISPLAY 'XXXXXXXX INGRESE UN VALOR CORRECTO: XXXXXXXX'
              DISPLAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
              DISPLAY '                                            '
           END-IF.

       2100-LECTURA-PATENTE.
           INITIALIZE VEH-REGISTRO.
           MOVE WS-INPUT TO PATENTE-KEY-FD.

           READ AUTOS RECORD INTO  VEH-REGISTRO
             KEY IS PATENTE-KEY-FD

           EVALUATE FS-AUTOS
               WHEN '00'
                    CONTINUE
               WHEN '23'
                    DISPLAY 'PATENTE NO ENCONTRADA'
                    MOVE ALL '*' TO VEH-REGISTRO
               WHEN OTHER
                    DISPLAY 'ERROR EN LECTURA DE AUTOS: ' FS-AUTOS
                    DISPLAY 'CLAVE PATENTE: ' VEH-PATENTE
           END-EVALUATE.

           PERFORM 2200-MOSTRAR.

       2200-MOSTRAR.

           DISPLAY " ".
           DISPLAY "ID AUTO         : " VEH-ID
           DISPLAY "PATENTE DEL AUTO: " VEH-PATENTE.
           DISPLAY "MARCA DEL AUTO  : " VEH-MARCA.
           DISPLAY "COLOR DEL AUTO  : " VEH-COLOR.
           DISPLAY "MODELO DE AUTO  : " VEH-MODELO.

       2300-BUSCAR-PATENTE.
           DISPLAY "INGRESE PATENTE. FINALIZAR = FIN"
           ACCEPT WS-INPUT.

           IF NOT 88-INPUT-FIN
              PERFORM 2100-LECTURA-PATENTE
           END-IF.

       2400-BUSCAR-MARCA.
           DISPLAY "INGRESE MARCA. FINALIZAR = FIN"
           ACCEPT WS-INPUT.

           IF NOT 88-INPUT-FIN
              PERFORM 2500-START-MARCA
              PERFORM 2600-LEER-MARCA
              PERFORM
                UNTIL VEH-MARCA NOT = WS-INPUT
                OR 88-AUTOS-EOF
                   PERFORM 2200-MOSTRAR
                   PERFORM 2600-LEER-MARCA
              END-PERFORM
           END-IF.


       2500-START-MARCA.
           INITIALIZE VEH-REGISTRO.
           MOVE WS-INPUT TO VEH-MARCA
                            MARCA-KEY-FD.

           START AUTOS
             KEY IS EQUAL MARCA-KEY-FD.
      *       KEY IS NOT > MARCA-KEY-FD.

           DISPLAY 'FS-AUTOS: ' FS-AUTOS.
           EVALUATE TRUE
               WHEN 88-AUTOS-OKEY
      *             ADD 1              TO WS-LEIDOS-AUTOS
                    CONTINUE

               WHEN 88-AUTOS-NOKEY
                    MOVE ALL '*'       TO VEH-REGISTRO

               WHEN OTHER
                    CONTINUE
           END-EVALUATE.

       2600-LEER-MARCA.
           INITIALIZE VEH-REGISTRO.
           READ AUTOS NEXT RECORD INTO VEH-REGISTRO .
           DISPLAY VEH-MARCA.
           EVALUATE TRUE
               WHEN 88-AUTOS-OKEY
      *             ADD 1              TO WS-LEIDOS-CLIENTES
                    DISPLAY 'NEXT LEIDO MARCA : '
                             VEH-MARCA

               WHEN 88-AUTOS-EOF
                    MOVE ALL '*'       TO VEH-REGISTRO

               WHEN OTHER
                    CONTINUE
           END-EVALUATE.
       2750-BUSCAR-ID.
           DISPLAY "INGRESE ID. FINALIZAR = FIN"
           ACCEPT WS-INPUT.

           IF NOT 88-INPUT-FIN
              PERFORM 2700-LEER-ID
              PERFORM 2200-MOSTRAR
           END-IF.

       2700-LEER-ID.
           INITIALIZE VEH-REGISTRO.
           MOVE WS-INPUT TO ID-KEY-FD.

           READ AUTOS INTO VEH-REGISTRO
             KEY IS ID-KEY-FD.

           DISPLAY VEH-ID.
           EVALUATE TRUE
               WHEN 88-AUTOS-OKEY
      *             ADD 1              TO WS-LEIDOS-CLIENTES
                    DISPLAY 'NEXT LEIDO ID : '
                             VEH-ID

               WHEN 88-AUTOS-EOF
                    MOVE ALL '*'       TO VEH-REGISTRO

               WHEN OTHER
                    CONTINUE
           END-EVALUATE.

       3000-FINALIZO.

           PERFORM 3100-CERRAR-ARCHIVOS.

       3100-CERRAR-ARCHIVOS.

           IF 88-AUTOS-OPEN-SI
               CLOSE AUTOS

               EVALUATE TRUE
                   WHEN 88-AUTOS-OKEY
                       CONTINUE
                   WHEN OTHER
                       DISPLAY 'ERROR CLOSE: ' FS-AUTOS
                       STOP RUN
               END-EVALUATE
           END-IF.
