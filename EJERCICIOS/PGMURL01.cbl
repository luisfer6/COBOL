      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMURL01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT URL       ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\url.txt'
                                 ORGANIZATION IS LINE SEQUENTIAL
                                 FILE STATUS  IS FS-URL.

           SELECT PAISES        ASSIGN       TO
           'C:\Users\Usuario\Desktop\OpenCobolIDE\GnuCOBOL\file\paises.t
      -    'xt'
                                 ORGANIZATION IS LINE SEQUENTIAL
                                 FILE STATUS  IS FS-PAISES.

       DATA DIVISION.
       FILE SECTION.
       FD  URL
           RECORDING MODE IS F
           BLOCK 0.
           01 URL-FD                     PIC X(76).
       FD  PAISES
           RECORDING MODE IS F
           BLOCK 0.
           01 PAISES-FD                  PIC X(52).

       WORKING-STORAGE SECTION.
       77  FS-URL                        PIC X(02).
           88 88-URL-OK                            VALUE '00'.
           88 88-URL-EOF                           VALUE '10'.

       77  FS-PAISES                     PIC X(02).
           88 88-PAISES-OK                         VALUE '00'.
           88 88-PAISES-EOF                        VALUE '10'.

       77  WCN-PAISES-TOPE               PIC 9(03)  VALUE 250.

       77  PAISES-LEIDOS                 PIC 9(03) VALUE 0.
           88 88-P-LEIDOS-LIMITE                   VALUE 251.

       77  AUX-POSICION-URL              PIC 9(02).
       77  AUX-URL                       PIC X(03).

       01  WS-PAISES.
           03 WS-PAISES-TABLA      OCCURS 250 TIMES
                                   INDEXED BY IDX-PAIS.
              05 WS-CODIGO-PAIS         PIC X(02).
              05 WS-NOMBRE-PAIS         PIC X(50).

       COPY WVISURL.

       COPY WCODPAIS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           INICIO.
           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO.

           PERFORM 3000-FINALIZAR.

           STOP RUN.

       1000-INICIO.

           PERFORM 1100-ABRIR-ARCHIVOS.

       1100-ABRIR-ARCHIVOS.

           OPEN INPUT URL

           EVALUATE TRUE
               WHEN 88-URL-OK
                    CONTINUE
               WHEN 88-URL-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN URL'
                    DISPLAY 'FILE STATUS' FS-URL
           END-EVALUATE

           OPEN INPUT PAISES

           EVALUATE TRUE
               WHEN 88-PAISES-OK
                    CONTINUE
               WHEN 88-PAISES-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN OPEN PAISES'
                    DISPLAY 'FILE STATUS' FS-PAISES
           END-EVALUATE.

       2000-PROCESO.

           PERFORM 2200-LECTURA-PAISES

           PERFORM 2300-CARGA-PAISES-OCCURS UNTIL 88-P-LEIDOS-LIMITE
                                            OR 88-PAISES-EOF
           PERFORM 2100-LECTURA-URL

           PERFORM 2400-BUSQUEDA-PAISES UNTIL 88-URL-EOF.

       2100-LECTURA-URL.

           INITIALIZE URL-REG

           READ URL        INTO URL-REG

           EVALUATE TRUE
               WHEN 88-URL-OK
                    CONTINUE
               WHEN 88-URL-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN READ URL'
                    DISPLAY 'FILE STATUS' FS-URL
                    PERFORM 3000-FINALIZAR
           END-EVALUATE.

       2200-LECTURA-PAISES.


           READ PAISES     INTO ITPAIS-REG

           EVALUATE TRUE
               WHEN 88-PAISES-OK
                    CONTINUE
               WHEN 88-PAISES-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR EN READ PAISES'
                    DISPLAY 'FILE STATUS' FS-PAISES
                    PERFORM 3000-FINALIZAR
           END-EVALUATE.

       2300-CARGA-PAISES-OCCURS.
      *    ORDENARLA Y HIGH VALUES 02
           ADD 1 TO PAISES-LEIDOS

           EVALUATE TRUE
               WHEN 88-P-LEIDOS-LIMITE
                    DISPLAY 'TABLA DE CODIGOS PAISES EXCEDIDA'
                    DISPLAY 'LIMITE:' WCN-PAISES-TOPE
               WHEN OTHER
                    MOVE ITPAIS-REG TO WS-PAISES-TABLA(PAISES-LEIDOS)
                    PERFORM 2200-LECTURA-PAISES
           END-EVALUATE.

       2400-BUSQUEDA-PAISES.

           INITIALIZE AUX-URL AUX-POSICION-URL

           PERFORM VARYING IDX-PAIS FROM 1 BY 1
                                      UNTIL IDX-PAIS > LENGTH OF URL-URL
                                      OR AUX-URL NOT EQUAL ' '
               EVALUATE URL-URL(IDX-PAIS:1)
                   WHEN ' '
                      SUBTRACT 3 FROM  IDX-PAIS GIVING AUX-POSICION-URL
                      MOVE URL-URL(AUX-POSICION-URL:3) TO AUX-URL
                   WHEN OTHER
                      CONTINUE
               END-EVALUATE
           END-PERFORM

      * SI URL MAS EXTENSA QUE 2 BYTES NO BUSCA
           SET IDX-PAIS TO 1
           IF AUX-URL(1:1) = '.'
              PERFORM 2500-BUSCAR-PAIS
           ELSE
              PERFORM 3400-PRINT-DATA-NO-ENCONTRADO
           END-IF.
           PERFORM 2100-LECTURA-URL.

      *      UNSTRING URL-URL
      *       DELIMITED BY 'com.'
      *            INTO AUX-URL
      *                 AUX-POSICION-URL

      *      INITIALIZE PAISES-LEIDOS

      *    PERFORM UNTIL 88-P-LEIDOS-LIMITE
      *           OR AUX-POSICION-URL EQUAL WS-CODIGO-PAIS(PAISES-LEIDOS)
      *     ADD 1 TO PAISES-LEIDOS
      *     EVALUATE TRUE
      *        WHEN AUX-POSICION-URL EQUAL WS-CODIGO-PAIS(PAISES-LEIDOS)
      *             PERFORM 3300-PRINT-DATA-ENCONTRADO
      *         WHEN 88-P-LEIDOS-LIMITE
      *              PERFORM 3400-PRINT-DATA-NO-ENCONTRADO
      *         WHEN OTHER
      *             CONTINUE
      *      END-EVALUATE
      *    END-PERFORM.
      *     PERFORM 2100-LECTURA-URL.

       2500-BUSCAR-PAIS.
           SEARCH WS-PAISES-TABLA
               AT END
                   PERFORM 3400-PRINT-DATA-NO-ENCONTRADO
               WHEN AUX-URL(2:2) EQUAL WS-CODIGO-PAIS(IDX-PAIS)
                   PERFORM 3300-PRINT-DATA-ENCONTRADO
           END-SEARCH.

       3000-FINALIZAR.

           PERFORM 3100-CIERRE-ARCHIVOS.
           STOP RUN.

       3100-CIERRE-ARCHIVOS.

           CLOSE URL
           EVALUATE TRUE
               WHEN 88-URL-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR CLOSE URL FS: ' FS-URL
           END-EVALUATE.

           CLOSE PAISES.
            EVALUATE TRUE
                WHEN 88-URL-OK
                     CONTINUE
                WHEN OTHER
                     DISPLAY 'ERROR CLOSE URL FS: ' FS-URL
           END-EVALUATE.

       3300-PRINT-DATA-ENCONTRADO.
           DISPLAY ':::::::::::::::::::::::::::::::::::::::::::::::::::'
                   ':::::::::::'.
           DISPLAY ' URL      : ' URL-URL.
           DISPLAY ' PAIS     : ' WS-NOMBRE-PAIS(IDX-PAIS).
           DISPLAY ' USUARIO  : ' URL-USUARIO.
           DISPLAY ' HORA     : ' URL-HORA.

       3400-PRINT-DATA-NO-ENCONTRADO.
           DISPLAY ':::::::::::::::::::::::::::::::::::::::::::::::::::'
                   ':::::::::::'.
           DISPLAY ' URL      : ' URL-URL.
           DISPLAY ' PAIS     : DESCONOCIDO'.
           DISPLAY ' USUARIO  : ' URL-USUARIO.
           DISPLAY ' HORA     : ' URL-HORA.
       END PROGRAM PGMURL01.
