      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      *2)    Complete la descripción en la  DATA DIVISION
      * para el siguiente registro de la WORKING STORAGE SECTION
      * de tal manera que corresponda con la siguiente conformación
      * de registro:
      *  a.1-16    Blanco
      *    17-25    Total
      *    26-35    Blanco
      *    36-45    Salario Total Hombres. Se muestra con punto decimal
      *    y 2 lugares decimales
      *    46-50    Blanco
      *    51-60    Salario Total Mujeres. Se muestra con punto decimal
      *    y 2 lugares decimales

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO2A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *------------------------
       01  WS-ESTRUCTURA.
           05 FILLER             PIC X(16)    VALUE ALL SPACES.
           05 TOTAL              PIC 9(09)    VALUE 0.
           05 FILLER             PIC X(09)    VALUE ALL SPACES.
           05 SALARIO-TOTAL-HOM  PIC 9(09)V99 VALUE 0.
           05 FILLER             PIC X(04)    VALUE ALL SPACES.
           05 SALARIO-TOTAL-MUJ  PIC 9(09)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM EJERCICIO2A.
