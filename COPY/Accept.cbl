      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGACCEPT.
      ******************************************************************
      *Utiliza los verbos ACEPTAR y MOSTRAR para aceptar un registro de
      *estudiante del usuario y mostrar algunos de los campos.También
      *muestra cómo se puede utilizar ACCEPT para obtener la fecha y
      *hora del sistema.
      *El AAAAMMDD en "ACEPTAR CurrentDate FROM DATE AAAAMMDD".
      *Es un comando de formato que asegura que la fecha contenga un
      *año de 4 dígitos. Si no se utiliza, el año proporcionado por el
      *sistema .
      *Solo contendrá dos dígitos que pueden causar un problema en el
      *año 2000.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ALUMNO.
           02 ALUMNO-ID        PIC 9(7).
           02 ALUMNO-NOMBRE.
              03 APELLIDO      PIC X(8).
              03 INICIALES     PIC XX.
           02 COD-CURSO        PIC X(4).
           02 GENERO           PIC X.

      *     YYMMDD *
       01  CURRENT-DATE.
           02 CURRENT-AÑO      PIC 9(4).
           02 CURRENT-MES      PIC 99.
           02 CURRENT-DIA      PIC 99.

      *     YYDDD  *
       01  DIA-DEL-AÑO.
           02 FILLER           PIC 9(4).
           02 DIA-AÑO          PIC 9(3).


      * HHMMSSss   s = S/100
       01  CURRENT-TIME.
           02 CURRENT-HORA     PIC 99.
           02 CURRENT-MINUTOS  PIC 99.
           02 FILLER           PIC 9(4).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY 'Ingrese los datos del alumno'.
           DISPLAY 'Entrar - ID, apellido, iniciales, codigo de curso,'
                   'genero: '.
           DISPLAY '************************************'.
           ACCEPT  ALUMNO.
           ACCEPT  CURRENT-DATE FROM DATE YYYYMMDD.
           ACCEPT  DIA-DEL-AÑO  FROM DAY YYYYDDD.
           ACCEPT  CURRENT-TIME FROM TIME.
           DISPLAY 'El nombre es : ' INICIALES SPACE APELLIDO.
           DISPLAY 'El numero id alumno: ' ALUMNO-ID.
           DISPLAY 'La fecha  es : '  SPACE CURRENT-MES
                                      SPACE CURRENT-AÑO.
           DISPLAY 'Hoy es el dia ' DIA-AÑO ' del ano'.
           DISPLAY 'El tiempo es ' CURRENT-HORA ':' CURRENT-MINUTOS.
           DISPLAY ' '.
           STOP RUN.
       END PROGRAM PGACCEPT.
