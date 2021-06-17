PROGRAMA: PGMDBL01 ; ALU0001        
JCL : JCLDB201        
ARCHIVO SALIDA: ALU0001.CURSOS.PRINT   
*Adjunto se encuentra el archivo PNG Listado, el cual es una muestra del listado imprimido por el programa*     
Se consideró como datos relevantes del cliente a los campos : NRO CLIENTE; TIPO DOC; NRO DOC; NOMBRE ; APELLIDO Y FECHA DE ALTA.        
  
Especificaciones:
Programa Listador de todos los clientes encontrados en la base de datos DB2 de CLIENTES y sus respectivas cuentas 
en base de datos DB2 de CUENTAS.
Al final del programa muestra los totales de control de:          
  #Cantidad Clientes encontrados.     
  #Cantidad Cuentas encontradas.        
  #Cantidad Cuentas Grabadas.       
  #Cantidad Clientes Grabados.      
  #Cantidad Clientes Sin Cuentas.     
      
Tablas: DB2 CLIENTES: ITPFBIO.TBCLIENT      
        DB2 CUENTAS: ITPFBIO.TBCUENTAS
        
Descripcion del Programa:
Solo se leen las filas de la tabla cuyo número de cliente corresponda al ALU0001.   
Para ello, se tomo el número de ALU desde la SYSIN con la sentencia ACCEPT.   
Se armo un CURSOR que trajo todos los clientes que fueron dados de alta por dicho ALU y con esta selección   
se imprimio en el listado de salida; agregando los datos de las cuentas asociadas al cliente.  

Por cada FILA del CURSOR definido para ITPFBIO.TBCLIENT; se armo un CURSOR para la tabla ITPFBIO.TBCUENTAS 
para traer todas las cuentas asociadas al cliente correspondiente y poder listarlas a continuación 
de la fila del CLIENTE.
Si existiera algún cliente sin cuentas se especifica en el listado con la leyenda “Cliente sin
Cuentas” (CURSOR VACÍO).

Cantidad de líneas por página 60.
Tener presente que como es un programa listador; el archivo de SALIDA tendrá 1 byte
menos de largo que el archivo que se declarará en el JCL correspondiente. Esto se debe a que habrá 
WRITE,AFTER, y por ello se agrega un byte más en forma automática.
O sea que ; si el registro del archivo de salida tiene 132 bytes; en el JCL se lo declaro como de 133 bytes.
