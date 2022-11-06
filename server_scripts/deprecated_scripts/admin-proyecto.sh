#!/bin/bash


# Colores comunes
ROJO='\033[0;31m'
NARANJA='\033[0;33m'
VERDE='\033[0;32m'
AMARILLO='\033[0;93m'

# Mismos colores pero en negrita
NROJO='\033[1;31m'
NNARANJA='\033[1;33m'
NVERDE='\033[1;32m'
NAMARILLO='\033[1;93m'

# Seteo colores
NC='\033[0m' # No Color


uso() {
    echo "Uso: ./admin-proyecto.sh [iniciar|detener|reiniciar|estado|actualizar]"
    exit 0
}

 # Chequea si el servidor de prolog esta en ejecucion
 #   (recordar que el servicio de React es el servicio padre,
 #    asique si esta el hijo tambien esta el padre)
esta_en_ejecucion(){
    ACTIVO=$(sudo systemctl is-active flick-color-prolog)   
    if [[ ${ACTIVO} == 'active' ]]; then
        return 1
    else
        return 0
    fi
}

# Si no esta en ejecucion, lo ejecuta
iniciar(){
    echo -e
    esta_en_ejecucion
    if [ $? -eq "1" ]; then
        echo -e "${NROJO}[ERROR]${NC} Ya estaba en ejecucion."
    else
        echo -e "${NNARANJA}[STATUS]${NC} Intentando ejecutar..."
        sleep 2
        sudo systemctl start flick-color-react
        sleep 2
        echo -e "${NVERDE}[OK]${NC} Ejecucion exitosa."
        echo -e
        sleep 3 # para que quede mas lindo
        echo -e "${NNARANJA}[STATUS]${NC} Cargando base de datos..."
        sleep 5 # para que se termine de ejecutar y poder cargar la bdd
        # Ruta del backup
        PATH_DB="${HOME}/Git/flick-color/pengines_server/apps/proylcc/backup.txt"
        # Guardo el contenido del backup en una variable
        DB=$(<${PATH_DB})
        # Ejecuto el backup en el servidor de Prolog
        /usr/bin/tmux send-keys -t flick-color-prolog "${DB}" Enter
        sleep 2
        echo -e "${NVERDE}[OK]${NC} Base de datos cargada."
    fi 
}

# Si esta en ejecucion, lo detiene 
detener(){
    echo -e
    esta_en_ejecucion
    if [ $? -eq "0" ]; then
        echo -e "${NROJO}[ERROR]${NC} No estaba en ejecucion. Nada para detener."
    else
        echo -e "${NNARANJA}[STATUS]${NC} Haciendo backup..."
        source ${HOME}/Git/flick-color/db_save.sh
        sleep 6 # espero unos segundos para asegurar que el backup finalice
        echo -e "${NVERDE}[OK]${NC} Backup exitoso."
        echo -e
        sleep 2
        echo -e "${NNARANJA}[STATUS]${NC} Deteniendo..."
        sleep 2
        sudo systemctl stop flick-color-react
        sleep 1
        echo -e "${NVERDE}[OK]${NC} Detenido exitosamente."
    fi  
}

# Muestra por consola si esta ejecutandose
estado(){
    echo -e
    esta_en_ejecucion
    if [ $? -eq "1" ]; then
        echo -e "${NNARANJA}[STATUS]${NC} En ejecucion."
    else
        echo -e "${NNARANJA}[STATUS]${NC} No se encuentra en ejecucion."
    fi
    #echo
    #sudo systemctl status flick-color-react
	#sudo systemctl status flick-color-prolog
    #echo
}

reiniciar(){
    echo
    echo -e "${NNARANJA}[STATUS]${NC} Reiniciando..."
	detener
    sleep 5 # para que termine de detenerse
	iniciar
    echo -e "${NVERDE}[OK]${NC} Reinicio exitoso."
}

actualizar(){
  detener
  echo -e
  echo -e "${NNARANJA}[STATUS]${NC} Actualizando..."
  git pull
  echo -e
  sleep 1
  echo -e "${NVERDE}[OK]${NC} Actualizado exitosamente."
  iniciar
}

# Segun el parametro ingresado, ejecuta la funcion correspondiente
case "$1" in
  "iniciar")
    iniciar
    ;;
  "detener")
    detener
    ;;
  "estado")
    estado
    ;;
  "reiniciar")
    reiniciar
    ;;
  "actualizar")
    actualizar
    ;;
  *)
    uso # en caso de que el parametro no sea ninguno de los anteriores
    ;;
esac

