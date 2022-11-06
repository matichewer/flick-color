#!/bin/bash

# Colores comunes
ROJO='\033[0;31m'
NARANJA='\033[0;33m'
VERDE='\033[0;32m'
AMARILLO='\033[0;93m'

NEGRITA='\e[1m'
NC='\033[0m' # No Color

CHECK='\xE2\x9C\x94'
RED_X='\u274c'

react_en_ejecucion(){
    # TMUX_REACT OUTPUT:
    #   0 = la sesion de tmux existe
    #   1 = la sesion de tmux no existe
    TMUX_REACT=$(tmux has-session -t flick-color-react 2>/dev/null && echo 0 || echo 1)

    # SYSTEMD_REACT OUTPUT:
    #   inactive = el servicio esta inactivo
    #   active = el servicio esta activo
    SYSTEMD_REACT=$(sudo systemctl is-active flick-color-react)   

    if [[ ${SYSTEMD_REACT} == 'active' && ${TMUX_REACT} == "0" ]]; then
      return 0 # el servidor de react esta activo
    else
      return 1  # el servidor de react esta inactivo o tiene algun problema
    fi
}

 # recordar que el servicio de React es el servicio padre,
 # asique si el hijo (Prolog) esta en ejecucion, entonces tambien esta el padre (React)
prolog_en_ejecucion(){
    # TMUX_PROLOG OUTPUT:
    #   0 = la sesion de tmux existe
    #   1 = la sesion de tmux no existe   
    TMUX_PROLOG=$(tmux has-session -t flick-color-prolog 2>/dev/null && echo 0 || echo 1)

    # SYSTEMD_PROLOG OUTPUT:
    #   inactive = el servicio esta inactivo
    #   active = el servicio esta activo
    SYSTEMD_PROLOG=$(sudo systemctl is-active flick-color-prolog)  

    if [[ ${SYSTEMD_PROLOG} == 'active' && ${TMUX_PROLOG} == "0" ]]; then
      return 0 # el servidor de prolog esta activo
    else
      return 1 # el servidor de prolog esta inactivo o tiene algun problema
    fi
}

mostrar_backup(){
    bat -l pl ${HOME}/Git/flick-color/pengines_server/apps/proylcc/backup.txt
}

realizar_backup(){  
    echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Haciendo backup..."
    source ${HOME}/Git/flick-color/server_scripts/db_save.sh
    sleep 6 # por las dudas para que finalice correctamente
    echo -e "${VERDE}${NEGRITA}[OK]${NC} Backup realizado exitosamente"
    mostrar_backup
}
 
cargar_backup(){
    echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Cargando base de datos..."
    source ${HOME}/Git/flick-color/server_scripts/db_load.sh
    sleep 3
    echo -e "${VERDE}${NEGRITA}[OK]${NC} Base de datos cargada."
    mostrar_backup
}

# Si no esta en ejecucion, lo ejecuta
iniciar(){
    if prolog_en_ejecucion; then
        echo -e "${ROJO}${NEGRITA}[ERROR]${NC} Ya estaba en ejecucion."
    else
        echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Intentando ejecutar ambos servidores..."
        sleep 1
        sudo systemctl start flick-color-react
        sleep 8
        if react_en_ejecucion && prolog_en_ejecucion; then
            echo -e "${VERDE}${NEGRITA}[OK]${NC} Ejecucion exitosa."
        else
            echo -e "${ROJO}${NEGRITA}[ERROR]${NC} Hubo un problema intentando ejecutar levantar los servidores"
            exit 1
        fi
        sleep 1 # para que quede mas lindo
        echo -e
        cargar_backup
    fi 
}

# Si esta en ejecucion, lo detiene 
detener(){
    if ! prolog_en_ejecucion; then
        echo -e "${ROJO}${NEGRITA}[ERROR]${NC} No estaba en ejecucion. Nada para detener."
    else
        realizar_backup
        sleep 1 # para que quede mas lindo
        echo -e
        echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Deteniendo..."
        sleep 1
        sudo systemctl stop flick-color-react
        sleep 1
        echo -e "${VERDE}${NEGRITA}[OK]${NC} Detenido exitosamente"
    fi  
}

reiniciar(){
    echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Reiniciando..."
    detener
    sleep 5 # para que termine de detenerse
    echo -e
    iniciar
    echo -e "${VERDE}${NEGRITA}[OK]${NC} Reiniciado exitosamente."
}

actualizar(){   
    detener
    sleep 5 # para que termine de detenerse
    echo -e "${NARANJA}${NEGRITA}[STATUS]${NC} Actualizando..."
    git pull
    echo -e "${VERDE}${NEGRITA}[OK]${NC} Actualizado exitosamente."
    iniciar
}

registrar_nuevo_record(){
    if ! prolog_en_ejecucion; then
        echo -e "${ROJO}${NEGRITA}[ERROR]${NC} El servidor debe estar en ejecucion para registrar un nuevo record"
    else
        echo -e "Ingrese el nombre que desea registrar (sin comillas): "
        read nombre
        echo -e "Ingrese la puntuacion: "
        read puntuacion
        /usr/bin/tmux send-keys -t flick-color-prolog "proylcc:newRecord(\"${nombre}\",${puntuacion},_)." Enter
        echo -e "${VERDE}${NEGRITA}[OK]${NC} Puntuacion registrada exitosamente."
        echo -e 
        echo -e "${NARANJA}CONSEJO:${NC}"
        echo -e "   Puede ver el estado del servidor ejecutando lo siguiente en la consola: "
        echo -e "       tmux attach-session -t flick-color-prolog"
        echo -e "   Una vez dentro del servidor, para salir debe presionar: Ctrl B y luego la D"
    fi
}

borrar_record(){
    if ! prolog_en_ejecucion; then
        echo -e "${ROJO}${NEGRITA}[ERROR]${NC} El servidor debe estar en ejecucion para registrar un nuevo record"
    else
        echo -e "Ingrese el nombre que desea borrar (sin comillas): "
        read nombre
        /usr/bin/tmux send-keys -t flick-color-prolog "proylcc:borrarRecord(\"${nombre}\",_)." Enter
        echo -e "${VERDE}${NEGRITA}[OK]${NC} Puntuacion borrada exitosamente."
        echo -e 
        echo -e "${NARANJA}CONSEJO:${NC}"
        echo -e "   Puede ver el estado del servidor ejecutando lo siguien te en la consola: "
        echo -e "       tmux attach-session -t flick-color-prolog"
        echo -e "   Una vez dentro del servidor, para salir debe presionar: Ctrl B y luego la D"
    fi
}

echo -e
echo -e "${NEGRITA}STATUS DE SERVIDORES:${NC}"
if $(react_en_ejecucion); then
    echo -e "   React  -> en ejecucion ${VERDE}${CHECK}${NC}"
else
    echo -e "   React  -> detenido ${RED_X}"
fi

if $(prolog_en_ejecucion); then
    echo -e "   Prolog -> en ejecucion ${VERDE}${CHECK}${NC}"
else
    echo -e "   Prolog -> detenido ${RED_X}"
fi

#echo -e "Status React: $(react_en_ejecucion)"
#echo -e "Status Prolog: $(prolog_en_ejecucion)"
echo -e
echo -e "Selecciona una opcion:"
#echo -e
echo -e "   ${NARANJA}${NEGRITA}1.${NC} Iniciar ambos servidores (inicia, carga backup)"
echo -e "   ${NARANJA}${NEGRITA}2.${NC} Detener ambos servidores (hace backup, detiene)"
echo -e "   ${NARANJA}${NEGRITA}3.${NC} Reiniciar (hace backup, detiene, inicia, carga backup)"
echo -e "   ${NARANJA}${NEGRITA}4.${NC} Actualizar (reinicia pero entre medio actualiza desde git)"
echo -e "   ${NARANJA}${NEGRITA}5.${NC} Mostrar backup actual"
echo -e "   ${NARANJA}${NEGRITA}6.${NC} Realizar backup"
echo -e "   ${NARANJA}${NEGRITA}7.${NC} Cargar backup al servidor"
echo -e "   ${NARANJA}${NEGRITA}8.${NC} Registrar un nuevo record"
echo -e "   ${NARANJA}${NEGRITA}9.${NC} Borrar un record"

read opcion
echo
case $opcion in
    1)
        iniciar
    ;;
    2)
        detener
    ;;
    3)
        reiniciar
    ;;
    4)
        actualizar
    ;;
    5)
        mostrar_backup
    ;;
    6)
        realizar_backup
    ;;
    7)
        cargar_backup
    ;;
    8)
        registrar_nuevo_record
    ;;
    9)
        borrar_record
    ;;
    *)
  	    echo -e "${ROJO}${NEGRITA}[ERROR]${NC} Opcion incorrecta. Saliendo...${NC}"
        exit 1
    ;;
esac
