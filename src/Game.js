import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';

// CommonJS
const Swal = require('sweetalert2')


/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "red";
    case "v": return "violet";
    case "p": return "pink";
    case "g": return "green";
    case "b": return "blue";
    case "y": return "yellow";
    default: return color;
  }
}
class Game extends React.Component {

  pengine;
  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      cantidadDeCapturados: 0,
      historial: [], // historial de colores
      origen: undefined, // celda de origen
      listaCapturados: [],
      ayudaSecuenciaColores: [],
      ayudaCapturados: 0,
      nombreJugador: undefined,
      records: [], // tabla de puntuaciones
    };
    this.handleClick = this.handleClick.bind(this);
    this.origenSeleccionado = this.origenSeleccionado.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);

    this.getNombre();
  }

  handlePengineCreate() {
    this.getRecords();
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");

    // si el usuario no elige celda de origen, entonces por defecto es la [0,0]
    const fila = this.state.origen ? this.state.origen[0] : 0;
    const columna = this.state.origen ? this.state.origen[1] : 0;
  


    if (this.state.listaCapturados.length === 0) {

      this.setState({
        origen: [0,0]
      })
      const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
      const querySInit = "iniciarConOrigenDefault(" + gridS + "," +color + ",NewGrid,NuevaListaCapturados,CantCapturados)";
      this.setState({
          waiting: true
      });
      this.pengine.query(querySInit, (success, response) => {   
          if (success) {
              this.state.historial.push(color)
              this.setState({
                  grid: response['NewGrid'],
                  turns: this.state.turns + 1,
                  waiting: false,
                  cantidadDeCapturados: response['CantCapturados'],
                  complete: response['CantCapturados']===196, // complete es Verdadero si gano
                  listaCapturados: response['NuevaListaCapturados'],
              });            
          } else {
                this.setState({
                    waiting: false
                });
          }
      });
    }

    else{
        const queryS = "flick(" + gridS + "," + fila + "," + columna + "," + color + ",Grid," + JSON.stringify(this.state.listaCapturados).replaceAll('"', "") + ",NuevaListaCapturados,CantCapturados)";
        this.setState({
          waiting: true
        });
        this.pengine.query(queryS, (success, response) => {      
          if (success) {
              this.state.historial.push(color)
              this.setState({
                  grid: response['Grid'],
                  turns: this.state.turns + 1,
                  waiting: false,
                  cantidadDeCapturados: response['CantCapturados'],
                  complete: response['CantCapturados']===196, // complete es Verdadero si gano
                  listaCapturados: response['NuevaListaCapturados'],
              });

              // si ganamos mostramos un aviso
              if(this.state.complete){  
                  this.registrarRecord(); 
                  Swal.fire({
                    title: "¡Felicitaciones!",
                    text: "Ganaste con " + this.state.turns + " turnos.",
                    icon: "success",
                  }).then(() => {
                      window.location.reload()      
                  });
              }        
          } else {
            // Prolog query will fail when the clicked color coincides with that in the top left cell.
            this.setState({
              waiting: false
            });
          }
        });
      }
    }
  
  // funcion del origen
  origenSeleccionado(pos){
      this.setState({
          origen: pos
      })

      const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
      const fila = pos[0];
      const columna = pos[1];
      const queryS = "iniciarConOrigenSeleccionado(" + gridS + "," + fila + "," + columna + ",Color,NuevaListaCapturados,CantCapturados)";
      this.setState({
          waiting: true
      });
      this.pengine.query(queryS, (success, response) => {   
          if (success) {
              this.state.historial.push(response['Color'])
              this.setState({
                  turns: this.state.turns + 1,
                  waiting: false,
                  cantidadDeCapturados: response['CantCapturados'],
                  complete: response['CantCapturados']===196, // complete es Verdadero si gano
                  listaCapturados: response['NuevaListaCapturados'],
              });            
          } else {
                this.setState({
                    waiting: false
                });
          }
      });
  }

// boton ayuda
handleHelp(){
    if (!this.state.origen){
        Swal.fire({
          title: "Error",
          icon: "error",
          text: "Primero seleccione un origen para empezar a jugar.",
          showConfirmButton: false,
          timer: 2000
        })
    }
    else{
        var profundidad = parseInt(document.getElementById("profundidad").value);
        if (profundidad<1 || profundidad>7){
            Swal.fire({
              position: 'center',
              icon: 'error',
              title: 'Ingrese un número entre 1 y 7',
              showConfirmButton: false,
              timer: 2000
            })
            return;
        }       
            Swal.fire({
              position: 'center',
              title: 'Calculando la mejor estrategia...',
              icon: 'info',
              showConfirmButton: false,
            })

        const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
        const origen = JSON.stringify(this.state.origen).replaceAll('"', "");
        const capturados = JSON.stringify(this.state.listaCapturados).replaceAll('"', "");        
        const queryS = "botonAyuda(" + gridS + ","+ origen+","+ capturados+","+ profundidad+", SecuenciaColores, NewCantidadAdyacentes)";
        this.setState({
            waiting: true
        });
        this.pengine.query(queryS, (success, response) => {   
            if (success) {
                this.setState({
                    waiting: false,
                    ayudaSecuenciaColores: response['SecuenciaColores'],
                    ayudaCapturados: response['NewCantidadAdyacentes'],
                }); 
              Swal.close()                
              Swal.fire({
                position: 'center',
                icon: 'success',
                title: '¡Mejor estrategia encontrada!',
                showConfirmButton: false,
                timer: 2000
              })
            } else {
                  this.setState({
                      waiting: false
                  });
            }
        });
    }
  } 


  registrarRecord(){    
    const queryRecord = "newRecord(" + this.state.nombreJugador + "," + this.state.turns +", AllRecords)."
    console.log(queryRecord)
    this.setState({
        waiting: true
    });
    this.pengine.query(queryRecord, (success, response) => {    
      if (success) {
          this.setState({
            waiting: false,
            records: response['AllRecords'],
        });
      }  });
    console.log("Records obtenidos en la funcion: "+ this.state.records)
  }

  getRecords(){         
    const queryS = "getRecords(Records)";
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {               
        if (success) {
          this.setState({
              records: response['Records'],
              waiting: false,
        })
      }
    })
    console.log("los records en la función: " + this.state.records);
  }

  async getNombre(){
    const { value: nombre } = await Swal.fire({
      title: '¡Bienvenido a Flick Color!',
      input: 'text',
      inputLabel: 'Ingrese su nombre para poder registrar su puntuación',
    })
    this.setState({
        nombreJugador: nombre
    }); 
  }


  render() {

    let records = this.state.records;


    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
         <div className="panelControl">
            <div className="leftPanel">
              <div className="buttonsPanel">
                {colors.map(color =>
                  <button
                    className="colorBtn"
                    style={{ backgroundColor: colorToCss(color) }}
                    onClick={() => this.handleClick(color)}
                    key={color}
                  />)}
              </div>
              <div className="turnsPanel">
                <div className="turnsLab">Turnos</div>
                <div className="turnsNum">{this.state.turns}</div>
              </div>
            </div>
            <div className="capturados">
              <div className="capturadosLab">Cantidad de capturados</div>
              <div className="capturadosNum">{this.state.cantidadDeCapturados}</div>
            </div> 
            <div className="estrategia">
              <div className="profundidadLab">Estrategia</div>
              <input className="profundidadNum" type='number' id="profundidad" min= "1" max="7" defaultValue="1"/> 
              <button className='BotonAyuda' onClick={() => this.handleHelp()}>Ayuda</button>
              <div className="capturadosEstrategia">{this.state.ayudaCapturados}</div>
              <div className="ayudaLab">Mejor estrategia</div>    
                  <div className="stateAyuda">{this.state.ayudaSecuenciaColores.map((colorS,i)=>
                      <Square
                        className={"squaresAyuda"}
                        value={colorS}
                        key={i}                    
                      />)}
                  </div>
            </div> 
        </div>
        <Board 
            grid={this.state.grid} 
            origen={this.state.origen}
            origenSeleccionado={!this.state.origen ? this.origenSeleccionado : undefined}   
        />
        <div className="historial">
          <div className="historialLab">Historial</div>    
              <div className="stateHistorial">{this.state.historial.map((colorS,i)=>
                  <Square
                    className={"squaresHistorial"}
                    value={colorS}
                    key={i}                    
                  />)}
              </div>
        </div>



        <div className="records">
            <table>
              <thead>
                <tr>
                  {
                    <th>Nick - Turnos</th>
                  }
                </tr>
              </thead>
              <tbody>
                {records.slice(0, records.length).map((item, index) => {
                  return (
                    <tr>
                      <td>{item[0]}</td>
                      <td>{item[1]}</td>
                      <td>{item[2]}</td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
        </div>



      </div>
    );
  }
}

export default Game;
