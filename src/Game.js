import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';

// ES6 Modules or TypeScript
//import Swal from 'sweetalert2'

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
    };
    this.handleClick = this.handleClick.bind(this);
    this.origenSeleccionado = this.origenSeleccionado.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
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
  

    // ESTO DA WARNING: PREGUNTAR
    //  this.state.origen = this.state.origen ? this.state.origen : [0,0];
    /*
    if (!this.state.origen){
        this.setState({
          origen: [0,0]
        })
    }
    */


    if (this.state.listaCapturados.length === 0) {
      //if (this.state.origen){
        //this.state.listaCapturados.push(JSON.stringify(this.state.origen).replaceAll('"', ""));
      //}
      //else{
        //this.state.listaCapturados.push([0,0]);

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

      //}
    }

    else{
    const queryS = "flick(" + gridS + "," + fila + "," + columna + "," + color + ",Grid," + JSON.stringify(this.state.listaCapturados).replaceAll('"', "") + ",NuevaListaCapturados,CantCapturados)";
    //console.log(queryS);
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

            Swal.fire({
              title: "¡Felicitaciones!",
              text: "Ganaste con " + this.state.turns + " turnos",
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

handleChange(event){this.setState({estrategia:event.target.value});}

  render() {
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
              <input className="profundidadNum" type='number' min= "0" max="30" value={this.state.estrategia} onChange={this.handleChange}/>
              <button className='BotonAyuda' onClick={() => this.handleHelp()}>Ayuda</button>
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
      </div>
    );
  }
}

export default Game;
