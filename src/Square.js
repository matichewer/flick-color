import React from 'react';
import { colorToCss } from './Game';
//import {useState} from 'react';

class Square extends React.Component {

    constructor(props) {
        super(props);
        this.state = {
            value: null,
            botonActivo: true
        };        
    }
    render() {
        return (
            <div style={{ backgroundColor: colorToCss(this.props.value) }} >
                <button class="miBoton" disabled={!this.state.botonActivo} 
                    onClick={() => {
                        if (!this.props.botonActivo) {
                            this.setState({botonActivo: false})
                            this.setState({value:"X"})
                            console.log('color: ' + this.props.value)
                        }
                    } 
                }>
                    
                    {this.state.value}
                </button>
            </div>
        );


    }
}
export default Square;
