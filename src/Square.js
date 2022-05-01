import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {

    constructor(props) {
        super(props);
        this.state = {text: ""};
        this.state = {
            value: null,
        };
    }
    render() {
        return (
            <div style={{ backgroundColor: colorToCss(this.props.value) }} >
                <button class="miBoton"
                    onClick={() => {
                        this.setState({text:"X"})
                        console.log('color: ' + this.props.value)
                    } 
                }>{this.state.text}
                </button>
            </div>
        );
    }
}

export default Square;