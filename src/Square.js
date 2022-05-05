import React from 'react';
import { colorToCss } from './Game';
import "./index.css";

class Square extends React.Component {
      render() {
        return (
            <div style={{ backgroundColor: colorToCss(this.props.value) }} 
                onClick={this.props.onClick}
                className={this.props.className}
            />
        );
    }
}
export default Square;
