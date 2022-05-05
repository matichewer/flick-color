import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            onClick={this.props.origenSeleccionado && (() => this.props.origenSeleccionado([i, j]))}
                            className={ this.props.origen && 
                                        i === this.props.origen[0] &&
                                        j === this.props.origen[1] ? "celdaOrigen" : undefined}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;