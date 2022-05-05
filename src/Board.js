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
                            //onClick={() => this.props.onOriginSelected && this.props.onOriginSelected([i,j])}
                            onClick={this.props.onOrigenSelected && (() => this.props.onOrigenSelected([i, j]))}

                            //celdaOrigen={this.props.origin[0] === i && this.props.origin[1] === j}
                            className={this.props.origen && i === this.props.origen[0] && j === this.props.origen[1] ? "celdaOrigen" : undefined}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;